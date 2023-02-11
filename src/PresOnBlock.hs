{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module PresOnBlock where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON, Value (String))
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada as Ada
import Ledger.Value as Value 
import Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Prelude (IO, Semigroup (..), Show (..), String)
import qualified Prelude
import Text.Printf (printf)
import Wallet.Emulator.Wallet 


-- Onchain Validator for prescription

data PresDatum = PD 
    { patientID          :: PaymentPubKeyHash
    , prescriptionID     :: AssetClass -- Currency Symbol is Doctor ID and Token name is Prescription Hash
    , expiration         :: POSIXTime   
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''PresDatum

{-# INLINABLE mkPresValidator #-}
mkPresValidator :: PresDatum -> () -> ScriptContext -> Bool
mkPresValidator pD pR ctx =
    traceIfFalse "Patient's signature missing" signedByPatient &&
    traceIfFalse "Prescription has expired"  notExpired 
    where 
        txInfo = scriptContextTxInfo ctx
        signedByPatient = txSignedBy txInfo $ unPaymentPubKeyHash $ patientID pD
        notExpired = to (expiration pD) `contains` txInfoValidRange txInfo

data Prescription
instance Scripts.ValidatorTypes Prescription where
    type instance DatumType Prescription = PresDatum
    type instance RedeemerType Prescription = ()

presValidatorTyped :: Scripts.TypedValidator Prescription
presValidatorTyped = Scripts.mkTypedValidator @Prescription   
    $$(PlutusTx.compile [|| mkPresValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PresDatum @() 

prescriptValidator :: Validator
prescriptValidator = Scripts.validatorScript presValidatorTyped

prescriptAddress :: Address
prescriptAddress = scriptAddress prescriptValidator

-- Onchain minting policy for prescription ID.
-- Doctor's ID in the Currency symbol and the prescription hash as the TokenName.

{-# INLINABLE mkPrescriptionID #-}
mkPrescriptionID :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPrescriptionID doctorPKH () ctx = 
    traceIfFalse "Doctor signature missing " signedByDoctor &&
    traceIfFalse  "Only 1 prescriptionID can be minted " singlePrescription
    where
        info = scriptContextTxInfo ctx
        signedByDoctor = txSignedBy info $ unPaymentPubKeyHash doctorPKH 
        singlePrescription = case flattenValue (txInfoMint info) of
            [(_, _, pres)]  -> pres == 1
            _               -> False  


prescriptionIDPolicy :: PaymentPubKeyHash -> Scripts.MintingPolicy
prescriptionIDPolicy doctorPKH = 
    mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPrescriptionID ||])
    `PlutusTx.applyCode` PlutusTx.liftCode doctorPKH 

doctorID :: PaymentPubKeyHash -> CurrencySymbol
doctorID = scriptCurrencySymbol . prescriptionIDPolicy

--- Offchain code
{-}
data PrescribeParams = PP  -- this is essentially the same as the Prescription Datum
    { patient    :: PaymentPubKeyHash
    , presID     :: AssetClass
    , expi       :: POSIXTime
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
-}

data PrescriptionHash = PH 
    { getTokenName :: TokenName } deriving (Generic, ToJSON, FromJSON, ToSchema)

--type PrescriptionMintingSchema = Endpoint "mintPrescription" PrescriptionHash

--mintPrescription :: PrescriptionHash -> Contract w PrescriptionMintingSchema Text ()
mintPrescription :: AsContractError e => PrescriptionHash -> Contract w s e ()
mintPrescription (PH tn) = do
    pkh <- Contract.ownPaymentPubKeyHash 
    let val = Value.singleton (doctorID pkh) (tn) 1
        lookups = Constraints.mintingPolicy $ prescriptionIDPolicy pkh
        tx = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx 
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Prescription created %s" (show val) 

--mintPresEndpoints :: Contract () PrescriptionMintingSchema Text ()
--mintPresEndpoints = mintPresEndpoints' >> mintPresEndpoints
--    where
--        mintPresEndpoints' = awaitPromise $ endpoint @"mintPrescription" mintPrescription

-- prescribe and collect contracts

--type PrescriptionSchema = 
--        Endpoint "prescribe" PresDatum 
--    .\/ Endpoint "collectMeds" ()

prescribe :: AsContractError e => PresDatum -> Contract w s e ()
prescribe presDat = do
    let val = assetClassValue (prescriptionID presDat) 1 <> lovelaceValueOf 2000000
        tx = Constraints.mustPayToTheScript presDat val 
    ledgerTx <- submitTxConstraints presValidatorTyped tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Prescription issued"


collectMeds :: forall w s e. AsContractError e => Contract w s e ()
collectMeds = 
    do
        now   <- currentTime
        myPKH <- Contract.ownPaymentPubKeyHash
        utxos <- Map.filter (isForMe myPKH now) <$> utxosAt prescriptAddress
        if Map.null utxos then
            logInfo @String $ "no prescriptions available"
            else do
                let orefs    = fst <$> Map.toList utxos
                    lookups  = Constraints.unspentOutputs utxos <>
                               Constraints.otherScript prescriptValidator 
                    tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                         Constraints.mustValidateIn (interval now (now + 300))
                ledgerTx <- submitTxConstraintsWith @Void lookups tx 
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ printf "Prescription redeemed successfully"
    where   
        isForMe :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
        isForMe myPKH now o = case _ciTxOutDatum o of
            Left _ -> False
            Right (Datum e) -> case  PlutusTx.fromBuiltinData e of
                Nothing -> False
                Just d  -> patientID d == myPKH && expiration d >= now 
        
--prescribeEndpoints :: Contract () PrescriptionSchema Text ()
--prescribeEndpoints = awaitPromise (prescribe' `select` collectMeds') >> prescribeEndpoints
--    where
--        prescribe'   = endpoint @"prescribe" prescribe
--        collectMeds' = endpoint @"collectMeds" $ const collectMeds
--


type POBSchema = Endpoint "mintPrescription" PrescriptionHash
            .\/  Endpoint "prescribe" PresDatum
            .\/  Endpoint "collectMeds" ()

pobEndpoints :: Contract () POBSchema Text ()
pobEndpoints = do  
    awaitPromise $ endpoint @"mintPrescription" mintPrescription
    awaitPromise $ endpoint @"prescribe" prescribe
    awaitPromise $ endpoint @"collectMeds" $ const collectMeds 


--
mkSchemaDefinitions ''POBSchema

mkKnownCurrencies []

{-
trace1 :: EmulatorTrace ()
trace1 = do
    hDoc = activateContractWallet (knownWallet 1) endpoint
    hPatient = activateContractWallet (knownWallet 2)
-}





