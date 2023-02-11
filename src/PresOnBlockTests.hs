{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module PresOnBlockTests where

import PresOnBlock
import Plutus.Trace
import Wallet.Emulator.Wallet
import Ledger.TimeSlot
import Ledger.Value
import Data.Default (Default (..))
import Data.Functor (void)
import Control.Monad.Freer.Extras as Extras 
import qualified Plutus.Contract as Emulator
import PlutusTx.Builtins.Class (stringToBuiltinByteString)


docWallet     = knownWallet 1
patientWallet = knownWallet 2

mintPrescriptionPass :: EmulatorTrace ()
mintPrescriptionPass = do
    let tN = TokenName $ stringToBuiltinByteString "Dummyprescriptionhash"
    hDoctor <- activateContractWallet (knownWallet 1) pobEndpoints
    callEndpoint @"mintPrescription" hDoctor $ PH tN
    s <- waitNSlots 1
    Extras.logInfo $ "Prescription made " ++ show s 
    --return (hDoctor)

--happyPath :: EmulatorTrace ()
--happyPath = do 
--    hDoctor  <- activateContractWallet docWallet prescribeEndpoints
--    hPatient <- activateContractWallet patientWallet prescribeEndpoints
--    hPharma  <- activateContractWallet (knownWallet 3) prescribeEndpoints
--    let tN = TokenName $ stringToBuiltinByteString "Dummyprescriptionhash"
--        dpkh = mockWalletPaymentPubKeyHash docWallet
--        cS = doctorID dpkh  
--    callEndpoint @"prescribe" hDoctor PD
--        { patientID = mockWalletPaymentPubKeyHash patientWallet
--        , prescriptionID = AssetClass (cS, tN)
--        , expiration = slotToBeginPOSIXTime def 600
--        }
--    void $ waitUntilSlot 120
--    callEndpoint @"collectMeds" hPatient ()
--    s <- waitNSlots 2
--    Extras.logInfo $ "end of transaction " ++ show s

testCurrencySymbol :: CurrencySymbol
testCurrencySymbol = doctorID $ mockWalletPaymentPubKeyHash docWallet

testTokenName :: TokenName
testTokenName = TokenName $ stringToBuiltinByteString "DummyprescriptionHash"

testPrescriptionID = AssetClass (testCurrencySymbol, testTokenName)

--test1 = runEmulatorTraceIO mintPrescriptionPass 

fullTest :: EmulatorTrace ()
fullTest = do 
    hDoctor <- activateContractWallet docWallet pobEndpoints
    hPatient <- activateContractWallet patientWallet pobEndpoints
    let tN = TokenName $ stringToBuiltinByteString "Dummyprescriptionhash"
        dpkh = mockWalletPaymentPubKeyHash docWallet
        cS = doctorID dpkh  
    callEndpoint @"mintPrescription" hDoctor $ PH tN
    void $ waitNSlots 1
    callEndpoint @"prescribe" hDoctor PD
        { patientID = mockWalletPaymentPubKeyHash patientWallet
        , prescriptionID = AssetClass (cS, tN)
        , expiration = slotToBeginPOSIXTime def 600
        }
    void $ waitNSlots 11
    callEndpoint @"collectMeds" hPatient ()
    s3 <- waitNSlots 2 
    Extras.logInfo $ "Prescription made " ++ show s3

test2 = runEmulatorTraceIO fullTest
--testFlow = runEmulatorTraceIO (happyPath >> mintPrescriptionPass)





