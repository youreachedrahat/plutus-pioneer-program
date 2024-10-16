{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains, from, to)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript, ScriptContext (scriptContextTxInfo),
                                       TxInfo (txInfoValidRange))
import           PlutusTx             (compile, unstableMakeIsData)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx.Prelude     (Bool (..), ($), (&&), (||), (+))

import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = (signedByBeneficiary1 && deadlineNotReached) || (signedByBeneficiary2 && deadlineReached)
        where
            info :: TxInfo
            info = scriptContextTxInfo _ctx

            signedByBeneficiary1 :: Bool
            signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

            signedByBeneficiary2 :: Bool
            signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat


            deadlineReached :: Bool
            deadlineReached = contains (from $ (deadline _dat + 1)) $ txInfoValidRange info

            deadlineNotReached :: Bool
            deadlineNotReached = contains (to $ deadline _dat) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
