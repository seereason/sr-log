{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wno-orphans #-}

module SeeReason.LogOrphans where

import Data.SafeCopy (SafeCopy(version), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc(..))
import GHC.Stack.Types (CallStack(..))
import System.Log.Logger (Priority(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- Moved to sr-utils:Extra.SrcLocOrphans
