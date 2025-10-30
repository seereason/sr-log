{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SeeReason.LogOrphans where

import Data.SafeCopy (SafeCopy(version), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Stack (callStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import GHC.Stack.Types (CallStack(..))
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), prettyShow, text )


deriving instance Generic CallStack
deriving instance Eq CallStack
deriving instance Ord CallStack
deriving instance Serialize CallStack
deriving instance SafeCopy CallStack

#if !MIN_VERSION_base(4,15,0)
deriving instance Generic SrcLoc
#endif
deriving instance Ord SrcLoc
deriving instance Serialize SrcLoc
deriving instance SafeCopy SrcLoc
deriving instance Typeable SrcLoc
instance Pretty SrcLoc where
  pPrint SrcLoc{..} = text (srcLocModule <> ":" <> Prelude.show srcLocStartLine)
