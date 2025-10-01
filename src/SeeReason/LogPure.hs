-- | Logging package pure functions

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module SeeReason.LogPure
  ( callSiteOnly
  , logString
  , logStringOld
  ) where

import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Time (diffUTCTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import GHC.Stack (callStack, HasCallStack, SrcLoc(..))
import SeeReason.SrcLoc
import System.Log.Logger (Priority(..))
--import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)
import Text.Printf (printf)

-- | Display the function and module where the logger was called
callSiteOnly = take 2
{-# DEPRECATED callSiteOnly "Use take 2" #-}

logString :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String -> String
logString fn msg =
  pre <> take (lim - len) msg <> suf
  where
    len = length pre + length suf
    pre = compactStack (take 2 locs) <> " - "
    suf = if length locs > 2 then formattedStack locs else ""
    locs :: [(String, SrcLoc)]
    locs = fn $ dropModuleFrames {-LogPure-} $ dropModuleFrames {-SrcLoc-} getStack
    lim =
#if defined(darwin_HOST_OS)
          2002
#else
          60000
#endif

formattedStack :: [(String, SrcLoc)] -> String
formattedStack locs =
  "\n    (" <> intercalate "\n     " stk <> ")"
  where
    stk = fmap (intercalate " > ") (groupsOf 5 (compactLocs (reverse locs)))

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l =
  case splitAt n l of
    ([], []) -> []
    (l', l'') -> l' : groupsOf n l''

logStringOld  :: UTCTime -> UTCTime -> Priority -> String -> String
logStringOld prev time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    unwords $ [timestring, fromMaybe "???" (prettyLocN callStack 1), "-", msg] <> bool [] ["(" <> show priority <> ")"] (priority == DEBUG)
    where timestring =
#if MIN_VERSION_time(1,9,0)
            formatTime defaultTimeLocale "%T%4Q"
#else
            (("elapsed: " <>) . (printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
              (diffUTCTime time prev)
