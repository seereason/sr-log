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
  ( getStack
  , dropLogPackageFrames
  , callSiteOnly
  -- * Full stack formatting
  , loc'
  , locs
  -- * Compact stack formatting
  , srcloc
  , srcloccol
  , srcfunloc
  , srcfunloccol
  , srclocList
  , srclocs
  , compactStack
  -- * Log string formatting
  , locDrop
  , logString
  , logStringOld
  ) where

import Control.Lens(ix, preview, to)
import Data.Bool (bool)
import Data.List (intercalate, intersperse, isPrefixOf)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.String (IsString(fromString))
import Data.Time (diffUTCTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import GHC.Stack (CallStack, callStack, fromCallSiteList, getCallStack, HasCallStack, SrcLoc(..))
import System.Log.Logger (Priority(..))
--import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)
import Text.Printf (printf)

mintercalate :: Monoid s => s -> [s] -> s
mintercalate x xs = mconcat (intersperse x xs)

-- | Verbosely format the location of the nth level up in a call stack
loc' :: CallStack -> Int -> Maybe String
loc' stack n =
  preview (to getCallStack . ix n . to prettyLoc) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

-- | Verbosely format the full call stack starting at the nth level up.
locs :: CallStack -> Int -> String
locs stack n =
  (intercalate " -> " . fmap prettyLoc . drop n . getCallStack) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

-- | Compactly format a call stack.
srclocs :: (HasCallStack, IsString s, Monoid s) => CallStack -> s
-- The space before the arrow allows the console to add line breaks.
-- The space after is omitted to make these line breaks more
-- consistent.
srclocs = mintercalate (fromString " →") . srclocList

-- | List of more compactly pretty printed CallStack location
srclocList :: (HasCallStack, IsString s) => CallStack -> [s]
srclocList = fmap (fromString . srcloc . snd) . reverse . getCallStack

-- | Compactly format a source location
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc loc = fromString (srcLocModule loc) <> ":" <> fromString (show (srcLocStartLine loc))

-- | Compactly format a source location with a function name
srcfunloc :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloc loc f = fromString (srcLocModule loc) <> "." <> f <> ":" <> fromString (show (srcLocStartLine loc))

-- | With start column
srcloccol :: (HasCallStack, IsString s, Semigroup s) => SrcLoc -> s
srcloccol loc = srcloc loc <> ":" <> fromString (show (srcLocStartCol loc))

srcfunloccol :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloccol loc f = srcfunloc loc f <> ":" <> fromString (show (srcLocStartLine loc))

type Locs = [(String, SrcLoc)]

dropLogPackageFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropLogPackageFrames = dropWhile (isPrefixOf "SeeReason.Log" . srcLocModule . snd)

-- | Display the function and module where the logger was called
callSiteOnly = take 2
{-# DEPRECATED callSiteOnly "Use take 2" #-}

-- | Get the portion of the stack before we entered any SeeReason.Log module.
getStack :: HasCallStack => [(String, SrcLoc)]
getStack = dropLogPackageFrames $ getCallStack callStack

-- | Build the prefix of a log message, after applying a function to
-- the call stack.
locDrop :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String
locDrop fn = compactStack (fn getStack)
{-# DEPRECATED locDrop "Use compactStack (fn getStack)" #-}

compactStack :: forall s. (IsString s, Monoid s, HasCallStack) => [(String, SrcLoc)] -> s
compactStack [] = "(no CallStack)"
compactStack [(callee, loc)] = fromString callee <> " ← " <> srcloccol loc
compactStack [(_, loc), (caller, _)] = srcloccol loc <> "." <> fromString caller
compactStack ((callee, loc) : more@((caller, _) : _)) =
  mconcat (intersperse (" ← " :: s)
            (-- fromString callee :
             srcfunloc loc (fromString caller) :
             stacktail (fmap snd more)))
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [loc] = [srcloccol loc]
    stacktail (loc : more) = srcloc loc : stacktail more

logString :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String -> String
logString fn msg =
  pre <> take (lim - len) msg <> suf
  where
    len = length pre + length suf
    pre = compactStack (take 2 locs) <> " - "
    suf = if length locs > 2 then " (" <> compactStack locs <> ")" else ""
    locs = fn getStack
    lim =
#if defined(darwin_HOST_OS)
          2002
#else
          60000
#endif

logStringOld  :: UTCTime -> UTCTime -> Priority -> String -> String
logStringOld prev time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    unwords $ [timestring, fromMaybe "???" (loc' callStack 1), "-", msg] <> bool [] ["(" <> show priority <> ")"] (priority == DEBUG)
    where timestring =
#if MIN_VERSION_time(1,9,0)
            formatTime defaultTimeLocale "%T%4Q"
#else
            (("elapsed: " <>) . (printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
              (diffUTCTime time prev)
