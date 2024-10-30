-- | Source location functions

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
{-# OPTIONS -Wall -Werror=unused-top-binds -Werror=unused-imports #-}

module SeeReason.SrcLoc
  ( getStack
  , dropThisPackageFrames
  -- * Full stack formatting
  , prettyLoc
  , prettyLocN
  , locs
  -- * Compact stack formatting
  , srcloc
  , srcloccol
  , srcfunloc
  , srcfunloccol
  , srclocList
  , srclocs
  , compactStack
  , compactStack'
  , compactLocs
  -- * Log string formatting
  , locDrop
  -- , tests, testloc, testlocs, teststack
  ) where

import Control.Lens(ix, preview, to)
import Data.List (intercalate, intersperse, isPrefixOf)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.String (IsString(fromString))
import GHC.Stack (CallStack, callStack, getCallStack, HasCallStack, SrcLoc(..))

mintercalate :: Monoid s => s -> [s] -> s
mintercalate x xs = mconcat (intersperse x xs)

-- | Verbosely format the location of the nth level up in a call stack
prettyLocN :: CallStack -> Int -> Maybe String
prettyLocN stack n = preview (to getCallStack . ix n . to (prettyLoc . snd)) stack

prettyLoc :: SrcLoc -> String
prettyLoc SrcLoc{..} =
  foldr (++) ""
    [ srcLocModule, ":"
    , show srcLocStartLine {-, ":"
    , show srcLocStartCol-} ]

-- | Verbosely format the full call stack starting at the nth level up.
locs :: CallStack -> Int -> String
locs stack n =
  (intercalate " -> " . fmap (prettyLoc . snd) . drop n . getCallStack) stack

-- | Compactly format a call stack.
srclocs :: (HasCallStack, IsString s, Monoid s) => CallStack -> s
-- The space before the arrow allows the console to add line breaks.
-- The space after is omitted to make these line breaks more
-- consistent.
srclocs = mintercalate (fromString " →") . srclocList

-- | List of more compactly pretty printed CallStack location.
-- Reversed so main comes first.
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

thisPackage :: String
thisPackage = "sr-log-"

dropThisPackageFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropThisPackageFrames = dropWhile (isPrefixOf thisPackage . srcLocPackage . snd)

-- | Get the portion of the stack before we entered any SeeReason.Log module.
getStack :: HasCallStack => [(String, SrcLoc)]
getStack = dropThisPackageFrames $ getCallStack callStack

-- | Build the prefix of a log message, after applying a function to
-- the call stack.
locDrop :: HasCallStack => ([(String, SrcLoc)] -> [(String, SrcLoc)]) -> String
locDrop fn = compactStack (fn getStack)
{-# DEPRECATED locDrop "Use compactStack (fn getStack)" #-}

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
compactStack :: forall s. (IsString s, Monoid s, HasCallStack) => [(String, SrcLoc)] -> s
compactStack = mconcat . intersperse (" ← " :: s) . compactLocs

compactStack' :: forall s. (IsString s, Monoid s, HasCallStack) => [(String, SrcLoc)] -> s
compactStack' = mconcat . intersperse (" < " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s, HasCallStack) => [(String, SrcLoc)] -> [s]
compactLocs [] = ["(no CallStack)"]
compactLocs [(callee, loc)] = [fromString callee, srcloccol loc]
compactLocs [(_, loc), (caller, _)] = [srcloccol loc <> "." <> fromString caller]
compactLocs ((_, loc) : more@((caller, _) : _)) =
  srcfunloc loc (fromString caller) : stacktail (fmap snd more)
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [loc'] = [srcloccol loc']
    stacktail (loc' : more') = srcloc loc' : stacktail more'
