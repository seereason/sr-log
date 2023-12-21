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
  -- * Log string formatting
  , locDrop
  , tests, testloc, testlocs, teststack
  ) where

import Control.Lens(ix, preview, to)
import Data.List (intercalate, intersperse, isPrefixOf)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.String (IsString(fromString))
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import GHC.Stack (CallStack, callStack, getCallStack, HasCallStack, SrcLoc(..))
import GHC.Stack.Types (CallStack(..))
import Test.HUnit

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
thisPackage = "Extra.SrcLoc"

dropThisPackageFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropThisPackageFrames = dropWhile (isPrefixOf thisPackage . srcLocModule . snd)

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
compactStack [] = "(no CallStack)"
compactStack [(callee, loc)] = fromString callee <> " ← " <> srcloccol loc
compactStack [(_, loc), (caller, _)] = srcloccol loc <> "." <> fromString caller
compactStack ((_, loc) : more@((caller, _) : _)) =
  mconcat (intersperse (" ← " :: s)
            (-- fromString callee :
             srcfunloc loc (fromString caller) :
             stacktail (fmap snd more)))
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [loc'] = [srcloccol loc']
    stacktail (loc' : more') = srcloc loc' : stacktail more'

-- | more documentation than tests
tests :: IO ()
tests =
  runTestTT cases >>= putStrLn . show
  where
    cases :: Test
    cases =
      TestList
      [ TestCase (assertEqual "prettyLoc"
                   "Data.Map:20"
                   (prettyLoc testloc))
      , TestCase (assertEqual "srcloc"
                   "Data.Map:20"
                   (srcloc @String testloc))
      , TestCase (assertEqual "srclocs"
                   ("Main:44 \8594" <>
                    "AppraisalClient.AppraisalClient:90 \8594" <>
                    "AppraisalClient.AppraisalClient:131 \8594" <>
                    "AppraisalBase.Layout.Viewport:61")
                   (srclocs @String teststack))
      , TestCase (assertEqual "srcloccol"
                   "Data.Map:20:1"
                   (srcloccol @String testloc))
      , TestCase (assertEqual "srclocList"
                   ["Main:44",
                    "AppraisalClient.AppraisalClient:90",
                    "AppraisalClient.AppraisalClient:131",
                    "AppraisalBase.Layout.Viewport:61"]
                   (srclocList @String teststack))
      , TestCase (assertEqual "srcfunloc"
                   "Data.Map.foo:20"
                   (srcfunloc @String testloc "foo"))
      , TestCase (assertEqual "srcfunloccol"
                   "Data.Map.foo:20"
                   (srcfunloc @String testloc "foo"))
      , TestCase (assertEqual "compactStack"
                   ("AppraisalBase.Layout.Viewport.layoutViewport:61 \8592 " <>
                    "AppraisalClient.AppraisalClient:131 \8592 " <>
                    "AppraisalClient.AppraisalClient:90 \8592 " <>
                    "Main:44:3")
                   (compactStack @String testlocs))
      , TestCase (assertEqual "locs 0"
                   ("AppraisalBase.Layout.Viewport:61 -> " <>
                    "AppraisalClient.AppraisalClient:131 -> " <>
                    "AppraisalClient.AppraisalClient:90 -> Main:44")
                   (locs teststack 0))
      , TestCase (assertEqual "locs 2"
                   "AppraisalClient.AppraisalClient:90 -> Main:44"
                   (locs teststack 2))
      , TestCase (assertEqual "prettyLocN 2"
                   (Just "AppraisalClient.AppraisalClient:90")
                   (prettyLocN teststack 2))

      ]

testloc :: SrcLoc
testloc = SrcLoc "containers" "Data.Map" "<file>" 20 1 21 10

teststack :: CallStack
teststack = f testlocs
  where f :: [(String, SrcLoc)] -> CallStack
        f [] = EmptyCallStack
        f ((name, loc) : more) = PushCallStack name loc (f more)

testlocs :: [(String, SrcLoc)]
testlocs =
  [("viewportSkeleton",
    SrcLoc {srcLocPackage = "happstack-ghcjs-common-0.1.3-inplace",
            srcLocModule = "AppraisalBase.Layout.Viewport",
            srcLocFile = "src/AppraisalBase/Layout/Viewport.hs",
            srcLocStartLine = 61, srcLocStartCol = 9, srcLocEndLine = 61, srcLocEndCol = 60}),
   ("layoutViewport",
    SrcLoc {srcLocPackage = "happstack-ghcjs-common-0.1.3-inplace",
            srcLocModule = "AppraisalClient.AppraisalClient",
            srcLocFile = "src/AppraisalClient/AppraisalClient.hs",
            srcLocStartLine = 131, srcLocStartCol = 20, srcLocEndLine = 131, srcLocEndCol = 40}),
   ("appraisalUI",
    SrcLoc {srcLocPackage = "happstack-ghcjs-common-0.1.3-inplace",
            srcLocModule = "AppraisalClient.AppraisalClient",
            srcLocFile = "src/AppraisalClient/AppraisalClient.hs",
            srcLocStartLine = 90, srcLocStartCol = 32, srcLocEndLine = 90, srcLocEndCol = 43}),
   ("clientMain",
    SrcLoc {srcLocPackage = "main",
            srcLocModule = "Main",
            srcLocFile = "src/Main.hs",
            srcLocStartLine = 44, srcLocStartCol = 3, srcLocEndLine = 44, srcLocEndCol = 72})]
