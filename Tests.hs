{-# LANGUAGE LambdaCase, TypeApplications #-}

import Control.Lens (preview, to, ix)
import Data.List (intercalate)
import GHC.Stack (callStack, getCallStack, HasCallStack, SrcLoc(..))
import GHC.Stack.Types (CallStack(EmptyCallStack, PushCallStack))
import SeeReason.Log -- (tests)
import System.Exit
import Test.HUnit hiding (Path)
import Text.PrettyPrint.HughesPJClass (prettyShow)

main :: HasCallStack => IO ()
main =
  runTestTT tests >>= \case Counts {errors = 0, failures = 0} -> return ()
                            _ -> exitWith $ ExitFailure 1

-- | These are not really tests, they are more like documentation.
tests :: HasCallStack => Test
tests =
  TestList
      [ TestCase (assertEqual "prettyShow"
                   "Data.Map:20"
                   (prettyShow testloc))
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
                   ("AppraisalBase.Layout.Viewport.layoutViewport:61 < " <>
                    "AppraisalClient.AppraisalClient:131 < " <>
                    "AppraisalClient.AppraisalClient:90 < " <>
                    "Main:44:3")
                   (compactStack @String testlocs))
      , TestCase (assertEqual "compactStack'"
                   "AppraisalBase.Layout.Viewport.layoutViewport:61 < AppraisalClient.AppraisalClient:131 < AppraisalClient.AppraisalClient:90 < Main:44:3"
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
      , TestCase (assertEqual "getStack"
                {- [("getStack", SrcLoc {srcLocPackage = "main", srcLocModule = "Main", srcLocFile = "Tests.hs",
                                         srcLocStartLine = 70, srcLocStartCol = 26, srcLocEndLine = 70, srcLocEndCol = 34}),
                     ("tests",SrcLoc {srcLocPackage = "main", srcLocModule = "Main", srcLocFile = "Tests.hs", srcLocStartLine = 11,
                                      srcLocStartCol = 13, srcLocEndLine = 11, srcLocEndCol = 18}),
                     ("main",SrcLoc {srcLocPackage = "main", srcLocModule = "Main", srcLocFile = "Tests.hs",
                                     srcLocStartLine = 10, srcLocStartCol = 1, srcLocEndLine = 10, srcLocEndCol = 1})]
                   (show getStack) -}
                   3
                   (length getStack))
      ]

-- | Verbosely format the location of the nth level up in a call stack
prettyLocN :: CallStack -> Int -> Maybe String
prettyLocN stack n = preview (to getCallStack . ix n . to (prettyShow . snd)) stack
 
-- | Verbosely format the full call stack starting at the nth level up.
locs :: CallStack -> Int -> String
locs stack n =
  (intercalate " -> " . fmap (prettyShow . snd) . drop n . getCallStack) stack

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
