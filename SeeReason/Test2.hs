module SeeReason.Test2 where

import SeeReason.Log
import GHC.Stack

{- Expected output:

SeeReason.Test2.f4:23 - alogWithStack
CallStack (from HasCallStack):
  f4, called at SeeReason/Test.hs:17:6 in main:Main
  f3, called at SeeReason/Test.hs:14:6 in main:Main
  f2, called at SeeReason/Test.hs:11:6 in main:Main
  f1, called at SeeReason/Test.hs:8:8 in main:Main
  main, called at SeeReason/Test.hs:8:1 in main:Main
SeeReason.Test2.f4:24 <- Main:17 <- Main:14 <- Main:11 <- Main:8 - full
SeeReason.Test2.f4:25 <- Main:17 - alog2
SeeReason.Test2.f4:26 - alog
-}

f4 :: HasCallStack => IO ()
f4 = do
  putStrLn (logString callSiteOnly "alogWithStack" <> "\n" <> prettyCallStack (locDrop' fullStack callStack))
  putStrLn (logString fullStack "full")
  putStrLn (logString callSitePlus "alog2")
  putStrLn (logString callSiteOnly "alog")
