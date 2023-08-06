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
  alogWithStack ALERT "alogWithStack"
  alog ALERT "alog"
  alogDrop id ALERT "alogDrop id"
  alogDrop (take 5) ALERT "alogDrop (take 5)"
  alogDrop (drop 2) ALERT "alogDrop (drop 2)"
  putStrLn "---------------------"
  putStrLn (logString (take 2) "alogWithStack" <> "\n" <> prettyCallStack (locDrop' fullStack callStack))
  putStrLn (logString fullStack "full")
  putStrLn (logString (take 3) "alog2")
  putStrLn (logString (take 2) "alog")
