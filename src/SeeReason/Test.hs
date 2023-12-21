module Main where

import SeeReason.Log
import SeeReason.Test2 (f4)
import GHC.Stack

main :: HasCallStack => IO ()
main = f1

f1 :: HasCallStack => IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = f4
