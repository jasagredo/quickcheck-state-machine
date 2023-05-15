-- |

module Main where

import Test.QuickCheck
import Test.StateMachine.Sequential
import MemoryReference

import           Control.Exception
                   (catch)
import           Prelude
import           System.Exit
                   (ExitCode(..))
import           Test.Tasty
                   (TestTree, defaultMain, testGroup, withResource)
import           Test.Tasty.QuickCheck
                   (expectFailure, testProperty, withMaxSuccess)

main = defaultMain $
  testGroup "MemoryReference"
      [ -- testProperty  "NoBugSeq"                        (prop_sequential MemoryReference.None)
      -- , testProperty "LogicBug"          (expectFailure (prop_sequential Logic))
      -- , testProperty "RaceBugSequential"                (prop_sequential Race)
      -- ,
        testProperty "NoBugParallel"                    (prop_parallel MemoryReference.None)
       , testProperty "RaceBugParallel"   (prop_parallel   Race)
      -- , testProperty "CrashBugParallel"                 (prop_parallel'  Crash)
      -- , testProperty "CrashAndLogicBugParallel"
      --     (expectFailure (withMaxSuccess 10000 (prop_parallel' CrashAndLogic)))
      -- , testProperty "PreconditionFailed" prop_precondition
      -- , testProperty "ExistsCommands"     prop_existsCommands
      -- , testProperty "NoBug 1 thread"            (prop_nparallel MemoryReference.None 1)
      -- , testProperty "NoBug 2 threads"           (prop_nparallel MemoryReference.None 2)
      -- , testProperty "NoBug 3 threads"           (withMaxSuccess 80 $ prop_nparallel MemoryReference.None 3)
      -- , testProperty "NoBug 4 threads"           (withMaxSuccess 40 $ prop_nparallel MemoryReference.None 4)
      -- , testProperty "RaceBugParalleel 1 thread"  (prop_nparallel Race 1)
      -- , testProperty "RaceBugParalleel 2 threads" (expectFailure (prop_nparallel   Race 2))
      -- , testProperty "RaceBugParalleel 3 threads" (expectFailure (prop_nparallel   Race 3))
      -- , testProperty "RaceBugParalleel 4 threads" (expectFailure (prop_nparallel   Race 4))
      -- , testProperty "ShrinkParallelEquivalence" prop_pairs_shrink_parallel_equivalence
      -- , testProperty "ShrinkAndValidateParallelEquivalence" prop_pairs_shrinkAndValidate_equivalence
      -- , testProperty "ShrinkPairsEquialence"     prop_pairs_shrink_parallel
      ]
