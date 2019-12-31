{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Parallel (exhaustively,until) where

import Prelude hiding (until)

import Control.Concurrent.Async (Async,async,waitAny)
import Data.List


type Condition b = Monoid b => b -> b -> Bool

type Transform a b = (Eq a, Monoid b) => a -> IO ([a], b)


data Process a b = (Eq a, Monoid b) => Process
  { processCondition :: Condition b
  , processTransform :: Transform a b
  }


data Pool a b = (Eq a, Monoid b) => Pool
  { maxProcesses :: Int
  , pendingTasks :: [IO ([a], b)]
  , runningTasks :: [Async ([a], b)]
  , taskInputs :: [a]
  , taskOutput :: b
  }


run :: (Eq a, Monoid b) => Process a b -> Int -> [a] -> IO b
run process size inputs = do
  let pending = fmap (processTransform process) inputs
      running = []
      output = mempty
      pool = Pool size pending running inputs output
   in exec process pool


exhaustively :: (Eq a, Monoid b) => Transform a b -> Int -> [a] -> IO b
exhaustively transform = run $ Process (\_ _ -> False) transform


until :: (Eq a, Monoid b) => Condition b -> Transform a b -> Int -> [a] -> IO b
until condition transform = run $ Process condition transform


exec :: (Eq a, Monoid b) => Process a b -> Pool a b -> IO b
exec process pool =
  case (length $ pendingTasks pool, length $ runningTasks pool) of
    (nPending, nRunning) | nPending == 0 && nRunning == 0 ->
      return $ taskOutput pool
    (_, nRunning) | nRunning >= maxProcesses pool ->
      wait process pool
    (nPending, _) | nPending == 0 ->
      wait process pool
    (_, nRunning) -> do
      let count = maxProcesses pool - nRunning
          (tasks, pending') = splitAt count $ pendingTasks pool
      tasks' <- sequence $ fmap async tasks
      let running = runningTasks pool
          running' = running ++ tasks'
          pool' = pool { pendingTasks = pending', runningTasks = running' }
      exec process pool'


wait :: (Eq a, Monoid b) => Process a b -> Pool a b -> IO b
wait process pool = do
  let running = runningTasks pool
      output = taskOutput pool
  (task, (inputs', b)) <- waitAny running
  let output' = b <> output
  if (processCondition process) output output'
     then return output'
     else
     let inputs = taskInputs pool
         inputs'' = inputs' \\ inputs
         pending' = fmap (processTransform process) inputs''
      in exec process $ pool
        { pendingTasks = pendingTasks pool ++ pending'
        , runningTasks = [ r | r <- running, r /= task ]
        , taskInputs = inputs ++ inputs''
        , taskOutput = output'
        }
