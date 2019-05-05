{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2           #-}

module FusionSpec where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy
import           Polysemy.Error
import           Polysemy.Internal
import           Polysemy.Internal.Combinators
import           Polysemy.Internal.Effect
import           Polysemy.Internal.Union
import           Polysemy.State
import           Polysemy.Reader
import           Test.Hspec
import           Test.Inspection


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure e) = error e

shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` isSuccess


spec :: Spec
spec = do
  describe "fusion" $ do
    it "Union proofs should simplify" $ do
      shouldSucceed $(inspectTest $ 'countDown `hasNoType` ''SNat)
      shouldSucceed $(inspectTest $ 'countDownWithReader `hasNoType` ''SNat)

    it "internal uses of StateT should simplify" $ do
      shouldSucceed $(inspectTest $ 'countDown `doesNotUse` ''S.StateT)
      shouldSucceed $(inspectTest $ 'jank      `doesNotUse` ''S.StateT)

    it "internal uses of ExceptT should simplify" $ do
      shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` ''E.ExceptT)

    it "`runState . reinterpret` should fuse" $ do
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'reinterpret)
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'hoist)

    it "who needs Semantic even?" $ do
      shouldSucceed $(inspectTest $ 'countDown `doesNotUse` 'Sem)
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'Sem)
      shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` 'Sem)


go :: Sem '[State Int] Int
go = do
  n <- get
  if n <= 0
     then pure n
     else do
       put (n-1)
       go


tryIt :: Either Bool String
tryIt = run . runError @Bool $ do
  catch @Bool
    do
      throw False
    \_ -> pure "hello"


countDown :: Int -> Int
countDown start = fst $ run $ runState start go


jank :: Int -> Int
jank start = fst $ run $ runState start $ go

-- Like go, but add the reader value every step of the way.
goReader :: (Member (State Int) r, Member (Reader Int) r)
         => Sem r Int
goReader = do
  n <- get
  m <- ask
  if n <= 0
     then pure n
     else do
       put $ m + (n-1)
       goReader

-- Like countDown, but have some random reader effect mixed in.
countDownWithReader :: Int -> Int
countDownWithReader start
  = fst $ run
  $ runState start
  $ runReader start
  $ interceptH (help start) goReader
  where
    help :: forall x m r.
            (Member (State Int) r, Member (Reader Int) r)
         => Int -> Reader Int m x -> Tactical (Reader Int) m r x
    help i Ask = pureT 42
    help i (Local f m) = do
      mm <- runT m
      raise $ runReader (f 42) mm

