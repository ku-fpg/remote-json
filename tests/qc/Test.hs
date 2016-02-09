{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module:      Main
Copyright:   (C) 2015 The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson
Stability:   Experimental

@QuickCheck@ properties for natural transformations.
-}
module Main (main) where

import Data.Aeson (Value(..), toJSON)
import Data.Foldable (toList)
import Data.Sequence (Seq, fromList)

import Control.Natural (nat)
import qualified Control.Remote.Monad.JSON as JSON
import qualified Control.Remote.Monad.JSON.Router as R


import Test.QuickCheck 
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Poly (A(..))
import Test.QuickCheck.Monadic
import Test.QuickCheck.Gen.Unsafe (promote)

import Data.IORef

main :: IO ()
main = defaultMain testProperties

testProperties :: TestTree
testProperties = testGroup "QuickCheck remote monad properties"
    [ testProperty "push works remotely"                  $ prop_push
    , testProperty "pop works remotely"                   $ prop_pop 
    , testProperty "compare two remote monad strategies"  $ testRunRemoteMonad
    , testProperty "send (m >>= k) = send m >>= send . k" $ testRemoteMonadBindLaw
    , testProperty "send (return a) = return a"           $ testRemoteMonadReturnLaw
    ]


----------------------------------------------------------------
-- Basic stack machine, with its interpreter
runCall :: IORef [String] -> IORef [A] -> R.Call a -> IO a
runCall tr ref (R.CallNotification "push" (JSON.List [Number n])) = do
    let a :: A = A (round n)
    stack <- readIORef ref
    writeIORef ref (a : stack)
    modifyIORef tr (("push " ++ show a) :)
    return ()
runCall tr ref (R.CallMethod "pop" _) = do
    modifyIORef tr (("pop") :)
    stack <- readIORef ref
    res <- case stack of
      [] -> return Nothing 
      (x:xs) -> do
          writeIORef ref xs
          modifyIORef tr ((show x) :)
          return (Just (unA x))
    return $ toJSON res
runCall tr ref _ = R.methodNotFound

----------------------------------------------------------------
-- The different ways of running remote monads.

data RemoteMonad = RemoteMonad String (forall a . IORef [String] -> IORef [A] -> JSON.RPC a -> IO a)

instance Show RemoteMonad where
  show (RemoteMonad msg _) = "Remote Monad: " ++ msg
  
instance Arbitrary RemoteMonad where
  arbitrary = elements 
    [ runWeakRPC
    , runStrongRPC
    , runApplicativeRPC
    ]

--- This is a complete enumeration of ways of building remote monads
  
runWeakRPC :: RemoteMonad
runWeakRPC = RemoteMonad "WeakPacket" 
  $ \ tr ref -> JSON.send (JSON.weakSession (R.transport (R.router sequence (nat $ runCall tr ref))))

runStrongRPC :: RemoteMonad
runStrongRPC = RemoteMonad "StrongPacket" 
  $ \ tr ref -> JSON.send (JSON.strongSession (R.transport (R.router sequence (nat $ runCall tr ref))))

runApplicativeRPC :: RemoteMonad
runApplicativeRPC = RemoteMonad "ApplicativePacket" 
  $ \ tr ref -> JSON.send (JSON.applicativeSession (R.transport (R.router sequence (nat $ runCall tr ref))))


----------------------------------------------------------------

data DeviceM = Device (IORef [String]) (IORef [A]) (forall a . JSON.RPC a -> IO a)

sendM :: DeviceM -> JSON.RPC a -> IO a
sendM (Device _ _ f) = f

newDevice :: [A] 
          -> RemoteMonad
          -> IO DeviceM
newDevice xs (RemoteMonad _ f) = do
  tr <- newIORef []
  ref <- newIORef xs
  return $ Device tr ref $ f tr ref

readDevice :: DeviceM -> IO [A]
readDevice (Device _ ref _) = readIORef ref

cmpDevices :: DeviceM -> DeviceM -> IO Bool
cmpDevices d1 d2 = (==) <$> readDevice d1 <*> readDevice d2

-- returns backwards, but is for cmp or debugging anyway
traceDevice :: DeviceM -> IO [String]
traceDevice (Device tr _ _) = readIORef tr 

----------------------------------------------------------------

push :: A -> JSON.RPC ()
push (A n) = JSON.notification "push" $ JSON.List [Number $ fromIntegral $ n]

pop :: JSON.RPC (Maybe A)
pop = fmap (fmap A) $ JSON.method "pop" $ JSON.None

----------------------------------------------------------------

newtype Remote a = Remote (JSON.RPC a)

instance Show (Remote a) where
  show _ = "<REMOTE>"

instance Arbitrary (Remote A) where
  arbitrary = sized $ \ n -> Remote <$> arbitraryRemoteMonadA n

----------------------------------------------------------------

data RemoteBind :: * -> * where
  RemoteBind :: Arbitrary a => JSON.RPC a -> (a -> JSON.RPC b) -> RemoteBind b

instance Show (RemoteBind a) where
  show _ = "<REMOTEBIND>"

----------------------------------------------------------------

arbitraryRemoteMonad' :: (CoArbitrary a, Arbitrary a) => [Gen (JSON.RPC a)] -> Int -> Gen (JSON.RPC a)
arbitraryRemoteMonad' base 0 = oneof base 
arbitraryRemoteMonad' base n = frequency 
  [ (1 , oneof base)
  , (1 , do RemoteBind m k <- arbitraryBind (arbitraryRemoteMonad' base) n
            return (m >>= k)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m1 >> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            f  <- arbitrary
            return (fmap f m1 <*> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m1 *> m2)
    )
  , (1 , do m1 <- arbitraryRemoteMonadA (n `div` 2)
            m2 <- arbitraryRemoteMonad' base (n `div` 2)
            return (m2 <* m1) -- reversed, because we want to return m2's result
    )
  ]

arbitraryRemoteMonadUnit :: Int -> Gen (JSON.RPC ())
arbitraryRemoteMonadUnit = arbitraryRemoteMonad'
  [ return (return ())
  , push <$> arbitrary
  ]

arbitraryRemoteMonadMaybeA :: Int -> Gen (JSON.RPC (Maybe A))
arbitraryRemoteMonadMaybeA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  , return $ pop
  ]

arbitraryRemoteMonadA :: Int -> Gen (JSON.RPC A)
arbitraryRemoteMonadA = arbitraryRemoteMonad'
  [ return <$> arbitrary
  ]

arbitraryBind :: (Int -> Gen (JSON.RPC a)) -> Int -> Gen (RemoteBind a)
arbitraryBind f n = oneof
  [ do m <- arbitraryRemoteMonadUnit (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2))  -- look for a better way of doing this
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadMaybeA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2)) 
       return $ RemoteBind m k
  , do m <- arbitraryRemoteMonadA (n `div` 2)
       k  <- promote (`coarbitrary` f (n `div` 2)) 
       return $ RemoteBind m k
  ]

--------------------------------------------------------------------------

-- Test the remote push primitive
prop_push :: RemoteMonad -> [A] -> A -> Property
prop_push runMe xs x = monadicIO $ do
    dev <- run $ newDevice xs runMe
    ()  <- run $ sendM dev (push x)
    ys  <- run $ readDevice  dev
    assert (ys == (x : xs))

-- Test the remote pop primitive
prop_pop :: RemoteMonad -> [A] -> Property
prop_pop runMe xs = monadicIO $ do
    dev <- run $ newDevice xs runMe
    r   <- run $ sendM dev pop
    ys  <- run $ readDevice  dev
    case xs of
      [] -> assert (r == Nothing && ys == [])
      (x':xs') -> assert (r == Just x' && ys == xs')

-- Check that two remote monad configurations given the same trace and same result
testRunRemoteMonad :: RemoteMonad -> RemoteMonad -> Remote A -> [A] -> Property
testRunRemoteMonad runMe1 runMe2 (Remote m) xs = monadicIO $ do
    dev1 <- run $ newDevice xs runMe1
    r1   <- run $ sendM dev1 m
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

    dev2 <- run $ newDevice xs runMe2
    r2   <- run $ sendM dev2 m
    tr2  <- run $ traceDevice dev2
    st2  <- run $ readDevice dev2
    
--    monitor $ collect $ (tr1,tr2)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)
    
-- Check remote monad laws
testRemoteMonadBindLaw :: RemoteMonad -> [A] -> Property
testRemoteMonadBindLaw runMe xs = monadicIO $ do
    RemoteBind m k <- pick (sized $ arbitraryBind arbitraryRemoteMonadA)

    dev1 <- run $ newDevice xs runMe
    a    <- run $ sendM dev1 m
    r1   <- run $ sendM dev1 (k a)
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

    dev2 <- run $ newDevice xs runMe
    r2   <- run $ sendM dev2 (m >>= k)
    tr2  <- run $ traceDevice dev2
    st2  <- run $ readDevice dev2

--    monitor $ collect $ (runMe, tr1)
    assert (r1 == r2 && tr1 == tr2 && st1 == st2)

-- Check remote monad laws
testRemoteMonadReturnLaw :: RemoteMonad -> [A] -> A -> Property
testRemoteMonadReturnLaw runMe xs x = monadicIO $ do

    dev1 <- run $ newDevice xs runMe
    x'   <- run $ sendM dev1 (return x)
    tr1  <- run $ traceDevice dev1
    st1  <- run $ readDevice dev1

--    monitor $ collect $ (runMe, tr1)
    assert (x == x' && tr1 == [] && st1 == xs)

