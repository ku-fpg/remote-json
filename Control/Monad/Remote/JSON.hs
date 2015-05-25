{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

{-|
Module:      Control.Monad.Remote.JSON where
Copyright:   (C) 2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Monad.Remote.JSON where

data RPC :: * -> * where
 Pure :: a ->                     RPC a
 Bind :: RPC a -> (a -> RPC b) -> RPC b
 Ap   :: RPC (a -> b) -> RPC a -> RPC b

