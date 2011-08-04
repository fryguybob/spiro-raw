{-# LANGUAGE ForeignFunctionInterface 
           , FlexibleContexts
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Spiro
-- Copyright   :  (c) 2011 diagrams team (see LICENSE)
-- License     :  GPL (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Two-dimensional splines from libspiro.
--
-----------------------------------------------------------------------------

module Graphics.Spiro.Raw
       (
         spiroToBezier
       , PostscriptLike(..)
       , SpiroType(..)
       , SpiroPoint(..)
       ) where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

import Control.Monad.IO.Class
import Control.Concurrent.MVar

type CBool = CInt

class MonadIO m => PostscriptLike m where
    moveTo'  :: Double -> Double -> Bool -> m ()
    moveTo :: Double -> Double -> m ()
    moveTo x y = moveTo' x y False
    lineTo :: Double -> Double -> m ()
    quadTo :: Double -> Double -> Double -> Double -> m ()
    curveTo :: Double -> Double -> Double -> Double -> Double -> Double -> m ()
    markKnot :: Int -> m ()

data SpiroType = SpiroCorner | SpiroG4 | SpiroG2 | SpiroLeft | SpiroRight
data SpiroPoint = SpiroPoint { spiroPoint :: (Double, Double), spiroType :: SpiroType }

spiroTypeChar :: SpiroType -> CChar
spiroTypeChar SpiroCorner = castCharToCChar 'v'
spiroTypeChar SpiroG4     = castCharToCChar 'o'
spiroTypeChar SpiroG2     = castCharToCChar 'c'
spiroTypeChar SpiroLeft   = castCharToCChar '['
spiroTypeChar SpiroRight  = castCharToCChar ']'

spiroToBezier :: PostscriptLike m => Bool -> [SpiroPoint] -> m ()
spiroToBezier closed ps = do
    m <- liftIO $ newMVar (return ())
    liftIO $ spiroToBezier' m closed ps
    a <- liftIO $ takeMVar m
    a

spiroToBezier' :: PostscriptLike m => MVar (m ()) -> Bool -> [SpiroPoint] -> IO ()
spiroToBezier' context closed ps = liftIO $ do
    wrap mkM $ \m ->
     wrap mkL $ \l ->
      wrap mkQ $ \q ->
       wrap mkC $ \c ->
        wrap mkK $ \k ->
          let bc = BezCtx m l q c k in
            alloca $ \ctx -> do
               poke ctx bc
               allocaArray n $ \ptr -> do
                pokeArray ptr xs
                c_SpiroCPsToBezier ptr (fromIntegral n) (fromBool closed) ctx
  where
    wrap :: IO (FunPtr a) -> (Ptr () -> IO ()) -> IO ()
    wrap mk cont = do
           hm <- mk
           cont (castFunPtrToPtr hm)
           freeHaskellFunPtr hm
    mkM = mkMoveto   $ \_ a b c       -> fromMVar context $ moveTo'  (realToFrac a) (realToFrac b) (c /= 0)
    mkL = mkLineto   $ \_ a b         -> fromMVar context $ lineTo   (realToFrac a) (realToFrac b)
    mkQ = mkQuadto   $ \_ a b c d     -> fromMVar context $ quadTo   (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)
    mkC = mkCurveto  $ \_ a b c d e f -> fromMVar context $ curveTo  (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d) (realToFrac e) (realToFrac f)
    mkK = mkMarkknot $ \_ a           -> fromMVar context $ markKnot (fromIntegral a)
    
    fromMVar m f = do 
      a <- takeMVar m
      putMVar m (a >> f)

    n = length ps
    xs = map (\(SpiroPoint (x,y) t) -> SpiroCP (realToFrac x) (realToFrac y) (spiroTypeChar t)) ps

#include "bezctx.h"
#include "spiroentrypoints.h"

data SpiroCP = SpiroCP { spiro_x :: CDouble, spiro_y :: CDouble, spiro_ty :: CChar }
    deriving (Show, Eq, Ord)
    
instance Storable SpiroCP where
    sizeOf  _ = (#size spiro_cp)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        x <- (#peek spiro_cp, x) ptr
        y <- (#peek spiro_cp, y) ptr
        ty <- (#peek spiro_cp, ty) ptr
        return $ SpiroCP x y ty
    poke ptr (SpiroCP x y ty) = do
        (#poke spiro_cp, x) ptr x
        (#poke spiro_cp, y) ptr y
        (#poke spiro_cp, ty) ptr ty

data BezCtx = BezCtx 
   { 
     moveto :: Ptr ()
   , lineto :: Ptr ()
   , quadto :: Ptr ()
   , curveto :: Ptr ()
   , mark_knot :: Ptr ()
   }

instance Storable BezCtx where 
    sizeOf _ = (#size bezctx)
    alignment _ = alignment (undefined :: FunPtr (CDouble -> IO ()))
    peek ptr = do
        m <- (#peek bezctx, moveto) ptr
        l <- (#peek bezctx, lineto) ptr
        q <- (#peek bezctx, quadto) ptr
        c <- (#peek bezctx, curveto) ptr
        k <- (#peek bezctx, mark_knot) ptr
        return $ BezCtx m l q c k
    poke ptr (BezCtx m l q c k) = do
        (#poke bezctx, moveto) ptr m
        (#poke bezctx, lineto) ptr l
        (#poke bezctx, quadto) ptr q
        (#poke bezctx, curveto) ptr c
        (#poke bezctx, mark_knot) ptr k

foreign import ccall safe "spiroentrypoints.h SpiroCPsToBezier" 
    c_SpiroCPsToBezier :: Ptr SpiroCP -> CInt -> CBool -> Ptr BezCtx -> IO ()

type Moveto  = Ptr () -> CDouble -> CDouble -> CBool -> IO ()
type Lineto  = Ptr () -> CDouble -> CDouble -> IO ()
type Quadto  = Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
type Curveto = Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
type Markknot = Ptr () -> CInt -> IO ()

foreign import ccall "wrapper" mkMoveto   :: Moveto   -> IO (FunPtr Moveto)
foreign import ccall "wrapper" mkLineto   :: Lineto   -> IO (FunPtr Lineto)
foreign import ccall "wrapper" mkQuadto   :: Quadto   -> IO (FunPtr Quadto)
foreign import ccall "wrapper" mkCurveto  :: Curveto  -> IO (FunPtr Curveto)
foreign import ccall "wrapper" mkMarkknot :: Markknot -> IO (FunPtr Markknot)