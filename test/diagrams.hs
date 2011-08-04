{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , FlexibleInstances
  #-}
import Graphics.Spiro.Raw

import Diagrams.Prelude (straight, bezier3, Point(..), stroke, pathLike, Segment(..), R2)
import Diagrams.Backend.Cairo.CmdLine

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Lazy

import Control.Newtype

{- newtype PathPostscript a = C { runPathPostscript ::  }
    deriving (Functor,Applicative,Monad,MonadIO)

instance Newtype (PathPostscript a) (WriterT [Segment (Double,Double)] IO a) where
  pack = C
  unpack = runPathPostscript
-}
instance PostscriptLike (WriterT [Segment (Double,Double)] IO) where
  moveTo' x y b | b         = moveTo x y
                | otherwise = tell [straight (x,y)]
  moveTo x y = tell [straight (x,y)]
  lineTo x y = tell [straight (x,y)]
  quadTo x y x' y' = tell [bezier3 (x,y) (x,y) (x',y')] -- ???
  curveTo x y x' y' x'' y'' = tell [bezier3 (x,y) (x',y') (x'',y'')]
  markKnot _ = return ()

main = do
    let closed = True
    ss <- execWriterT . spiroToBezier closed . map (\p -> SpiroPoint p SpiroG2) $
           [(0,10),(2,2),(3.5,8),(4.5,6)]
    defaultMain (stroke $ pathLike (P (0,0)) closed (rel (0,0) ss))
  where
    rel _ [] = []
    rel p ((Linear a):ss) = Linear (a - p) : rel a ss
    rel p ((Cubic a b c):ss) = Cubic (a - p) (b - p) (c - p) : rel c ss
