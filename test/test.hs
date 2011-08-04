{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleContexts
           , FlexibleInstances
  #-}
import Graphics.Spiro.Raw

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad

import Control.Newtype

newtype ConsolePostscript a = C { runConsolePostscript :: IO a }
    deriving (Functor,Applicative,Monad,MonadIO)

instance Newtype (ConsolePostscript a) (IO a) where
  pack = C
  unpack = runConsolePostscript

console = pack . putStrLn . concat

instance PostscriptLike ConsolePostscript where
  moveTo' x y b | b         = console ["newpath"] >> moveTo x y
                | otherwise = moveTo x y
  moveTo x y = console [show x, " ", show y, " moveto"]
  lineTo x y = console [show x, " ", show y, " lineto"]
  quadTo x y x' y' = console 
    [ show x,  " ", show y,  " "
    , show x', " ", show y', " quadto"]
  curveTo x y x' y' x'' y'' = console 
    [ show x,   " ", show y,   " "
    , show x',  " ", show y',  " "
    , show x'', " ", show y'', " curveto"
    ]
  markKnot n = console ["% ", show n, " mark_knot"]

main = runConsolePostscript $ spiroToBezier False
  [ SpiroPoint (0,0) SpiroG2
  , SpiroPoint (1,0) SpiroG2
  , SpiroPoint (1,1) SpiroG2
  , SpiroPoint (0,1) SpiroG2
  ]
