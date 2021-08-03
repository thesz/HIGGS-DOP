module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import System.IO

import Net

readTest :: IO Dataset
readTest = readDataset "test.csv"

main = do
	labeledSamples <- readTest
	Just net <- readNet
	evalNet "evaluating on test set" net labeledSamples
	return ()
