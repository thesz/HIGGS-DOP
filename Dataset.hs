{-# LANGUATE DeriveGeneric #-}

module Dataset where

import Control.DeepSeq

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.IO

type VectorD = UV.Vector Double

type Dataset = [(Double, VectorD)]

readDataset :: String -> IO Dataset
readDataset fn = do
	putStrLn $ "Reading data set from "++show fn
	text <- readFile fn
	let	ls = lines text
		doubles str = case reads s of
			(r, "") : _ -> r
			_ -> error $ "can't parse doubles from "++show str
			where
				s = concat ["[", str, "]"] 
		toLabeledInput str = (l, UV.fromList is)
			where
				l : is = doubles str
		r = map toLabeledInput ls
	liftRnf (\(a,b) -> a `seq` b `seq` ()) r `seq` return r

datasetWidth :: Dataset -> Int
datasetWidth ds = UV.length $ snd $ head ds
