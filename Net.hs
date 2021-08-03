{-# LANGUAGE BangPatterns #-}

module Net(module Net, module Dataset) where

import Control.Exception

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.Directory

import Dataset

-- | o(x) = B + (diag(D) + UV^T)x
--
-- I.e., multiply input by diagonal matrix added to outer product matrix and add bias.
data DOP = DOP
	{ dopD, dopU, dopV, dopB :: !VectorD
	}
	deriving (Read, Show)

data LogReg = LogReg { logRegW :: !VectorD, logRegB :: !Double}
	deriving (Read, Show)

data Net =
		NetDOP	!DOP !Net
	|	NetLR	!LogReg
	deriving (Read, Show)

defaultNet :: Int -> Net
defaultNet n = NetLR (LogReg (UV.replicate n (1/fromIntegral n)) 0)

defaultDOP :: Int -> Int -> DOP
defaultDOP n _ = DOP
	--(UV.replicate n 1)
	--(UV.replicate n $ (1/fromIntegral n)^2)
	--(UV.replicate n 0)
	--(UV.fromList $ take n $ [1/fromIntegral n, 0] ++ repeat 0)
	--(UV.fromList $ take n $ [0, 1/fromIntegral n] ++ repeat 0)
	--(UV.replicate n (1-1/fromIntegral n))
	--(UV.replicate n $ (1/fromIntegral n)^2)
	--(UV.replicate n $ (1/fromIntegral n)^2)
	(UV.replicate n 1)
	(UV.fromList $ take n $ [1/fromIntegral n, 0] ++ repeat 0)
	(UV.fromList $ take n $ [0, 1/fromIntegral n] ++ repeat 0)
	(UV.replicate n 0)

netDepth :: Net -> Int
netDepth (NetLR _) = 0
netDepth (NetDOP _ net) = 1 + netDepth net

netFilename :: String
netFilename = "net.txt"

readNet :: IO (Maybe Net)
readNet = do
	catch	(fmap (Just . read) $ readFile netFilename)
		(\e -> do { putStrLn $ "net read error " ++ show (e :: SomeException); return Nothing})

writeNet :: Net -> IO ()
writeNet net = do
	let	tmp = netFilename ++ ".tmp"
	writeFile tmp $ show net
	renameFile tmp netFilename
	

nonlinCoef :: Double
nonlinCoef = 0.1
nonlin :: Double -> Double
nonlin x
	| x >= 0 = x
	| otherwise = nonlinCoef * x

dnonlin :: Double -> Double
dnonlin x
	| x >= 0 = 1
	| otherwise = nonlinCoef

applyDOP :: DOP -> VectorD -> VectorD
applyDOP (DOP d u v b) input = UV.zipWith (+) uvTinp $ UV.zipWith (+) b dinp
	where
		dinp = UV.zipWith (*) d input
		vTinp = UV.sum $ UV.zipWith (*) v input
		uvTinp = UV.map (* vTinp) u

applyNL :: VectorD -> VectorD
applyNL = UV.map nonlin

applyDOPNL :: DOP -> VectorD -> VectorD
applyDOPNL dop = applyNL . applyDOP dop

applyLR :: LogReg -> VectorD -> Double
applyLR (LogReg w b) inputs = 1 / (1 + exp (- x))
	where
		x = b + (UV.sum $ UV.zipWith (*) w inputs)

applyNet :: Net -> VectorD -> Double
applyNet (NetDOP dop net) input = applyNet net $ applyDOPNL dop input
applyNet (NetLR lr) input = applyLR lr input

applyNetBackProp :: Net -> VectorD -> Double -> (Double, Net)
applyNetBackProp net input label = (y, g) 
	where
		(g, y, _) = fb net input
		fb net@(NetLR (LogReg w b)) input = (g, y, UV.map (* d) w)
			where
				y = applyNet net input
				d = label - y
				g = NetLR $ LogReg (UV.map (* d) input) d
		fb (NetDOP dop@(DOP d u v b) net) input = (g, y, errAfterDOP)
			where
				afterDOP = applyDOP dop input
				afterNL = applyNL afterDOP
				(g', y, err) = fb net afterNL
				errAfterNL = UV.zipWith (*) err $ UV.map dnonlin afterDOP
				errAfterD = UV.zipWith (*) errAfterNL d
				uInput = UV.sum $ UV.zipWith (*) u input
				vErr = UV.sum $ UV.zipWith (*) v errAfterNL
				errAfterUV = UV.map (* vErr) u
				errAfterDOP = UV.zipWith (+) errAfterD errAfterUV
				gb = errAfterNL
				gv = UV.map (* uInput) errAfterNL
				gu = UV.map (* vErr) input
				gd = UV.zipWith (*) input errAfterNL
				g = NetDOP (DOP gd gu gv gb) g'

interpolateNet :: Double -> Net -> Double -> Net -> Net
interpolateNet alpha (NetDOP dop0 n0) beta (NetDOP dop1 n1) = NetDOP dop $ interpolateNet alpha n0 beta n1
	where
		DOP d0 u0 v0 b0 = dop0
		DOP d1 u1 v1 b1 = dop1
		interpolate x0 x1 = UV.zipWith (+) (UV.map (* alpha) x0) $ UV.map (* beta) x1
		dop = DOP
			(interpolate d0 d1)
			(interpolate u0 u1)
			(interpolate v0 v1)
			(interpolate b0 b1)
interpolateNet alpha (NetLR lr0) beta (NetLR lr1) = NetLR lr
	where
		LogReg w0 b0 = lr0
		LogReg w1 b1 = lr1
		interpolate x0 x1 = UV.zipWith (+) (UV.map (* alpha) x0) $ UV.map (* beta) x1
		lr = LogReg (interpolate w0 w1) (alpha * b0 + beta * b1)
interpolateNet _ _ _ _ = error "depth mismatch"

netInner :: Net -> Net -> Double
netInner (NetLR (LogReg wa ba)) (NetLR (LogReg wb bb)) = bb*ba + UV.sum (UV.zipWith (*) wa wb)
netInner (NetDOP dopa neta) (NetDOP dopb netb) = dopInner dopa dopb + netInner neta netb
netInner _ _ = error "structure mismatch in netInner"

dopInner :: DOP -> DOP -> Double
dopInner (DOP da ua va ba) (DOP db ub vb bb) =
	sum $
	map UV.sum $
	zipWith (UV.zipWith (*)) [da, ua, va, ba] [db, ub, vb, bb]

remapInputs :: Dataset -> Net -> Dataset
remapInputs ds (NetLR _) = ds
remapInputs ds (NetDOP dop net) = remapInputs (map alterInputs ds) net
	where
		alterInputs (l, inputs) = (l, applyDOPNL dop inputs)

evalNet :: String -> Net -> Dataset -> IO Double
evalNet msg net ds = do
	(cnt, correct, loss) <- loop 0 0 0 ds
	putStrLn $ msg++": accuracy is "++show (fromIntegral correct / cnt)++", log loss "++show loss
	return loss
	where
		loop !cnt !correct !loss [] = return (cnt, correct, loss / cnt)
		loop !cnt !correct !loss ((label, input) : ds) = do
			let	y = applyNet net input
				cnt' = cnt + 1
				correct' = correct + fromEnum ((label >= 0.5) == (y >= 0.5))
				loss' = loss - (label * log y + (1 - label) * log (1 - y))
			loop cnt' correct' loss' ds

combineNets :: Net -> Net -> Net
combineNets (NetLR _) net = net
combineNets (NetDOP dop net) net' = NetDOP dop $ combineNets net net'
