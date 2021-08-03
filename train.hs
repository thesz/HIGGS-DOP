{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import System.IO

import Dataset
import Net

readTrain :: IO Dataset
readTrain = readDataset "train.csv"

gradient :: Net -> Dataset -> IO (Double, Net)
gradient net ds = do
	loop 0 0 zeroGrad ds
	where
		zeroGrad = interpolateNet 0 net 0 net
		loop cnt lossSum gradSum [] =
			return (lossSum / cnt, interpolateNet (1/cnt) gradSum 0 gradSum)
		loop !cnt !lossSum !gradSum ((l, input):ds) = do
			loop (cnt + 1) (lossSum + loss) (interpolateNet 1 gradSum 1 grad) ds
			where
				loss = negate $ l * log y + (1 - l) * log (1 - y)
				(y, grad) = applyNetBackProp net input l

lineDirection :: Net -> Dataset -> Net -> Net -> IO (Double, Net, Net)
lineDirection net ds prevGrad prevDir = do
	(loss, grad') <- gradient net ds
	let	grad'p = netInner grad' prevGrad
		p2 = netInner prevGrad prevGrad
		grad = grad' --interpolateNet 1 grad' (negate $ grad'p / sqrt p2) prevGrad
		g2 = netInner grad grad
		dp = interpolateNet 1 grad (-1) prevGrad
		w =
			--g2 / p2
			--if netInner grad prevGrad < 0 then 0 else g2 / p2
			max 0 $ netInner grad dp / p2
		dir = interpolateNet 1 grad w prevDir
	putStrLn $ "w is " ++ show w
	return (loss, dir, grad)

solveMin :: (Double, Double, Double) -> (Double, Double) -> Double
solveMin (l0, l1, l2) (mu, alphaMu) = t
	where
		b1 = l1 - l0
		b2 = l2 - l0
		x1 = mu^2
		x2 = alphaMu^2
		y1 = mu
		y2 = alphaMu
		d = x1*y2 - x2 * y1
		da = b1 * y2 - b2 * y1
		db = x1 * b2 - x2 * b1
		a = da/d
		b = db/d
		t = negate $ b / (2 * a)

searchLineGR :: Double -> Double -> Net -> Net -> Dataset -> IO (Double, Net)
searchLineGR _mu l0 n0 dir ds = do
	putStrLn $ "mu "++show mu++". n0 len "++show (len n0)++", dir len "++show (len dir)
	p1 <- atT mu
	p2 <- atT (phi * mu)
	putStrLn $ "Growing"
	lmr <- grow ((0, l0), p1, p2)
	putStrLn "Shrinking"
	t <- shrink lmr
	return (sqrt $ mu * t, interpolateNet 1 n0 t dir)
	where
		dirLen = len dir
		n0Len = len n0
		mu = n0Len / 400 / dirLen
		len n = sqrt $ netInner n n
		phi = (sqrt 5 + 1) / 2
		atT t = do
			loss <- evalNet ("  loss at "++ show t) (interpolateNet 1 n0 t dir) ds
			return (t, loss)
		grow (l, m, r)
			| snd m > snd r = do
				r' <- atT ((fst r - fst l) * phi + fst l)
				grow (m, r, r')
			| otherwise = return (l, m, r)
		shrink (l, m, r)
			| fst r - fst l < 1e-2 = return $
				fst l + solveMin (snd l, snd m, snd r) (fst m - fst l, fst r - fst l)
			| otherwise = do
				let	x = fst l
					w = fst r - x
					t = x + (if fst m - x > w / 2 then w - w / phi else w / phi)
				--putStrLn $ "  triplet is " ++show (l, m, r)++", t selected is "++show t
				n <- atT t
				shrink $ if t > fst m
					then if isMin m n then (l, m, n) else (m, n, r)
					else if isMin n m then (l, n, m) else (n, m, r)
			where
				isMin (_, b) (_, c) = b <= c

searchLineOld :: Double -> Double -> Net -> Net -> Dataset -> IO (Double, Net)
searchLineOld mu l0 n0 dir ds = do
	putStrLn $ "mu is "++show mu
	loss1 <- evalNet ("  evaluating first loss") (interpolateNet 1 n0 mu dir) ds
	if loss1 > l0
		then if mu < 1e-5
			then return (mu, n0)
			else do
				putStrLn $ "Halving mu, step is too big"
				searchLine (mu / 2) l0 n0 dir ds
		else do
			loss2 <- evalNet ("  evaluating second loss") (interpolateNet 1 n0 (alpha * mu) dir) ds
			threePoints loss1 loss2 mu
	where
		threePoints loss1 loss2 mu'
			| loss2 < loss1 = do
				let	mu'' = alpha * mu'
				loss2' <- evalNet ("  evaluating additional extra point") (interpolateNet 1 n0 (alpha * mu'') dir) ds
				if loss2' >= loss2
					then do
						let minT = mu' + solveMin (loss1, loss2, loss2')
								(mu'' - mu', alpha * mu'' - mu')
						return (mu * 1.1, interpolateNet 1 n0 minT dir)
					else threePoints loss2 loss2' mu''
			| otherwise = do
				let	minT = solveMin (l0, loss1, loss2) (mu', alpha * mu')
					mu'' =	--sqrt $ mu * minT
						if minT >= mu then mu * 1.1 else mu * 0.9
				return (mu'', interpolateNet 1 n0 minT dir)
		alpha = (sqrt 5 + 1)/2

searchLine = searchLineGR

trainLoop mu prevLoss prevGrad prevDir toTrain ds = do
	(loss, dir, grad) <- lineDirection toTrain ds prevGrad prevDir
	putStrLn $ "loss after direction estimation: "++show loss
	if loss > prevLoss + 1e-3
		then return toTrain
		else do
			(mu', atMin) <- searchLine mu loss toTrain dir ds
			if mu' < 1e-5
				then do
					putStrLn $ "mu is small, local min"
					return atMin
				else do
					writeNet atMin
					trainLoop mu' loss grad dir atMin ds

defaultNetwork :: Int -> Net
defaultNetwork dw = construct 12
	where
		construct 0 = defaultNet dw
		construct n = NetDOP (defaultDOP dw n) $ construct (n-1)

train :: Dataset -> Net -> IO Net
train ds net = do
	let	msg = unwords ["network with", show $ netDepth net, "layers"]
	evalNet msg net ds
	putStrLn "bootstrapping"
	(loss, grad) <- gradient net ds
	putStrLn $ "initial loss "++show loss
	(mu, atMin) <- searchLine 0.1 loss net grad ds
	trainLoop mu loss grad grad atMin ds
	where
		dw = datasetWidth ds

main = do
	hSetBuffering stdout NoBuffering
	labeledSamples <- readTrain
	putStrLn $ "train dataset read"
	maybeNet <- readNet
	net <- case maybeNet of
		Nothing -> do
			putStrLn "network cannot be read"
			return $ defaultNetwork (datasetWidth labeledSamples)
		Just net -> return net
	net <- train labeledSamples net
	writeNet net
