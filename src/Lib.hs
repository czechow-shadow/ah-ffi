module Lib where

import Protolude

import System.Random
import System.Timeout

import Foreign.C

import PCREcompile


foreign import ccall "mymath.h mysin"
  c_sin :: CDouble -> CDouble
  
fastSin :: Double -> Double
fastSin x = realToFrac (c_sin (realToFrac x))



-- go :: IO ()
-- go = do
--   putText "Running"
--   putText $ "Sin: " <> show (fastSin 4.3)
--   putText $ show [1 :: Int .. N]
--   putText $ show $ compile "*" []


go :: IO ()
go = do
  void $ forkIO $ void $ timeout (1000 * 1000 * 5) $ go'
  void $ forkIO $ void $ timeout (1000 * 1000 * 890) $ go''
  void $ timeout (1000 * 1000 * 890) $ go2


go2 :: IO ()
go2 = forever $ do
  putText "Running"
  threadDelay $ 1000 * 1000

  
go' :: IO ()
go' = do
  putText "Started"
  let xs = [1 .. 1024 * 16 * 16]
  res <- foldM (\acc x -> do threadDelay $ 10
                             pure $ acc + x) (0 :: Double) xs
  let avg = res / (realToFrac $ length xs)
  putText $ show res
  putText $ show avg

go'' :: IO ()
go'' = forever $ do
  g <- newStdGen
  let ys = take 2 $ randomRs ('a', 'z') g :: [Char]
  let xs = map f [toS $ ys]
  -- xs <- pure $ map f ["fox", "fox"]
  -- xs <- pure $ map f ["fox", "fox"]
  putText $ show xs
  threadDelay $ 100 * 1

  where
    f :: ByteString -> Maybe [ByteString]
    f xs = do
      --case compile ".*fox.*" [] of
      case compile xs [] of
        Right r -> match r "quick brown fox" []
        Left _ -> Nothing
      
