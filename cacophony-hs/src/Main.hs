import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Sound.PortMidi

f :: Int -> IO (Maybe Int)
f x = return $ Just x

g :: Int -> IO String
g x = return $ show x

-- How can I collapse this into MaybeT IO Int (or does that even make sense?)
combined :: Int -> IO (Maybe String)
combined x = do
  y <- f x
  return $ case y of
    Just y' -> Just $ g y'
    Nothing -> return Nothing


main :: IO ()
main = do
  initialize
  inputDeviceId <- getDefaultInputDeviceID
  case inputDeviceId of
    Nothing   -> putStrLn "No default input device"
    Just d -> do
      deviceInfo <- getDeviceInfo d
      putStrLn $ show deviceInfo
  rt <- terminate
  putStrLn $ show rt
