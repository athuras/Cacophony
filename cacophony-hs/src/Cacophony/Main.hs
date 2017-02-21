module Cacophony.Main where

import Sound.PortMidi

main :: IO ()
main = do
  initialize
  inputDeviceId <- getDefaultInputDeviceID
  case inputDeviceId of
    Nothing -> putStrLn "No default input device"
    Just d  -> do
      deviceInfo <- getDeviceInfo d
      print deviceInfo
  rt <- terminate
  print rt
