module Lib (
    getMidiDevices
  , showDefaultDevice
  , showDevices
  ) where

import Sound.PortMidi

showDefaultDevice :: IO ()
showDefaultDevice = do
  initialize
  Just deviceId <- getDefaultInputDeviceID
  dInfo <- getDeviceInfo deviceId
  print $ show dInfo
  terminate
  print "Done"


getMidiDevices :: IO ([DeviceID])
getMidiDevices = do
  c <- countDevices
  return $ map fromIntegral [0..(toInteger c)]

showDevices :: IO ()
showDevices = do
  initialize
  devices <- getMidiDevices
  sequence_ (map (\d -> do
    dInfo <- getDeviceInfo d
    print $ show dInfo) devices)
  err <- terminate
  print $ show err
