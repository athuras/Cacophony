module Lib
    ( showMidiDevices
    ) where

import Sound.PortMidi

showMidiDevices :: IO ()
showMidiDevices = do
  initialize
  Just deviceId <- getDefaultInputDeviceID
  dInfo <- getDeviceInfo deviceId
  print $ show dInfo
  terminate
  print "Done"
