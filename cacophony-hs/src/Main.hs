import Sound.PortMidi

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
