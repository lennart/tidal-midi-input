module Sound.Tidal.MIDI.Input where

import qualified Sound.PortMidi as PM
import Sound.Tidal.Pattern

import System.IO.Unsafe

import Foreign.C

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar


readCtrl :: PM.PMStream -> IO (Maybe (CLong, CLong))
readCtrl dev = do
  ee <- PM.readEvents dev
  case ee of
    Right PM.NoError -> return Nothing
    Left evts ->
      do
        case evts of
          [] -> return Nothing
          (x:xs) -> do
            let m = PM.message x
            return (Just (PM.data1 m, PM.data2 m))

handleKey :: MVar (CLong) -> Maybe (CLong, CLong) -> IO ()
handleKey knob1 v = do
  case v of
    Nothing -> return ()
    Just (cc, val) -> do
      case cc of
        28 -> do
          forkIO $ f knob1 val

          return ()
            where f knob1 val = do
                                  swapMVar knob1 val
                                  return ()

        x -> putStrLn ("Received a MIDI CC Val of: " ++ show v)

readKnobM :: MVar (CLong) -> IO (Maybe CLong)
readKnobM m = tryReadMVar m

readKnob :: MVar (CLong) -> Maybe CLong
readKnob m = unsafePerformIO $ readKnobM m

normMIDIRange :: (Integral a, Floating b) => a -> b
normMIDIRange a = (fromIntegral a) / 127

knobPattern :: MVar (CLong) -> Pattern Double
knobPattern m = maybeListToPat [normMIDIRange <$> readKnob m]

kr = knobPattern


inputproxy :: Int -> PM.DeviceID -> IO (MVar (CLong))
inputproxy latency deviceID = do
  PM.initialize
  do
    result <- PM.openInput deviceID
    knob1 <- newMVar (0 :: CLong)
    case result of
      Right err ->
        do
          putStrLn ("Failed opening Midi Input Port: " ++ show deviceID ++ " - " ++ show err)
          return knob1
      Left conn ->
        do
          forkIO $ loop conn knob1
          return knob1
            where loop conn knob1 = do v <- readCtrl conn
                                       handleKey knob1 v
                                       threadDelay latency
                                       loop conn knob1
