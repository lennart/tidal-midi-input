module Sound.Tidal.MIDI.Input where

import qualified Sound.PortMidi as PM
import Sound.Tidal.Pattern

import Data.List

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

handleKeys :: [(MVar (CLong))] -> [Int] -> Maybe (CLong, CLong) -> IO ()
handleKeys knobs ccs v = do
  case v of
    Nothing -> return ()
    Just (cc, val) -> do
      case (elemIndex (fromIntegral cc) ccs) of
        Just i -> do
          let knob = (knobs!!i)
          forkIO $ f knob val

          return ()
            where f knob val = do
                                  swapMVar knob val
                                  return ()

        Nothing -> putStrLn ("Received an unmapped MIDI CC: " ++ show v)

readKnobM :: MVar (CLong) -> IO (Maybe CLong)
readKnobM m = tryReadMVar m

readKnob :: MVar (CLong) -> Maybe CLong
readKnob m = unsafePerformIO $ readKnobM m

normMIDIRange :: (Integral a, Fractional b) => a -> b
normMIDIRange a = (fromIntegral a) / 127

knobPattern :: MVar (CLong) -> Pattern Double
knobPattern m = maybeListToPat [normMIDIRange <$> readKnob m]

knobValue :: MVar (CLong) -> Rational
knobValue m = case normMIDIRange <$> readKnob m of
                Just i -> toRational i
                Nothing -> 0.0

kv = knobValue
kr = knobPattern

makeCCMVar :: Int -> IO (MVar (CLong))
makeCCMVar _ = newMVar (0 :: CLong)

makeList :: (Int -> b) -> [Int] -> [b]
makeList f (x:xs) = makeList f xs ++ [f x]
makeList f [x] = [f x]
makeList f [] = []

inputproxy :: Int -> PM.DeviceID -> [Int] -> IO ([MVar (CLong)])
inputproxy latency deviceID ccs = do
  PM.initialize
  do
    result <- PM.openInput deviceID
    knobs <- sequence $ makeList (makeCCMVar) [1..(length ccs)]
    case result of
      Right err ->
        do
          putStrLn ("Failed opening Midi Input Port: " ++ show deviceID ++ " - " ++ show err)
          return knobs
      Left conn ->
        do
          forkIO $ loop conn knobs
          return knobs
            where loop conn knobs = do v <- readCtrl conn
                                       handleKeys knobs ccs v
                                       threadDelay latency
                                       loop conn knobs
