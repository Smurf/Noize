{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.Ptr
import qualified Data.Map.Strict as Map

--JUNK FOR TESTING ONLY--
import Control.Concurrent (threadDelay)
import Control.Monad

--import Sound.SFML
import Sound.SFML.LowLevel

data Mixer = Mixer {    channels    :: Maybe (Map.Map String Channel),
                        masterVol   :: Float,
                        music       :: Maybe (Ptr Music)
                   }
    deriving (Show)

data Channel = Channel {    sndData :: Ptr Sound,
                            alias   :: String,
                            status  :: Status,
                            volume  :: Float
                       }
    deriving (Show)

initMixer :: Mixer
initMixer = Mixer Nothing 100.0 Nothing

addChannel :: Mixer -> FilePath -> String -> Float -> IO (Mixer)
addChannel mix@(Mixer chans vol music) inFile name chanVol = do
    case music of
        Nothing -> do
            chan <- allocateChan
            let chans' = Just (Map.singleton name chan)
            return (Mixer chans' vol music)

        Just m -> do
            chan <- allocateChan
            let chans' = liftM (Map.insert name chan) chans
            return (Mixer chans' vol music)

    where allocateChan = do
            buffer  <- sfSoundBuffer_CreateFromFile inFile
            snd     <- sfSound_Create
            sfSound_SetBuffer snd buffer

            sfSound_SetVolume snd chanVol
            stat <- getSoundStatus snd

            return (Channel snd name stat chanVol)

loadMusic :: Mixer -> FilePath -> IO (Mixer)
loadMusic (Mixer chans vol music) inFile = do
    case music of
        Nothing -> do
            music' <- sfMusic_CreateFromFile inFile
            sfMusic_SetVolume music' vol
            
            return (Mixer chans vol (Just music'))
        
        Just music -> do
            sfMusic_Stop music
            sfMusic_Destroy music
            
            music' <- sfMusic_CreateFromFile inFile
            sfMusic_SetVolume music' vol
            
            return (Mixer chans vol (Just music'))

playMusic :: Mixer -> IO (Mixer)
playMusic mix@(Mixer chans vol music) = do
    case music of
        Nothing -> do
            print ("NO MUSIC LOADED")
            return mix
        Just music -> do
            sfMusic_Play music
            return mix
          
setPan :: Mixer -> String -> (Float, Float, Float) -> IO (Mixer) 
setPan mix@(Mixer chans vol music) chanName (x, y, z) = do
    case chans of
        Nothing -> do
            print ("NO CHANNELS TO PAN")
            return mix
        Just chans -> do
            let chan = chans Map.! chanName
            sfSound_SetPosition (sndData chan) x y z
            return mix

startChannel mix@(Mixer chans vol music) chanName = do
    case chans of
        Nothing -> do
            print ("NO CHANNELS TO START")
            return mix
        Just chans -> do
            let chan = chans Map.! chanName
            sfSound_Play (sndData chan)
            return mix
main = do
    let mix = initMixer
        inFile = "/home/sam/src/Noize/shpongle-mono.ogg"
    --mix' <- loadMusic mix inFile
    --playMusic mix'
    mix' <- addChannel mix inFile "shpongle" 100.0
    startChannel mix' "shpongle"
    setPan mix' "shpongle" (0.0, 100.0, 0.0)
    forever $ do (threadDelay 10000)
