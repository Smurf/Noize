{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.Ptr
import Control.Concurrent (threadDelay)
import Control.Monad

--import Sound.SFML
import Sound.SFML.LowLevel

data Mixer = Mixer {    channels    :: Maybe [Channel],
                        masterVol   :: Float,
                        music       :: Maybe (Ptr Music)
                   }

data Channel = Channel {    sndData :: Ptr Sound,
                            status  :: Status,
                            volume  :: Float
                       }


initMixer :: Mixer
initMixer = Mixer Nothing 100.0 Nothing

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
            
main = do
    let mix = initMixer
        inFile = "/home/sam/src/Noize/shpongle.wav"
    mix' <- loadMusic mix inFile
    playMusic mix'
    forever $ do (threadDelay 10000)
