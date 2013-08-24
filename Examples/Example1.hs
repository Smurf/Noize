module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Debug.Trace
import Control.Monad

import Sound.Mixer.Noize

setupMusic = do
    get
    withMusic "/home/sam/src/Noize/shpongle.ogg"
    
    addChannel "/home/sam/src/Noize/shpongle-mono.ogg" "shpong2" 100.0
    startChannel "shpong2"
    channelPan "shpong2" (50.0, 0.0, 0.0)
    
    addChannel "/home/sam/src/Noize/shpongle.wav" "shpongwav" 50.0
    startChannel "shpongwav"
    channelPan "shpongwav" ( -50.0, 0.0, 0.0)

    removeChannel "shpong2"
    removeMusic
    ph <- musicPlayingOffset
    liftIO $ print $ show ph
    pauseChannels
--build with
--ghc --make Example1.hs -i./../src
main = do
    let mix = initMixer
    mix' <- withMixer mix $ setupMusic
    print $ show mix'
    --mix <- addChannel mix "/home/sam/src/Noize/shpongle-mono.ogg" "shpong2" 100.0
    --mix <- startChannel mix "shpong2"
    forever $ do (threadDelay 1000)
