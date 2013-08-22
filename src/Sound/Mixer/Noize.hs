{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is a work in progress.
-- All of the commented functions below are
-- to be exported.
module Sound.Mixer.Noize (
        -- * Mix Control
        Mixer(..),
        initMixer,
        
        -- * Channel
        Channel(..),
        -- ** Adding channels
        addChannel,
        -- ** Removing channels
        --removeChannel,
        -- ** Controlling channels
        startChannel,
        --stopChannel
        --pauseChannel
        
        -- * Music
        Music(..),
        -- ** Loading music
        loadMusic,
        withMusic,
        -- ** Controlling music
        playMusic
        --pauseMusic
        --stopMusic
        )
    where

import Foreign.Ptr
import qualified Data.Map.Strict as Map

--JUNK FOR TESTING ONLY--
import Control.Concurrent (threadDelay)
import Control.Monad

--import Sound.SFML
import Sound.SFML.LowLevel

data Channel = Channel {    sndData :: Ptr Sound, -- ^ The pointer to the memory
                                                  -- address where the sound is
                            alias   :: String,  -- ^ The name of the channel
                            status  :: Status,  -- ^ The status of the channel
                            volume  :: Float    -- ^ The volume of the channel
                       }
    deriving (Show)

data Mixer = Mixer {    channels    :: Maybe (Map.Map String Channel),
                        masterVol   :: Float,
                        music       :: Maybe (Ptr Music)
                   }
    deriving (Show)

-- | Add a channel to a mixer. This checks if the name
-- of the channel already exists and if it does it will not
-- add it to the mix.
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

-- | This will pan a MONO channel in 3d space
-- relative to 0,0,0. 
channelPan  :: Mixer -- ^ Mixer where the channel resides
            -> String -- ^ Name of the channel
            -> (Float, Float, Float)  -- ^ (x, y, z)
            -> IO () 
channelPan mix@(Mixer chans vol music) chanName (x, y, z) = do
    case chans of
        Nothing -> do
            print ("NO CHANNELS TO PAN")
            return ()
        Just chans -> do
            let chan = chans Map.! chanName
            sfSound_SetPosition (sndData chan) x y z
            return () 

-- | Initialize a mixer.  This must be called first!
initMixer :: Mixer
initMixer = Mixer Nothing 100.0 Nothing

-- | loadMusic will load a file from the given path but not
-- play it as background music.  If music is already playing
-- it will stop and free the memory associated with the
-- music. Music volume is controlled by the global volume.
loadMusic   :: Mixer        -- ^ Mixer to load music into
            -> FilePath     -- ^ FilePath of music
            -> IO (Mixer)   -- ^ New mixer with music loaded into it
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

-- | This will play the music that is loaded via
-- loadMusic. 
playMusic :: Mixer -> IO ()
playMusic mix@(Mixer chans vol music) = do
    case music of
        Nothing -> do
            print ("NO MUSIC LOADED")
            return ()
        Just music -> do
            sfMusic_Play music
            return ()

  
-- | Starts a channel by name.
startChannel    :: Mixer    -- ^ Mixer to search for the channel in
                -> String   -- ^ Name of the channel to adjust
                -> IO ()
startChannel mix@(Mixer chans vol music) chanName = do
    case chans of
        Nothing -> do
            print ("NO CHANNELS TO START")
            return ()
        Just chans -> do
            let chan = chans Map.! chanName
            sfSound_Play (sndData chan)
            return ()

-- | Encapsulates the playMusic functionality.
-- Loads a file and then starts it
withMusic   :: Mixer -- ^ Mixer to use
            -> FilePath -- ^ FilePath of music to load and start
            -> IO ()
withMusic mix inFile = do
    mix' <- loadMusic mix inFile
    playMusic mix'

main = do
    let mix = initMixer
        inFile = "/home/sam/src/Noize/shpongle-mono.ogg"
    --mix' <- loadMusic mix inFile
    --playMusic mix'
    mix' <- addChannel mix inFile "shpongle2" 100.0
    startChannel mix' "shpongle2"
    channelPan mix' "shpongle2" (50.0, 0.0, 0.0)
    forever $ do (threadDelay 10000)
