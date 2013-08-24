{-# LANGUAGE ScopedTypeVariables #-}
-- | This module is a work in progress.
-- This work will eventually be merged into the
-- sfml-audio module under Sound.SFML.Mixer
--
-- Functions that return IO (Mixer) are inherently
-- stateful towards the mixer.  Meaning that they
-- modify and update the status of the channels in
-- the mixer.
module Sound.Mixer.Noize (
        -- * Mixer
        Mixer(..),
        -- ** Construction and Destruction
        initMixer,
        destroyMixer,
        -- ** Controlling mixers
        --startMix
        --pauseMix
        --stopMix
        
        -- * Channel
        Channel(..),
        -- ** Adding channels
        addChannel,
        -- ** Removing channels
        removeChannel,
        -- ** Controlling channels
        startChannel,
        stopChannel,
        pauseChannel,
        
        -- * Music
        Music(..),
        -- ** Loading music
        loadMusic,
        withMusic,
        destroyMusic,
        -- ** Controlling music
        startMusic,
        pauseMusic,
        stopMusic
        )
    where

import Foreign.Ptr
import qualified Data.Traversable as T
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

data Mixer = Mixer {    channels    :: Map.Map String Channel,
                        masterVol   :: Float,
                        music       :: Maybe (Ptr Music)
                   }
    deriving (Show)

-- | Add a channel to a mixer. This checks if the name
-- of the channel already exists and if it does it will not
-- add it to the mix.
addChannel :: Mixer -> FilePath -> String -> Float -> IO (Mixer)
addChannel mix@(Mixer chans vol music) inFile name chanVol = do
    let chan = Map.lookup name chans
    case chan of
        --Doesn't exist
        Nothing -> do
            chan <- allocateChan
            let chans' = (Map.insert name chan) chans
            return (Mixer chans' vol music)
        --Does exist
        Just chan' -> do
            print("Channel "++(show name)++" already exists in mixer!")
            return mix

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
    let chan' = Map.lookup chanName chans
    case chan' of
        Nothing -> do
            print ("NO CHANNELS TO PAN")
            return ()
        Just chan' -> do
            
            sfSound_SetPosition (sndData chan') x y z
            return () 

destroyChannel (Channel snd _ _ _) = do
    sfSound_Stop snd
    sfSound_Destroy snd

-- | Destroys a mixer.  First stops all sounds playing
-- then frees ptrs associated with a mixer.
-- @destroyMixer myMix == initMixer@
destroyMixer    :: Mixer        -- ^ Mixer to destroy
                -> IO (Mixer)   -- ^ == initMixer
destroyMixer mix@(Mixer chans vol music) = do
    stopMixer mix
    destroyMusic mix
    T.mapM destroyChannel chans

    return initMixer


-- | This will destroy the music playing and insert
-- @Nothing@.
destroyMusic :: Mixer -> IO (Mixer)
destroyMusic mix@(Mixer chans vol music) = do
    case music of
        Nothing -> do
            return mix
        Just music -> do
            sfMusic_Stop music
            sfMusic_Destroy music
            
            return (Mixer chans vol Nothing)

-- | Initialize a mixer.  This must be called first!
initMixer :: Mixer
initMixer = Mixer Map.empty 100.0 Nothing

-- | loadMusic will load a file from the given path but not
-- play it as background music.  If music is already playing
-- it will stop and free the memory associated with the
-- music. Music volume is controlled by the global volume.
loadMusic   :: Mixer        -- ^ Mixer to load music into
            -> FilePath     -- ^ FilePath of music
            -> IO (Mixer)   -- ^ New mixer with music loaded into it
loadMusic mix@(Mixer chans vol music) inFile = do
    case music of
        Nothing -> do
            music' <- sfMusic_CreateFromFile inFile
            sfMusic_SetVolume music' vol
            
            return (Mixer chans vol (Just music'))
        
        Just music -> do
            destroyMusic mix
            
            music' <- sfMusic_CreateFromFile inFile
            sfMusic_SetVolume music' vol
            
            return (Mixer chans vol (Just music'))

-- | Pauses the supplied channel by name.
pauseChannel    :: Mixer        -- ^ Mixer to search for channel in
                -> String       -- ^ Name of channel
                -> IO (Mixer)   -- ^ Mixer with updated channel list
pauseChannel mix@(Mixer chans vol music) chanName = do
    let chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            print ("No channel to pause named "++(show chanName))
            return mix
        Just (Channel snd name stat vol) -> do
            sfSound_Pause snd
            stat' <- getSoundStatus snd
            
            let chan'   = Channel snd name stat vol
                chans'  = Map.insert name chan' chans
            
            return (Mixer chans' vol music)
 
-- | If music is playing pause music, otherwise nothing is done.
pauseMusic :: Mixer -> IO ()
pauseMusic mix@(Mixer _ _ music) = do
    case music of
        Nothing -> return ()
        Just music -> do
            sfMusic_Pause music
            return ()

-- | Removes a channel by name.
removeChannel   :: Mixer        -- ^ Mixer to search in.
                -> String       -- ^ Name of channel to search for
                -> IO (Mixer)   -- ^ New mixer with updated channels.
removeChannel mix@(Mixer chans vol music) chanName = do
    let chan = Map.lookup chanName chans
    case chan of
        Just chan'@(Channel snd name stat vol) -> do
            let chans'  = Map.delete chanName chans
            
            destroyChannel chan'
            return (Mixer chans' vol music)
            
        Nothing -> do
            print ("No channel named "++(show chanName)++"to remove!")
            return mix

-- | Starts a channel by name.
startChannel    :: Mixer        -- ^ Mixer to search for the channel in
                -> String       -- ^ Name of the channel to adjust
                -> IO (Mixer)   -- ^ Mixer with updated channel list.
startChannel mix@(Mixer chans vol music) chanName = do
    let chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            print ("No channel named "++(show chanName)++" to start!")
            return mix
        Just (Channel snd name stat vol) -> do
            sfSound_Play snd
            stat' <- getSoundStatus snd

            let chan' = Channel snd name stat' vol
                chans' = Map.insert name chan' chans
            
            return (Mixer chans' vol music)
-- | This will play the music that is loaded via
-- loadMusic. 
startMusic :: Mixer -> IO ()
startMusic mix@(Mixer chans vol music) = do
    case music of
        Nothing -> do
            print ("NO MUSIC LOADED")
            return ()
        Just music -> do
            sfMusic_Play music
            return ()


-- | Stops a channel by  name.
-- Returns an updated mixer.
stopChannel     :: Mixer        -- ^ Mixer to search for channel in
                -> String       -- ^ Name of the channel to adjust
                -> IO (Mixer)   -- ^ Mixer with an updated channel list
stopChannel mix@(Mixer chans vol music) chanName = do
    let chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            print ("No channel to stop named "++(show chanName))
            return mix
        Just chan' -> do
            
            chan''      <- _stopChannel chan'
            let chans'  = Map.insert chanName chan'' chans
            
            return (Mixer chans' vol music)

_stopChannel :: Channel -> IO Channel
_stopChannel chan@(Channel snd alias stat vol) = do
    sfSound_Stop snd
    stat' <- getSoundStatus snd
    return (Channel snd alias stat' vol)

--stopChannels :: Mixer -> IO Mixer
stopChannels mix@(Mixer chans vol music) = do
    chans' <- T.mapM (_stopChannel) chans
    return (Mixer chans' vol music)

stopMixer   :: Mixer
            -> IO (Mixer)
stopMixer mix@(Mixer chans vol music) = do
    stopMusic mix
    stopChannels mix

-- | If music is playing stop music, otherwise nothing is done.
-- Stop differs from pause by returning the playhead to the
-- beginning of the music.
stopMusic :: Mixer -> IO ()
stopMusic mix@(Mixer _ _  music) = do
    case music of
        Nothing -> return ()
        Just music -> do
            sfMusic_Stop music
            return ()

-- | Encapsulates the startMusic functionality.
-- Loads a file and then starts it
withMusic   :: Mixer -- ^ Mixer to use
            -> FilePath -- ^ FilePath of music to load and start
            -> IO ()
withMusic mix inFile = do
    mix' <- loadMusic mix inFile
    startMusic mix'

