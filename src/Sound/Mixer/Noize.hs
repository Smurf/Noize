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
        withMixer,
        startMixer,
        pauseMixer,
        stopMixer,
        
        -- * Channel
        Channel(..),
        -- ** Adding channels
        addChannel,
        -- ** Removing channels
        removeChannel,
        -- ** Controlling an individual channel
        -- *** Playing State
        startChannel,
        stopChannel,
        pauseChannel,
        -- *** Playhead Position    
        seekChannel,
        playheadPosition,
        -- *** 3D Position
        channelPan,
        
        -- ** Controlling all channels
        -- *** Playing State
        startChannels,
        stopChannels,
        pauseChannels,

        -- * Music
        Music(..),
        -- ** Loading music
        loadMusic,
        withMusic,
        removeMusic,
        -- ** Controlling music
        -- *** Playing State
        startMusic,
        pauseMusic,
        stopMusic,
        -- *** Playhead Position
        seekMusic,
        musicPlayingOffset,
        )
    where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad

import Foreign.Ptr
import Sound.SFML.LowLevel

import qualified Data.Map.Strict as Map
import qualified Data.Traversable as T

data Channel = Channel {    sndData :: Ptr Sound, -- ^ The pointer to the memory
                                                  -- address where the sound is
                            alias       :: String,  -- ^ The name of the channel
                            status      :: Status,  -- ^ The status of the channel
                            duration    :: Float,     -- ^ Duration of channel in seconds
                            sampleRate  :: Int,     -- ^ samples/sec of channel
                            chanCount   :: Int,     -- ^ Number of channels in sound
                            volume      :: Float   -- ^ The volume of the channel
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
addChannel  :: FilePath     -- ^ File path of sound loaded into channel.
            -> String       -- ^ Name of channel.
            -> Float        -- ^ Initial volume of channel.
            -> StateT Mixer IO (Mixer)
addChannel inFile name chanVol = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup name chans
    case chan of
        --Doesn't exist
        Nothing -> do
            chan <- liftIO $ allocateChan
            let chans' = (Map.insert name chan) chans
            put mix{channels=chans'}
            return mix{channels=chans'}
        --Does exist
        Just chan' -> do
            liftIO $ print("Channel "++(show name)++" already exists in mixer!")
            return mix

    where allocateChan = do
            buffer  <- sfSoundBuffer_CreateFromFile inFile
            snd     <- sfSound_Create
            sfSound_SetBuffer snd buffer

            sfSound_SetVolume snd chanVol
            stat    <- getSoundStatus snd
            dur     <- sfSoundBuffer_GetDuration buffer
            rate    <- sfSoundBuffer_GetSampleRate buffer
            chanCt  <- sfSoundBuffer_GetChannelsCount buffer

            return (Channel snd name stat dur (fromIntegral rate) (fromIntegral chanCt) chanVol)

-- | This will pan a MONO channel in 3d space
-- relative to 0,0,0. 
channelPan  :: String -- ^ Name of the channel
            -> (Float, Float, Float)  -- ^ (x, y, z)
            -> StateT Mixer IO () 
channelPan chanName (x, y, z) = do
    mix <- get
    let chans = channels mix
        chan' = Map.lookup chanName chans
    case chan' of
        Nothing -> do
            liftIO $ print ("NO CHANNELS TO PAN")
        Just chan' -> do
            liftIO $ sfSound_SetPosition (sndData chan') x y z

destroyChannel :: Channel -> IO ()
destroyChannel chan = do
    sfSound_Stop snd
    sfSound_Destroy snd
    where snd = sndData chan

-- | Destroys a mixer.  First stops all sounds playing
-- then frees ptrs associated with a mixer.
-- @destroyMixer myMix == initMixer@
destroyMixer :: StateT Mixer IO (Mixer)
destroyMixer = do
    mix <- get
    let chans = channels mix
    stopMixer
    removeMusic
    liftIO $ T.mapM destroyChannel chans

    return initMixer

-- | Initialize a mixer.  This must be called first!
initMixer :: Mixer
initMixer = Mixer Map.empty 100.0 Nothing

-- | loadMusic will load a file from the given path but not
-- play it as background music.  If music is already playing
-- it will stop and free the memory associated with the
-- music. Music volume is controlled by the global volume.
loadMusic   :: FilePath     -- ^ FilePath of music
            -> StateT Mixer IO (Mixer)   -- ^ New mixer with music loaded into it
loadMusic inFile = do
    mix <- get
    let music'   = music mix
        vol     = masterVol mix
    case music' of
        Nothing -> do
            music' <- liftIO $ sfMusic_CreateFromFile inFile
            liftIO $ sfMusic_SetVolume music' vol
            
            put mix{music=(Just music')}
            return mix
        Just music' -> do
            removeMusic 
            
            music' <- liftIO $ sfMusic_CreateFromFile inFile
            liftIO $ sfMusic_SetVolume music' vol
            
            put mix{music=(Just music')}
            return mix
-- | The current playhead position on the music in a mixer.
musicPlayingOffset  :: StateT Mixer IO (Float)   -- ^ New playhead position in seconds.
musicPlayingOffset = do
    mix <- get
    let music' = music mix
    case music' of
        Nothing -> do
            liftIO $ print ("No music loaded!")
            return (0.0)
        Just music' -> do
            liftIO $ sfMusic_GetPlayingOffset music'


-- | Pauses the supplied channel by name.
pauseChannel    :: String       -- ^ Name of channel
                -> StateT Mixer IO (Mixer)   -- ^ Mixer with updated channel list
pauseChannel chanName = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            liftIO $ print ("No channel to pause named "++(show chanName))
            return mix
        Just chan' -> do
            
            chan''      <- liftIO $ _pauseChannel chan'
            let chans'  = Map.insert chanName chan'' chans
            
            return mix{channels=chans'}

_pauseChannel chan = do
    sfSound_Pause snd
    stat' <- getSoundStatus snd
    return chan{status = stat'}
    where snd = sndData chan


-- | Pauses all channels in a mixer.
pauseChannels :: StateT Mixer IO (Mixer)
pauseChannels = do
    mix <- get
    let chans = channels mix
    
    chans' <- liftIO $ T.mapM _pauseChannel chans
    return mix{channels=chans'}

-- | Pauses all channels and music in a mixer.
pauseMixer :: StateT Mixer IO (Mixer)
pauseMixer = do
    get
    pauseMusic
    pauseChannels

-- | If music is playing pause music, otherwise nothing is done.
pauseMusic :: StateT Mixer IO ()
pauseMusic = do
    mix <- get
    let music' = music mix
    case music' of
        Nothing -> do 
            return ()
        Just music' -> do
            liftIO $ sfMusic_Pause music'

-- | Returns the playhead position of a named channel.
playheadPosition    :: String   -- ^ Channel name
                    -> StateT Mixer IO Float -- ^ Position of playback in seconds
playheadPosition chanName = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Just chan' -> do
            liftIO $ sfSound_GetPlayingOffset (sndData chan')
        
        Nothing -> do
            liftIO $ print ("No channel of name "++(show chanName)++" to get playhead position")
            liftIO $ return 0.0


-- | Removes a channel by name.
removeChannel   :: String       -- ^ Name of channel to search for
                -> StateT Mixer IO (Mixer)   -- ^ New mixer with updated channels.
removeChannel chanName = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Just chan' -> do
            let chans'  = Map.delete chanName chans
            
            liftIO $ destroyChannel chan'
            put mix{channels=chans'}
            return mix{channels=chans'}
            
        Nothing -> do
            liftIO $ print ("No channel named "++(show chanName)++"to remove!")
            return mix


-- | This will destroy the music playing and insert
-- @Nothing@.
removeMusic :: StateT Mixer IO (Mixer)
removeMusic = do
    mix <- get
    let music' = music mix
    case music' of
        Nothing -> do
            return mix

        Just music' -> do
            liftIO $ sfMusic_Stop music'
            liftIO $ sfMusic_Destroy music'
            
            put mix{music=Nothing}
            return (mix{music=Nothing})

-- | Seeks channel to time given in seconds
-- This is 'side-effect' free on the Mixer supplied.
seekChannel :: String   -- ^ Channel name
            -> Float    -- ^ Time in seconds
            -> StateT Mixer IO ()
seekChannel chanName toTime = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            liftIO $ print ("No channel by the name of "++(show chanName)++"to seek!")

        Just chan' -> do
            liftIO $ sfSound_SetPlayingOffset (sndData chan') toTime


-- | Seeks the music in a mixer to the given offset in seconds.
seekMusic   :: Float    -- ^ Offset in seconds
            -> StateT Mixer IO ()
seekMusic toTime = do
    mix <- get
    let music' = music mix 
    case music' of
        Nothing -> do
            liftIO $ print ("No music to seek!")
        Just m -> do 
            liftIO $ sfMusic_SetPlayingOffset m toTime


-- | Starts a channel by name.
startChannel    :: String       -- ^ Name of the channel to adjust
                -> StateT Mixer IO (Mixer)   -- ^ Mixer with updated channel list.
startChannel chanName = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            liftIO $ print ("No channel named "++(show chanName)++" to start!")
            liftIO $ print ("channels: "++(show chans))
            return mix

        Just chan' -> do
            chan'' <- liftIO $ _startChannel chan'
            let chans' = Map.insert chanName chan'' chans
            
            return mix{channels=chans'}

_startChannel :: Channel -> IO Channel
_startChannel chan = do
    sfSound_Play snd
    stat' <- getSoundStatus snd
    return chan{status = stat'}
    where snd = sndData chan


startChannels :: StateT Mixer IO (Mixer)  
startChannels = do
    mix <- get
    let chans = channels mix
    chans' <- liftIO $ T.mapM _startChannel chans
    return mix{channels=chans'}

-- | Starts the music and all sound channels in the mixer.
startMixer :: StateT Mixer IO (Mixer)
startMixer = do
    get
    startMusic
    startChannels

-- | This will play the music that is loaded via
-- loadMusic. 
startMusic :: StateT Mixer IO()
startMusic = do
    mix <- get
    let music' = music mix
    case music' of
        Nothing -> do
            liftIO $ print ("NO MUSIC LOADED")
        Just music' -> do
            liftIO $ sfMusic_Play music'


-- | Stops a channel by  name.
-- Returns an updated mixer.
stopChannel     :: String       -- ^ Name of the channel to adjust
                -> StateT Mixer IO (Mixer)   -- ^ Mixer with an updated channel list
stopChannel chanName = do
    mix <- get
    let chans = channels mix
        chan = Map.lookup chanName chans
    case chan of
        Nothing -> do
            liftIO $ print ("No channel to stop named "++(show chanName))
            return mix
        
        Just chan' -> do
            chan''      <- liftIO $ _stopChannel chan'
            let chans'  = Map.insert chanName chan'' chans
            
            return mix{channels=chans'}

_stopChannel :: Channel -> IO Channel
_stopChannel chan = do
    sfSound_Stop snd
    stat' <- getSoundStatus snd
    return chan{status = stat'}
    where snd = sndData chan

-- | Stops all channels playing.
stopChannels :: StateT Mixer IO (Mixer) 
stopChannels = do
    mix <- get
    let chans = channels mix
    chans' <- liftIO $ T.mapM (_stopChannel) chans
    return mix{channels=chans'}

-- | Stops all channels and music in a mixer.
stopMixer  :: StateT Mixer IO (Mixer) 
stopMixer = do
    get
    stopMusic 
    stopChannels

-- | If music is playing stop music, otherwise nothing is done.
-- Stop differs from pause by returning the playhead to the
-- beginning of the music.
stopMusic :: StateT Mixer IO ()
stopMusic = do
    mix <- get
    let music' = music mix
    case music' of
        Nothing -> do 
            return ()
        Just music' -> do
            liftIO $ sfMusic_Stop music'

-- | Encapsulates the startMusic functionality.
-- Loads a file and then starts it
withMusic   :: FilePath -- ^ FilePath of music to load and start
            -> StateT Mixer IO ()
withMusic inFile = do
    get
    loadMusic inFile
    startMusic

-- | Run a StateT action on a mixer and return the resulting mixer.
withMixer :: Monad m => Mixer -> StateT Mixer m a -> m a
withMixer = flip evalStateT

