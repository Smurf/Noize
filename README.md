Noize
=====

Make noise with sfml-audio easily.  This will eventually be merged into the [sfml-audio](https://github.com/Smurf/sfml-audio) package.

How to use Noize
=====

Noize is split into two parts `Sound` and `Music` datatypes.  These two parts are unified by a `StateT Mixer m a` type. The `Mixer` contains the `Music` playing and all channels of sound in a `Map String Channel` type.  Stateful operations (those that modify a `Channel` in the mixer) return `StateT Mixer IO ()`. This is because every `Channel` has stateful information associated with it.

Functions that modify the `Music` associated with the mixer do not return a modified `Mixer` because a `Mixer` can only have one `Music` channel loaded or playing at a time.  

Please run `cabal configure && cabal haddock` to generate documentation.

Example
=====

```haskell

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.State.Strict
import Control.Monad

import Sound.Mixer.Noize

setupMusic = do
    get
    withMusic "/shpongle.ogg"
    
    addChannel "/shpongle-mono.ogg" "shpong2" 100.0
    startChannel "shpong2"
    channelPan "shpong2" (50.0, 0.0, 0.0)
    
    addChannel "/shpongle.wav" "shpongwav" 50.0
    startChannel "shpongwav"
    channelPan "shpongwav" ( -50.0, 0.0, 0.0)

    removeChannel "shpong2"
    removeMusic

--build with
--ghc --make Example1.hs -i./../src
main = do
    let mix = initMixer
    mix' <- withMixer mix $ setupMusic

    print $ show mix' --Verify things were added/removed
    forever $ do (threadDelay 1000)
        
```
