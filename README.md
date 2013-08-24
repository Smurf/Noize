Noize
=====

Make noise with sfml-audio easily.  This will eventually be merged into the [sfml-audio](https://github.com/Smurf/sfml-audio) package.

How to use Noize
=====

Noize is split into two parts `Sound` and `Music` datatypes.  These two parts are unified by a `Mixer` type. The `Mixer` contains the `Music` playing and all channels of sound in a `Map String Channel` type.  Stateful operations (those that modify a `Channel` in the mixer) return a new `Mixer`. This is because every `Channel` has stateful information associated with it.

Functions that modify the `Music` associated with the mixer do not return a modified `Mixer` because a `Mixer` can only have one `Music` channel loaded or playing at a time.

Please run `cabal configure` and `cabal haddock` to generate documentation.

Example
=====

```haskell
    main = do
        mix <- initMixer
        withMusic mix "someFile.ogg" --Loads and starts channel
        mix <- addChannel mix "anotherFile.ogg" "channelName" 100.0
```
