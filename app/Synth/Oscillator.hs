module Synth.Oscillator where

import Synth.Types
import Synth.Constants
import Music.Types

oscillator :: Hz -> WaveForm -> Seconds -> Audio
oscillator hz wf t = map (wf . (*step)) [0.0 .. sampleRate * t]
    where
        step = (2 * pi * hz) / sampleRate

-- Look at my current project please, and i need help in how to structure it. I currently just have an oscilator which takes in hz, a waveForm and the length of the output audio. I want a way to be able to use musical notes to create a wave with the oscilator but im not sure on a good maintainable way to do this. I want some sort of data/type which represents music and allows me to write music. Eventually ill also have a midi parser which converts it to this so i can just use midi files to generate music. im not sure on a good way to do this. can you help me structure this please