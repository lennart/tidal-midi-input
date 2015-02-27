# Tidal MIDI input
Feed MIDI into Tidal, make CC values available as Pattern

given an MIDI capable input device like a controller that sends MIDI CC values,

this will allow to use the given CC value as a pattern in Tidal, e.g.:


```haskell
d1 $ sound "bd" |+| speed (kr knob1)
```

Where `knob1` is a placeholder for the current value of a certain CC the controller will send.

## Setup

in your .ghci file add the following:

```haskell
import Sound.Tidal.MIDI.Input

[knob1, knob2] <- inputproxy 100000 1 [21, 22]
```

where `1` is the PortMIDI Device ID of the controller and [21, 22] is a list of Controller number
as given needed by your MIDI Controller. This returns a list of knobs which can be read from as seen above using `kr`.
