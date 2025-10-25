module Music.Types where

--- Define some music types here ---

type Pitch = Float
type Beats = Float
type Velocity = Float

data Music = Note Pitch Beats Velocity
           | Rest Beats
           | Seq Music Music -- notes one after another 
           | Par Music Music -- notes simultaniously
           deriving (Show, Eq)

