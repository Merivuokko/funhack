-- | Psuedo-random number generator effect

module FunHack.Random
    (
        Random,
        evalRandom,
        uniform,
        uniformR,

        -- * Re-exports from System.Random
        R.RandomGen, R.StdGen, R.initStdGen, R.mkStdGen
    ) where

import Effectful
import Effectful.Dispatch.Static
import System.Random qualified as R

-- | A pseudo-random number generator effect
data Random :: Effect

-- | A wrapper type for storing a generic random number generator in the
-- effect.
data GenWrapper = forall g. R.RandomGen g => GenWrapper g

type instance DispatchOf Random = Static NoSideEffects
newtype instance StaticRep Random = Random GenWrapper

-- | Run a pseudo-random number generator effect. It is not possible to recover the generator.
evalRandom :: R.RandomGen g => g -> Eff (Random : es) a -> Eff es a
evalRandom gen = evalStaticRep (Random $! GenWrapper $! gen)

-- | Call a function from the @random@ library with a generaor and put the
-- returned gneerator back to the effect.
withGen :: (Random :> es) => (forall g. R.RandomGen g => g -> (a, g)) -> Eff es a
withGen f = do
    Random (GenWrapper gen) <- getStaticRep
    let (val, gen') = f gen
    putStaticRep $! Random $! GenWrapper $! gen'
    pure $! val

-- | Return a random value distributed uniformly across values of type a.
uniform :: (Random :> es, R.Uniform a) => Eff es a
uniform = withGen R.uniform

-- | Return a random value of type a uniformly distributed across a given
-- range (inclusive).
uniformR :: (Random :> es, R.UniformRange a) => (a, a) -> Eff es a
uniformR range = withGen (R.uniformR range)
