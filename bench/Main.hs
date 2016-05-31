
import Criterion.Main

-- vectors
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- functors
import Control.Applicative
import Control.Monad (mapM_,replicateM)
import Data.Foldable (sequenceA_)
import Control.Monad.Trans.Class
-- state
import Control.Monad.Trans.State

-- phantom-state
import Control.Applicative.PhantomState

createVector1 :: Int -> V.Vector Int
createVector1 n = V.create $ do
  v <- MV.unsafeNew n
  let step = do 
        i <- get
        lift $ MV.write v i i
        put (i+1)
  execStateT (replicateM n step) 0
  return v

replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ n f = sequenceA_ (replicate n f)

createVector2 :: Int -> V.Vector Int
createVector2 n = V.create $ do
  v <- MV.unsafeNew n
  let step = useAndChangeState $ \i -> MV.write v i i >> pure (i+1)
  runPhantomStateT (replicateA_ n step) 0
  return v

createVector3 :: Int -> V.Vector Int
createVector3 n = V.create $ do
  v <- MV.unsafeNew n
  mapM_ (\i -> MV.write v i i) [0..n-1]
  return v

main :: IO ()
main = defaultMain
  [ bgroup "vector"
      [ bench "generate" $ nf (\i -> V.generate i id) 10000
      , bench "state" $ nf createVector1 10000
      , bench "phantom-state" $ nf createVector2 10000
      , bench "mapM_" $ nf createVector3 10000
        ]
  ]
