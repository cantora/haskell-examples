import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Applicative
import Pipes
import qualified Pipes.Prelude as PP
import System.IO (isEOF)

addSentinel :: (Monad m, Ord a, Show a) => a -> Producer a m () -> Producer a m ()
addSentinel sentinel prod = do
  for prod $ \x -> yield x
  yield sentinel

buildVector :: (PrimMonad m, Ord a) => a -> Consumer a m (V.Vector a)
buildVector sentinel =
  do
    vec <- lift $ MV.new 1
    loop 0 vec
  where
    loop n vec = do
      x <- await
      lift $ MV.write vec n x
      if (x <= sentinel)
        then lift $ V.unsafeFreeze vec
        else do
          vec <- lift $ MV.unsafeGrow vec 1
          loop (n+1) vec

stdinChar :: Producer Char IO ()
stdinChar = do
  eof <- lift isEOF
  if (not eof)
    then do
      c <- lift getChar
      yield c
      stdinChar
    else do
      lift $ putStrLn "got eof"

main = do
  putStrLn "hey"
  --runEffect $ for stdinChar (\x -> lift $ putStrLn $ "char: " ++ (show x))
  let sentinel = '\x00'
  v <- runEffect $ (error "uh oh" <$ addSentinel sentinel stdinChar) >-> (buildVector sentinel)
  putStrLn $ "v: " ++ (show v)
  putStrLn "done"
