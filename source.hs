{-# LANGUAGE 
    PackageImports,
    MultiParamTypeClasses,
    FunctionalDependencies,
    TypeSynonymInstances,
    FlexibleInstances,
    GeneralizedNewtypeDeriving
    #-}

import "mtl" Control.Monad.State.Lazy

class (Monad m) => Source m a | m -> a where
  produce :: m (Maybe a)

instance Source (State [a]) a where
  produce = do
    xs <- get
    case xs of
      []     -> return Nothing
      (x:xs) -> do
        put xs
        return (Just x)

newtype ConsoleLineSource a = CLS { unCLS :: IO a } deriving (Monad, MonadIO)
instance Source ConsoleLineSource String where
  produce = do
    x <- liftIO $ getLine
    return $ Just x

newtype ConsoleCharSource a = CCS { unCCS :: IO a } deriving (Monad, MonadIO)
instance Source ConsoleCharSource Char where
  produce = do
    x <- liftIO $ getChar
    return $ Just x

srcClassTest :: (Source m a) => m [a]
srcClassTest =
  produce >>= (blah [])
  where
    blah acc Nothing  = return acc
    blah acc (Just x) =
      produce >>= (blah (x:acc))

srcClassTest2 :: (Source m a) => Int -> m [a]
srcClassTest2 max =
  produce >>= (blah 1 [])
  where
    blah _     acc Nothing  = return acc
    blah count acc (Just x)
      | count >= max = return (x:acc)
      | otherwise    = produce >>= (blah (count+1) (x:acc))

srcTake2 :: (Source m a) => m [a]
srcTake2 = srcClassTest2 2


main = do
  putStrLn "hey"
  putStrLn $ show $ runState srcClassTest [1..5]
  putStrLn $ show $ runState srcTake2 [1..5]
  strs <- unCLS srcTake2
  putStrLn $ show strs
  chars <- unCCS (srcClassTest2 10)
  putStrLn chars
  putStrLn "done"