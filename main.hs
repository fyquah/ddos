import Control.Concurrent (
  forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM)
import Network.HTTP

-- to attack infinitely, simply call attack (-1)
attack :: MVar () -> Int -> IO ()
attack done 0 = putMVar done ()
attack done times = do
  response <- simpleHTTP (getRequest "http://waynedev.me/")
  (fmap (take 100) . getResponseBody $ response) >>= putStrLn
  attack done $ (if times < 0 then -1 else times - 1)


main :: IO ()
main = do
  let nThreads = 4
      nAttacks = 1000
  done <- newEmptyMVar
  threadIds <- replicateM nThreads $ forkIO (attack done 2)
  replicateM nThreads $ takeMVar done
  return ()
