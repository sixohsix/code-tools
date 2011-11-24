
import Control.Monad (when)
import Crypto.Random (newGenIO)
import qualified Network.Socket as NetSock
import Network.TLS (client, defaultParams)
import System.IO (IOMode (ReadWriteMode))
import System.Environment (getArgs)

usage :: String
usage = "USAGE:\n  imap_killa server"


connectSockHandle server service = do
  ai <- NetSock.getAddrInfo Nothing (Just server) (Just service)
  _ <- if null ai
    then fail "Failed to look up address."
    else return ()
  addrI <- return $ head ai
  sock <- NetSock.socket
          (NetSock.addrFamily addrI)
          NetSock.Stream
          NetSock.defaultProtocol
  _ <- NetSock.connect sock (NetSock.addrAddress addrI)
  NetSock.socketToHandle sock ReadWriteMode


connectImapTls server = do
  handle <- connectSockHandle server "imaps"
  g <- newGenIO
  client defaultParams g handle


doOrDie :: IO (Either String ())
doOrDie = do
  args <- getArgs
  _ <- return $ when (null args) (Left usage)
  imap <- let server = (args !! 0)
          in connectImapTls server
  return $ Right ()


main :: IO ()
main = do
  ret <- doOrDie
  case ret of
    Left err -> (putStrLn err)
    Right () -> return ()
