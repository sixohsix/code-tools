
import Control.Monad.Error
import Control.Monad (when)
import Crypto.Random (newGenIO, SystemRandom)
import qualified Network.Socket as NetSock
import Network.HaskellNet.IMAP (IMAPConnection, connectStream)
import Network.TLS (client, defaultParams, TLSCtx, ctxConnection)
import Network.TLS.Extra (connectionClient)
import System.IO (IOMode (ReadWriteMode), Handle)
import System.Environment (getArgs)

usage :: String
usage = "USAGE:\n  imap_killa server"

connectImapTls :: String -> IO (IMAPConnection Handle)
connectImapTls server = do
  g <- newGenIO :: IO SystemRandom
  tlsConn <- connectionClient server "993" defaultParams g
  connectStream $ ctxConnection tlsConn

connectHttps server = do
  g <- newGenIO :: IO SystemRandom
  connectionClient server "443" defaultParams g

main :: IO ()
main = do
  args <- getArgs
  imap <- let server = (args !! 0)
          in connectImapTls server
  return ()
