module Service where
    
import BetCommands
import EmailConversion

import Control.Arrow                   ( second )
import Control.Concurrent              ( forkIO, threadDelay )
import Control.Exception               ( mask_ )
import Control.Monad                   ( when, forever )
import qualified Data.ByteString as B  ( ByteString, putStrLn )
import Data.Char                       ( isDigit )
import Network.HaskellNet.IMAP.Connection as I  ( IMAPConnection )
import Network.HaskellNet.IMAP   as I  ( list, select, search, SearchQuery(..), fetch
                                       , connectIMAPPort, login 
                                       )
import Network.Socket            as S  ( HostName, PortNumber )
import System.Directory                ( canonicalizePath )
import System.Environment              ( getArgs )
import System.Exit                     ( exitFailure )
import System.IO                       ( Handle )
import System.IO.Error                 ( isDoesNotExistError )

import SSLWrap                         ( mapSSL )
import Network.Mail.Mime

type UserName = String
type Password = String
type Label = String

gmail_conf = IMAPConf
    {
      icHostname    = "imap.gmail.com",
      icPort        = 993,
      icUsername    = "default@jonathanfischoff.com",
      icPasswd      = "Easyasabc123",
      icSSLWrapPort = 3004
    }

send_email env outgoing = do 
    mapM_ (renderSendMail . to_email env) outgoing

main = do 
    processor <- start 
    
    process_loop (processor send_email)

data IMAPConf = IMAPConf 
    { icHostname :: S.HostName
    , icPort :: S.PortNumber
    , icUsername :: UserName
    , icPasswd :: Password
    , icSSLWrapPort :: S.PortNumber
    }
    
process_loop processor = forever $ do
    getNewEmails gmail_conf (onEmail processor)
      
    threadDelay$ 500*10000
      
onEmail processor email = do
    print email
    let incoming = from_email email 
    processor incoming
    
getNewEmails conf f = withIMAP conf $ \ic -> do
        I.search ic [NEWs]
             >>= mapM_ (\uid -> I.fetch ic uid >>= f)
       
       
withIMAP :: IMAPConf -> (I.IMAPConnection Handle -> IO a) -> IO a
withIMAP c action = do
  -- launch thread for wrapping tcp with SSL
  cafile <- canonicalizePath "cacert.pem"
  _ <- mask_$ forkIO$ mapSSL cafile (icSSLWrapPort c) (icHostname c) (icPort c)
  
  -- start imap communication
  threadDelay$ 500*1000
  putStrLn$ "Connecting to "++icHostname c++":"++show (icPort c)++" (wrapping with ssl through localhost:"++show (icSSLWrapPort c)++") ..."
  ic <- connectIMAPPort "localhost" (icSSLWrapPort c)
  putStrLn$ "Authenticating user "++icUsername c++" ..."
  I.login ic (icUsername c) (icPasswd c)
  action ic
