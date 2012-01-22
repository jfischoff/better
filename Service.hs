{-# LANGUAGE OverloadedStrings #-}
module Service where
    
import BetCommands
import EmailConversion

import Network.Socket
import Network.Mail.Mime
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Debug.Trace
import Control.Applicative
import Safe hiding (lookupJust)
import Data.List
import Connection
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS

type UserName = BS.ByteString
type Password = BS.ByteString
type Label = String

data IMAPConf = IMAPConf 
    { 
      host             :: HostName,
      port             :: PortNumber,
      username         :: UserName,
      password         :: Password,
      ca_cert_filepath :: FilePath 
    }
--default
gmail_conf = IMAPConf "imap.gmail.com" 993
      "default@jonathanfischoff.com" "Easyasabc123"
      "cacert.pem"
    
lookupJust k h = fromJustNote "lookupJust" $ H.lookup k h

send_email env outgoing = do 
    mapM_ (\(x, p) -> renderSendMail $ to_email (lookupJust x env) p) outgoing 

main = do    
    processor <- start 
    process_loop (processor send_email from_many_emails)

process_loop processor = forever $ do
    email_infos <- get_email_info 
        (ca_cert_filepath gmail_conf)
        (host gmail_conf)
        (port gmail_conf)
        (username gmail_conf)
        (password gmail_conf)
     
    processor email_infos
      
    threadDelay $ 1000*10000
          
 
    
    
