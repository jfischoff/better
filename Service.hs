{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Control.Exception
import Control.Monad
import Prelude hiding (catch)

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

send_and_render env (x, p) = do
    let email = to_email (lookupJust x env) p
    print ("email " ++ show email)
    renderSendMail email

send_email env outgoing = do 
    mapM_ (send_and_render env) outgoing

--main = test_main
main = real_main

test_main = do
    processor <- start 

    processor send_email from_many_emails start_message
    
real_main = do     
    processor <- start 
    (forever $ process_loop (processor send_email from_many_emails) `catch` 
        (\(e :: SomeException)-> print $ show e))
        


process_loop processor =  do

    email_infos <- get_email_info 
        (ca_cert_filepath gmail_conf)
        (host gmail_conf)
        (port gmail_conf)
        (username gmail_conf)
        (password gmail_conf)
     
    processor email_infos
      
    threadDelay $ 1000*10000
    
start_message = [("* 24 FETCH (UID 50 FLAGS (\\Seen) BODY[TEXT] {28}\r\nVcf\r\n\r\nSent from my iPhone\r\n)\r\n","* 24 FETCH (UID 50 BODY[HEADER.FIELDS (to from cc bcc subject)] {133}\r\nSubject: Ghc\r\nFrom: Jonathan Fischoff <jonathangfischoff@gmail.com>\r\nCc: me@jonathanfischoff.com\r\nTo: create@jonathanfischoff.com\r\n\r\n)\r\n")]
          
 
    
    
