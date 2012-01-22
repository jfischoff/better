{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module Connection where
import qualified OpenSSL.Session as SSL 
import OpenSSL                          ( withOpenSSL )   
import Network.Socket               
import Network.BSD                      
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Exception                ( bracket, finally, bracketOnError )
import Control.Monad                    
import qualified System.IO       as I  
import Text.Parsec

from_right (Right x) = x

r p byte_string = from_right $ runP p () "" byte_string

search_results_parser = do
    string "* SEARCH"
    optional $ string " "
    sepBy (many1 digit) $ string " "
    
tag ssl x = do
               SSL.write ssl $ BS.concat ["tag ", x, "\r\n"]
               read_until_empty ssl
            

fetch_info ssl uid = do
    fields <- tag ssl $ BS.concat ["uid fetch ", uid, 
        " body[header.fields (to from cc bcc subject)]"]
    print uid
    body <- tag ssl $ BS.concat ["uid fetch ", uid, " body[text]"]
    print body

    print fields
    return (body, fields)

--main = get_email_info 
    
    
get_email_info ca_cert_filepath host port user password = do    
    (ssl, socket) <- secure_connection ca_cert_filepath host port
    let tag' = tag ssl
    read_until_empty ssl
    tag' $ BS.concat ["login ", user, " ", password] 
    tag' "select \"INBOX\""  
    search_results <- tag' "search UNSEEN" 
    let uids = r search_results_parser search_results 
    
    infos <- mapM (fetch_info ssl) $ map BSC.pack uids
    print infos
    SSL.shutdown ssl SSL.Unidirectional
    --sClose socket
    
    return (infos)
              
read_until_empty ssl = read_until_empty' ssl BS.empty

read_until_empty' ssl old_bytes = do 
    bytes <- SSL.read ssl 256
    let new_bytes = BS.append old_bytes bytes
    if (BS.length bytes == 256)
        then read_until_empty' ssl new_bytes
        else return new_bytes                          
             
secure_connection cafile out_host out_port = withOpenSSL $ do
    sout <- socket AF_INET Stream defaultProtocol
    he <- getHostByName out_host        
    connect sout (SockAddrInet (fromIntegral out_port) (hostAddress he))
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
    SSL.contextSetVerificationMode ctx (SSL.VerifyPeer True False Nothing)
    SSL.contextSetCAFile ctx cafile

    ssl <- SSL.connection ctx sout
      
    SSL.connect ssl
    return (ssl, sout)
