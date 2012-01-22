{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module EmailParser where
    
import Network.Mail.Mime
import Text.Parsec

from_right (Right x) = x

parse_email (body_bytes, header_bytes) = result where
    parts = r body_parser body_bytes
    (from, to, cc, bcc, subject) = r headers_parser header_bytes
    
    result = Mail from to cc bcc [] parts
    
headers_parser = error "headers_parser"
from_parser  = return $ Address (Just "error") "error"
to_parser    = error "to_parser"
cc_parser    = error "cc_parser"
bcc_parser   = error "bcc_parser"
body_parser = error "body_parser"
    

    
r p byte_string = from_right $ runP p () "" byte_string
