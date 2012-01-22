{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, TupleSections #-}
module EmailParser where
    
import Network.Mail.Mime
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Perm
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative hiding (many, (<|>), optional)
import Data.Maybe
import qualified Data.Text as T
import Debug.Trace
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT


from_right (Right x) = x

--try a new way
--make the different parser options that
--return an array of the same type different constructors
--one of the values is nil

--fold through and update the defaults

data Fields = S String
            | T [Address]
            | B [Address]
            | C [Address]
            | F Address
            | Nil
            deriving(Show)

empty_address = Address Nothing T.empty

test_headers = BS.concat ["* 20 FETCH (UID 23 BODY[HEADER.FIELDS (to from cc bcc subject)]",
               " {109}\r\nSubject: Fwd: Bet\r\nFrom: Jonathan Fischoff <jonath", 
               "angfischoff@gmail.com>\r\nTo: create@jonathanfischoff.com\r\n\r\n)\r\n"]
               
test_body = BS.concat ["* 1 FETCH (UID 27 BODY[TEXT] {35}\r\n\r\nParse this",
    "\r\nSent from my iPhone\r\n)\r\n"]
    
body_parser = do
    manyTill anyChar $ string "\r\n"
    body <- manyTill anyChar $ try (string "\r\n)\r\n")
    return $ (:[]) $ (:[]) $ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing [] $ 
        LT.encodeUtf8 $ LT.pack body
               
cc_test = BSC.pack "\r\n"

parse_email (body_bytes, header_bytes) = result where
    parts = r body_parser body_bytes
    
    fields = r headers_parser header_bytes
    
    result = foldl unpack_fields (Mail empty_address [] [] [] [] parts) fields
    
    
unpack_fields mail (S subject) = mail{mailHeaders = (BSC.pack "Subject", T.pack subject):(mailHeaders mail)}
unpack_fields mail (T to)      = mail{mailTo = to}
unpack_fields mail (B bcc)     = mail{mailBcc = bcc}
unpack_fields mail (C cc)      = mail{mailCc = cc}
unpack_fields mail (F from)    = mail{mailFrom = from}
unpack_fields mail (Nil)       = mail

update_subject mail subject = mail
 
packJust x = if isJust x then T.pack <$> x else Nothing
    
headers_parser = do
    many $ try $ satisfy (not . ('\r'==))
    many1 field_parser
    
    
field_parser =  try subject_parser
            <|> try to_parser
            <|> try from_parser
            <|> try cc_parser
            <|> try bcc_parser  
            <|> nil_parser          
            
nil_parser = do
    many1 anyToken
    return Nil  

address_parser = try name_and_address_parser 
              <|> just_address_parser
 
traceIt x = trace (show x) x
              
email_address = do
    user   <- many1 $ try $ satisfy (not . ('@'==))
    at     <- char '@'
    domain <- many1 $ try $ satisfy (not . ('\r'==))
    return (user ++ (at:domain))
              
just_address_parser = do
    address <- email_address
    return $ Address Nothing $ T.pack address
    
name_and_address_parser = do
    name <- optionMaybe $ manyTill anyToken $ lookAhead $ char '<'
    address <- between (char '<') (char '>') (many $ try $ satisfy (not . ('>'==)))
    return $ Address (packJust name) $ T.pack address
    
from_parser  = do
    string "\r\n"
    string "From: "
    F <$> address_parser

many1_address_parser label c = do
    string "\r\n"
    string label
    c <$> many1 (try address_parser)

many_address_parser label c = do
    string "\r\n"
    optional $ string label
    c <$> many (try address_parser)

to_parser  = many1_address_parser "To: "  T
cc_parser  = many_address_parser  "CC: "  C
bcc_parser = many_address_parser  "BCC: " B


subject_parser = do
    string "\r\n"
    string "Subject: "
    subject_parts <- many $ try $ satisfy (not . ('\r'==))
    return $ S subject_parts
    
    
r p byte_string = from_right $ runP p () "" byte_string

test_headers_parser = runP headers_parser () "" test_headers

test_cc_parser  = runP cc_parser () "" cc_test

test_body_parser = runP body_parser () "" test_body

instance Show Address where
    show (Address x y) = show x ++ show y
    
instance Show Part where
    show (Part x y z w u) = concat [show x,
        show z, show w, show u]
    
    
