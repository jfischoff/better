{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module EmailConversion where
    
import BetCommands hiding (traceIt)
import Network.Mail.Mime
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import qualified Data.ByteString as BS
import EmailParser
import Control.Monad
import Control.Monad.Maybe
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Data.List
import Data.List.Split
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Data.Maybe
import Safe
import Data.Char
import Debug.Trace

{-
instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT $ return Nothing
  mplus x y = MaybeT $ do
    mx <- runMaybeT x
    case mx of
      Nothing -> runMaybeT y
      Just _  -> return mx
-}

--todo Create a google apps account with a catch all email account

make_address_accept bet_id = Address (Just "BetYah") $ 
    T.pack $ "bet_" ++ (show bet_id) ++ "_accept@jonathanfischoff.com"
    
make_address_complete bet_id = Address (Just "BetYah") $ 
        T.pack $ "bet_" ++ (show bet_id) ++ "_complete@jonathanfischoff.com"

simple_message text = [Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
            $ LT.encodeUtf8 $ LT.pack text]
            
to_bet_message from body = "You've just got bet! " ++ from ++ 
    " has challenged you with the following:\n" ++
    body ++ "\n\nTo accept this bet, simply reply to this message." 
    
user_accepted_message body user = "Its on. " ++ user ++ " just took on the bet:\n\n" ++ body ++ 
    "\n\n" ++
    "When the bet is finished reply to this message with \"I Won\" or \"I Lost\" in the subject."
  
position_message env to user position = if is_owner env to
                                            then case position of
                                                        For ->     "won"
                                                        Against -> "lost"
                                            else case position of
                                                        For ->     "lost"
                                                        Against -> "won"
        
user_completed_message env to body user position = user ++ " has just ended the bet\n\n" ++ body ++ 
    "\n and thinks that you " ++ (position_message env to user position) ++ "." ++
    "\n\nrespond to this message with \"Win\" or \"Lost\" in the subject, to let use know " ++ 
    "if you think."
        
resolution_message body resolution = "The bet:\n\n" ++ body ++ "\nis over,  " ++
 (decided_message resolution) ++ "\n\nTo create a new bet, replay to this message with bet and CC your friends"
 
decided_message Undecided = "and it was undecided."
decided_message (Decided Won) = "and you won!"
decided_message (Decided Lost) = "and you lost!"

complete_message bet = "The bet\n" ++ (body bet) ++ "\nhas started!\n\n" ++
        "When the bet is finished reply to this message with \"I Won\" or \"I Lost\" in the subject."
    
make_create_address = Address (Just "BetYah") "create@jonathanfischoff.com"
    
simple_mail address to message = Mail address 
        [Address Nothing (T.pack to)] [] [] [] [simple_message message]

to_email :: BetEnv -> OutgoingPayload -> Mail
to_email env (OutgoingPayload to (SendBet body from)) = 
    simple_mail (make_address_accept $ bet_id $ bet env) to $ to_bet_message from body
to_email env (OutgoingPayload to (ParticipantAccepted user)) =
    simple_mail (make_address_complete $ bet_id $ bet env) to $ user_accepted_message (body $ bet env) user
to_email env (OutgoingPayload to (ParticipantCompleted user position)) = 
    simple_mail (make_address_complete $ bet_id $ bet env) to $ 
    user_completed_message env to (body $ bet env) user position
to_email env (OutgoingPayload to (Resolution result)) = simple_mail make_create_address to $ resolution_message (body $ bet env) result
to_email env (OutgoingPayload to CompleteInstructions) = simple_mail (make_address_complete $ bet_id $ bet env) to $
    complete_message (bet env)

fromRight (Right x) = x

get_from = T.unpack . addressEmail . mailFrom
get_to   = T.unpack . addressEmail . headNote "get_to" . mailTo

type BetId = Int

type EmailParts = (BS.ByteString, BS.ByteString)

from_many_emails = mapM from_email'

from_email' :: EmailParts -> State Env (BetId, IncomingPayload)
from_email' email_parts = do
    value <- runMaybeT . from_email $ email_parts
    return $ fromJustNote "from_email'" value

--traceIt x = trace (show x) x

env_or_default bet_id h = result where
    first = H.lookup (traceIt bet_id) h
    result = case first of 
                (Just x) -> Just x
                Nothing -> Just start_bet_env



from_email :: EmailParts -> MaybeT (State Env) (BetId, IncomingPayload)
from_email email_parts = do
    let email = traceIt $ parse_email email_parts 

    bet_id <- get_or_add_bet_id $ traceIt $ get_to email
    
    bet_env <- MaybeT $ gets (env_or_default (traceIt bet_id))  
                            
    command <- MaybeT $ return $ to_command (traceIt bet_env) $ trace "hey" email 
    
    let payload = IncomingPayload (traceItNote "from!!!! " $ get_from email) (traceIt command)
    
    return  $ (bet_id, payload)

call x f = f x
    
to_command :: BetEnv -> Mail -> Maybe Command
to_command env email = msum $ map (call email) [parse_create, parse_accepted, 
    parse_complete env]

is_infix_of_email_address subset email = isInfixOf subset (get_to email)

is_create_address = traceIt . is_infix_of_email_address "create"
    
parse_create email = do
    guard(is_create_address email)
    let emails = parse_competitors email
    bet_body <- parse_bet_body email
    return $ Create bet_body emails
    
    
parse_bet_body email = return . BSCL.unpack . partContent . headNote "parse_bet_body 1" . 
    headNote "parse_bet_body 0" . mailParts $ email
    
parse_competitors email = do
    let emails_from_cc   = parse_emails_from_address $ mailCc email
    let emails_from_bcc  = parse_emails_from_address $ mailBcc email
    let emails_from_body = parse_emails_from_body (email_body email)
    return $ (concat.concat) [emails_from_cc, emails_from_bcc, emails_from_body]
    
parse_emails_from_address addresses = map (\(Address _ x) -> T.unpack x) addresses 
parse_emails_from_body addresses = []

email_body email = BSCL.unpack . partContent . headNote "email_body 1" . 
    headNote "email_body 0" . mailParts $ email
    
is_accepted_address = is_infix_of_email_address "accept"

parse_accepted email = do
    guard(is_accepted_address email)
    return Accepted

is_complete_email = is_infix_of_email_address "complete"

parse_complete env email = do
    guard(is_complete_email email)
    position <- parse_position env email
    return $ Complete position
    
parse_position bet_env email = return $ result where
    from = get_from email
    is_owner' = is_owner bet_env from
    result = if ("won" `isInfixOf` subject)
                    then if is_owner'
                            then For
                            else Against
                    else if is_owner'
                            then Against
                            else For
                
    subject = map toLower $ T.unpack $ get_subject email
    
get_subject email = snd . head . filter (("Subject"==). fst) $ mailHeaders email
    
    
get_or_add_bet_id :: String -> MaybeT (State Env) Int
get_or_add_bet_id to = get_bet_id to `mplus` (MaybeT create_bet_id)


--get_bet_id :: String -> Maybe Int
get_bet_id to = do 
    guard((1<).length . splitOn "_" $ to)
    return . read . head . tail . splitOn "_" $ to
    

create_bet_id :: State Env (Maybe Int)
create_bet_id = gets (Just . maximum . (0:) . H.keys) 
    
    
test_input = [("* 22 FETCH (UID 48 FLAGS (\\Seen) BODY[TEXT] {29}\r\nGhhf\r\n\r\nSent from my iPhone\r\n)\r\n","* 22 FETCH (UID 48 BODY[HEADER.FIELDS (to from cc bcc subject)] {134}\r\nSubject: Test\r\nFrom: Jonathan Fischoff <jonathangfischoff@gmail.com>\r\nCc: me@jonathanfischoff.com\r\nTo: create@jonathanfischoff.com\r\n\r\n)\r\n")]
    
--main = do
--    print $ runState (from_many_emails test_input) start_env
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        