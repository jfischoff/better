{-# LANGUAGE OverloadedStrings #-}
module EmailConversion where
    
import BetCommands
import Network.Mail.Mime
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text as T
import qualified Data.ByteString as BS

--todo Create a google apps account with a catch all email account

make_address_accept bet_id = Address (Just "BetYah") $ 
    T.pack $ "bet_" ++ (show bet_id) ++ "_accept@jonathanfischoff.com"
    
make_address_complete bet_id = Address (Just "BetYah") $ 
        T.pack $ "bet_" ++ (show bet_id) ++ "_complete@jonathanfischoff.com"

simple_message text = [Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
            $ LT.encodeUtf8 $ LT.pack text]
            
to_bet_message body from = "You've just got bet! " ++ from ++ " has challenged you with the following:\n" ++
    body ++ "\n\nTo accept this bet, simply reply to this message." 
    
user_accepted_message body user = "Its on. " ++ user ++ " just took on the bet:\n\n" ++ body ++ "\n\n" ++
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
        
resolution_message body resolution = "The bet:\n\n" ++ body ++ "has is end, " ++
 (decided_message resolution) ++ "\n\nTo create a new bet, replay to this message with bet and CC your friends"
 
decided_message Undecided = "and it was undecided."
decided_message (Decided Won) = "and you won!"
decided_message (Decided Lost) = "and you lost!"

complete_message bet = "The bet\n" ++ (body bet) ++ "\nhas started!\n\n" ++
        "When the bet is finished reply to this message with \"I Won\" or \"I Lost\" in the subject."
    
make_create_address = Address (Just "BetYah") "create@jonathanfischoff.com"
    
simple_mail address to message = Mail address 
        [Address Nothing (T.pack to)] [] [] [] [simple_message message]

to_email :: Env -> OutgoingPayload -> Mail
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
    
from_email :: BS.ByteString -> IncomingPayload
from_email email = result where
    result = ac "from"
    --I think the only ones that matter are create and complete
    --basically look at the email address
    --if it is create use the body to make a new bet
    --if it is complete look at the subject