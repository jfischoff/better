{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, KindSignatures, FlexibleInstances, TypeOperators, 
    TupleSections, NoMonomorphismRestriction #-}

--A few thoughts
--I should be able to rerun this thing from the emails and get the right next step


import Control.Monad.State
import Data.List
import Text.Parsec hiding (satisfy)
import Text.Parsec.Perm
import Text.Parsec.Prim
import Data.Foldable (foldlM)
import Text.Parsec.Pos
import Control.Applicative hiding ((<|>))

data Response = Success 
              | Fail String
          deriving(Show, Eq, Read, Ord)
              
type Email  = String
type Emails = [Email]
type UserId = Email
type BetId  = Int

data Position = For
              | Against
              deriving(Show, Eq, Read, Ord)

data Result = Decided Position
            | Undecided
             deriving(Show, Eq, Read, Ord)
             
data IncomingMessage = U UserMessage
                     | Failure OutgoingMessage
                 deriving(Show, Eq, Read, Ord)
                 
data UserMessage = Accepted 
                 | Created String Emails
                 | BetForwarded Emails
                 | BetCompleted Position 
            deriving(Show, Eq, Read, Ord)
            

            
data OutgoingMessage = Challenge UserId String
                     | ParticipantAccepted UserId --notifications
                     | ParticipantForwarded UserId  
                     | ParticipantCompleted UserId
                     | ChallengeFailure UserId   
                     | Resolution Result
             deriving(Show, Eq, Read, Ord)


type IncomingPayload  = (UserId, IncomingMessage)
type OutgoingPayload  = (UserId, OutgoingMessage)
type OutgoingPayloads = [OutgoingPayload]

data Payload = I IncomingPayload
             | O OutgoingPayload
             deriving(Show, Eq, Ord, Read)
             
type Payloads = [Payload]
             
is_incoming (I _) = True
is_incoming _  = False

is_outgoing (O _) = True
is_outgoing _ = False 

is_created (I (_, (U (Created _ _)))) = True
is_created _ = False

is_challenge (O (_, (Challenge _ _))) = True
is_challenge _ = False

is_challenge_failure (ChallengeFailure _ ) = True
is_challenge_failure _ = False

from_payload (_, x) = x

from_failure (Failure x) = x

from_user (U x) = x

is_accepted (Accepted) = True
is_accepted _ = False

from_incoming (I payload) = payload
from_outgoing (O payload) = payload

is_completed (BetCompleted _ ) = True
is_completed _ = False

incoming_challenge_failed = is_challenge_failure . from_failure . from_payload . from_incoming
incoming_accepted = is_accepted . from_user . from_payload . from_incoming
incoming_completed = is_completed . from_user . from_payload . from_incoming

--Maybe means we are waiting, Potential means they are waiting and we should send
data Potential a = Expected Payload
                 | Actual a

data BetData = BetData
    {
        body :: String,
        from :: Email,
        to   :: [Email]
    }

data BetExpression = CreateBet BetData [Potential BetExpression]
                   | SentChallenge UserId String (Maybe BetExpression)
                   | ChallengedFailed (Potential BetExpression)
                   | ChallengedAccepted (Potential BetExpression) [Position]
                   | ChallengedForwarded (Potential BetExpression) [Potential BetExpression]
                   | AcceptedMessage
                   | ForwardedMessage 
                   | FailureMessage

type PayloadParser m u a = ParsecT Payloads u m a
type BetExpressionParser m u = ParsecT Payloads u m BetExpression
type PotentialBetExpressionParser m u = ParsecT Payloads u m (Potential BetExpression)

satisfy :: (Monad m) => (Payload -> Bool) -> ParsecT Payloads u m Payload
satisfy test = tokenPrim showTok posFromTok testTok
    where
      testTok x           = if test x then Just x else Nothing
      showTok x           = show x
      posFromTok pos x xs = incSourceColumn pos 1
      
satisfy_challenge f = satisfy (\x -> 
                                        if is_challenge x
                                            then f x
                                            else False
                              )
                              
satisfy_challenge_to_and_from to from = satisfy_challenge 
    (\(O (from', (Challenge to' _))) -> from == from' && to == to')
      
incoming = satisfy is_incoming
outgoing = satisfy is_outgoing

incoming_challenge_failure = satisfy incoming_challenge_failed
incoming_challenge_accepted = satisfy incoming_accepted
incoming_completed_parser = satisfy incoming_completed

challenge_failure = satisfy (is_challenge_failure . from_payload . from_outgoing)


create_bet_parser :: (Monad m) => BetExpressionParser m u
create_bet_parser = do 
    (from, U (Created body to)) <- (from_incoming) <$> satisfy is_created
    let bet_data = BetData body from to
    sent_messages <- create_potential_sent_challenges bet_data
    return $ CreateBet bet_data sent_messages
    
create_potential_sent_challenges :: (Monad m) => BetData -> PayloadParser m u [Potential BetExpression]
create_potential_sent_challenges bet_data = do 
    let defaults = create_sent_challenges_defaults bet_data
    let parsers  = create_sent_challenges_parsers bet_data
    permutation_parser (map Expected defaults) parsers

create_sent_challenges_defaults :: BetData -> Payloads    
create_sent_challenges_defaults bet_data = result where
    result = map (\x -> O $ (x, Challenge (body bet_data) (from bet_data))) $ to bet_data
    
create_sent_challenges_parsers :: (Monad m) => BetData -> [PayloadParser m u BetExpression]
create_sent_challenges_parsers bet_data = map (\x -> create_sent_challenge_parser x (from bet_data)) $ 
    to bet_data
    
create_sent_challenge_parser ::  (Monad m) => Email -> Email -> PayloadParser m u BetExpression  
create_sent_challenge_parser to from = do
    payload <- satisfy_challenge_to_and_from to from
    challenge_to_sent_challenge payload
    
challenge_to_sent_challenge :: (Monad m) => Payload -> PayloadParser m u BetExpression      
challenge_to_sent_challenge (O (to, (Challenge from body))) = do 
    response <- optionMaybe $ challenge_response_parser body to from
    return $ SentChallenge to body response
    
challenge_response_parser body to from = try (challenge_failed_parser to from)
                                 <|> try (challenge_accepted_parser to from)
                                 <|> (challenge_forwarded_parser body to from)
                            
challenge_failed_parser to from = do 
    incoming <- incoming_challenge_failure
    outgoing <- mk_p (create_fail_message to from) failure_message_parser
    return $ ChallengedFailed outgoing
    
create_fail_message to from = result where
    result = O (to, ChallengeFailure from)
    
failure_message_parser = do
    x <- challenge_failure
    return FailureMessage
    
challenge_accepted_parser body to from = do
    incoming <- incoming_challenge_accepted
    input <- getInput
    let positions = parse_positions input
    outgoing <- mk_p (create_accepted_message to from) (challenge_accepted_message_parser to from)
    return $ ChallengedAccepted outgoing positions
    
create_accepted_message to from = result where
    result = undefined
    

challenge_forwarded_parser body to from = do
    I (_, (BetForwarded emails)) <- incoming_challenge_forwarded to
    outgoing <- mk_p (create_forwarded_message to from) (challenge_forwarded_message_parser to from)
    sent_messages <- create_potential_sent_challenges $ BetData body from to
    return $ ChallengedForwarded outgoing sent_messages
    
parse_positions input_stream = filter incoming_completed input_stream
    
    --I need find a BetCompleted anywhere
    

mk_p :: (Monad m) => Payload -> BetExpressionParser m u -> PotentialBetExpressionParser m u
mk_p payload parser =  (Actual <$> parser) 
                   <|> (return $ Expected payload)

permutation_parser :: (Monad m) => [Potential a] 
                   -> [BetExpressionParser m u] 
                   -> PayloadParser m u [Potential BetExpression]
permutation_parser defaults' parsers' = do
     let (defaults, parsers) = add_indices defaults' parsers'  
     parsed_values <- many1 $ choice parsers
     return $ replace_defaults defaults parsed_values
     
add_indices = undefined

replace_defaults = undefined
  


    




