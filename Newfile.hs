module Newfile where
import Data.Maybe
import Network.Mail.Mime
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Data.IORef
import Control.Applicative
    
type Email = String    
type Emails = [Email]

data Position = For
              | Against
              deriving(Show, Eq)
    
data IncomingPayload = IncomingPayload
    {
        from         :: Email,
        --notify       :: Maybe Email,
        command      :: Command
    }
    deriving(Show, Eq)
    
data Command = Create String Emails
             | Accepted
--             | Forward Emails
             | Complete Position
        deriving(Show, Eq)

data OutgoingPayload = OutgoingPayload
    {
        to            :: Email,
        --to_notify     :: Maybe Email,
        response_type :: Response  
    }
        deriving(Show, Eq)

type UserId = Email

data Result = Decided Position
            | Undecided
            | Unresolved
             deriving(Show, Eq)
        
fromDecided (Decided x) = x     
isFor For = True
isFor _   = False

isAgainst Against = True
isAgainst _ = False

data Response = SendBet String UserId
              | ParticipantAccepted UserId --notifications
--              | ParticipantForwarded UserId  
              | ParticipantCompleted UserId Position
              | ChallengeFailure UserId   
              | Resolution Result
              deriving(Show, Eq)

create_send_bet from body to = OutgoingPayload to (SendBet body to)


data Bet = Bet
    {
        body   :: String,
        bet_id :: Int
    }
    deriving(Show, Eq)
             
data Env = Env
    {
        incoming_payloads :: [IncomingPayload],
        response_queue    :: [OutgoingPayload],
        sent_responses    :: [OutgoingPayload],
        user_positions    :: H.HashMap UserId (Maybe Position),
        bet               :: Bet
    }
    deriving(Show, Eq)


is_create (Create _ _) = True
is_create _ = False 

is_create_payload x = is_create $ command x

safeHead xs | length xs > 0 = Just (head xs)
            | otherwise     = Nothing

get_bet_body env = do 
    shead    <- safeHead $ filter is_create_payload $ incoming_payloads env 
    return $ (\(Create body _) -> body) $ command $ shead

create_forward_notify notify from = OutgoingPayload notify (ParticipantAccepted from)

response :: Env -> IncomingPayload -> [OutgoingPayload]
response env (IncomingPayload from (Create body emails)) = 
    map (create_send_bet from body) emails 
--response env (IncomingPayload from notify (Forward emails)) = 
--    (create_forward_notify (fromJust notify) from) :
--    (map (create_send_bet from (fromJust $ get_bet_body env)) emails)
response env i@(IncomingPayload from (Complete position)) = 
    map (\x -> OutgoingPayload x (ParticipantCompleted from position)) $ get_notifiers env i
response env i@(IncomingPayload from Accepted) = 
    map (\x -> OutgoingPayload x (ParticipantAccepted from)) $ get_notifiers env i
    
    
get_notifiers env (IncomingPayload _ (Create _ _)) = []
get_notifiers env (IncomingPayload from (Complete position)) = filter (from /=) $ get_all_users env
get_notifiers env (IncomingPayload _ Accepted) = [get_owner env]

get_owner env = (\(IncomingPayload from _) -> from) $ head $ filter is_create_payload $ incoming_payloads env
    
update_position env user_id position = result where
    positions = user_positions env
    new_positions = H.insert user_id position positions
    result = env {user_positions = new_positions }
    
is_resolved env = all isJust $ H.elems $ user_positions env

get_all_users env = H.keys $ user_positions env

get_all_positions env = H.elems $ user_positions env

get_resolution env = if is_resolved env
                        then if all (isFor . fromJust) $ get_all_positions env
                                then Decided For
                                else if all (isAgainst . fromJust) $ get_all_positions env
                                       then Decided Against
                                       else Undecided
                        else Unresolved

make_resolved_responses env = result where
    users = get_all_users env
    result = map (make_resolved_response (get_resolution env)) users

make_resolved_response resolution user = OutgoingPayload user (Resolution resolution)
 
--type EnvState m a = StateT Env m a
    
update_response_queue env responses = env {response_queue = (response_queue env) ++ responses }

add_user env from = env {user_positions = H.insert from Nothing $ user_positions env}

add_user_and_update env x = result where
    responses = response env x
    env' = update_response_queue env responses
    result = add_user env' $ from x
    
set_bet_body env body = env {bet = (bet $ env){ body = body } }
      
update_env :: Env -> IncomingPayload -> Env
update_env env x@(IncomingPayload from (Complete position)) = result where
    responses = response env x
    env' = update_position env from $ Just position
    resolved_responses = if is_resolved env'
                            then make_resolved_responses env'
                            else []
    env'' = update_response_queue env' (resolved_responses ++ responses)
    result = append_incoming_responses env'' x
update_env env x@(IncomingPayload from (Accepted)) = append_incoming_responses (add_user_and_update env x) x
--update_env env x@(IncomingPayload from _ (Forward _)) = add_user_and_update env x
update_env env x@(IncomingPayload from (Create body _)) = result where
    env' = add_user_and_update env x
    env'' = set_bet_body env' body
    result = append_incoming_responses env'' x

append_incoming_responses env x = env{incoming_payloads = x:(incoming_payloads env)}

process_responses :: Env -> IO ()
process_responses env = do
    mapM_ send_response $ response_queue env
     
send_response :: OutgoingPayload -> IO ()
send_response payload = renderSendMail $ (to_mail payload)

--to_mail :: OutgoingPayload -> Mail
--to_mail = undefined

--from_mail :: Mail -> IncomingPayload
--from_mail = undefined

--TODO
--Rewrite with state and a mutable reference
--test in ghci
--write emails to folks
--add email sending and processing
--add multiple bets

type EnvState m a = StateT Env m a

update :: Monad m => IncomingPayload -> EnvState m ()
update incoming = do
    input <- get
    let output = update_env input incoming
    put (output)
    
--get_queue :: EnvState m [OutgoingPayload]
get_queue = do
    env <- get
    return $ response_queue env
    
--get_sent_responses :: EnvState m [OutgoingPayload]
--get_sent_responses = sent_responses <$> get

append_sent_responses responses env = env {sent_responses = (sent_responses env) ++ responses}  
    
--append_to_sent_responses :: [OutgoingPayload] -> EnvState m ()
append_to_sent_responses responses = modify (append_sent_responses responses)
    
clear_queue' env = env {response_queue = []}    
    
clear_queue = modify clear_queue'
    
--persist_outgoing :: EnvState m ()
persist_outgoing = do 
    queue <- get_queue
    append_to_sent_responses queue
    clear_queue

        
--update_loop :: ([OutgoingPayload] -> m ()) -> IncomingPayload -> EnvState m ()
update_loop incoming = do 
    update incoming
    outgoing <- get_queue 
    persist_outgoing
    return outgoing

start_env =  Env
        {
            incoming_payloads  = [],
            response_queue     = [],
            sent_responses     = [],
            user_positions     = H.empty,
            bet                = Bet { body = "", bet_id = -1}
        }

--start :: IO (IncomingPayload -> IO ())
start = do
    state <- newIORef start_env
    return $ process_input state

--process_input :: IORef Env -> IncomingPayload -> IO ()
process_input ref incoming = do 
    env <- readIORef ref


    print incoming
    let (outgoing, new_env) = runState (update_loop incoming) env
    mapM_ print outgoing
    
    writeIORef ref new_env
    return new_env
    


cr from body emails = IncomingPayload from $ Create body emails
ac from = IncomingPayload from Accepted
comp from position = IncomingPayload from $ Complete position
    