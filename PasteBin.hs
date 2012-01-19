has_user (AddUser user_email env) email | user_email == email = True
                                        | otherwise           = has_user env email

get_new_betid (AddUser _ e) x = get_new_betid e x

--    AcceptBet                  :: UserId -> BetId -> Env -> Env

--    RegisterForNotifications   :: UserId -> BetId -> Env -> Env
--    UnregisterForNotifications :: UserId -> BetId -> Env -> Env

AddBet      :: UserId -> BetId -> String -> Emails -> Env -> Env
ForwardBet  :: UserId -> BetId -> Emails -> Env -> Env
CompleteBet :: UserId -> BetId -> Env -> Env
AcceptBet   :: UserId -> BetId -> Env -> Env

get_new_betid :: Env -> Int -> Int
get_new_betid EmptyEnv x = x
get_new_betid (AddBet _ y _ _ e) x = get_new_betid e (max y x)
get_new_betid (AcceptBet _ y e) x = get_new_betid e (max y x)
get_new_betid (ForwardBet _ y _ e) x = get_new_betid e (max y x)
get_new_betid (CompleteBet _ y e) x = get_new_betid e (max y x)
get_new_betid (Send _ y _ e) x = get_new_betid e (max y x)
get_new_betid (Received _ y _ _ e) x = get_new_betid e (max y x)

has_user :: Env -> Email -> Bool
has_user EmptyEnv email = False
has_user (AddBet _ _ _ _ e) x                 = has_user e x
has_user (AcceptBet _ _ e) x                  = has_user e x
has_user (ForwardBet _ _ _ e) x               = has_user e x
has_user (CompleteBet _ _ e) x                = has_user e x
has_user (Send _ _ _  e) x  = has_user e x
has_user (Received user_email _ _ Success e) email | user_email == email = True
                                                 | otherwise           = has_user e email

get_env EmptyEnv = error "Do Not use this for anything that can be empty!"
get_env (AddBet _ _ _ _ e) = e
get_env (AcceptBet _ _ e) = e
get_env (ForwardBet _ _ _ e) = e
get_env (CompleteBet _ _ e) = e
get_env (Send _ _ _ e) = e
get_env (Received _ _ _ _ e) = e

{-
data Bet :: * where
    EmptyBet      :: Bet
    Forward       :: UserId -> Emails -> Bet -> Bet
    Complete      :: UserId -> Bet -> Bet
    Sent          :: UserId -> Bet -> Bet
    Delivered     :: UserId -> Notification -> Bet -> Bet
    Accept        :: UserId -> Bet -> Bet
-}

--make a function for adding one to the max UserId and BetID
get_new_betid :: Env -> Int -> Int
get_new_betid EmptyEnv x = x
get_new_betid (AddBet _ y _ _ e) x = get_new_betid e (max y x)
get_new_betid (AcceptBet _ y e) x = get_new_betid e (max y x)
get_new_betid (ForwardBet _ y _ e) x = get_new_betid e (max y x)
get_new_betid (CompleteBet _ y e) x = get_new_betid e (max y x)
get_new_betid (Send _ y _ e) x = get_new_betid e (max y x)
get_new_betid (Received _ y _ _ e) x = get_new_betid e (max y x)

has_user :: Env -> Email -> Bool
has_user EmptyEnv email = False
has_user (AddBet _ _ _ _ e) x                 = has_user e x
has_user (AcceptBet _ _ e) x                  = has_user e x
has_user (ForwardBet _ _ _ e) x               = has_user e x
has_user (CompleteBet _ _ e) x                = has_user e x
has_user (Send _ _ _  e) x  = has_user e x
has_user (Received user_email _ _ Success e) email | user_email == email = True
                                                 | otherwise           = has_user e email

get_env EmptyEnv = error "Do Not use this for anything that can be empty!"
get_env (AddBet _ _ _ _ e) = e
get_env (AcceptBet _ _ e) = e
get_env (ForwardBet _ _ _ e) = e
get_env (CompleteBet _ _ e) = e
get_env (Send _ _ _ e) = e
get_env (Received _ _ _ _ e) = e




collect_messages_to_send' :: Env -> OutgoingPayloads ->  OutgoingPayloads
collect_messages_to_send' EmptyEnv xs = xs
collect_messages_to_send' (AddBet user_id bet_id body emails e) xs = 
    collect_messages_to_send' e (xs ++ (map (, bet_id, Challenge user_id body) emails)) 
collect_messages_to_send' ()

    
    --the bet
    --
has xs x | (length $ filter (x==) xs) > 0 = True
         | otherwise = False

find_sender :: UserId -> BetId -> Env -> Maybe UserId
find_sender u b EmptyEnv = Nothing
find_sender u b (AddBet owner bet_id _ emails env) | emails `has` u && b == bet_id = Just owner
                                                   | otherwise = find_sender u b env
find_sender u b env = find_sender u b (get_env env)

data Env :: * where
    EmptyEnv    :: Env
    Send        :: UserId -> BetId -> Message -> Env -> Env
    Received    :: UserId -> BetId -> Message -> Response -> Env -> Env 
        deriving(Show)
{-
send_bet_email :: Bet -> Email -> Env -> IO ()
send_bet_email bet email env = undefined

add_bet :: Monad m => UserId -> String -> Emails -> Env -> m Env
add_bet user_id body emails env = result where
    let bet_id = get_new_betid env
    mapM_ (send_bet_email (NewBet bet_id body) env) emails
    return $ AddBet user_id bet_id body emails env

-}


{-
collect_outgoing_payloads :: EnvState m OutgoingPayloads
collect_outgoing_payloads = do
    env <- get
    let payloads = collect_outgoing_payloads' env 
    put (apply_outgoing_payloads payloads env)
    return payloads





        
apply_response = liftM apply_response'

apply_response' env response = undefined
        
process_response :: IncomingPayload -> EnvState m OutgoingPayloads
process_response incoming_payload = do 
    apply_response incoming_payload
    collect_outgoing_payloads
        
-}
--
--
--
--
--

get_bet_id (I (_, b, _, _)) = b
get_bet_id (O (_, b, _)) = b

type Env = [Payload]
        
data ExpectedResponse = ExpectedResponse 
    {
        incoming_payload  :: IncomingPayload,
        expected_payloads :: OutgoingPayloads,
        actual_payloads   :: OutgoingPayloads
    }
    
collect_from_expected :: ExpectedResponse -> OutgoingPayloads
collect_from_expected exp = (expected_payloads exp) \\ (actual_payloads exp)
        
type EnvState m a = StateT Env m a 

apply_outgoing_payload env payload = (O payload):env 

apply_outgoing_payloads :: OutgoingPayloads -> Env -> Env
apply_outgoing_payloads payloads env = foldl apply_outgoing_payload env payloads

collect_outgoing_payloads' :: Env -> OutgoingPayloads
collect_outgoing_payloads' env = concatMap collect_from_expected $ create_expected env    

create_expected env = result where
    result = undefined
    bet_payloads = groupBy (\x y -> get_bet_id x == get_bet_id y) env
    expected = map mk_expected_lists bet_payloads
    --find the actual for the betid
 
incoming = filter is_incoming
outgoing = filter is_outgoing
    
mk_expected_lists bet_payloads = map (mk_expected bet_payloads) $ incoming bet_payloads

mk_expected bet_payloads x = result where
    exp = create_with_payload x
    actual = any (\x -> any (x==) (expected_payloads $ exp)) $ outgoing bet_payloads 
    result = exp {actual_payloads = actual}
    
create_with_payload (u, b, m) = stimulus_response u m








data BetData = BetData
    {
        body :: String,
        from :: Email,
        emails   :: [Email]
    }

data Potential a = Expected Payload
                 | Actual a

data ResponseTree = ResponseTree BetData [Potential SentChallenge]

data SentChallenge = SentChallenge UserId String (Maybe ChallengeResponse)

data ChallengeResponse = ChallengedFailed (Potential FailureMessage)
                       | ChallengedAccepted (Potential AcceptedMessage) (Maybe [Position]) 
                       | ChallengedForwarded (Potential ForwardedMessage) [Potential SentChallenge]
                       
data AcceptedMessage
data ForwardedMessage 
data FailureMessage

class CollectPotentials a where
    collectPotentials :: a -> Payloads

instance CollectPotentials ResponseTree where
    collectPotentials (ResponseTree _ xs) = concatMap collectPotentials xs
    
instance CollectPotentials SentChallenge where
    collectPotentials (SentChallenge _ _ Nothing) = []
    collectPotentials (SentChallenge _ _ (Just x)) = collectPotentials x
    
instance CollectPotentials ChallengeResponse where
    collectPotentials (ChallengedFailed x) = collectPotentials x
    collectPotentials (ChallengedAccepted x _) = collectPotentials x
    collectPotentials (ChallengedForwarded x xs) = collectPotentials x ++ 
        (concatMap collectPotentials xs)

instance CollectPotentials (Potential a) where
    collectPotentials (Expected x) = [x]
    collectPotentials _ = []
    
    
    
    {-
    instance FromPayloads ResponseTree where
        fromPayloads = do
            bet_data <- parse_bet_data
            sent_emails <- create_sent_parsers (body bet_data) (from bet_data) (emails bet_data)

            return $ ResponseTree bet_data sent_emails

    create_sent_parsers body from emails = 
        permutation_parser $ unzip $
            map (\(i, e) -> ((i, expected_sent_parser from e body), create_sent_parser i e)) $ 
                zip emails [0..]

    expected_sent_parser from to bet_data = O (to, Challenge from bet_data)

    create_sent_parser i email = do 
        Challenge from body <- create_sent_parser' email

        return (i, SentChallenge )

    is_challenge x = True

    create_sent_parser' email = token show posFromTok testTok
        where
          testTok (_, y@(x, m)) = if x == email && is_challenge m then Just y else Nothing
          posFromTok (pos,t)  = pos


    permutation_parser :: (Monad m, Stream Payloads m Payload) =>  
                          ([(Int, Payload)], [ParsecT Payloads u m (Int, SentChallenge)])
                       -> ParsecT Payloads u m [Potential SentChallenge] 
    permutation_parser (defaults, parsers) = do
         parsed_values <- many1 $ choice parsers
         return $ replace_defaults defaults parsed_values

    replace_defaults :: [(Int, Payload)] -> [(Int, SentChallenge)] -> [Potential SentChallenge]
    replace_defaults defaults parsed_values = undefined

    -}


    -- first try each one 
    -- if any succeeds keep going
    -- if they all fail stop
    -- continue return if 

    -- I am building a option parser
    -- with a bunch of optional parsers that 
    -- that use the email to generate the 
    -- default parser
    {-

    create_sent_parsers :: Emails -> ParsecT Payload u m [Potential SentChallenge] 
    create_sent_parsers emails = permute $ foldl (<|?>) 
        (listify <$?> (create_default $ head emails, create_send_parser $ head emails))
           map (\x -> (create_default x,             create_send_parser x))             $ tail emails where
            listify = undefined

    create_send_parser :: Email -> ParsecT Payload u m (Potential SentChallenge)
    create_send_parser x = undefined

    create_default :: Email -> Potential SentChallenge
    create_default x = undefined

    -}

    -- instance FromPayloads (Potential SentChallenge) where
    --    fromPayloads = do




    {-

    There seems like there should be some sort grammar if there is a parser
    I think there 

    There is 
        There is a response dag
        You parse the dag
        and then there is a step from one tree to the next
        it converges

    its like I want a reverse parser
    I want all of the parts that don't parse
    and I want to complete them with what there should be 

    -}

    {-
    stimulus_response              
    stimulus_response u (U Accepted) = \bet_env -> (find_ancestors u, ParticipantAccepted u)
    stimulus_response u (Created String Emails) = 
    stimulus_response u BetForwarded Emails
    stimulus_response u BetCompleted Position 
    stimulus_response u BetCompleted Position =
    -}
    
    
    
    
    
    
what is the difference between the stream and the tree data?

In the stream you have ids that link to other nodes
wait

this how you should think of it
the stream is a context
or something similar

if it is context, what is the graph and what is the 
    












    
    