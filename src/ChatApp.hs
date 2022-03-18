{-
Erblin Marku
This is a Chat application simulator
-}

module ChatApp (ChatSystem) where

--these are the external libraries which have been imported for this app
import Control.Applicative 
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Network
import System.Environment (lookupEnv)
import System.IO (hSetBuffering, BufferMode (LineBuffering), hClose)
import Text.Printf (printf)

--this is the client module which has been imported to the main executable system
import ClientSystem

data State = State { user :: TVar (Int) }

serverVar :: IO State
serverVar = State <$> (newTVarIO 0)

--In this function the client joins the ChatSystem and message sending and reception is also done here
talkVar :: ClientSystem -> IO ()
talkVar client@ClientSystem { username = u
                            , handle = h 
                            , channel = c
                            } = do
    hSetBuffering h LineBuffering
    atomically $ writeTChan c (Update $ (show u) ++ " has joined")
    _ <- race (issuer client) (receiver client)
    return ()

--when the client leaves the ChatSystem, this section broadcasts the closure of the client handle.
haltVar :: ClientSystem -> IO ()
haltVar ClientSystem { username = u
                        , handle = h 
                        , channel = c
                        } = do
    atomically $ writeTChan c (Update $ (show u) ++ " has left")
    hClose h


--this section is used for multi-threading implementation and 
--showing the server state by getting the recent messages form the server.

mtVar :: State -> TChan (Message) -> Socket -> IO ()
mtVar server@State {user = us} c s = do
    (hdl, host, port) <- accept s
    _ <- printf "Accepted connection from %s: %s\n" host (show port)
    newchan <- atomically $ dupTChan c
    atomically $ modifyTVar us (+ 1)
    newname <- atomically $ readTVar us
    client <- mkClient newname hdl newchan
    _ <- forkFinally (talkVar client) (\_ -> haltVar client)
    mtVar server newchan s


-- This is where the ChatSystem server begins to operate. 
--CHAT_SERVER_PORT is searched at this point where 5000 default is used

chatSystem :: IO ()
chatSystem = withSocketsDo $ do
    _ <- printf "Default port set to 5000\n"
    port <- findPort
    server <- serverVar
    _ <- printf "listening on Port: %d\n" (toInteger port)
    chan <- newTChanIO
    bracket (listenOn (PortNumber port)) (sClose) (mtVar server chan)
    return ()

findPort :: IO PortNumber
findPort = do
    chatport <- lookupEnv "CHAT_SERVER_PORT"
    let port = case chatport of
                                Nothing  -> 5000
                                Just str -> fromIntegral (read str :: Int)
    return port
