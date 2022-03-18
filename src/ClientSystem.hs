
{-
This is the ClientSystem in which we consctruct the User and Message
-}

module ClientSystem  where

import Control.Applicative
import Control.Monad (forever)
import Control.Concurrent.STM
import System.IO (hPutStrLn, hGetLine, Handle)

--This is the format of the message where the username and the messages are constructed
--this also checks to the server when the client leaves the ChatSystem and when they join the ChatSystem
data Message = Post Username String | Update String deriving (Show, Eq, Ord)

type Username = Int

--this shows the composition of the client identification
--can be identified by the broadcast,
--the channel or the username which is just but a Int.
data ClientSystem = 
    ClientSystem
    { username :: Username
    , handle :: Handle
    , channel :: TChan (Message)
    }

mkClient :: Username -> Handle -> TChan (Message) -> IO ClientSystem
mkClient x y z = 
     ClientSystem <$> return x <*> return y <*> (atomically (dupTChan z))
--found this way of using functor monad on strings with different lengths, so the brackets <> act in some kind of fzipWhith for example


messageSender :: ClientSystem -> IO ()
messageSender ClientSystem { username = x
                            , handle = y 
                            , channel = z
                            } = forever $ do
    line <- hGetLine y
    let messageValue = Post x line
    atomically $ writeTChan z messageValue

--Here, the message receipient gets the message check on a real time basis
messageDest :: ClientSystem -> IO ()
messageDest client@ClientSystem { channel = z} = forever $ do
      messageValue <- atomically $ readTChan z
      messageTrans client messageValue


messageTrans :: ClientSystem -> Message -> IO ()
messageTrans ClientSystem {username = x , handle = y}
    message = do
    hPutStrLn y $
        case message of
            Post user messageValue -> if (user /= x)
                then (show user) ++ ": " ++ messageValue
                else ""
            Update messageValue    ->  messageValue
