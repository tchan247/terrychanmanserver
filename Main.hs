{-|
    Copyright © 2014–2015 Miëtek Bak.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and 
    associated documentation files (the “Software”), to deal in the Software without restriction, 
    including without limitation the rights to use, copy, modify, merge, publish, distribute, 
    sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or 
    substantial portions of the Software.

    The Software is provided “as is”, without warranty of any kind, express or implied, including but 
    not limited to the warranties of merchantability, fitness for a particular purpose and 
    noninfringement. In no event shall the authors or copyright holders be liable for any claim, 
    damages or other liability, whether in an action of contract, tort or otherwise, arising from, out 
    of or in connection with the software or the use or other dealings in the software.

    Except as contained in this notice, the names of the above copyright holders shall not be used in 
    advertising or otherwise to promote the sale, use or other dealings in the Software without prior 
    written authorization.
-}

{-# LANGUAGE TypeOperators, DataKinds, KindSignatures #-}

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T


newtype Note = Note
    { contents :: Text
    }
  deriving (Generic, Show)

instance FromJSON Note
instance ToJSON Note


emptyNotes :: IO (TVar [Note])
emptyNotes =
    newTVarIO []

getNotes :: MonadIO m => TVar [Note] -> m [Note]
getNotes notes =
    liftIO $ readTVarIO notes

postNote :: MonadIO m => TVar [Note] -> Note -> m [Note]
postNote notes note =
    liftIO $ do
      T.putStrLn $ contents note
      atomically $ do
        oldNotes <- readTVar notes
        let newNotes = note : oldNotes
        writeTVar notes newNotes
        return newNotes


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody Note :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

server :: Text -> TVar [Note] -> Server NoteAPI
server home notes =
         return home
    :<|> getNotes notes
    :<|> postNote notes


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Test" T.pack $
                 lookup "TEST_HOME" env
    notes <- emptyNotes
    run port $ serve noteAPI $ server home notes
