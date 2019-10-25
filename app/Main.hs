{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import           Twitch
import           Data.Sort
import           Data.Aeson
import           GHC.Generics
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar.Strict

import           Data.Aeson.Encode.Pretty       ( encodePretty )
import           Network.Mime                   ( defaultMimeLookup )
import           Control.Monad                  ( unless )
import           System.Directory               ( doesFileExist )
import           System.FilePath.Posix          ( takeFileName )
import           Data.String.Conversions        ( cs )
import           Control.Concurrent.Async       ( concurrently_ )

import           Data.ByteString               as B
import           Data.ByteString.Lazy          as BL

import qualified Data.Bimap                    as Map
import qualified Web.Scotty                    as Scotty
import qualified Data.Text.Lazy                as TL
import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.ByteString.Base16.Lazy   as Hex

type Mapping = Map.Bimap Text Text

type Text = TL.Text

data Pair = Pair {
  file :: Text
, hash :: Text
} deriving (Generic, Show)

instance FromJSON Pair

instance ToJSON Pair where
  toEncoding = genericToEncoding defaultOptions

main :: IO ()
main = do
  f <- catch (BL.readFile "mappings.json") newEmptyJSON
  m <- newMVar $ unmarshal f
  concurrently_ (defaultMain $ "files/*" |> \s -> modifyMapping m s)
                (Scotty.scotty 4242 $ Scotty.get "/:id" $ findFile m)

newEmptyJSON :: IOError -> IO BL.ByteString
newEmptyJSON _ = do
  BL.writeFile "mappings.json" "[]"
  pure "[]"

modifyMapping :: MVar Mapping -> FilePath -> IO ()
modifyMapping v s = do
  f <- BL.readFile s
  let h = SHA256.hashlazy f
  let x = cs $ Hex.encode $ BL.fromStrict h :: Text
  m <- takeMVar v
  let m' = Map.insert (cs s) x m
  marshalWrite "mappings.json" m'
  putMVar v m'

findFile :: MVar Mapping -> Scotty.ActionM ()
findFile v = do
  m <- liftIO $ readMVar v
  i <- Scotty.param "id"
  f <- liftIO $ fetch Map.lookupR m i
  e <- liftIO $ doesFileExist $ cs f
  unless e $ liftIO $ cleanMapping f v
  serveFileOrNone e f

serveFileOrNone :: Bool -> TL.Text -> Scotty.ActionM ()
serveFileOrNone True f = do
  Scotty.file $ cs f
  Scotty.setHeader "Content-Type" $ cs $ defaultMimeLookup $ TL.toStrict f
  Scotty.setHeader "Content-Disposition"
    $ TL.concat ["inline; filename=\"", cs $ takeFileName $ cs f, "\""]
serveFileOrNone _ _ = Scotty.text "File not found"

fetch :: (Text -> Mapping -> IO Text) -> Mapping -> Text -> IO Text
fetch f m k = catch (f k m) handle
 where
  handle :: SomeException -> IO Text
  handle _ = pure "File not found"

cleanMapping :: Text -> MVar Mapping -> IO ()
cleanMapping p v = do
  m <- takeMVar v
  let m' = Map.delete (cs p) m
  marshalWrite "mappings.json" m'
  putMVar v m'

marshalWrite :: FilePath -> Mapping -> IO ()
marshalWrite p m = BL.writeFile p $ encodePretty $ sortBy s $ Map.fold f [] m
 where
  f :: Text -> Text -> [Pair] -> [Pair]
  f a b c = Pair a b : c
  s :: (Pair -> Pair -> Ordering)
  s a b | (file a) < (file b) = LT
        | otherwise           = GT

unmarshal :: BL.ByteString -> Mapping
unmarshal s = case decode s :: Maybe [Pair] of
  Just p -> insertAll p Map.empty
  _      -> Map.empty

insertAll :: [Pair] -> Mapping -> Mapping
insertAll (p : ps) m = insertAll ps $ Map.insert (file p) (hash p) m
insertAll _        m = m
