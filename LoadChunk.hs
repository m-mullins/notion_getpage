{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}

module LoadChunk where

import Control.Lens
import Network.HTTP.Client (createCookieJar, Cookie(..))
import Network.Wreq
import Data.Aeson 
import Data.Aeson.Types
import Data.Aeson.Lens
import GHC.Generics
import Data.Void
import Data.Maybe
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import Data.Typeable
import Data.Map.Strict as DM
import Data.Text
import Text.Regex
import Configuration.Dotenv
import System.Environment
import Data.Set as Set
import Data.ByteString.Char8 as Char8

data Cursor = Cursor { stack::[String] }
              deriving (Show, Generic)
instance ToJSON Cursor

data ChunkRequest = ChunkRequest { pageId::String,
                                   limit::Int,
                                   cursor::Cursor,
                                   chunkNumber::Int,
                                   verticalColumns::Bool }
                        deriving (Show, Generic)
instance ToJSON ChunkRequest

getApi :: String -> String
getApi = (++) "https://www.notion.so/api/v3/"

createChunkRequest :: String -> Value
createChunkRequest pageId =
  toJSON $ ChunkRequest pageId 100000 (Cursor []) 0 False
        
toDashedPageId :: String -> String
toDashedPageId pageId =
  let
    rx = mkRegex "(\\w{8})\\-?(\\w{4})\\-?(\\w{4})\\-?(\\w{4})\\-?(\\w{12})"
  in
    case (matchRegex rx pageId) of
      Just s -> subRegex rx pageId "\\1-\\2-\\3-\\4-\\5"
      Nothing -> error $ "Invalivd Page ID:" ++ pageId


loadChunk apiKey pageId = do
   let cookieH = Char8.pack $ "token_v2=" ++ apiKey ++ ";"
   let opts = defaults & header "cookie" .~ [cookieH]
   let chunkReq = createChunkRequest $ toDashedPageId pageId
   postWith opts (getApi "loadPageChunk") chunkReq

isPage ::  DM.Map Text Value -> Bool
isPage mp =
   case (DM.lookup "type" mp) of
     Just (String s) -> s == "page"
     _ -> False
 
objectToList :: Value -> [(Text, Value)]
objectToList (Object x) = HM.toList x
objectToList _ = []

objectToMap :: Value -> DM.Map Text Value
objectToMap v = DM.fromList $ objectToList v

blockIsPage :: [(Text,Value)] -> [(Text,Value)]
blockIsPage =
    Prelude.filter (isPage . objectToMap .
                    fromJust . DM.lookup "value" . objectToMap . snd)

findPageBlocks resp =
    let
      response = resp ^? responseBody . key "recordMap" . key "block" 
    in
      Prelude.map fst $ blockIsPage $ objectToList $ fromJust response

main = do
    _ <- loadFile defaultConfig
    pageId <- getEnv "listPageId"
    apiKey <- getEnv "notionToken"
    let apiPageId = toDashedPageId pageId
    r <- loadChunk apiKey pageId
    let pages = Set.fromList $ Prelude.filter ((/=) $ Data.Text.pack apiPageId) $ findPageBlocks r
    print pages
