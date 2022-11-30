module Game.Sxako.Cli.Brick.ChessBomb (
  GameInfo,
  getGameInfo,
) where

import Control.Exception.Safe
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Network.HTTP.Client

{-
  ChessBomb API for getting position from broadcast games.
 -}

type GameInfo = ((T.Text, T.Text), UTCTime, T.Text)

getGameInfo :: Manager -> String -> IO (Either String GameInfo)
getGameInfo mgr eventPath =
  catchAnyDeep (getGameInfo' mgr eventPath) (\e -> pure $ Left (displayException e))

getGameInfo' :: Manager -> String -> IO (Either String GameInfo)
getGameInfo' mgr eventPath = do
  reqPre <- parseRequest $ "https://nxt.chessbomb.com/events/api/game/" <> eventPath
  let req =
        setRequestCheckStatus $
          reqPre
            { method = "POST"
            , requestBody = "{\"markerAnalysis\":999999999}"
            }
  resp <- httpLbs (setRequestCheckStatus req) mgr
  case eitherDecode' @Object (responseBody resp) of
    Left err -> pure $ Left $ "JSON parse error: " <> show err
    Right v0
      | Just (Object game) <- KM.lookup "game" v0
        , Just (String rawFen) <- KM.lookup "currentFEN" game
        , Just (String rawTime) <- KM.lookup "updateAt" game
        , Just (Object wp) <- KM.lookup "white" game
        , Just (String wn) <- KM.lookup "name" wp
        , Just (Object bp) <- KM.lookup "black" game
        , Just (String bn) <- KM.lookup "name" bp ->
        do
          t <- iso8601ParseM @IO @UTCTime (T.unpack rawTime)
          pure $ Right ((wn, bn), t, rawFen)
      | otherwise -> pure $ Left "JSON: unexpected structure"
