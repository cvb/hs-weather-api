{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module WeatherApi.Google (initApi) where

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI
import WeatherApi
import Control.Monad (liftM)
import Codec.Binary.UTF8.String

apiUrl  = "http://www.google.com/ig/api?"

type Lang = String
type Enc  = String

-- | Make config for use with WeatherApi functions
initApi :: Lang -> Enc -> Config
initApi   lang   enc =
    let params = [("hl", lang), ("oe", enc)]
        urn c  = urlEncodeVars $ params ++ [("weather", encodeString c)]
    in Config { apiHost  = "www.google.com"
              , apiPort  = 80
              , queryFun = makeQueryFun urn
              }

retrieve s urn =
    case parseURI $ apiUrl ++ urn of
      Nothing  -> return $ Left $ NetworkError "Invalid URL"
      Just uri -> get s uri

get s uri =
    do
      eresp <- sendHTTP s (Request uri GET [] "")
      case eresp of
        Left err  -> return $ Left $ NetworkError $ show err
        Right res -> return $ Right $ rspBody res

atTag tag = deep (isElem >>> hasName tag)
dataAtTag tag = atTag tag >>> getAttrValue "data"

parseWeather = atTag "current_conditions" >>>
  proc x -> do
    tempF'         <- dataAtTag "temp_f"         -< x
    tempC'         <- dataAtTag "temp_c"         -< x
    humidity'      <- dataAtTag "humidity"       -< x
    windCondition' <- dataAtTag "wind_condition" -< x
    condition'     <- dataAtTag "condition"      -< x
    returnA -< Weather
      { tempF         = read tempF'
      , tempC         = read tempC'
      , humidity      = humidity'
      , windCondition = windCondition'
      , condition     = condition'
      }

parseXML = readString [ withValidate no
                      , withRemoveWS yes
                      ]

-- | This return function witch will actualy retrieve and parse weather from stream
makeQueryFun :: (String -> String) -> (HandleStream String) -> String -> IO ApiResponse
makeQueryFun q stream city =
    do
      resp <- retrieve stream $ q city
      case liftM parseXML resp of
        Left  a -> return $ Left a
        Right a -> do
          r <- runX(a >>> parseWeather)
          case r of
            []     -> return $ Left $ NotFoundError "can't retrieve weather"
            (x:xs) -> return $ Right x
