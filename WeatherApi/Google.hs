{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module WeatherApi.Google(Config(..)
                        , WeatherApiHandler
                        , Weather(..)
                        , initApi
                        , weather
                        ) where

import Text.XML.HXT.Core
import Network.HTTP
import Network.URI

apiUrl = "http://www.google.com/ig/api?"

data Config = Config { language :: String
                     , encoding :: String
                     } deriving (Show)

defaultConfig = Config { language = "en", encoding = "utf-8" }

data WeatherApiHandler = WeatherApiHandler (String -> String)

data Weather = Weather { tempF, tempC  :: Double
                       , humidity      :: String
                       , windCondition :: String
                       , condition     :: String
                       } deriving (Eq, Show)

initApi :: Config -> WeatherApiHandler
initApi config =
    let params = [("hl", language config), ("oe", encoding config)]
        urn    = \c -> urlEncodeVars $ params ++ [("weather", c)]
    in WeatherApiHandler urn

retrieve urn =
    case parseURI $ apiUrl ++ urn of
      Nothing  -> return $ Left "Invalid URL"
      Just uri -> get uri

get uri =
    do
      eresp <- simpleHTTP (Request uri GET [] "")
      case eresp of
        Left err  -> return $ Left $ show err
        Right res -> return $ Right $ rspBody res

atTag tag = deep (isElem >>> hasName tag)
dataAtTag tag = atTag tag >>> getAttrValue "data"

getWeather = atTag "current_conditions" >>>
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

parseXML doc = readString [ withValidate no
                          , withRemoveWS yes
                          ] doc

weather (WeatherApiHandler h) city =
    do
      resp <- retrieve $ h city
      xml  <- return $ (resp >>= return . parseXML)
      case xml of
        Left a  -> return $ Left a
        Right a -> do
                 r <- runX(a >>> getWeather)
                 case r of
                   [] -> return $ Left "can't retrieve weather"
                   (x:xs) -> return $ Right x
