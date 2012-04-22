module WeatherApi (WeatherApiHandler(..)
                  ,Config(..)
                  ,Weather(..)
                  ,getWeather
                  ,getWeather'
                  ,mkWeatherHandler
                  ,isHandlerAlive
                  ,closeHandler) where

import Data.Pool
import Network.HTTP
import Network.URI

data Config = Config { apiHost  :: String
                     , apiPort  :: Int
                     , queryFun :: (HandleStream String) ->
                                  String                ->
                                      IO (Either String Weather)
                     }

data WeatherApiHandler = WeatherApiHandler
    { stream :: IO (HandleStream String)
    , config :: Config
    }

data Weather = Weather { tempF, tempC  :: Double
                       , humidity      :: String
                       , windCondition :: String
                       , condition     :: String
                       } deriving (Eq, Show)

mkWeatherHandler c@(Config apiHost apiPort queryFun) =
  WeatherApiHandler { stream = openStream apiHost apiPort
                    , config = c
                    }

isHandlerAlive (WeatherApiHandler stream (Config h p _)) =
    stream >>= \s -> isTCPConnectedTo s (EndPoint h p)

closeHandler (WeatherApiHandler stream _) = stream >>= closeQuick

getWeather (WeatherApiHandler stream c) city =
    stream >>= \s -> (queryFun c) s city

getWeather' (Config apiHost apiPort queryFun) city =
    openStream apiHost apiPort >>= \s -> queryFun s city


