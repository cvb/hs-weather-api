-- | Usage:
--
-- required imports
--
-- > import WeatherApi
-- > import WeatherApi.Google
--
-- With handler in case server will alow you to make
-- few requests with one connection
--
-- >>> let h = mkWeatherHandler $ initApi "en" "utf-8"
-- >>> getWeather h "moscow"
-- Right (Weather { tempF = 75.0
--                , tempC = 24.0
--                , humidity = "Humidity: 25%"
--                , windCondition = "Wind: S at 16 mph"
--                , condition = "Clear"
--                })
--
-- Simple case
--
-- >>> getWeather' (initApi "en" "utf-8") "moscow"
-- Right (Weather { tempF = 75.0
--                , tempC = 24.0
--                , humidity = "Humidity: 25%"
--                , windCondition = "Wind: S at 16 mph"
--                , condition = "Clear"
--                })

module WeatherApi (WeatherApiHandler(..)
                  ,Config(..)
                  ,Weather(..)
                  ,ApiError(..)
                  ,ApiResponse(..)
                  ,getWeather
                  ,getWeather'
                  ,mkWeatherHandler
                  ,isHandlerAlive
                  ,closeHandler) where

import Network.HTTP
import Network.URI

data Config = Config { apiHost  :: String
                     , apiPort  :: Int
                     , queryFun :: HandleStream String ->
                                  String              ->
                                      IO (ApiResponse)
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

data ApiError    = NotFoundError String | NetworkError String deriving Show
type ApiResponse = Either ApiError Weather

mkWeatherHandler c@(Config apiHost apiPort queryFun) =
  WeatherApiHandler { stream = openStream apiHost apiPort
                    , config = c
                    }

isHandlerAlive (WeatherApiHandler stream (Config h p _)) =
    stream >>= \s -> isTCPConnectedTo s (EndPoint h p)

closeHandler (WeatherApiHandler stream _) = stream >>= closeQuick

-- | Retrieve weather using existing handler
getWeather (WeatherApiHandler stream c) city =
    stream >>= \s -> queryFun c s city

-- | Retrieve weather using just config
-- It's usefull when you don't need one connection for few requests
getWeather' (Config apiHost apiPort queryFun) city =
    openStream apiHost apiPort >>= \s -> queryFun s city


