{-# LANGUAGE DeriveGeneric #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import System.Process
import Data.Aeson
import Data.ByteString.Lazy as BS
import Data.List as L
import GHC.Generics
import Control.Monad

data MockConfig = MockConfig
  { mockFile      :: Maybe FilePath
  , shouldProfile :: Bool
  , cmd           :: String
  }

data Mock = Mock
  { command :: String
  , output  :: String
  } deriving (Generic, Show)

instance ToJSON Mock where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Mock

mockConfig :: Parser MockConfig
mockConfig = MockConfig
  <$> optional (strOption
     ( long "mockFile"
    <> short 'm'
    <> metavar "FILE"
    <> help "Optional Map File for Mock Overrides" ))
  <*> switch
     ( long "profile"
    <> short 'p'
    <> help "Whether to print profiling information" )
  <*> strOption
     ( long "cmd"
    <> metavar "COMMAND"
    <> short 'c'
    <> help "Command Input")

main :: IO ()
main = performCmd =<< execParser opts
  where
    opts = info (mockConfig <**> helper)
       ( fullDesc
      <> progDesc "Intercept and shadow shell commands"
      <> header   "MockShell -- A middleware for mocking shell commands")

loadMocks :: FilePath -> IO (Maybe [Mock])
loadMocks fp = fmap decode (BS.readFile fp)

findMock :: String -> Maybe [Mock] -> Maybe Mock
findMock c mmocks  = mmocks >>= L.find ((==c) . Main.command)

profile :: String -> Maybe [Mock] -> IO ()
profile c mmocks = do
  Prelude.putStrLn $ "PROFILE :: cmd :: " <> c
  case mmocks of
    Nothing -> return ()
    Just mocks -> Prelude.putStrLn $ "PROFILE :: mocks :: " <> show mocks

performCmd :: MockConfig -> IO ()
performCmd MockConfig{mockFile=Nothing, cmd=c} = callCommand c
performCmd (MockConfig (Just fp) p c) = do
  mmocks <- loadMocks fp
  when p $ profile c mmocks
  case findMock c mmocks of
      Nothing -> callCommand c
      Just mock -> Prelude.putStr $ output mock