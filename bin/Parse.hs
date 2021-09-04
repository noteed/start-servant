{-# LANGUAGE ApplicativeDo #-}
module Parse
  ( parseConf
  , parseConfInMode 
  , parseFullConf 
  ) where

import qualified Prototype.Runtime as Rt 
import qualified Options.Applicative as A 

-- | Parse any configuration irrespective of the mode we're running in. 
parseConf :: A.Parser Rt.Conf
parseConf = do
  _cAppName <- appName
  _cLogLevel <- logLevel
  _cServerPort <- serverPort
  pure Rt.Conf {..}
  where
    appName = A.strOption
      $ A.long "application-name"
      <> A.short 'N'
      <> A.help "Application name, will be used in logs etc."
      <> A.metavar "STRING"
      <> A.value (Rt._cAppName def) 
      <> A.showDefault 

    logLevel = A.option (A.eitherReader readEither)
      $  A.long "log-level"
      <> A.short 'L'
      <> A.help "Logging level; eg. Level 0 = DEBUG etc."
      <> A.metavar "LOG_LEVEL"
      <> A.value (Rt._cLogLevel def) 
      <> A.showDefault 

    serverPort = A.option A.auto
      $  A.long "http-server-port"
      <> A.short 'P'
      <> A.help "Server port to fire up http serrver on."
      <> A.metavar "PORT"  
      <> A.value (Rt._cServerPort def) 
      <> A.showDefault 

-- | A sum type indicating which mode the user has indicated to run the application in.
data ConfInMode =
  ConfStmMode Rt.Conf
  | ConfPostgresMode Rt.Conf 
  deriving Show 

-- | Parse the configuration, along with a user-indicated mode. 
parseConfInMode :: A.Parser ConfInMode 
parseConfInMode =
  A.subparser $ stmMode <> postgresMode 
  where
    stmMode = A.command "stm" $ A.info (ConfStmMode <$> parseConfHelper) $ A.progDesc "Run in STM mode"  
    postgresMode = A.command "postgres" $ A.info (ConfPostgresMode <$> parseConfHelper) $ A.progDesc "Run in Postgres mode"  
    parseConfHelper = parseConf <**> A.helper 

-- | Parse the entire configuration.
parseFullConf :: A.ParserInfo ConfInMode
parseFullConf = A.info (parseConfInMode <**> A.helper) ( A.fullDesc
                                                      <> A.progDesc "start-servant prototyping system"
                                                      <> A.header "Start Servant"
                                                       ) 
