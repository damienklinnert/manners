module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import qualified System.Console.GetOpt as Opt

import Control.Monad (when)

import qualified Service
import qualified Runner

data CliOption = Help | Version deriving (Eq)

options :: [Opt.OptDescr CliOption]
options =
 [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Print usage"
 , Opt.Option ['v'] ["version"] (Opt.NoArg Version) "Print version information"
 ]

data CliCommand = Usage | FakeProvider | FakeConsumer
getCommand :: [String] -> Maybe CliCommand
getCommand [] = Just Usage
getCommand (x:_)
  | x == "fake-provider" = Just FakeProvider
  | x == "fake-consumer" = Just FakeConsumer
  | otherwise            = Nothing


main :: IO ()
main = do
  (opts, rest, errors) <- Opt.getOpt Opt.Permute options <$> getArgs

  when (errors /= []) $ putStrLn (printOptionFail errors) >> exitFailure

  when (elem Help opts) $ usage >> exitSuccess
  when (elem Version opts) $ version >> exitSuccess

  let command = getCommand rest
  case command of (Just Usage)        -> usage
                  (Just FakeProvider) -> fakeProvider
                  (Just FakeConsumer) -> fakeConsumer $ tail rest
                  Nothing             -> putStrLn (printCommandFail rest) >> exitFailure

printOptionFail :: [String] -> String
printOptionFail errors = ((foldl (++) "" (map ("manners: " ++) errors)) ++ "See 'manners --help'.")

printCommandFail :: [String] -> String
printCommandFail rest = ("manners: '" ++ (head rest) ++ "' is not a manners command\nSee 'manners --help'.")

usage :: IO ()
usage = putStrLn $ opts ++ cmds
  where
    header = "Usage: manners [OPTIONS] COMMAND [arg...]\n\nGet your services behaved.\n\nOptions:"
    opts = Opt.usageInfo header options
    cmds = "\nCommands:\n    fake-provider\tStart a provider mock service\n    fake-consumer\tRun a fake consumer\n\n"

version :: IO ()
version = putStrLn "manners version 0.4.1.0"

fakeProvider :: IO ()
fakeProvider = Service.runProviderService 1234

fakeConsumer :: [String] -> IO ()
fakeConsumer [contract, url] = Runner.runContract contract url
fakeConsumer _ = do
  putStrLn "manners: Invalid number of arguments"
  putStrLn "Usage: manners fake-consumer CONTRACT_PATH PROVIDER_URL"
  exitFailure