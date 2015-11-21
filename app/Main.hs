module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import qualified System.Console.GetOpt as Opt

import Data.List (find)

data CliOption = Help | Version deriving (Eq)

options :: [Opt.OptDescr CliOption]
options =
 [ Opt.Option ['h'] ["help"] (Opt.NoArg Help) "Print usage"
 , Opt.Option ['v'] ["version"] (Opt.NoArg Version) "Print version information"
 ]

data CliCommand = Usage | FakeProvider
getCommand :: [String] -> Maybe CliCommand
getCommand [] = Just Usage
getCommand (x:_)
  | x == "fake-provider" = Just FakeProvider
  | otherwise            = Nothing


main :: IO ()
main = do
  args <- getArgs
  let (opts, rest, errors) = Opt.getOpt Opt.Permute options args

  if errors == [] then return () else putStrLn (printOptionFail errors) >> exitFailure

  case (find (== Help) opts) of (Just _) -> usage >> exitSuccess
                                _ -> return ()
  case (find (== Version) opts) of (Just _) -> version >> exitSuccess
                                   _ -> return ()

  let command = getCommand rest
  case command of (Just Usage)        -> usage
                  (Just FakeProvider) -> fakeProvider
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
    cmds = "\nCommands:\n    fake-provider\tStart a provider mock service\n\n"

version :: IO ()
version = putStrLn "manners version 0.1.0.0"

fakeProvider :: IO ()
fakeProvider = putStrLn "success"