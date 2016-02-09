module Lib (
  someFunc
  , configBuilder
  , defaultConfig
  , profile'
  , goFaster'
  , extract
  , (&)
  , extend
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Option = String

data Config = MakeConfig [Option] deriving (Show)

configBuilder :: [Option] -> Config
configBuilder = MakeConfig

defaultConfig :: [Option] -> Config
defaultConfig options = MakeConfig $ ["-Wall"] ++ options

profile' :: ([Option] -> Config) -> ([Option] -> Config)
profile' builder =
  \options -> builder (["-prof", "-auto-all"] ++ options)

goFaster' :: ([Option] -> Config) -> ([Option] -> Config)
goFaster' builder =
  \options -> builder (["-O2"] ++ options)

extract :: ([Option] -> Config) -> Config
extract builder = builder []

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 0 &

extend :: (([Option] -> Config) ->              Config)
       -> ([Option] -> Config) -> ([Option] -> Config)
extend setter builder =
  \opts2 -> setter (\opts1 -> builder (opts1 ++ opts2))
