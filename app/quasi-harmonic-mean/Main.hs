-- | Yeah
module Main where

import Quasi (simulate)

import Options.Applicative
import Data.List (intercalate)


-- Command-line options

data QuasiMonteCarloOptions = QuasiMonteCarloOptions
  { number :: Int
  , method :: Char
  , is_pipelining :: Bool
  }

buffonOptionsParser :: Parser QuasiMonteCarloOptions
buffonOptionsParser =
  QuasiMonteCarloOptions
    <$> option
      auto
      ( long "number"
          <> short 'n'
          <> metavar "INT"
          <> help "Number of iterations"
      )
    <*> option
      readMethod
      ( metavar "METHOD"
          <> short 'M'
          <> long "method"
          <> help "Method to use: A, B, C, D"
      )
    <*> switch
      ( long "csv"
          <> short 'c'
          <> help "prints csv"
      )

mainArgParse :: IO QuasiMonteCarloOptions
mainArgParse = execParser opts
  where
    opts = info
      (buffonOptionsParser <**> helper)
      (fullDesc
        <> progDesc "Comparison: Pseudo random number generation vs Quasi-random number generation"
        <> header "pseudo-quasi comparison"
      )

readMethod :: ReadM Char
readMethod = eitherReader $ \arg ->
  case arg of
    [c] | c `elem` "ABCD" -> Right c
    _ -> Left "Method must be 'A', 'B', 'C', or 'D'."

-- Main


main :: IO ()
main = do
  options <- mainArgParse
  simulate (number options)
