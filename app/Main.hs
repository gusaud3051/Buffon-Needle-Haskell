module Main (main) where

import Buffon (runBuffonA, runBuffonB, runBuffonC)
import Options.Applicative
import System.Random.Stateful (mkStdGen, newIOGenM)

-- Command-line options

data BuffonOptions = BuffonOptions
  { number :: Int
  , method :: Char
  }

buffonOptionsParser :: Parser BuffonOptions
buffonOptionsParser =
  BuffonOptions
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
          <> help "Method to use: 'A'(mine) or 'B'(other's) or 'C'(mine, but faster due to absense of stdout)"
      )

readMethod :: ReadM Char
readMethod = eitherReader $ \arg ->
  case arg of
    [c] | c `elem` "ABC" -> Right c
    _ -> Left "Method must be 'A', 'B', or 'C'"

-- Main

main :: IO ()
main = do
  options <- execParser opts
  result <- case method options of
    'A' -> runBuffonA (number options)
    'B' -> runBuffonB (number options)
    'C' -> runBuffonC (number options)
    _ -> error "Invalid method"
  putStrLn $ "Estimated pi: " <> show result
 where
  opts =
    info
      (buffonOptionsParser <**> helper)
      ( fullDesc
          <> progDesc "Estimate pi using Buffon's needle problem"
          <> header "Buffon Simulator"
      )
