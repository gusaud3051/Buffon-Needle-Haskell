module Main (main) where

import Buffon (runBuffonA, runBuffonB, runBuffonC)
import Options.Applicative
import Data.List (intercalate)

-- Command-line options

data BuffonOptions = BuffonOptions
  { number :: Int
  , method :: Char
  , is_pipelining :: Bool
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
          <> help "Method to use: 'A'(mine) or 'B'(inspired by someone's implementation) or 'C'(mine, but faster due to absense of stdout)"
      )
    <*> switch
      ( long "Pipelining"
          <> short 'p'
          <> help "Pipelining mode: prints with following format: Method number estimated_pi. Using with A will automatically change the method to B."
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
    'A' -> (if (is_pipelining options) then runBuffonB else runBuffonA) (number options)
    'B' -> runBuffonB (number options)
    'C' -> runBuffonC (number options)
    _ -> error "Invalid method"
  let format = if (is_pipelining options) then intercalate ", " . map ($ options) $ [return . method, show . number, const $ show result] else "Estimated pi: " <> show result
  putStrLn format
 where
  opts =
    info
      (buffonOptionsParser <**> helper)
      ( fullDesc
          <> progDesc "Estimate pi using Buffon's needle problem"
          <> header "Buffon Simulator"
      )
