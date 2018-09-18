module Main where

import Language.Haskell.Parser
import Language.Haskell.Pretty

main :: IO ()
main
 = do putStrLn "Welcome to hreq"
      modstr <- readFile "examples/SimpleF.hs"
      putStrLn ("Module text:\n\n"++modstr)
      case parseModule modstr of
       ParseFailed loc str
        -> putStrLn (unlines [show loc, str ])
       ParseOk hsmod
        -> do putStrLn "Module AST:\n"
              let aststr = show hsmod
              putStrLn aststr
              writeFile "examples/SimpleF.ast" aststr
              putStrLn "\nIsn't it pretty?\n"
              putStrLn (prettyPrint hsmod)
              putStrLn "Goodbye"
