module Main where

import Language.Haskell.Parser
import Language.Haskell.Pretty

main :: IO ()
main
 = do putStrLn "Welcome to hreq"
      let modstr = unlines [ "module X where"
                           , ""
                           , "f x  =  x + 1"
                           , ""
                           , "prop_f0       =  f 0 == 1"
                           , "prop_f1       =  f 1 == 2"
                           , "prop_f' x     =  f x > x"
                           , "prop_f_inv x  =  f (x-1) == x"
                           ]
      putStrLn ("Module text:\n\n"++modstr)
      case parseModule modstr of
       ParseFailed loc str
        -> putStrLn (unlines [show loc, str ])
       ParseOk hsmod
        -> do putStrLn "Module AST:\n"
              putStrLn $ show hsmod
              putStrLn "\nIsn't it pretty?\n"
              putStrLn (prettyPrint hsmod)
              putStrLn "Goodbye"
