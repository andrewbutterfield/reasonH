module Main where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import REPL

main :: IO ()
main
 = do putStrLn $ show $ idParse "Welcome to hreq"
      repl


data HReqState
  = HReq { hmod :: Maybe HsModule
         }
  deriving Show

hreqs0 = HReq Nothing

type HReqCmd       =  REPLCmd      HReqState
type HReqCmdDescr  =  REPLCmdDescr HReqState
type HReqExit      =  REPLExit     HReqState
type HReqCommands  =  REPLCommands HReqState
type HReqConfig    =  REPLConfig   HReqState

hreqPrompt :: Bool -> HReqState -> String
hreqPrompt _ _ = ": "

hreqEOFreplacmement = [nquit]

hreqParser = wordParse

hreqQuitCmds = [nquit] ; nquit = "q"

hreqQuit :: HReqExit
hreqQuit _ hreqs = putStrLn "\nGoodbye!\n" >> return (True, hreqs)

hreqHelpCmds = ["?"]

hreqCommands :: HReqCommands
hreqCommands = [ cmdLoad ]

-- we don't use these features in the top-level REPL
hreqEndCondition _ = False
hreqEndTidy _ hreqs = return hreqs

hreqConfig
  = REPLC
      hreqPrompt
      hreqEOFreplacmement
      hreqParser
      hreqQuitCmds
      hreqQuit
      hreqHelpCmds
      hreqCommands
      hreqEndCondition
      hreqEndTidy

repl :: IO ()
repl
  = do runREPL hreqWelcome hreqConfig hreqs0
       return ()

hreqWelcome = unlines
 [ "Welcome to hreq"
 , "Type '?' for help."
 ]

cmdLoad :: HReqCmdDescr
cmdLoad
  = ( "ld"
    , "load Haskell source"
    , unlines
        [ "ld <fname>  -- load examples/<fname>.hs"
        ]
    , loadSource )

loadSource [] hreqs = putStrLn "no file given" >> return hreqs
loadSource (fnroot:_) hreqs
  = do  let fname = fnroot ++ ".hs"
        modstr <- readFile ("examples/"++fname)
        putStrLn ("Module text:\n\n"++modstr)
        let result = parseModuleWithMode (ParseMode fname) modstr
        case result of
         ParseFailed loc str
          -> putStrLn (unlines [show loc, str ]) >> return hreqs
         ParseOk hsmod
          -> do putStrLn "Module AST:\n"
                let aststr = show hsmod
                putStrLn aststr
                writeFile ("examples/"++fnroot++".ast") aststr
                putStrLn "\nIsn't it pretty?\n"
                putStrLn (prettyPrint hsmod)
                return hreqs{ hmod = Just hsmod }
