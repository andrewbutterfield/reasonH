module Main where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import REPL
import AST
import Matching
import HParse
import Theory

main :: IO ()
main
 = do putStrLn $ show $ idParse "Welcome to hreq"
      repl


repl :: IO ()
repl
  = do runREPL hreqWelcome hreqConfig hreqs0
       return ()

hreqWelcome = unlines
 [ "Welcome to hreq"
 , "Type '?' for help."
 ]

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


data HReqState
  = HReq { hmod :: Maybe Mdl
         , hthry :: Maybe Theory
         }
  deriving Show

hreqs0 = HReq Nothing Nothing

type HReqCmd       =  REPLCmd      HReqState
type HReqCmdDescr  =  REPLCmdDescr HReqState
type HReqExit      =  REPLExit     HReqState
type HReqCommands  =  REPLCommands HReqState
type HReqConfig    =  REPLConfig   HReqState

hreqPrompt :: Bool -> HReqState -> String
hreqPrompt _ _ = "hprover> "

hreqEOFreplacmement = [nquit]

hreqParser = wordParse

hreqQuitCmds = [nquit] ; nquit = "q"

hreqQuit :: HReqExit
hreqQuit _ hreqs = putStrLn "\nGoodbye!\n" >> return (True, hreqs)

hreqHelpCmds = ["?"]


-- we don't use these features in the top-level REPL
hreqEndCondition _ = False
hreqEndTidy _ hreqs = return hreqs

hreqCommands :: HReqCommands
hreqCommands = [ cmdLoadHaskell, cmdLoadTheory ]

cmdLoadHaskell :: HReqCmdDescr
cmdLoadHaskell
  = ( "lh"
    , "load Haskell source"
    , unlines
        [ "lh <fname>  -- load examples/<fname>.hs"
        ]
    , loadSource )

loadSource [] hreqs = putStrLn "no file given" >> return hreqs
loadSource (fnroot:_) hreqs
  = do  let fname = fnroot ++ ".hs"
        modstr <- readFile ("examples/"++fname)
        putStrLn ("Module text:\n\n"++modstr)
        mdl <- parseHModule fname modstr
        putStrLn "Module AST:\n"
        let aststr = show mdl
        putStrLn aststr
        writeFile ("examples/"++fnroot++".ast") aststr
        return hreqs{ hmod = Just mdl }


cmdLoadTheory :: HReqCmdDescr
cmdLoadTheory
  = ( "lt"
    , "load Theory source"
    , unlines
        [ "lt <fname>  -- load examples/<fname>.thr"
        ]
    , loadTheory )

loadTheory [] hreqs = putStrLn "no file given" >> return hreqs
loadTheory (fnroot:_) hreqs
  = do  let fname = fnroot ++ ".thr"
        thrystr <- readFile ("examples/"++fname)
        putStrLn ("Theory text:\n\n"++thrystr)
        let result = parseTheory (ParseMode fname) thrystr
        case result of
         ParseFailed loc msg
          -> do putStrLn "Theory parse failed"
                putStrLn $ show loc
                putStrLn msg
                return hreqs
         ParseOk theory
          -> do putStrLn "Theory AST:\n"
                let aststr = show theory
                putStrLn aststr
                return hreqs{ hthry = Just theory }
