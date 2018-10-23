module Main where

import Data.List

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
  = HReq { hmods :: [Mdl]
         , hthrys :: [Theory]
         , currThry :: Maybe Theory
         }
  deriving Show

hmods__ f hrs = hrs{ hmods = f $ hmods hrs} ; hmods_ h = hmods__ $ const h
hthrys__ f hrs = hrs{ hthrys = f $ hthrys hrs} ; hthrys_ h = hthrys__ $ const h
currThry__ f hrs = hrs{ currThry = f $ currThry hrs}
currThry_ h = currThry__ $ const h

hreqs0 = HReq [] [] Nothing

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
hreqCommands = [ cmdShowState
               , cmdShowLaws
               -- , cmdLoadHaskell -- deprecated for now.
               , cmdLoadTheory
               ]

cmdShowState :: HReqCmdDescr
cmdShowState
  = ( "ss"
    , "show state"
    , "show short summary of state contents"
    , showState )

showState _ hreqs
  = do showHModNames   $ hmods    hreqs
       showTheoryNames $ hthrys   hreqs
       showCurrThry    $ currThry hreqs
       return hreqs

showHModNames [] = putStrLn "No Haskell Modules"
showHModNames hms = putStrLn ("Haskell Modules: " ++ shlist (map mname hms))

showTheoryNames [] = putStrLn "No Required Theories"
showTheoryNames thrys
  = putStrLn ("Required Theories: "++ shlist (map theoryName thrys))

showCurrThry Nothing = putStrLn "No Current Theory"
showCurrThry (Just thry) = putStrLn ("Current Theory: "++theoryName thry)

shlist strs = intercalate ", " strs


cmdShowLaws :: HReqCmdDescr
cmdShowLaws
  = ( "ll"
    , "show 'law' names"
    , "ll -- show all 'law' names"
    , showLaws )

showLaws _ hreqs
  = do sequence_ $ map showHModLaws $ hmods hreqs
       sequence_ $ map showTheoryLaws $ hthrys hreqs
       case currThry hreqs of
         Nothing    -> putStrLn "No Current Theory"
         Just thry  -> do showTheoryLaws thry
                          showTheorems thry
       return hreqs

showHModLaws hmod
 = do putStrLn ("Laws in "++mname hmod)
      sequence_ $ map showDecl $ topdecls hmod

showDecl (Fun []) = putStrLn "  !dud Fun!"
showDecl (Fun (m:_))  =  putStrLn ("  " ++ fname m)
showDecl (Bind (Var n) _ _) = putStrLn ("  " ++ n)
showDecl _ = putStrLn "  ??"

showTheoryLaws thry
  = do putStrLn ("Laws in "++theoryName thry)
       sequence_ $ map showLaw $ thLaws thry

showLaw law = putStrLn ("  "++ lawName law)

showTheorems thry
  = do putStrLn ("Theorems in "++theoryName thry)
       sequence_ $ map showTheorem $ thTheorems thry

showTheorem thrm = putStrLn ("  "++ thmName thrm)

-- deprecated for now
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
  = do  mdl <- readHaskell fnroot
        putStrLn "Module AST:\n"
        let aststr = show mdl
        putStrLn aststr
        writeFile ("examples/"++fnroot++".ast") aststr
        return $ hmods__ (++[mdl]) hreqs

readHaskell fnroot
  = do let fname = fnroot ++ ".hs"
       modstr <- readFile ("examples/"++fname)
       parseHModule fname modstr

cmdLoadTheory :: HReqCmdDescr
cmdLoadTheory
  = ( "lt"
    , "load Theory source"
    , unlines
        [ "lt <fname>  -- load examples/<fname>.thr"
        , " -- also loads all haskell modules and theories that it imports"
        ]
    , loadTheory )

loadTheory [] hreqs = putStrLn "no file given" >> return hreqs
loadTheory (fnroot:_) hreqs
  = do theory <- readTheory fnroot
       putStrLn "Theory AST:\n"
       let aststr = show theory
       putStrLn aststr
       loadDependencies theory hreqs

readTheory fnroot
  = do let fname = fnroot ++ ".thr"
       thrystr <- readFile ("examples/"++fname)
       let result = parseTheory (ParseMode fname) thrystr
       case result of
         ParseFailed loc msg
          -> do putStrLn "Theory parse failed"
                putStrLn $ show loc
                putStrLn msg
                fail msg
         ParseOk theory
          -> return theory

loadDependencies theory hreqs
  = do hms <- loadModDeps $ hkImports theory
       ths <- loadThryDeps $ thImports theory
       return $ currThry_ (Just theory)
              $ hthrys_ ths
              $ hmods_ hms
              $ hreqs

loadModDeps []  = return []
loadModDeps (n:ns)
  = do m <- readHaskell n
       ms <- loadModDeps ns
       return (m:ms)

loadThryDeps [] = return []
loadThryDeps (t:ts)
  = do thry <- readTheory t
       thrys <- loadThryDeps ts
       return (thry:thrys)
