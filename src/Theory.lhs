\section{Theory}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Theory where

import Data.Char

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Utilities

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}


\subsection{Theory Document Structure}

Typically a keyword at the start of a line introduces something.
We start with \texttt{THEORY} and zero or more imports:
\def\TOPSYNTAX{\texttt{
\\THEORY <TheoryName>
\\IMPORT-THEORY <Name>
\\IMPORT-HASKELL <Name>
}}

\TOPSYNTAX

These are followed by zero or more entries
that describe laws, induction schemes and theorems.

Laws are described by the following ``one-liner'' construct:
\def\LAWSYNTAX{\texttt{
\\LAW <name> <br?> <expr>
}}

\LAWSYNTAX

Here, \verb"<br?>" means that the following part
is either entirely on this line,
or else occupies a number of subsequent lines.
There can be a blank line before it,
and must be a blank line after it.
The following part itself must
not have blank lines embedded in it.

An induction-scheme is described by the following four lines:
\def\INDSCHEMASYNTAX{\texttt{
\\INDUCTION-SCHEME <Type>
\\BASE <value>
\\STEP <var> --> <expr>
\\INJ  <br?> ( <expr> )  ===  ( <expr> )
}}

\INDSCHEMASYNTAX

The parentheses in the last line seem to be necessary for now.

\newpage

A theorem has the following top-level structure:
\def\THEOREMSYNTAX{\texttt{
\\THEOREM <name>  <br?> <expr>
\\STRATEGY <strategy>
\\  <strategy-body>
\\QED <name>
}}

\THEOREMSYNTAX

Strategies include:
\def\ReduceAll{ReduceAll}
\def\ReduceLHS{ReduceLHS}
\def\ReduceRHS{ReduceRHS}
\def\ReduceBoth{ReduceBoth}
\def\STRATEGIES{\texttt{
\\\ReduceAll
\\\ReduceLHS
\\\ReduceRHS
\\\ReduceBoth
}}
\def\Induction{Induction}
\def\DOINDUCTION{\texttt{
\\\Induction <type1> <ind-var1> .. <typeN> <ind-varN>
}}
\def\SDOINDUCTION{\texttt{
\\STRATEGY Induction <type1> <ind-var1> .. <typeN> <ind-varN>
}}

\STRATEGIES
\DOINDUCTION

The choice of strategy will then determine the resulting structure:
\def\INDUCTIONSYNTAX{\texttt{
\\BASE <var1> = <val1> .. <varN> = <valN> <br!> <expr>
\\<one of the other four strategies>
\\QED BASE
\\STEP <var1> --> <expr1> .. <varN> --> <exprN>
\\ASSUME <br?> <expr>
\\SHOW <br?> <expr>
\\<one of the other four strategies>
\\QED STEP
}}
\def\REDBOTHSYNTAX{\texttt{
\\LHS
\\<calculation>
\\RHS
\\<calculation>
}}
\begin{description}
  \item [ReduceAll]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceLHS]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceRHS]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceBoth]~\\
   \REDBOTHSYNTAX
  \item [Induction]~\\
    \INDUCTIONSYNTAX
    \\Here, \verb"<br!>" is similar to \verb"<br?>",
    except that a line break at this point is mandatory.
\end{description}

A calculation is a sequence of formul\ae\ seperated by justification lines,
which always start with an equal sign. Blank lines are allowed
around justification lines.
\def\CALCSYNTAX{\texttt{
\\<expr1>
\\ = <justification1>
\\ ...
\\ = <justificationN>
\\<exprN+1>
}}

\CALCSYNTAX

\newpage
The justification format is as follows:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}



\newpage
\subsection{Datatypes}

\TOPSYNTAX \dots
\begin{code}
data Theory
 = THEORY {
     theoryName  :: String
   , thImports   :: [String]  -- Theory Names
   , hkImports   :: [String]  -- Haskell Module names
   , thLaws      :: [Law]
   , thIndScheme :: [InductionScheme]
   , thTheorems  :: [Theorem]
   }
 deriving Show

thImports__   f thry = thry{ thImports   = f $ thImports thry }
hkImports__   f thry = thry{ hkImports   = f $ hkImports thry }
thLaws__      f thry = thry{ thLaws      = f $ thLaws    thry }
thIndScheme__ f thry = thry{ thIndScheme = f $ thIndScheme thry }
thTheorems__  f thry = thry{ thTheorems  = f $ thTheorems thry }
\end{code}

\LAWSYNTAX
\begin{code}
data Law
 = LAW {
     lawName :: String
   , lawEqn :: HsExp
   }
 deriving Show
\end{code}

\INDSCHEMASYNTAX
\begin{code}
data InductionScheme
 = IND {
     indType :: String
   , indBase :: HsExp             -- base value
   , indStep :: (String, HsExp)   -- induction var to step expression
   , indInj :: (HsExp,HsExp)      -- bits equal whole
   }
 deriving Show
\end{code}

\THEOREMSYNTAX
\begin{code}
data Theorem
 = THEOREM {
     thmName :: String
   , strategy :: Strategy
   }
 deriving Show
\end{code}

\STRATEGIES
\begin{code}
data Strategy
 = ReduceAll Calculation
 | ReduceLHS Calculation
 | ReduceRHS Calculation
 | ReduceBoth Calculation Calculation
\end{code}
\SDOINDUCTION
\INDUCTIONSYNTAX
\begin{code}
 | Induction {
     iVars :: [(String,String)] -- type var
   , baseVals :: [(String,HsExp)] -- var = value
   , baseStrategy :: Strategy
   , steps :: [(String,HsExp)] -- var --> expr
   , assume :: HsExp
   , iGoal :: HsExp
   , stepStrategy :: Strategy
   }
 deriving Show
\end{code}

\CALCSYNTAX
\begin{code}
data Calculation
 = CALC {
     goal :: HsExp
   , calcs :: [(Justification,HsExp)]
   }
 deriving Show
\end{code}

\newpage
Justifications:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}
\begin{code}
data Justification
 = BECAUSE {
     jrel :: JRel
   , law :: JLaw
   , usage :: Usage
   , focus :: Focus
   }
 deriving Show
data JRel = JEq deriving (Eq, Show)
data JLaw = L String | D String Int | IH | CS | SMP deriving (Eq, Show)
data Usage = Whole | L2R | R2L deriving (Eq, Show)
data Focus = Top | At String Int deriving (Eq, Show)
\end{code}

\newpage
\subsection{Parser Top-Level}

A polymorphic, monadic parser type:
\begin{code}
type Line = (Int,String)
type Lines = [Line]
type Parser m a  = Lines -> m (a,Lines)
\end{code}
A \texttt{SrcLoc}-based monadic failure:
\begin{code}
pFail :: Monad m => ParseMode -> Int -> Int -> String -> m a
pFail pmode lno colno msg
  = fail (parseFilename pmode ++ ':':show lno++ ":"++show colno++" "++msg)
\end{code}


We start by adding in an ``empty'' theory as an accumulating
parameter,
breaking input into numbered lines
and starting the proper parsing.
\begin{code}
parseTheory :: ParseMode -> String -> ParseResult Theory
parseTheory pmode str
  = case theoryParser pmode theory0 $ zip [1..] $ lines str of
      But msgs  ->  ParseFailed (SrcLoc (parseFilename pmode) 1 1)
                      $ unlines' msgs
      Yes (thry,_) -> ParseOk thry

theory0 = THEORY { theoryName = "?", thImports = [], hkImports = []
                 , thLaws = [], thIndScheme = [], thTheorems = [] }
\end{code}
We start proper parsing by looking for \texttt{THEORY <TheoryName>}
on the first line:
\begin{code}
theoryParser :: Monad m => ParseMode -> Theory -> Parser m Theory
theoryParser pmode theory []
 = pFail pmode 0 0 "Empty file"
theoryParser pmode theory ((lno,str):lns)
 | not gotKey    =  pFail pmode lno 1 "THEORY <TheoryName> expected"
 | otherwise     =  parseRest pmode theory' lns
 where
   (gotKey,keyedName) = parseKeyAndName "THEORY" str
   theory' = theory{theoryName = keyedName}
\end{code}


\begin{code}
parseRest :: Monad m => ParseMode -> Theory -> Parser m Theory
parseRest pmode theory [] = return (theory, [])
parseRest pmode theory (ln@(lno,str):lns)
 | emptyLine str  =  parseRest pmode theory lns
 | gotImpTheory   =  parseRest pmode (thImports__ (++[thryName]) theory) lns
 | gotImpCode     =  parseRest pmode (hkImports__ (++[codeName]) theory) lns
 | gotIndSchema   =  parseIndSchema pmode theory typeName lno lns
 | gotLaw         =  parseLaw pmode theory lwName lno lrest lns
 | gotTheorem     =  parseTheorem pmode theory thrmName lno trest lns
 | otherwise      =  parseRest pmode theory lns
 where
   (gotImpTheory, thryName) = parseKeyAndName "IMPORT-THEORY"  str
   (gotImpCode,   codeName) = parseKeyAndName "IMPORT-HASKELL" str
   (gotLaw, lwName, lrest)   = parseOneLinerStart "LAW" str
   (gotIndSchema, typeName) = parseKeyAndName "INDUCTION-SCHEME" str
   (gotTheorem, thrmName, trest) = parseOneLinerStart "THEOREM" str
\end{code}

\subsection{Parse Laws}

\LAWSYNTAX
\begin{code}
parseLaw :: Monad m
         => ParseMode -> Theory  -> String -> Int -> String -> Lines
         -> m (Theory, Lines)
parseLaw pmode theory lwName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      But msgs
        ->  pFail pmode lno 1 $ unlines msgs
      Yes (expr, lns')
        ->  parseRest pmode (thLaws__ (++[LAW lwName expr]) theory) lns'

parseExprChunk :: Monad m
               => ParseMode -> Int -> String -> Lines
               -> m (HsExp, Lines)
parseExprChunk pmode lno rest lns
 | emptyLine rest  =  parseExpr pmode restlns chunk
 | otherwise       =  parseExpr pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

\subsection{Parse Induction Schemata}

\INDSCHEMASYNTAX
\begin{code}
parseIndSchema pmode theory typeName lno (ln1:ln2:ln3:lns)
 | not gotBase  =  pFail pmode (lno+1) 1 "missing BASE"
 | not gotStep  =  pFail pmode (lno+2) 1 "missing STEP"
 | not gotInj   =  pFail pmode (lno+3) 1 "missing INJ"
 | otherwise
     =  case parseEquivChunk pmode (lno+3) ln3rest lns of
         Nothing
           ->  pFail pmode lno 1 "Injective law expected"
         Just ((e1,e2), lns')
           ->  parseRest pmode
                         (thIndScheme__ (++[ ind{indInj=(e1,e2)} ]) theory)
                         lns'
 where
   (gotBase,bValue) = parseKeyAndValue pmode "BASE" $ snd ln1
   (gotStep,sVar,eStep) = parseKeyNameKeyValue pmode "STEP" "-->" $ snd ln2
   len = length "INJ"
   (ln3inj,ln3rest) = splitAt len $ snd ln3
   gotInj = ln3inj == "INJ"
   ind = IND typeName bValue (sVar,eStep) (hs42,hs42)
parseIndSchema pmode theory typeName lno _
  = pFail pmode lno 0 "Incomplete Induction Schema"

parseEquivChunk pmode lno rest lns
 | emptyLine rest  =  parseEquiv pmode restlns chunk
 | otherwise       =  parseEquiv pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}


\newpage
\subsection{Parse Theorems}

\THEOREMSYNTAX
\begin{code}
parseTheorem pmode theory thrmName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      Nothing
        ->  pFail pmode lno 0 "Theorem expected"
      Just (goal, lns')
        ->  parseProof pmode theory thrmName goal lns'

parseProof pmode theory thrmName goal [] = pFail pmode maxBound 0 "missing proof"
parseProof pmode theory thrmName goal (ln:lns)
  | gotReduce     =  do (strat,lns') <- parseReduction pmode rstrat lns
                        let thry = THEOREM thrmName strat
                        return ( thTheorems__ (++[thry]) theory, lns' )
  | gotInduction  =  pFail pmode (fst ln) 0 "parseInduction NYI"
  | otherwise     =  pFail pmode (fst ln) 0 "STRATEGY <strategy> expected."
  where
    (gotReduce,rstrat) = parseRedStrat $ snd ln
    (gotInduction,istrat) = parseIndStrat $ snd ln
\end{code}

\STRATEGIES
\begin{code}
parseRedStrat str
  | stratSpec == ["STRATEGY","ReduceAll"]   =  (True,ReduceAll  udefc)
  | stratSpec == ["STRATEGY","ReduceLHS"]   =  (True,ReduceLHS  udefc)
  | stratSpec == ["STRATEGY","ReduceRHS"]   =  (True,ReduceRHS  udefc)
  | stratSpec == ["STRATEGY","ReduceBoth"]  =  (True,ReduceBoth udefc udefc)
  | otherwise  =  (False,error "parseRedStrateg NYI")
  where
    stratSpec = words str
    udefc = error "undefined reduce calculation"
\end{code}

\texttt{
\\\ReduceBoth
\REDBOTHSYNTAX
}
\begin{code}
parseReduction pmode (ReduceBoth _ _) lns
 = pFail pmode 0 0 "parseReduction ReduceBoth NYI"
\end{code}

\texttt{
\\\ReduceAll | \ReduceLHS | \ReduceRHS
\\<Calculation>
}
\begin{code}
parseReduction pmode (ReduceAll _) lns  =  parseReduction' pmode ReduceAll lns
parseReduction pmode (ReduceLHS _) lns  =  parseReduction' pmode ReduceLHS lns
parseReduction pmode (ReduceRHS _) lns  =  parseReduction' pmode ReduceRHS lns

calcStop = ["QED","RHS"]

parseReduction' pmode reduce lns
 = do (calc, lns') <- parseCalculation pmode lns
      -- expect calcStop
      completeCalc pmode reduce calc lns'

completeCalc pmode _ _ [] = pFail pmode 0 0 "missing calc end"
completeCalc pmode reduce calc ((num,str):lns)
 | trim str `elem` calcStop  =  return (reduce calc,lns)
 | otherwise = pFail pmode num 0 ("improper calc end: "++str)
\end{code}


\SDOINDUCTION
\begin{code}
parseIndStrat ln = (False,"parseIndStrateg NYI")
\end{code}


\CALCSYNTAX
\begin{code}
type Steps = [(Line,Lines)]
\end{code}

This requires multiple ``chunks'' to be parsed.
Blank lines are separators,
as are lines beginning with a leading space followed by a single equal sign.
A calculation is ended by a line starting with ``QED'' or ``RHS''.
\begin{code}
parseCalculation :: Monad m => ParseMode -> Parser m Calculation
parseCalculation pmode lns
  = do (calcChunks,rest) <- takeLinesBefore calcStop lns
       ((fstChunk,sepChunks),_) <- splitLinesOn pmode isJustificationLn calcChunks
       (goalPred,_) <- parseExpr pmode [] fstChunk
       steps <- parseSteps pmode sepChunks
       return (CALC goalPred steps, rest)
\end{code}

Break line-list at the first use of a designated keyword,
discarding empty lines along the way
\begin{code}
takeLinesBefore :: Monad m => [String] -> Parser m Lines
takeLinesBefore _ [] = return ( [], [] )
takeLinesBefore keys lns@(ln:lns')
 | null lnwords              =  takeLinesBefore keys lns'
 | head lnwords `elem` keys  =  return ( [], lns )
 | otherwise                 =  do (before,after) <- takeLinesBefore keys lns'
                                   return ( ln:before, after )
 where lnwords = words $ snd ln
\end{code}

A justification line has a first word that is an equals-sign (for now).
\begin{code}
isJustificationLn :: Line -> Bool
isJustificationLn (_,str)  =  case words str of
                                []     ->  False
                                (w:_)  ->  w `elem` ["="]
\end{code}

\newpage
Split into maximal chunks seperated by lines that satisfy \texttt{splitHere}:
\begin{code}
splitLinesOn :: Monad m => ParseMode -> (Line -> Bool) -> Parser m (Lines,Steps)

-- we expect at least one line before split
splitLinesOn pmode splitHere [] = pFail pmode 0 0 "nothing to split"
splitLinesOn pmode splitHere (ln:lns)
 | splitHere ln  = pFail pmode (fst ln) 0 "splitter at start"
 | otherwise  =  splitLinesOn' pmode splitHere [ln] lns

-- seen initial chunk, looking for first split
splitLinesOn' pmode splitHere knuhc []  =  return ((reverse knuhc,[]),[])
splitLinesOn' pmode splitHere knuhc (ln:lns)
 | splitHere ln  =  splitLinesOn'' pmode splitHere (reverse knuhc) [] ln [] lns
 | otherwise  = splitLinesOn' pmode splitHere (ln:knuhc) lns

-- found split
-- accumulating post-split chunk
splitLinesOn'' pmode splitHere chunk0 spets split knuhc []
 | null knuhc  =  pFail pmode (fst split) 0 "ends on split"
 | otherwise  =  return ( ( chunk0
                          , reverse ((split, reverse knuhc):spets) )
                        , [] )
splitLinesOn'' pmode splitHere chunk0 spets split knuhc (ln:lns)
 | splitHere ln  =  splitLinesOn'' pmode splitHere
                                 chunk0 ((split, reverse knuhc):spets) ln [] lns
 | otherwise  = splitLinesOn'' pmode splitHere chunk0 spets split (ln:knuhc) lns
\end{code}

Parsing calculation steps:
\begin{code}
parseSteps :: Monad m => ParseMode -> Steps -> m [(Justification,HsExp)]
parseSteps pmode [] = return []
parseSteps pmode ((justify,chunk):rest)
  = do just <- parseJustification pmode justify
       (exp,_) <- parseExpr pmode [] chunk
       steps <- parseSteps pmode rest
       return ((just,exp):steps)
\end{code}

\newpage
Parsing a justification.

\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}

Parsing of whole line --- need at least two words
\begin{code}
parseJustification :: Monad m => ParseMode -> Line -> m Justification
parseJustification pmode (lno,str)
 = case words str of
    (w1:w2:wrest) ->  do jr <- parseJRel w1
                         parseJustify pmode lno jr wrest w2
    _ ->  pFail pmode lno 0 "incomplete justification"
 where
    parseJRel "="  =  return JEq
    parseJRel  x   =  pFail pmode lno 1 ("unrecognised relation: "++x)
\end{code}

Parsing given at least two words, the first of which is OK.
If we get a succesful parse, we ignore anything leftover.
\begin{code}
parseJustify :: Monad m => ParseMode -> Int -> JRel -> [String] -> String
             -> m Justification
parseJustify pmode lno jr wrest w2
 | w2 == "LAW"    = parseLawName pmode lno jr     wrest
 | w2 == "DEF"    = parseDef     pmode lno jr     wrest
 | w2 == "INDHYP" = parseUsage   pmode lno jr IH  wrest
 | w2 == "CASE"   = parseUsage   pmode lno jr CS  wrest
 | w2 == "SIMP"   = parseUsage   pmode lno jr SMP wrest
 | otherwise      = pFail        pmode lno  1 ("unrecognised law: "++w2)
\end{code}


Seen a \texttt{LAW}, expecting a \texttt{fname}
\begin{code}
parseLawName pmode lno jr []         =  pFail pmode lno 0 "LAW missing name"
parseLawName pmode lno jr (w:wrest)  =  parseUsage pmode lno jr (L w) wrest
\end{code}

\newpage
Seen a \texttt{DEF}, expecting a \texttt{fname[.i]}
\begin{code}
parseDef pmode lno jr [] = pFail pmode lno 0 "DEF missing name"
parseDef pmode lno jr (w:wrest) =  parseUsage pmode lno jr (mkD w) wrest

mkD w -- any error in '.loc' results in value 0
  | null dotloc      =  D w 0
  | null loc         =  D nm 0
  | all isDigit loc  =  D nm $ read loc
  | otherwise        =  D nm 0
  where
    (nm,dotloc) = break (=='.') w
    loc = tail dotloc
\end{code}

Seen law, looking for optional usage.
\begin{code}
parseUsage pmode lno jr jlaw []
                     =  return $ BECAUSE jr jlaw (defUsage jlaw) (defFocus jlaw)
parseUsage pmode lno jr jlaw ws@(w:wrest)
  | w == "l2r"  =  parseFocus pmode lno jr jlaw L2R              wrest
  | w == "r2l"  =  parseFocus pmode lno jr jlaw R2L              wrest
  | otherwise   =  parseFocus pmode lno jr jlaw (defUsage jlaw) ws

defUsage (D _ _)  =  L2R
defUsage _        =  Whole
\end{code}

Seen law and possible usage, looking for optional focus.
\begin{code}
parseFocus pmode lno jr jlaw u []
                                 =  return $ BECAUSE jr jlaw u $ defFocus jlaw
parseFocus pmode lno jr jlaw u [w]
                                 =  return $ BECAUSE jr jlaw u $ At w 0
parseFocus pmode lno jr jlaw u (w1:w2:_)
  | all isDigit w2               =  return $ BECAUSE jr jlaw u $ At w1 $ read w2
  | otherwise                    =  return $ BECAUSE jr jlaw u $ At w1 0

defFocus (D n _)  =  At n 0
defFocus _        =  Top
\end{code}

\newpage
\subsection{Parsing Expressions and Equivalences}

\begin{code}
parseExpr :: Monad m => ParseMode -> Lines -> Parser m HsExp
parseExpr pmode restlns [] = pFail pmode 0 0 "no expression!"
parseExpr pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (modstrf chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> return (getNakedExpression hsmod, restlns)
  where
    modstrf [(_,str)]
      = unlines [ "module NakedExpr where"
                , "nakedExpr = "++str ]
    modstrf chunk
      = unlines ( [ "module NakedExpr where"
                  , "nakedExpr = " ]
                  ++ map snd chunk )
\end{code}

\begin{code}
parseEquiv :: Monad m => ParseMode -> Lines -> Parser m (HsExp, HsExp)
parseEquiv pmode restlns [] = pFail pmode 0 0 "no equivalence!"
parseEquiv pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (modstrf chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> return (getNakedEquivalence hsmod, restlns)
  where
    modstrf [(_,str)]
      = unlines [ "module NakedExpr where"
                , "infix 3 ==="
                , "nakedExpr = "++str ]
    modstrf chunk
      = unlines ( [ "module NakedExpr where"
                  , "infix 3 ==="
                  , "nakedExpr = " ]
                  ++ map snd chunk )
\end{code}

\subsection{Extracting Expressions and Equivalences}

\begin{code}
getNakedExpression :: HsModule -> HsExp
getNakedExpression
 (HsModule _ _ _ _ [ HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ]) = hsexp
getNakedExpression _ = hs42

hs42 = HsLit (HsInt 42)
\end{code}




\begin{code}
getNakedEquivalence :: HsModule -> (HsExp,HsExp)
getNakedEquivalence
 (HsModule _ _ _ _ [ _, HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
   = case hsexp of
       (HsInfixApp e1 (HsQVarOp (UnQual (HsSymbol "==="))) e2)  ->  (e1,e2)
       _               ->  (hs42,hs42)
getNakedEquivalence _  =   (hs42,hs42)
\end{code}



\newpage
\subsection{``One-Liner'' Parsing}

\begin{code}
emptyLine = all isSpace
\end{code}

We return a boolean that is true if the parse suceeds.
\begin{code}
parseKeyAndName key str
  = case words str of
      [w1,w2] | w1 == key  ->  (True,  w2)
      _                    ->  (False, error ("Expecting '"++key++"' and name"))
\end{code}

\begin{code}
parseKeyAndValue pmode key str
  = case words str of
      (w1:wrest) | w1 == key
        -> case parseExpr pmode [] [(0,unwords wrest)] of
            Nothing -> (False, error ("Bad value: "++ unwords wrest))
            Just (hsexp,_) ->  (True,  hsexp)
      _                    ->  (False, error ("Expecting '"++key++"' and value"))
\end{code}

\begin{code}
parseKeyNameKeyValue pmode key1 key2 str
  = case words str of
      (w1:w2:w3:wrest) | w1 == key1 && w3 == key2
        -> case parseExpr pmode [] [(0,unwords wrest)] of
            Nothing -> (False, "", error ("Bad value: "++ unwords wrest))
            Just (hsexp,_) ->  (True,  w2, hsexp)
      _                    ->  (False, "", error ("Expecting '"++key2++"' and value"))
\end{code}

\begin{code}
parseOneLinerStart key str
  = case words str of
      (w1:w2:rest) | w1 == key  ->  (True,  w2, unwords rest)
      _                         ->  ( False
                                    , error "parseOneLinerStart failed!"
                                    , str)
\end{code}

\newpage
\subsection{Chunk Parser}

A chunk is zero or more empty lines,
followed by one or more non-empty lines,
followed by at least one empty line,
or the end of the list of lines.
\begin{code}
getChunk []       =  ([],[])

getChunk (ln@(_,str):lns)
 | emptyLine str  =  getChunk       lns
 | otherwise      =  getChunk' [ln] lns

getChunk' snl []  =  (reverse snl, [])

getChunk' snl (ln@(_,str):lns)
 | emptyLine str  =  (reverse snl,lns)
 | otherwise      =  getChunk' (ln:snl) lns
\end{code}
