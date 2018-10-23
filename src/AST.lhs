\section{Abstract Syntax Tree}
\begin{haskell}
Copyright  Andrew Buttefield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module AST
(
  Expr(..), Match(..), Decl(..), Mdl(..)
, hsModule2Mdl, hsDecl2Decl, hsExp2Expr
)
where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

We need a simplified AST for haskell.
We don't need any source-locs,
and we really don't need to distinguish identifiers from symbols,
handle qualified names,
or treat patterns differently to general expressions.
Wildcard patterns (or even irrefutable ones)
can be handled using names.


\subsection{Simplified Haskell AST}

We simplify things dramatically.
First, expressions:
\begin{code}
data Expr
  = LBool Bool | LInt Int | LChar Char
  | Var String
  | App Expr Expr
  | If Expr Expr Expr
  | GrdExpr [(Expr,Expr)]
  | Let [Decl] Expr
  | PApp String [Expr]
  deriving (Eq,Show)
\end{code}

\newpage
Next, matchings:
\begin{code}
data Match = Match { fname ::  String  -- function name
                   , lhspat :: [Expr]  -- LHS patterns
                   , rhs :: Expr    -- RHS outcome
                   , ldecls :: [Decl]  -- local declarations
                   }
           deriving (Eq, Show)
\end{code}
Then, declarations:
\begin{code}
data Decl
  = Fun [Match]
  | Bind Expr Expr [Decl]
  | Syntax -- not relevant to this tool !
  | Type String -- just noting name for now - to be addressed later
  deriving (Eq, Show)
\end{code}

Finally, modules (ignoring exports)
\begin{code}
data Mdl = Mdl { mname :: String
               , imps :: [Import]
               , topdecls :: [Decl]
               }
         deriving Show

data Import = Imp { imname :: String
                  , asnmame :: Maybe String
                  }
            deriving Show
\end{code}

\subsection{Simplifying Strings}

\begin{code}
hsName2Str :: HsName -> String
hsName2Str (HsIdent str)  = str
hsName2Str (HsSymbol str) = str

hsSpcCon2Str :: HsSpecialCon -> String
hsSpcCon2Str HsUnitCon  =  "()"
hsSpcCon2Str HsListCon  =  "[]"
hsSpcCon2Str HsFunCon   =  "->"
hsSpcCon2Str HsCons     =  ":"
hsSpcCon2Str (HsTupleCon i)  = "("++replicate (i-1) ','++")"

hsQName2Str :: HsQName -> String
hsQName2Str (Qual (Module m) nm) = m ++ '.':hsName2Str nm
hsQName2Str (UnQual nm) = hsName2Str nm
hsQName2Str (Special hsc) = hsSpcCon2Str hsc

hsQOp2Str :: HsQOp -> String
hsQOp2Str (HsQVarOp hsq)  = hsQName2Str hsq
hsQOp2Str (HsQConOp hsq)  = hsQName2Str hsq

hsExp2Str :: HsExp -> String
hsExp2Str (HsVar qnm)  = hsQName2Str qnm
hsExp2Str (HsCon qnm)  = hsQName2Str qnm
hsExp2Str hse = error ("hsExp2Str invalid for "++show hse)

hsPat2Str :: HsPat -> String
hsPat2Str (HsPVar pnm) = hsName2Str pnm
hsPat2Str hsp = error ("hsPat2Str invalid for "++show hsp)
\end{code}

\subsection{Simplifying Literals}

\begin{code}
hsLit2Expr :: HsLiteral -> Expr
hsLit2Expr (HsInt i)  = LInt $ fromInteger i
hsLit2Expr (HsChar c) = LChar c
hsLit2Expr lit = error ("hsLit2Expr NYIf "++show lit)
\end{code}

\subsection{Simplifying Parsed Expressions}

\begin{code}
hsExp2Expr :: HsExp -> Expr
hsExp2Expr (HsVar hsq)  =  Var $ hsQName2Str hsq
hsExp2Expr (HsCon hsq)  =  Var $ hsQName2Str hsq
hsExp2Expr (HsLit lit)  =  hsLit2Expr lit
hsExp2Expr (HsInfixApp e1 op e2)
  =  App (App (Var $ hsQOp2Str op) (hsExp2Expr e1)) (hsExp2Expr e2)
hsExp2Expr (HsApp e1 e2)
  =  App (hsExp2Expr e1) (hsExp2Expr e2)
hsExp2Expr (HsIf hse1 hse2 hse3)
  = If (hsExp2Expr hse1) (hsExp2Expr hse2) (hsExp2Expr hse2)
hsExp2Expr (HsParen hse)  =  hsExp2Expr hse
hsExp2Expr (HsList hses)  =  hsExps2Expr hses
hsExp2Expr hse  =  error ("hsExp2Expr NYIf "++show hse)
\end{code}

\begin{code}
eNull = Var "[]"
eCons = Var ":"
hsExps2Expr :: [HsExp] -> Expr
hsExps2Expr []          =  eNull
hsExps2Expr (hse:hses)  =  App (App eCons $ hsExp2Expr hse) $ hsExps2Expr hses
\end{code}

For now, we view righthand-sides as expressions
\begin{code}
hsRhs2Expr :: HsRhs -> Expr
hsRhs2Expr (HsUnGuardedRhs hse)     =  hsExp2Expr hse
hsRhs2Expr (HsGuardedRhss grdrhss)  =  GrdExpr $ map hsGrdRHs2Expr2 grdrhss

hsGrdRHs2Expr2 :: HsGuardedRhs -> (Expr, Expr)
hsGrdRHs2Expr2 (HsGuardedRhs _ grd rhs) = (hsExp2Expr grd, hsExp2Expr rhs)
\end{code}

For now, we view patterns as expressions
\begin{code}
hsPat2Expr :: HsPat -> Expr
hsPat2Expr (HsPVar hsn) = Var $ hsName2Str hsn
hsPat2Expr (HsPLit lit) = hsLit2Expr lit
hsPat2Expr (HsPList hspats) = hsPats2Expr hspats
hsPat2Expr (HsPParen hspat) = hsPat2Expr hspat
hsPat2Expr (HsPInfixApp p1 op p2)
  =  App (App (Var $ hsQName2Str op) (hsPat2Expr p1)) (hsPat2Expr p2)
hsPat2Expr HsPWildCard = Var "_"
hsPat2Expr (HsPAsPat nm hspat)
 =  App (App (Var "@") $ Var $ hsName2Str nm) $ hsPat2Expr hspat
hsPat2Expr (HsPApp qnm hspats)  = PApp (hsQName2Str qnm) $ map hsPat2Expr hspats

hsPat2Expr hsp = error ("hsPat2Expr NYIf "++show hsp)

hsPats2Expr :: [HsPat] -> Expr
hsPats2Expr []  = eNull
hsPats2Expr (hspat:hspats)
  = App (App eCons $ hsPat2Expr hspat) $ hsPats2Expr hspats
\end{code}

\subsection{Simplifying Parsed Matches}

\begin{code}
hsMatch2Match :: HsMatch -> Match
hsMatch2Match (HsMatch _ nm pats rhs decls)
  = Match (hsName2Str nm)
          (map hsPat2Expr pats)
          (hsRhs2Expr rhs)
          (map hsDecl2Decl decls)
\end{code}

\subsection{Simplifying Parsed Declarations}

\begin{code}
hsDecl2Decl :: HsDecl -> Decl
hsDecl2Decl (HsFunBind hsMatches) = Fun $ map hsMatch2Match hsMatches

hsDecl2Decl (HsPatBind _ hspat hsrhs hsdecls)
 = Bind (hsPat2Expr hspat) (hsRhs2Expr hsrhs) (map hsDecl2Decl hsdecls)

-- ignore type signatures and declarations for now, just note name
hsDecl2Decl (HsTypeSig _ hsn _)        = Type "::"
hsDecl2Decl (HsTypeDecl _ hsn _ _) = Type $ hsName2Str hsn
hsDecl2Decl (HsDataDecl _ _ hsn _ _ _) = Type $ hsName2Str hsn
hsDecl2Decl (HsNewTypeDecl _ _ hsn _ _ _) = Type $ hsName2Str hsn

hsDecl2Decl (HsInfixDecl _ _ _ _) = Syntax
hsDecl2Decl hsd = error ("hsDecl2Decl NYIf "++show hsd)
\end{code}

\subsection{Simplifying Parsed Modules}

\begin{code}
hsModule2Mdl :: HsModule -> Mdl
hsModule2Mdl (HsModule _ (Module nm) _ imports decls)
  = Mdl nm (map hsImpDcl2Imp imports) (map hsDecl2Decl decls)

hsImpDcl2Imp :: HsImportDecl -> Import
hsImpDcl2Imp hsID
 = Imp (hsMod2Str $ importModule hsID)
       (hsModAs2MStr $ importAs hsID)

hsMod2Str :: Module -> String
hsMod2Str (Module str) = str

hsModAs2MStr :: Maybe Module -> Maybe String
hsModAs2MStr Nothing = Nothing
hsModAs2MStr (Just m) = Just $ hsMod2Str m
\end{code}


\newpage
\subsection{Examples}

\texttt{"1+(2+3)"} parses as:
\begin{haskell}
HsInfixApp
  (HsLit (HsInt 1))
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsParen
    ( HsInfixApp (HsLit (HsInt 2))
                 (HsQVarOp (UnQual (HsSymbol "+")))
                 (HsLit (HsInt 3))
    )
  )
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [ LInt 1, App "+" [ LInt 2, LInt 3] ]
\end{haskell}

\texttt{"1+2+3"} parses as
\begin{haskell}
HsInfixApp
  (HsInfixApp
    (HsLit (HsInt 1))
    (HsQVarOp (UnQual (HsSymbol "+")))
    (HsLit (HsInt 2))
  )
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsLit (HsInt 3))
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [LInt 1, LInt 2, LInt 3]
\end{haskell}

\texttt{"(1+2)+3"} parses as
\begin{haskell}
HsInfixApp
  (HsParen
    (HsInfixApp
      (HsLit (HsInt 1))
      (HsQVarOp (UnQual (HsSymbol "+")))
      (HsLit (HsInt 2))
    )
  )
  (HsQVarOp (UnQual (HsSymbol "+")))
  (HsLit (HsInt 3))
\end{haskell}
Suggested form:
\begin{haskell}
App "+" [App "+" [LInt 1, LInt 2], LInt 3]
\end{haskell}

\texttt{"[] ++ ys = ys"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsSymbol "++")
  [ HsPList []
  , HsPVar (HsIdent "ys") ]
  (HsUnGuardedRhs (HsVar (UnQual (HsIdent "ys"))))
  [] -- where-clauses
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "++" [ LNull, Var "ys" ]) (Var "ys") []
\end{haskell}

\newpage
\texttt{"(x:xs) ++ ys = x:(xs++ys)"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsSymbol "++")
  [ HsPParen ( HsPInfixApp (HsPVar (HsIdent "x"))
                           (Special HsCons)
                           (HsPVar (HsIdent "xs"))
             )
  , HsPVar (HsIdent "ys") ]
  ( HsUnGuardedRhs
      ( HsInfixApp
         (HsVar (UnQual (HsIdent "x")))
         (HsQConOp (Special HsCons))
         ( HsParen
           ( HsInfixApp (HsVar (UnQual (HsIdent "xs")))
                        (HsQVarOp (UnQual (HsSymbol "++")))
                        (HsVar (UnQual (HsIdent "ys")))
           )
         )
      )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "++" [App ":" [Var "x", Var "xs"], Var "ys"] )
      (App ":" [Var "x", App "++" [Var "xs", Var "ys"]])
      []
\end{haskell}

\texttt{"[] ++ ys = ys"}\\
\texttt{"(x:xs) ++ ys = x:(xs++ys)"} parses as:
\begin{haskell}
HsFunBind
  [ HsMatch (SrcLoc {...}  <as above>
  , HsMatch (SrcLoc {...}  <as above>
  ]
\end{haskell}
Suggested form:
\begin{haskell}
Fun [ Match <as above>, Match <as above> ]
\end{haskell}

\texttt{"reverse [] = []"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsIdent "reverse")
  [HsPList []]
  (HsUnGuardedRhs (HsList []))
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "reverse" [LNull]) (LNull) []
\end{haskell}

\newpage
\texttt{"reverse (x:xs) = reverse xs ++ [x]"} parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "FPC1.hs", srcLine = 7, srcColumn = 1})
  (HsIdent "reverse")
  [ HsPParen
      ( HsPInfixApp (HsPVar (HsIdent "x"))
                    (Special HsCons)
                    (HsPVar (HsIdent "xs")))
  ]
  ( HsUnGuardedRhs
     ( HsInfixApp
         ( HsApp (HsVar (UnQual (HsIdent "reverse")))
                 (HsVar (UnQual (HsIdent "xs")))
         )
         ( HsQVarOp (UnQual (HsSymbol "++")) )
         ( HsList [HsVar (UnQual (HsIdent "x"))] )
     )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "reverse" [App ":" [Var "x", Var "xs"]])
      (App "++" [App "reverse" [Var "xs"], List [Var "x"])
      []
\end{haskell}

\texttt{"length (\_:xs) = 1 + length xs"} parses as:
\begin{haskell}
HsMatch (SrcLoc {...})
  (HsIdent "length")
  [ HsPParen ( HsPInfixApp HsPWildCard
                           (Special HsCons) (HsPVar (HsIdent "xs"))
             )
  ]
  ( HsUnGuardedRhs
      ( HsInfixApp
          (HsLit (HsInt 1))
          (HsQVarOp (UnQual (HsSymbol "+")))
          ( HsApp (HsVar (UnQual (HsIdent "length")))
                  (HsVar (UnQual (HsIdent "xs")))
          )
      )
  )
  []
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "length" [App ":" [Var "_", Var "xs"]])
      (App "+" [LInt 1,App "length" [Var "xs"]])
      []
\end{haskell}

\newpage
\texttt{"ins x ys@(y:zs)"}\\
\texttt{" | x < y      =  x : ys"}\\
\texttt{" | x > y      =  y : ins x zs"}\\
\texttt{" | otherwise  =  ys"} parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "FPC1.hs", srcLine = 14, srcColumn = 1})
(HsIdent "ins")
[ HsPVar (HsIdent "x")
, HsPAsPat
    (HsIdent "ys")
    ( HsPParen
        ( HsPInfixApp
            (HsPVar (HsIdent "y"))
            (Special HsCons)
            (HsPVar (HsIdent "zs"))
        )
    )
]
( HsGuardedRhss
    [ HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 15, srcColumn = 2})
        ( HsInfixApp
           (HsVar (UnQual (HsIdent "x")))
           (HsQVarOp (UnQual (HsSymbol "<")))
           (HsVar (UnQual (HsIdent "y")))
        )
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "x")))
            (HsQConOp (Special HsCons))
            (HsVar (UnQual (HsIdent "ys")))
        )
    , HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 16, srcColumn = 2})
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "x")))
            (HsQVarOp (UnQual (HsSymbol ">")))
            (HsVar (UnQual (HsIdent "y")))
        )
        ( HsInfixApp
            (HsVar (UnQual (HsIdent "y")))
            (HsQConOp (Special HsCons))
            ( HsApp
                ( HsApp (HsVar (UnQual (HsIdent "ins")))
                        (HsVar (UnQual (HsIdent "x")))
                )
                (HsVar (UnQual (HsIdent "zs")))
            )
        )
    , HsGuardedRhs
        (SrcLoc {srcFilename = "FPC1.hs", srcLine = 17, srcColumn = 2})
        (HsVar (UnQual (HsIdent "otherwise")))
        (HsVar (UnQual (HsIdent "ys")))
    ])
[]
\end{haskell}
\newpage
Suggested form:
\begin{haskell}
Match (App "ins" [Var "x", App "@" [ys,App ":" [Var "y", Var "zs"]]])
      (GrdExpr [ ( App "<" [Var "x", Var "y"]
                 , App ":" [Var "x", Var "ys"] )
               , ( App ">" [Var "x", Var "y"]
                 , App ":"" [Var "y", App "ins" [Var "x", Var "zs"]] )
               , ( LTrue, Var "ys" )
               ]
      )
      []
\end{haskell}

\newpage
\texttt{"wf (x:xs)"}\\
\texttt{" | even xnum  = 'e'"}\\
\texttt{" | xnum < 60  = 's'"}\\
\texttt{" | otherwise  = 'L'"}\\
\texttt{" where"}\\
\texttt{"   xnum = ord x"}
parses as:
\begin{haskell}
HsMatch (SrcLoc {srcFilename = "Where.hs", srcLine = 5, srcColumn = 1})
  (HsIdent "wf")
  [ HsPParen
      ( HsPInfixApp (HsPVar (HsIdent "x"))
                    (Special HsCons)
                    (HsPVar (HsIdent "xs"))
      )
  ]
  ( HsGuardedRhss
      [ HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 6, srcColumn = 2})
         (HsApp (HsVar (UnQual (HsIdent "even")))
                (HsVar (UnQual (HsIdent "xnum")))
         )
         (HsLit (HsChar 'e'))
      , HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 7, srcColumn = 2})
         (HsInfixApp (HsVar (UnQual (HsIdent "xnum")))
                     (HsQVarOp (UnQual (HsSymbol "<")))
                     (HsLit (HsInt 60))
         )
         (HsLit (HsChar 's'))
      , HsGuardedRhs
         (SrcLoc {srcFilename = "Where.hs", srcLine = 8, srcColumn = 2})
         (HsVar (UnQual (HsIdent "otherwise")))
         (HsLit (HsChar 'L'))
      ]
  )
  [ HsPatBind
     (SrcLoc {srcFilename = "Where.hs", srcLine = 10, srcColumn = 4})
     ( HsPVar (HsIdent "xnum"))
     ( HsUnGuardedRhs
         ( HsApp (HsVar (UnQual (HsIdent "ord")))
                 (HsVar (UnQual (HsIdent "x")))
         )
     )
     []
   ]
\end{haskell}
Suggested form:
\begin{haskell}
Match (App "wf" [App ":" [Var "x", Var "xs"]])
      (GrdExpr [ ( App "even" [Var "xnum"]
                 , LChar 'e' )
               , ( App "<" [Var "xnum", LInt 60]
                 , LChar 's' )
               , ( LTrue, LChar 'L' )
               ]
      )
      [ Bind "xnum" (App "ord" [Var "xs"])]
\end{haskell}
%
% \texttt{"xxx"} parses as:
% \begin{haskell}
% xxx
% \end{haskell}
% Suggested form:
% \begin{haskell}
% xxx
% \end{haskell}

\newpage
Gathered forms:
\begin{haskell}
App "+" [ LInt 1, App "+" [ LInt 2, LInt 3] ]

App "+" [LInt 1, LInt 2, LInt 3]

App "+" [App "+" [LInt 1, LInt 2], LInt 3]

Match (App "++" [ LNull, Var "ys" ]) (Var "ys") []

Match (App "++" [App ":" [Var "x", Var "xs"], Var "ys"] )
      (App ":" [Var "x", App "++" [Var "xs", Var "ys"]])
      []

Fun [ Match <as above>, Match <as above> ]

Match (App "reverse" [LNull]) (LNull) []

Match (App "reverse" [App ":" [Var "x", Var "xs"]])
      (App "++" [App "reverse" [Var "xs"], List [Var "x"])
      []

Match (App "length" [App ":" [Var "_", Var "xs"]])
      (App "+" [LInt 1,App "length" [Var "xs"]])
      []

Match (App "ins" [Var "x", App "@" [ys,App ":" [Var "y", Var "zs"]]])
      (GrdExpr [ ( App "<" [Var "x", Var "y"]
                 , App ":" [Var "x", Var "ys"] )
               , ( App ">" [Var "x", Var "y"]
                 , App ":"" [Var "y", App "ins" [Var "x", Var "zs"]] )
               , ( LTrue, Var "ys" )
               ]
      )
      []

Match (App "wf" [App ":" [Var "x", Var "xs"]])
      (GrdExpr [ ( App "even" [Var "xnum"]
                 , LChar 'e' )
               , ( App "<" [Var "xnum", LInt 60]
                 , LChar 's' )
               , ( LTrue, LChar 'L' )
               ]
      )
      [ Bind "xnum" (App "ord" [Var "xs"])]
\end{haskell}
