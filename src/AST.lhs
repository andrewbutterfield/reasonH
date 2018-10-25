\section{Abstract Syntax Tree}
\begin{haskell}
Copyright  Andrew Butterfield (c) 2017--18

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module AST
(
  Expr(..), Match(..), Decl(..), Mdl(..)
, hsModule2Mdl, hsDecl2Decl, hsExp2Expr
  -- special variables:
, eNull, eCons
, pWild, pAs
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
 =  App (App (Var opn) ex1) ex2
  where
    opn = hsQOp2Str  op
    ex1 = hsExp2Expr e1
    ex2 = hsExp2Expr e2
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
pWild = Var "_"
pAs   = Var "@"
hsPat2Expr :: HsPat -> Expr
hsPat2Expr (HsPVar hsn) = Var $ hsName2Str hsn
hsPat2Expr (HsPLit lit) = hsLit2Expr lit
hsPat2Expr (HsPList hspats) = hsPats2Expr hspats
hsPat2Expr (HsPParen hspat) = hsPat2Expr hspat
hsPat2Expr (HsPInfixApp p1 op p2)
  =  App (App (Var $ hsQName2Str op) (hsPat2Expr p1)) (hsPat2Expr p2)
hsPat2Expr HsPWildCard = pWild
hsPat2Expr (HsPAsPat nm hspat)
 =  App (App pAs $ Var $ hsName2Str nm) $ hsPat2Expr hspat
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
