// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module TypesAST
type VarX = string
type AExp =
  | Num of int
  | TimesExpr of (AExp * AExp)
  | DivExpr of (AExp * AExp)
  | PlusExpr of (AExp * AExp)
  | MinusExpr of (AExp * AExp)
  | PowExpr of (AExp * AExp)
  | UPlusExpr of (AExp)
  | UMinusExpr of (AExp)
  | Xval of (VarX)
  | ArrayExpr of (string * AExp)


type BExp =
| BitAndB of BExp * BExp
| BitOrB of BExp * BExp
| LogAndB of BExp * BExp
| LogOrB of BExp * BExp
| LogNotB of BExp
| BEqual of AExp * AExp
| NotEqualB of AExp * AExp
| GThanB of AExp * AExp
| LThanB of AExp * AExp
| GEThanB of AExp * AExp
| LEThanB of AExp * AExp
| WutT  //  true 
| WutF //  false


type CExp =
    |AssignC of (VarX * AExp)
    |AssignArrayC of (VarX * AExp * AExp)
    |SkipC
    |StateC of (CExp *CExp)
    |IfStateC of (GCExp)
    |DoloopC of (GCExp)
and GCExp =
    | ARROWGC of (BExp * CExp)
    | StateGC of (GCExp *GCExp)
