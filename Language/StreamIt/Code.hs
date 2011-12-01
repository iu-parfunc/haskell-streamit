module Language.StreamIt.Code (code) where

import Data.List
import Data.Typeable

import Language.StreamIt.Core
import Language.StreamIt.Filter
import Language.StreamIt.Graph

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

-- | Generate StreamIt.
code :: TypeSig -> Name -> StreamNode -> IO ()
code ty name node = do
  writeFile (name ++ ".str") $
    (intercalate "\n\n" $ map codeFilter fs)
    ++ "\n" ++ (intercalate "\n\n" $ map codeNode gs)
    ++ "\n" ++ codeNode (ty, name, node) ++ "\n"
  where
    (fs, gs) = findDefs node

codeNode :: (TypeSig, Name, StreamNode) -> String
codeNode (ty, name, sn) = case sn of
  AddF _ n _      -> "add " ++ n ++ "();\n"
  AddN _ n _      -> "add " ++ n ++ "();\n"
  Pipeline a      -> ty ++ " pipeline " ++ name ++ " {\n"
                     ++ (indent $ codeNode (ty, name, a)) ++ "}\n"
  Chain a b       -> codeNode (ty, name, a) ++ codeNode (ty, name, b)
  Empty           -> ""

codeFilter :: (TypeSig, Name, Statement) -> String
codeFilter (ty, name, stmt) = ty ++ " filter " ++ name ++ "()\n{\n"
                              ++ indent (codeStmt name stmt)
                              ++ "}"

instance Show Statement where show = codeStmt "none"

codeStmt :: Name -> Statement -> String
codeStmt name a = case a of
  Decl a (Just b)  -> showConstType (const' b) ++ " " ++ show a ++ " = "
                      ++ showConst (const' b) ++ ";\n"
  Decl a Nothing   -> showVType a ++ " " ++ show a ++ ";\n"
  Assign a b       -> show a ++ " = " ++ codeExpr b ++ ";\n"
  Branch a b Null  -> "if (" ++ codeExpr a ++ ") {\n" ++ indent (codeStmt name b) ++ "}\n"
  Branch a b c     -> "if (" ++ codeExpr a ++ ") {\n" ++ indent (codeStmt name b)
                      ++ "}\nelse {\n" ++ indent (codeStmt name c) ++ "}\n"
  Sequence a b     -> codeStmt name a ++ codeStmt name b
  Init a           -> "init {\n" ++ indent (codeStmt name a) ++ "}\n"
  Work (a, b, c) d -> "work" ++ showFlowRate " push " a
                      ++ showFlowRate " pop " b
                      ++ showFlowRate " peek " c
                      ++ " {\n" ++ indent (codeStmt name d) ++ "}\n"
  Push a           -> "push(" ++ codeExpr a ++ ");\n"
  Pop              -> "pop();\n"
  Peek a           -> "peek(" ++ show a ++ ");\n"
  Println a        -> "println(" ++ codeExpr a ++ ");\n"
  Null             -> ""
  where

  codeExpr :: E a -> String
  codeExpr a = case a of
    Ref a     -> show a
    Const a   -> showConst $ const' a
    Add a b   -> group [codeExpr a, "+", codeExpr b]
    Sub a b   -> group [codeExpr a, "-", codeExpr b]
    Mul a b   -> group [codeExpr a, "*", showConst (const' b)]
    Div a b   -> group [codeExpr a, "/", showConst (const' b)]
    Mod a b   -> group [codeExpr a, "%", showConst (const' b)]
    Not a     -> group ["!", codeExpr a]
    And a b   -> group [codeExpr a, "&&",  codeExpr b]
    Or  a b   -> group [codeExpr a, "||",  codeExpr b]
    Eq  a b   -> group [codeExpr a, "==",  codeExpr b]
    Lt  a b   -> group [codeExpr a, "<",   codeExpr b]
    Gt  a b   -> group [codeExpr a, ">",   codeExpr b]
    Le  a b   -> group [codeExpr a, "<=",  codeExpr b]
    Ge  a b   -> group [codeExpr a, ">=",  codeExpr b]
    Mux a b c -> group [codeExpr a, "?", codeExpr b, ":", codeExpr c] 
    where
    group :: [String] -> String
    group a = "(" ++ intercalate " " a ++ ")"
    
  showFlowRate :: String -> E Int -> String
  showFlowRate token a = case a of
    Const 0 -> ""
    _       -> token ++ codeExpr a

showConst :: Const -> String
showConst a = case a of
  Bool  True  -> "true"
  Bool  False -> "false"
  Int   a     -> show a
  Float a     -> show a

showConstType :: Const -> String
showConstType a = case a of
  Bool  _ -> "boolean"
  Int   _ -> "int"
  Float _ -> "float"
  
showVType :: AllE a => V a -> String
showVType a = case show $ last (snd $ splitTyConApp (typeOf a)) of
  "Bool"  -> "boolean"
  "Int"   -> "int"
  "Float" -> "float"
  _       -> "void *"
