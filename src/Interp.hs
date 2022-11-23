https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Interp where

import Data.Maybe (fromJust)
import Common (Label, Var, Value(..), binary, unary)
import Sigma (Method(..), Term(..))

data MethodClosure = Closure Env Var Term
type Obj = [(Label, MethodClosure)]
type Mem = [Obj]

replace :: Int -> Label -> MethodClosure -> Mem -> Mem
replace i l closure mem =
  let (before, o : after) = splitAt i mem in
  before ++ [[if l == n then (n, closure) else (n, c) | (n, c) <- o]] ++ after

type Env = [(Var, Value)]

evaluate :: Term -> Env -> Mem -> Maybe (Value, Mem)
evaluate (Var x) env mem =
  case lookup x env of
    Just a -> Just (a, mem)
    Nothing -> Nothing
evaluate (Object ms) env mem =
  let m2mc (l, Method self term) = (l, Closure env self term) in
  Just (ObjRef (length mem), mem ++ [map m2mc ms])
evaluate (Invoke a l) env mem =
  case evaluate a env mem of
    Just (ObjRef i, mem') ->
      let ms = mem' !! i in
      case lookup l ms of
        Just (Closure env self term) -> evaluate term ((self, ObjRef i) : env) mem'
        _ -> Nothing
    _ -> Nothing
evaluate (Update a l (Method self term)) env mem =
  case evaluate a env mem of
    Just (ObjRef i, mem') ->
      Just (ObjRef i, replace i l (Closure env self term) mem')
    _ -> Nothing
evaluate (Lit v) env mem = Just (v, mem)
evaluate (Unary op a) env mem =
  case evaluate a env mem of
    Just (a', mem') -> Just (unary op a', mem')
    Nothing -> Nothing
evaluate (Binary op a b) env mem =
  case evaluate a env mem of
    Just (a', mem') -> case evaluate b env mem' of
      Just (b', mem'') -> Just (binary op a' b', mem'')
      Nothing -> Nothing
    Nothing -> Nothing
evaluate (If a b c) env mem = error "TODO: Question 1"
evaluate (Let x a b) env mem = error "TODO: Question 1"
evaluate (Clone a) env mem = error "TODO: Question 2"

execute :: Term -> Value
execute e = fst $ fromJust $ evaluate e [] []
