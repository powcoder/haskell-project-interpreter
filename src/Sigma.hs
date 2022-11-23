https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Sigma where

import Common (Label, Var, Value, UnaryOp (..), BinaryOp (..), showMethod)
import Data.List (intercalate)
import Prelude hiding (LT, GT, EQ)

data Term = Var Var
          | Object [(Label, Method)]
          | Invoke Term Label
          | Update Term Label Method
          | Clone Term
          | Lit Value
          | Unary UnaryOp Term
          | Binary BinaryOp Term Term
          | If Term Term Term
          | Let Var Term Term
          deriving Eq

instance Show Term where
  show (Var x) = x
  show (Object ms) = "{ " ++ intercalate ", " (map showMethod ms) ++ " }"
  show (Invoke a l) = show a ++ "." ++ l
  show (Update a l m) = show a ++ "." ++ l ++ " := " ++ show m
  show (Clone a) = "clone(" ++ show a ++ ")"
  show (Lit v) = show v
  show (Unary op a) =  show op ++ show a
  show (Binary op a b) = "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"
  show (If a b c) = "if (" ++ show a ++ ") " ++ show b ++ "; else " ++ show c
  show (Let x a b) = "var " ++ x ++ " = " ++ show a ++ "; " ++ show b

data Method = Method Var Term deriving Eq

instance Show Method where
  show (Method s t) = s ++ " => " ++ show t
