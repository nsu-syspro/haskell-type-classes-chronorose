-- The above pragma enables all warnings
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Task2 where

import Control.Applicative (Alternative (..))
import Task1 (Parse (..))

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op
  = Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving (Show)

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving (Show)

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
type OperandStack a op = [Expr a op]

instance (Parse a, Parse op) => Parse (Expr a op) where
  parse :: String -> Maybe (Expr a op)
  parse = flip parseExpr [] . words
    where
      parseExpr :: [String] -> OperandStack a op -> Maybe (Expr a op)
      parseExpr [] [x] = Just x
      parseExpr [] _ = Nothing
      parseExpr (x : xs) operands =
        foldr ((<|>) . (\f -> f x operands)) mempty [parseOp, parseOperand, parseVar]
          >>= parseExpr xs

      parseOp :: String -> OperandStack a op -> Maybe (OperandStack a op)
      parseOp op (x : y : rest) = (: rest) <$> (BinOp <$> parse op <*> Just y <*> Just x)
      parseOp _ _ = Nothing

      parsePrimitive constr operands = fmap ((: operands) . constr) . parse

      parseVar :: String -> OperandStack a op -> Maybe (OperandStack a op)
      parseVar = flip $ parsePrimitive Var

      parseOperand :: String -> OperandStack a op -> Maybe (OperandStack a op)
      parseOperand = flip $ parsePrimitive Lit

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

instance Parse IntOp where
  parse :: String -> Maybe IntOp
  parse "+" = Just Add
  parse "*" = Just Mul
  parse "-" = Just Sub
  parse _ = Nothing

instance Eval Integer IntOp where
  evalBinOp Add = (+)
  evalBinOp Mul = (*)
  evalBinOp Sub = (-)

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit x) = Just x
evalExpr xs (Var x) = lookup x xs
evalExpr xs (BinOp op l r) =
  evalExpr xs l
    >>= ( \left ->
            evalExpr xs r
              >>= (Just . evalBinOp op left)
        )

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @Integer @IntOp

-- lol xs str = parse str >>= evalExpr xs

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing
