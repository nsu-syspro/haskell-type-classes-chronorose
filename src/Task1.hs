{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

import Control.Applicative (Alternative (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Product (..), Sum (..))
import Text.Read (readMaybe)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr
  = Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving (Show)

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
evalIExpr :: IExpr -> Integer
evalIExpr (Lit x) = evalMonoid (getSum . runIdentity) (Identity . Sum) [x]
evalIExpr (Add x y) = evalMonoid getSum Sum (map evalIExpr [x, y])
evalIExpr (Mul x y) = evalMonoid getProduct Product (map evalIExpr [x, y])

evalMonoid :: (Monoid m) => (m -> Integer) -> (Integer -> m) -> [Integer] -> Integer
evalMonoid dest constr = dest . mconcat . map constr

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
type OperandStack = [IExpr]

instance Parse Integer where
  parse :: String -> Maybe Integer
  parse = readMaybe

instance Parse Bool where
  parse "true" = Just True
  parse "false" = Just False
  parse _ = Nothing

instance Parse String where
  parse :: String -> Maybe String
  parse = Just

instance Parse IExpr where
  parse :: String -> Maybe IExpr
  parse = flip parseIExpr [] . words
    where
      parseIExpr :: [String] -> OperandStack -> Maybe IExpr
      parseIExpr [] [x] = Just x
      parseIExpr [] _ = Nothing
      parseIExpr (x : xs) operands = foldr ((<|>) . (\f -> f x operands)) Nothing [parseOperand, parseOp] >>= parseIExpr xs

      parseOperand :: String -> OperandStack -> Maybe OperandStack
      parseOperand str operands = (: operands) . Lit <$> parse str

      parseOp :: String -> OperandStack -> Maybe OperandStack
      parseOp "+" (x : y : rest) = Just (Add y x : rest)
      parseOp "*" (x : y : rest) = Just (Mul y x : rest)
      parseOp _ _ = Nothing

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr str = evalIExpr <$> parse str
