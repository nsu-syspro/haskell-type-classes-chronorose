{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Data.List (nub)
import Task1 (Parse (..))
import Task2 (Eval (..), Expr (..), evalExpr)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satisfiable*
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
solveSAT :: String -> Maybe Bool
solveSAT str = or <$> results
  where
    sat :: Maybe (Expr Bool BoolOp)
    sat = parse str
    bools = generateBools . nub . gatherVars <$> sat
    results = bools >>= mapM (\bs -> sat >>= evalExpr bs)

generateBools :: [String] -> [[(String, Bool)]]
generateBools [] = [[]]
generateBools (v : vars) = [(v, b) : vs | b <- [True, False], vs <- generateBools vars]

gatherVars :: Expr a op -> [String]
gatherVars (Lit _) = []
gatherVars (Var x) = [x]
gatherVars (BinOp _ l r) = concatMap gatherVars [l, r]

data BoolOp = Xor | And | Or

newtype SATBool = SATBool {getBool :: Bool}

instance Parse BoolOp where
  parse :: String -> Maybe BoolOp
  parse "xor" = Just Xor
  parse "and" = Just And
  parse "or" = Just Or
  parse _ = Nothing

instance Eval Bool BoolOp where
  evalBinOp Xor = (/=)
  evalBinOp And = (&&)
  evalBinOp Or = (||)
