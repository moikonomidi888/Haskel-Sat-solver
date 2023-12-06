import Control.Monad
import Data.Maybe

lookupJust key = fromJust . lookup key

data Formula a = AND (Formula a) (Formula a) |
                 OR  (Formula a) (Formula a) |
                 NOT (Formula a)             |
                 VAR a deriving (Eq, Show)

eval::(Eq a) => Formula a -> [(a,Bool)] -> Bool
eval (VAR x) y = lookupJust x y
eval (AND x y) j = (eval x j) && (eval y j)
eval (OR x y) j = (eval x j) || (eval y j)
eval (NOT x) y = not (eval x y)

solvef:: (Eq a) => [(a, Bool)] -> Formula a -> Maybe [(a,Bool)]

solvef x (VAR y) = case lookup y x of
         Nothing -> Just ((y,False):x)
         Just b  -> if b
                    then Nothing
                    else Just x

solvef x (AND y z) = case solvef x y of
           Nothing -> solvef x z
           Just x' -> Just x'

solvef x (OR y z) = case solvef x y of
          Nothing -> Nothing
          Just x' -> solvef x' z

solvef x (NOT y) = solve x y

solve :: (Eq a) => [(a, Bool)] -> Formula a -> Maybe [(a,Bool)]

solve x (VAR y) = case lookup y x of
       Nothing  -> Just ((y,True):x)
       Just b   -> if b
                   then Just x
                   else Nothing

solve x (AND y z) = case solve x y of
          Nothing -> Nothing
          Just x' -> solve x' z

solve x (OR y z) = case solve x y of
         Nothing -> solve x z
         Just x' -> Just x'

solve x (NOT y) = solvef x y
