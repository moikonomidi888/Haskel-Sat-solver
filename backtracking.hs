import Data.Maybe
import Data.List
import System.IO
import System.Random (randomRIO)
import GHC.Types
import GHC.Classes
import GHC.CString
import GHC.Magic
import GHC.Prim
import GHC.Prim.Ext
import GHC.Err
import GHC.Maybe
import GHC.IO
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import GHC.Tuple (Solo (..))     -- Note [Depend on GHC.Tuple]
import GHC.Num.Integer ()        -- Note [Depend on GHC.Num.Integer]


data Formula a = AND (Formula a) (Formula a) |
                 OR  (Formula a) (Formula a) |
                 NOT (Formula a)|
                 VAR a deriving (Eq, Show)

data VBool a = VR  a |
                  T  |
                  F deriving (Eq, Show)

(+++)               :: VBool a -> VBool a -> VBool a
VR x +++ VR y = VR x
T +++ b       =  b
b +++ F       = F
b +++ T       =  b
F +++ b       = F

(//)               :: VBool a -> VBool a -> VBool a
VR x // VR y = VR x
T // b       = T
b // T       = T
F // b       = b
b // F       = b

neg               :: VBool a -> VBool a
neg (VR x) = VR x
neg T    =  F
neg F    =  T

splitList :: [Int] -> Int -> [Int]
splitList [] m = []
splitList x 0 = []
splitList (x:xs) m = x : splitList xs (m - 1)

splitList2 :: [Int] -> Int -> [Int]
splitList2 [] m = []
splitList2 x 0 = x
splitList2 x m = case ((length x) > m) of
                True  -> (drop ((length x) - m) x)
                False -> []

-- 2nd input : length , 3rd input: number of variables

auxGenerate :: [Int] -> Int -> Int -> Formula Int
auxGenerate [] x y = VAR 0
auxGenerate (z:zs) x y  = case x < 3 of
                 True -> case ((rem z 2) == 1) of
                    True -> NOT (VAR (rem (div z 2) y))
                    False -> VAR (rem (div z 2) y)
                 False -> (case ((rem z 2) == 1) of
                   False -> AND (auxGenerate (splitList zs k) k y)(auxGenerate (splitList2 zs k)(x - k - 1) y)
                   True  -> OR (auxGenerate (splitList zs k) k y)(auxGenerate (splitList2 zs k)(x - k - 1) y))
                           where k = (rem (div z 2)((x-2)+1))

generate :: Int -> Int -> IO(Formula Int)
generate x y = do
                  w <- (randomList x (2*x*y)); return (auxGenerate w x y)


randomList :: Int -> Int -> IO([Int])
randomList 0 u = return []
randomList n u = do
  r  <- randomRIO (0, u)
  rs <- (randomList j u)
  return (r:rs)
     where j = n - 1

extr :: [((a, Bool), Bool)] -> [(a, Bool)]
extr xs =  map fst xs


eval::(Eq a) => Formula a -> [(a,Bool)] -> VBool a
eval (VAR x) y   = case lookup x y of
                 Nothing    -> VR x
                 Just True  -> T
                 Just False -> F
eval (AND x y) j = eval x j +++ eval y j
eval (OR x y) j  = eval x j // eval y j
eval (NOT x) y   = neg (eval x y)

backtr :: [((a, Bool),Bool)] -> Maybe [((a, Bool),Bool)]
backtr (((x,b),False):xs) = Just (((x,(not b)),True):xs)
backtr (((x,b),True):xs)  = backtr xs
backtr  []              = Nothing

solve :: (Eq a) => [((a, Bool),Bool)] -> Formula a -> Maybe [(a,Bool)]

solve x phi = case (eval phi (extr x)) of
              VR y   ->  solve (((y, True),False):x) phi
              T      ->  Just (extr x)
              F      ->  case backtr x of
                         Just y -> solve y phi
                         Nothing -> Nothing

combinator :: IO (Formula Int) -> IO (Maybe [(Int, Bool)])
combinator x = do
                  y <- x; return (solve [] y)

conv :: IO (Maybe [(Int, Bool)]) -> IO Int
conv a = do
            g <- a;
            return (case g of
                   Just b  -> (length b)
                   Nothing -> 0)

functionator ::   Int -> Int -> Int -> IO [Int]
functionator 0 y z = return []
functionator x y z = do
                       p <- conv (combinator (generate y z)); u <- functionator (x-1) y z; return (p:u)
