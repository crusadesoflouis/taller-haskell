import Data.List
import Test.HUnit

data Proposition = Var String | Not Proposition | And Proposition Proposition | Or Proposition Proposition | Impl Proposition Proposition deriving Eq

type Assignment = String -> Bool

recProp :: (String -> b) -> (Proposition -> b -> b) -> (Proposition -> b -> Proposition -> b -> b) -> (Proposition -> b -> Proposition -> b -> b) -> (Proposition -> b -> Proposition -> b -> b) -> Proposition -> b
recProp casoVar casoNot casoAnd casoOr casoImpl prop = case prop of
      Var s -> casoVar s
      Not p -> casoNot p (rec p)
      And p q -> casoAnd p (rec p) q (rec q)
      Or p q -> casoOr p (rec p) q (rec q)
      Impl p q -> casoImpl p (rec p) q (rec q)
      where rec = recProp casoVar casoNot casoAnd casoOr casoImpl


-- TODO reescribir foldProp usando recProp. En cada caso no usar las proposiciones

foldProp ::  (String -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Proposition -> b
foldProp casoVar casoNot casoAnd casoOr casoImpl prop = case prop of
      Var s -> casoVar s
      Not p -> casoNot (rec p)
      And p q -> casoAnd (rec p) (rec q)
      Or p q -> casoOr (rec p) (rec q)
      Impl p q -> casoImpl (rec p) (rec q)
      where rec = foldProp casoVar casoNot casoAnd casoOr casoImpl


showVar s = s
showNot p = '\172' : p
showAnd p q = '(':p ++ (" \8743 " ++ q ++ ")")
showOr p q = '(':p ++ (" \8744 " ++ q ++ ")")
showImpl p q = '(':p ++ (" \8835 " ++ q ++ ")")

instance Show Proposition where
  show = foldProp showVar showNot showAnd showOr showImpl
--Códigos Unicode para simbolitos por si hay problemas de codificación:  \172 not, \8835 implica, \8743 and , \8744 or.
  
assignTrue :: [String] -> Assignment
assignTrue xs = (\ x -> elem x xs) 

evalVar ::  Assignment -> String -> Bool
evalVar a s = a s

-- flip
--evalVar :: String -> Assignment -> Bool
--evalVar s  a = a s

evalNot :: Bool -> Bool
evalNot = (not)  

evalAnd :: Bool -> Bool -> Bool
evalAnd = (&&) 

evalOr :: Bool -> Bool -> Bool
evalOr = (||) 

evalImpl :: Bool -> Bool -> Bool
evalImpl = (\p q -> not p || (p && q)) 

eval :: Assignment -> Proposition -> Bool
eval a prop = foldProp (evalVar a) evalNot evalAnd evalOr evalImpl prop 

idVal :: String -> Proposition
idVal s = Var s

idNot :: Proposition -> Proposition
idNot p = Not p

idAnd :: Proposition -> Proposition -> Proposition
idAnd p q = And p q

idOr :: Proposition -> Proposition -> Proposition
idOr p q = Or p q

evalImplProp :: Proposition -> Proposition -> Proposition
evalImplProp = (\p q -> Or (Not p) (And p q)) 

elimImpl :: Proposition -> Proposition
elimImpl prop = foldProp idVal idNot idAnd idOr evalImplProp prop


negateProp :: Proposition -> Proposition
negateProp = undefined

nnf :: Proposition -> Proposition
nnf = undefined

vars :: Proposition -> [String]
vars = undefined

parts :: [a] -> [[a]]
parts = undefined

sat :: Proposition -> [[String]]
sat = undefined

satisfiable :: Proposition -> Bool
satisfiable = undefined

tautology :: Proposition -> Bool
tautology = undefined

equivalent :: Proposition -> Proposition -> Bool
equivalent = undefined

-- Proposiciones de prueba --

f1 = Impl (Var "p") (Var "q")
f2 = Not(Impl (Var "p") (Var "q"))
f3 = Not (Var "p")
f4 = Or f1 f2
f5 = Impl (Var "r") (Var "r")


-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

testsEj1 = test [
  5 ~=? foldProp (const 1) (+1) (+) (+) (+) f4
  ]
  
testsEj2 = test [
  "\172(p \8835 q)"~=? show f2
  ]

testsEj3 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  True ~=?  eval (assignTrue ["p","q"]) (Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q")))
  ]

testsEj5 = test [
  eval (assignTrue ["p","q"]) (Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q"))) ~=?  eval (assignTrue ["p","q"]) (elimImpl(Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q")))),
  Var "q" ~=?  negateProp (Not $ Var "q"),
  Var "p" ~=?  nnf (Not $ Not $ Var "p")
  ]

testsEj6 = test [
  [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] ~=? parts [1,2,3]
  ]

