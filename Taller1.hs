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
evalVar a p = a p

-- flip
--evalVar :: String -> Assignment -> Bool
--evalVar s  a = a s

evalNot :: Bool -> Bool
evalNot = (not)  

evalAnd :: Bool -> Bool -> Bool
evalAnd = (&&) 

evalOr :: Bool -> Bool -> Bool
evalOr = (||) 

-- Chequeado con tabla de verdad
evalImpl :: Bool -> Bool -> Bool
evalImpl = (\p q -> not p || q) 

eval :: Assignment -> Proposition -> Bool
eval a prop = foldProp (evalVar a) evalNot evalAnd evalOr evalImpl prop 

-- Estos los hice porque no se como se haría una función identidad genérica en estos casos
idVal :: String -> Proposition
idVal s = Var s

idNot :: Proposition -> Proposition
idNot p = Not p

idAnd :: Proposition -> Proposition -> Proposition
idAnd p q = And p q

idOr :: Proposition -> Proposition -> Proposition
idOr p q = Or p q

-- No me gusta el nombre, cambienlo tranquilamente
evalImplProp :: Proposition -> Proposition -> Proposition
evalImplProp = (\p q -> Or (Not p) q) 

elimImpl :: Proposition -> Proposition
elimImpl prop = foldProp idVal idNot idAnd idOr evalImplProp prop

-- Este esta totalmente mal.
negateVal :: String -> Proposition
negateVal s = Not (Var s)

negateNot :: Proposition -> Proposition
negateNot p = Not p
                              
negateAnd :: Proposition -> Proposition -> Proposition
negateAnd p q = Or p q

negateOr :: Proposition -> Proposition -> Proposition
negateOr p q = And p q

negateImpl:: Proposition -> Proposition -> Proposition
negateImpl p q = And (Not p) q


idAndRec :: Proposition -> Proposition -> Proposition -> Proposition ->Proposition
idAndRec p prec q qrec = And prec qrec

idOrRec :: Proposition -> Proposition -> Proposition -> Proposition ->Proposition
idOrRec p prec q qrec = Or prec qrec

idImplRec :: Proposition -> Proposition -> Proposition -> Proposition ->Proposition
idImplRec p prec q qrec = Impl prec qrec

negateProp :: Proposition -> Proposition
negateProp prop = foldProp negateVal negateNot negateAnd negateOr negateImpl prop

nnfNot :: Proposition -> Proposition
nnfNot prec = case prec of
                Not p2 -> p2
                Var p2 -> Not $ Var p2
                otherwhise -> negateProp prec

nnf :: Proposition -> Proposition
nnf prop = foldProp idVal nnfNot idAnd idOr idOr (elimImpl prop)

varsVar :: String -> [String]
varsVar s = s:[]

varsNot :: [String] -> [String]
varsNot xs = xs

-- Estas tres claramente pueden ser las mismas, nub es una funcion de Data.List, saca repetidos.
varsAnd :: [String] -> [String] -> [String]
varsAnd xs ys = nub (xs ++ ys)

varsOr :: [String] -> [String] -> [String]
varsOr xs ys = nub (xs ++ ys)

varsImpl :: [String] -> [String] -> [String]
varsImpl xs ys = nub (xs ++ ys)

vars :: Proposition -> [String]
vars prop = foldProp varsVar varsNot varsAnd varsOr varsImpl prop

-- Esto no se si se puede, pero en el pdf dice que podemos usar Data.List y es una funcion de ahi.
parts :: [a] -> [[a]]
parts = subsequences

sat :: Proposition -> [[String]]
sat prop = filter ((flip (eval . assignTrue)) prop) (parts (vars prop))

satisfiable :: Proposition -> Bool
satisfiable prop = length (sat prop) > 0

tautology :: Proposition -> Bool
tautology prop = length (sat prop) == length (parts (vars prop))

equivalent :: Proposition -> Proposition -> Bool
equivalent p q = (eq1 p q) && (eq1 q p) 
                 where eq1 p2 q2 = foldr (\x rec -> rec && eval (assignTrue x) p2) True $ sat q2

-- Proposiciones de prueba --

f1 = Impl (Var "p") (Var "q")
f2 = Not(Impl (Var "p") (Var "q"))
f3 = Not (Var "p")
f4 = Or f1 f2
f5 = Impl (Var "r") (Var "r")
f6 = Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q"))
f7 = Not (And (Var "p") (Var "q"))


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
  eval (assignTrue ["p"]) f3 ~=? eval ("p" ==) f3
  ]


testsEj4 = test [
  True ~=?  eval (assignTrue ["p","q"]) f6
  ]

testsEj5 = test [
  eval (assignTrue ["p","q"]) f6 ~=?  eval (assignTrue ["p","q"]) (elimImpl f6),
  Var "q" ~=?  negateProp (Not $ Var "q"),
  Var "p" ~=?  nnf (Not $ Not $ Var "p")
  ]

testsEj6 = test [
  [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] ~=? parts [1,2,3]
  ]

