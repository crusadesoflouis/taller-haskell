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



instance Show Proposition where
  show = foldProp id showNot showAnd showOr showImpl
      where showNot p = '\172' : p
            showAnd p q = '(':p ++ (" \8743 " ++ q ++ ")")
            showOr p q = '(':p ++ (" \8744 " ++ q ++ ")")
            showImpl p q = '(':p ++ (" \8835 " ++ q ++ ")")

--Códigos Unicode para simbolitos por si hay problemas de codificación:  \172 not, \8835 implica, \8743 and , \8744 or.

assignTrue :: [String] -> Assignment
assignTrue  = flip elem 

eval :: Assignment -> Proposition -> Bool
eval a = foldProp (a $) not (&&) (||) (\p q -> not p || q) 

elimImpl :: Proposition -> Proposition
elimImpl = foldProp Var Not And Or evalImplProp 
                 where evalImplProp = (\p q -> Or (Not p) q) 

negateProp :: Proposition -> Proposition
negateProp = recProp (\s -> Not (Var s)) (\p prec -> p) negateAnd negateOr negateImpl
                 where negateAnd = (\_ prec _ qrec -> Or prec qrec)
                       negateOr = (\_ prec _ qrec -> And prec qrec)
                       negateImpl = (\p _ _ qrec -> And p qrec)

nnf :: Proposition -> Proposition
nnf = foldProp Var negateProp And Or (error "etc") . elimImpl 

vars :: Proposition -> [String]
vars  = foldProp (:[]) id union union union 

parts :: [a] -> [[a]]
parts  = foldr (\x rec -> (map (x:) rec) ++ rec) [[]] 

sat :: Proposition -> [[String]]
sat prop = filter ((flip (eval . assignTrue)) prop) (parts (vars prop))

satisfiable :: Proposition -> Bool
satisfiable = not . null . sat

tautology :: Proposition -> Bool
tautology prop = length (sat prop) == length (parts (vars prop))

equivalent :: Proposition -> Proposition -> Bool
equivalent p q = tautology (Impl p q) && tautology (Impl q p)

-- Proposiciones de prueba --

f1 = Impl (Var "p") (Var "q")
f2 = Not(Impl (Var "p") (Var "q"))
f3 = Not (Var "p")
f4 = Or f1 f2
f5 = Impl (Var "r") (Var "r")
f6 = Impl (And (Var "p") (Var "r")) (And (Not (Var "q")) (Var "q"))
f7 = Not (And (Var "p") (Var "q"))
f8 = And f1 f6
f9 = Not f3
f10 = (Impl f6 f6)
f11 = Or f2 f1
f12 = Not $ Not $ Not $ Not $ Not $ Not $ Var "p"



-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
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
  Var "p" ~=?  nnf (Not $ Not $ Var "p"),
  Var "p" ~=? nnf (f12)
  ]

testsEj6 = test [
  [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] ~=? parts [1,2,3],
  True ~=? tautology f10,
  True ~=? equivalent f4 f11
  ]

testsEj7 = test [
  True ~=? all (\p -> equivalent p (nnf p)) [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12] 
  ]
