module Examples(expr, sol)
where

expr :: Int -> String
-- Ejemplos con Var, Zero, Succ, Lam, App, listas y ZipWith
expr 1 = "0"
expr 2 = "succ(0)"
expr 3 = "x"
expr 4 = "succ(x)"
expr 5 = "f x"
expr 6 = "f 0"
expr 7 = "succ(f 0)"
expr 8 = "0 f"
expr 9 = "f 0 succ(succ(0))"
expr 10 = "f (0 succ(succ(0)))"
expr 11 = "f x y z"
expr 12 = "f (x y z)"
expr 13 = "f succ(x) y z"
expr 14 = "f (succ(x) y z)"
expr 15 = "x x"
expr 16 = "(\\x -> x)"
expr 17 = "(\\x -> 0)"
expr 18 = "(\\y -> x)"
expr 19 = "(\\x -> x) y"
expr 20 = "\\s -> \\x -> \\y -> s x y"
expr 21 = "\\s -> \\x -> \\y -> s (x y)"
expr 22 = "(\\s -> \\x -> \\y -> s) (x y)"
expr 23 = "pred(x)"
expr 24 = "pred(succ(0))"
expr 25 = "isZero(0)"
expr 26 = "isZero(succ(x))"
expr 27 = "isZero(f x)"
expr 28 = "true"
expr 29 = "false"
expr 30 = "if f x then 0 else 0"
expr 31 = "if true then 0 else succ(0)"
expr 32 = "if 0 then 0 else 0"
expr 33 = "if false then true else succ(0)"
expr 34 = "[]"
expr 35 = "0 :: succ(0) :: []"
expr 36 = "zip [] and [] with x,y ~> f x y"
expr 37 = "zip 0 :: [] and succ(0) :: [] with x,y ~> succ(f x y)"
expr n = error $ "La expresion " ++ show n ++ " no esta definida"

sol :: Int -> String
-- Juicios de tipado normalizados para las expresiones definidas
sol 1 = "{} >> 0 : Nat"
sol 2 = "{} >> succ(0) : Nat"
sol 3 = "{x:t0} >> x : t0"
sol 4 = "{x:Nat} >> succ(x) : Nat"
sol 5 = "{f:t0 -> t1, x:t0} >> f x : t1"
sol 6 = "{f:Nat -> t0} >> f 0 : t0"
sol 7 = "{f:Nat -> Nat} >> succ(f 0) : Nat"
sol 8 = "Cannot unify Nat and t0 -> t1"
sol 9 = "{f:Nat -> Nat -> t0} >> f 0 succ(succ(0)) : t0"
sol 10 = "Cannot unify Nat and Nat -> t1"
sol 11 = "{f:t0 -> t1 -> t2 -> t3, x:t0, y:t1, z:t2} >> f x y z : t3"
sol 12 = "{f:t2 -> t3, x:t0 -> t1 -> t2, y:t0, z:t1} >> f (x y z) : t3"
sol 13 = "{f:Nat -> t0 -> t1 -> t2, x:Nat, y:t0, z:t1} >> f succ(x) y z : t2"
sol 14 = "Cannot unify Nat and t2 -> t3"
sol 15 = "Cannot unify t1 and t1 -> t2"
sol 16 = "{} >> \\x:t0 -> x : t0 -> t0"
sol 17 = "{} >> \\x:t0 -> 0 : t0 -> Nat"
sol 18 = "{x:t0} >> \\y:t1 -> x : t1 -> t0"
sol 19 = "{y:t0} >> (\\x:t0 -> x) y : t0"
sol 20 = "{} >>\n\\s:t0 -> t1 -> t2 -> \\x:t0 -> \\y:t1 -> s x y\n: (t0 -> t1 -> t2) -> t0 -> t1 -> t2"
sol 21 = "{} >>\n\\s:t1 -> t2 -> \\x:t0 -> t1 -> \\y:t0 -> s (x y)\n: (t1 -> t2) -> (t0 -> t1) -> t0 -> t2"
sol 22 = "{x:t2 -> t3, y:t2} >>\n(\\s:t3 -> \\x:t1 -> \\y:t0 -> s) (x y)\n: t1 -> t0 -> t3"
sol 23 = "{x:Nat} >> pred(x) : Nat"
sol 24 = "{} >> pred(succ(0)) : Nat"
sol 25 = "{} >> isZero(0) : Bool"
sol 26 = "{x:Nat} >> isZero(succ(x)) : Bool"
sol 27 = "{f:t0 -> Nat, x:t0} >> isZero(f x) : Bool"
sol 28 = "{} >> true : Bool"
sol 29 = "{} >> false : Bool"
sol 30 = "{f:t0 -> Bool, x:t0} >>\nif (f x)\n then 0\n else 0\n: Nat"
sol 31 = "{} >>\nif true\n then 0\n else succ(0)\n: Nat"
sol 32 = "Cannot unify Nat and Bool"
sol 33 = "Cannot unify Bool and Nat"
sol 34 = "{} >> []{t0} : [t0]"
sol 35 = "{} >> 0 :: (succ(0) :: []{Nat}) : [Nat]"
sol 36 = "{f:t0 -> t1 -> t2} >>\nzip []{t0} and []{t1} with x,y ~> f x y\n: [t2]"
sol 37 = "{f:Nat -> Nat -> Nat} >>\nzip (0 :: []{Nat}) and (succ(0) :: []{Nat}) with x,y ~> succ(f x y)\n: [Nat]"
sol n = error $ "La solucion " ++ show n ++ " no esta definida"