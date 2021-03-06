---------------------------
--      Integrantes		---
---------------------------
-- Arroyo, Luis			  |
-- Godoy, Javier          |
-- Humpiri, Juan Carlos	  |
--------------------------

import Data.List
import Test.HUnit
import Midi.Midi

type Tono         = Integer
type Duracion     = Integer
type Instante     = Integer

data Melodia =
     Silencio Duracion |
     Nota Tono Duracion |
     Secuencia Melodia Melodia |
     Paralelo [Melodia]
  deriving (Show, Eq)

-- Funciones auxiliares dadas

foldNat :: a->(a->a)->Integer->a
foldNat caso0 casoSuc n | n == 0 = caso0
      | n > 0 = casoSuc (foldNat caso0 casoSuc (n-1))
      | otherwise = error "El argumento de foldNat no puede ser negativo."

-- Funciones pedidas

-- Ejercicio 1
superponer :: Melodia->Duracion->Melodia->Melodia
superponer m1 s m2 = Paralelo[m1,Secuencia (Silencio s) (m2)]

-- Sugerencia: usar foldNat
canon :: Duracion->Integer->Melodia->Melodia
canon duracion repeticion m1 = foldNat m1 (superponer  m1 duracion) (repeticion-1)

-- foldr1 : (a -> a -> a) -> [a] -> a

secuenciar :: [Melodia] -> Melodia --Se asume que la lista no es vacía.
secuenciar = foldr1 Secuencia 

-- secuenciar = foldr1 (\x rec Secuencia x rec)

-- Secuencia x (foldr1 (\x rec Secuencia x rec))
-- Secuencia x (Secuencia x (foldr1 (\x rec Secuencia x rec))
-- hasta que consume toda la secuencia

-- Ejercicio 2
canonInfinito :: Duracion->Melodia->Melodia
canonInfinito d m = foldr (\_ rec -> superponer m d rec) m [0..]

-- Paralelo[m,Secuencia (Silencio d) (foldr (\_ rec -> superponer m d rec) m [1..])]
-- Paralelo[m,Secuencia (Silencio d) (Paralelo[m,Secuencia (Silencio d) (.....)]]

-- pregunta por que no anda  foldr1 (\_ rec -> superponer m d rec) [0..]

-- Ejercicio 3
foldMelodia :: (Duracion -> b) -> (Tono -> Duracion -> b) -> (b -> b -> b) -> ([b]->b) -> Melodia -> b
foldMelodia caso_silencio caso_nota caso_secuencia caso_paralelo p = case p of
                                  (Silencio duracion) -> caso_silencio duracion
                                  (Nota tono duracion) -> caso_nota tono duracion
                                  (Secuencia m1 m2) -> caso_secuencia (rec m1) (rec m2)
                                  (Paralelo ms) -> caso_paralelo (map rec ms)
                                  where rec = foldMelodia caso_silencio caso_nota caso_secuencia caso_paralelo



mapMelodia :: (Tono -> Tono)->Melodia->Melodia
mapMelodia f = foldMelodia (Silencio) (\t d -> Nota (f t) d ) (Secuencia) (Paralelo)


transportar :: Integer -> Melodia -> Melodia
transportar n = mapMelodia ((+) n)


duracionTotal :: Melodia->Duracion
duracionTotal = foldMelodia (id) (\t d -> d) (+) (foldr (+) 0)


cambiarVelocidad :: Float->Melodia->Melodia--Sugerencia: usar round y fromIntegral

cambiarVelocidad factor= foldMelodia
                  (\d -> Silencio (rf factor d ) )
                  (\t d -> Nota t (rf factor d ) )
                  (Secuencia)
                  (Paralelo)
                  where rf a b = round(a * fromIntegral b)


invertir :: Melodia -> Melodia
invertir = foldMelodia
                  (Silencio)
                  (Nota)
                  (flip Secuencia)
                  (Paralelo)

-- Ejercicio 5
-- En instantes menores que 0 no suena ninguna nota. Se puede usar recursión explícita. Resaltar las partes del código que hacen que no se ajuste al esquema fold.
--Sugerencia: usar concatMap.

notasQueSuenan :: Instante->Melodia->[Tono]
notasQueSuenan n _ | n < 0 = []  
notasQueSuenan n p = case p of
  (Silencio d) -> []
  (Nota t d) -> if n < d then [t] else []
  (Secuencia m1 m2) -> if duracionTotal m1 > n then notasQueSuenan n m1 else notasQueSuenan (n-duracionTotal m1) m2
  (Paralelo xs) -> nub (concatMap (notasQueSuenan n) xs)


-------------------------------------------------
-- 	COMENTARIO 1: No se puede usar foldMelodia---
-------------------------------------------------
-- Porque al hacer recursión sobre la estructura de  
-- datos Secuencia de Melodía, el foldMelodia nos devuelve dos listas de tonos,y sobre 
-- estas no podemos saber la duración total de cada una de ellas, para así devolver 
-- alguna lista de Tono sobre la recursión que estamos haciendo.

-- Otra cosa interesante a notar, es que el Instante (n) que nos pasan no lo podemos 
-- modificar y reutilizar para hacer la recursión sobre la secuencia de la melodía y 
-- por ende no podemos elegir qué tonos vamos a devolver en la recursion en un 
-- instante (n -durancionTotal)

-- Otra cosa que también vemos que no podemos hacer, es intentar devolver el caso cuando
-- el Instante (n) toma valores negativos, a la hora de hacer la recursion con foldMelodia.

-------------------------------------------------------------
-- COMENTARIO 2: notasQueSuenan' :: Melodia->Instante->[Tono]
-------------------------------------------------------------
-- notasQueSuenan' m n = foldMelodia 
-- (\d -> []) 
-- (\t d -> if (n<d) then [t] else [] ) 
-- (\rec1 rec2 -> if (duracionTotal rec1) > n then rec1 else loQueQuieroDevolver)
-- (\lrec ->  concatMap (lrec))
-- where loQueQuieroDevolver rec2

-- Nos gustaria devolver esto:
-- where loQueQuieroDevolver = notasQueSuenan (n - (duracionTotal rec1)  rec2)

-- En este caso si nuestra función notasQueSuena, devolviera un función que va de 
-- (Instante -> [Tono]), en este caso sí podemos estar modificando en cada paso de 
-- la recursión el instante "n", ya que en cada paso de recursión debemos instanciar
-- la recursión para obtener la lista de tonos.
-- Pero todavía tenemos el problema que habíamos tratado en el "COMENTARIO 1", sobre la
-- duracionTotal de una melodía, para cuando debemos devolver la lista de tonos para la
-- estructura de (Secuecnia melodia1 melodia2), por ende tampoco podemos resolver el problema
-- intentado aplicar evaluación parcial.

-- Otro punto importante a destacar, es que usando notasQueSuenan', si podemos resolver el problema
-- que se da cuando el Instante (n) toma valores negativos. A continuación presentamos una forma de
-- cómo resolverlo:

-- notasQueSuenan' :: Melodia->(Instante->[Tono])
-- notasQueSuenan' m = foldMelodia (\d -> \instan -> []) 
-- (\t d -> \instan ->  if instan < 0 then [] else (if (instan<d) then [t] else []) )
-- (\rec1 rec2 -> \instan -> if instan < 0 then [] else (if (duracionTotal rec1) > instan then rec1 instan else rec2 (instan - (duracionTotal rec1))))
-- (\lrec -> \instan -> if instan < 0 then [] else concatMap (lrec instan))
-- duracionTotal rec1


-- Ejercicio 6

data Evento = On Instante Tono | Off Instante Tono deriving (Show, Eq)

--Sugerencia: usar listas por comprensión. No repetir eventos.
cambios :: Instante->[Tono]->[Tono]->[Evento]
cambios i t1 t2 = nub([ Off i x | x <- t1, not (elem x t2)] ++ [ On i y | y <- t2, not (elem y t1)])

--6 b
--Sugerencia: usar foldl sobre la lista de 0 a la duración.
eventosPorNotas :: (Instante->[Tono])->Duracion->[Evento]
eventosPorNotas f d = foldl (\acum x -> acum ++ if x == (d+1) then  cambios x (f (x-1)) [] else cambios x (f (x-1)) (f x)) (cambios 0 [] (f 0)) [1..(d+1)]



--6 c
eventos :: Melodia -> Duracion -> [Evento]
eventos m d = eventosPorNotas (flip notasQueSuenan m) d

----funciones auxiliares
---funcion auxiliar para probar el ejericio 6-----
funcionAnonima :: Instante -> [Tono]
funcionAnonima = (\x -> case x of 
                                0 -> [60] 
                                1 -> [60,64] 
                                2 -> [] 
                                3 -> [67])

funcionAnonima2 :: Instante -> [Tono]
funcionAnonima2 = (\x -> case x of 
                                0 -> []
                                1 -> [60,61,62]
                                2 -> [61,63]
                                3 -> [61]
                                4 -> [61,64]
                                5 -> [])

funcionLoca :: Melodia-> Instante -> [Tono]
funcionLoca m = flip notasQueSuenan m


takeM :: Integer -> Melodia -> Melodia

takeM _ (Silencio d) = Silencio d
takeM _ (Nota t d) = Nota t d
takeM n (Secuencia m1 m2) = Secuencia (takeM (n-1) m1) (takeM (n-1) m2)
takeM n (Paralelo xs) = Paralelo (map (\x -> takeM (n-1) x) xs)

-- GENERADOR

unev (On i x)  = (i, Left x)
unev (Off i x) = (i, Right x)

generarMidi :: String -> [Evento] -> IO ()
generarMidi archivo eventos = midiCreateFile archivo midiEvents
  where
    eventos' = let e = map unev eventos in zipWith (\(t0, _) (t1, e) -> (t1 - t0, e)) ((0, error ""):e) e
    midiEvents = case eventos' of
                   [] -> [midiNoteOn 1 0 0 10, midiNoteOn 1 0 0 0]
                   es -> toMidi es

toMidi = map (\(d, ev) -> case ev of
                Left  n -> midiNoteOn d 0 n 127
                Right n -> midiNoteOn d 0 n 0)

--Notas para pruebas.

_sol0 = Nota 55
_si0  = Nota 59
_do = Nota 60
_reb  = Nota 61
_re = Nota 62
_mib  = Nota 63
_mi = Nota 64
_fa = Nota 65
_fas  = Nota 66
_sol = Nota 67
_lab  = Nota 68
_la = Nota 69
_sib  = Nota 70
_si = Nota 71
_do2  = Nota 72
_reb2 = Nota 73
_re2  = Nota 74
_mib2 = Nota 75
_fa2  = Nota 77

-- Melodías para pruebas.

acorde :: Melodia
acorde = Paralelo [_do 10, Secuencia (Silencio 3) (_mi 7), Secuencia (Silencio 6) (_sol 4)]

doremi :: Melodia
doremi = secuenciar [_do 3, _re 1, _mi 3, _do 1, _mi 2, _do 2, _mi 4]

-- Pongan sus propias pruebas y melodías. Pueden definir más notas, la numeración es por semitonos.

-- Canon APL (autor: Pablo Barenbaum)

rhoRhoRhoOfX, alwaysEqualsOne, rhoIsDimensionRhoRhoRank, aplIsFun :: Melodia
rhoRhoRhoOfX = secuenciar $ map (\(d, f)->f d) [(4, _do), (4, _do), (3, _do), (1, _re), (4, _mi)]
alwaysEqualsOne = secuenciar $ map (\(d, f)->f d) [(3, _mi), (1, _re), (3, _mi), (1, _fa), (8, _sol)]
rhoIsDimensionRhoRhoRank = secuenciar $ map (\(d, f)->f d) [(12, _do2), (12, _sol), (12, _mi), (12, _do)]
aplIsFun = secuenciar $ map (\(d, f)->f d) [(3, _sol), (1, _fa), (3, _mi), (1, _re), (8, _do)]

mezcla :: Melodia
mezcla = Paralelo [rhoRhoRhoOfX, Secuencia (Silencio 4) alwaysEqualsOne, Secuencia (Silencio 8) rhoIsDimensionRhoRhoRank, Secuencia (Silencio 12) aplIsFun]

-- Cangrejo (autor: Pablo Barenbaum)

stac :: Tono -> Melodia
stac t = Secuencia (Nota t 9) (Silencio 1)

stacatto :: Melodia -> Melodia
stacatto = foldMelodia Silencio (\t d->stac t) Secuencia Paralelo

cangrejo1 = secuenciar $
         [Silencio 4, _do 2, _mib 2]
      ++ [_sol 2, _lab 4, Silencio 2]
      ++ [_si0 4, Silencio 2, _sol 4]
      ++ [_fas 4, _fa 4]
      ++ [_mi 2, Silencio 2, _mib 4]
      ++ [_re 2, _reb 2, _do 2]
      ++ [_si0 2, _sol0 2, _do 2, _fa 2]
      ++ [_mib 2, _re 4, Silencio 2]
      ++ [_do 2, _mi 2, Silencio 4]
cangrejo2 = secuenciar $ (map (\(d, f)->f d)) $
               [(2, _do), (2, _mib), (2, _sol), (2, _do2)]
            ++ [(1, _sib), (1, _do2), (1, _re2), (1, _mib2),
                (1, _fa2), (1, _mib2), (1, _re2), (1, _do2)]
            ++ [(1, _re2), (1, _sol), (1, _re2), (1, _fa2),
                (1, _mib2), (1, _re2), (1, _do2), (1, _si)]
            ++ [(1, _la), (1, _si), (1, _do2), (1, _mib2),
                (1, _re2), (1, _do2), (1, _si), (1, _la)]
            ++ [(1, _sol), (1, _lab), (1, _sib), (1, _reb2),
                (1, _do2), (1, _sib), (1, _lab), (1, _sol)]
            ++ [(1, _fa), (1, _sol), (1, _lab), (1, _sib),
                (1, _lab), (1, _sol), (1, _fa), (1, _mib)]
            ++ [(1, _re), (1, _mib), (1, _fa), (1, _sol),
                (1, _fa), (1, _mib), (1, _re), (1, _lab)]
            ++ [(1, _sol), (1, _fa), (1, _mib), (1, _do2),
                (1, _si), (1, _la), (1, _sol), (1, _fa)]
            ++ [(1, _mi), (1, _re), (1, _mi), (1, _sol),
                (1, _do2), (1, _sol), (1, _fa), (1, _sol)]

cangrejo = Secuencia c (invertir c)
  where c = Paralelo [cangrejo1, cangrejo2]

--

genMelodia :: String -> Melodia -> Duracion -> IO ()
genMelodia fn m dur = generarMidi fn (eventos m dur)

main :: IO ()
main = do
   putStr "Generando apl-is-fun.mid...\n"
   genMelodia "apl-is-fun.mid" (stacatto mezcla) 500
   putStr "Generando cangrejo.mid...\n"
   genMelodia "cangrejo.mid" (stacatto cangrejo) 1000

-- Tests
tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

testsEj1 = test [
  -- Ej (a)
  Paralelo [Silencio 10,Secuencia (Silencio 5) (Silencio 15)] ~=? superponer (Silencio 10) 5 (Silencio 15),
  Paralelo [Secuencia (Nota 10 10) (Nota 5 15), Secuencia (Silencio 7) (Nota 15 15)] ~=? superponer (Secuencia (Nota 10 10) (Nota 5 15)) 7 (Nota 15 15),
  Paralelo [Paralelo [Secuencia (Nota 50 5) (Silencio 15), Secuencia (Nota 60 10) (Nota 45 7)], Secuencia (Silencio 20) (Secuencia (Nota 15 15) (Nota 60 60))] ~=? superponer (Paralelo [Secuencia (Nota 50 5) (Silencio 15), Secuencia (Nota 60 10) (Nota 45 7)]) 20 (Secuencia (Nota 15 15) (Nota 60 60)),
  -- Ej (b)
  Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Nota 60 4)])] ~=? canon 2 3 (Nota 60 4),
  Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Paralelo [Nota 60 4,Secuencia (Silencio 2) (Nota 60 4)])])])])] ~=? canon 2 6 (Nota 60 4),
  Paralelo [Paralelo[Secuencia (Silencio 10) (Nota 60 4), (Nota 40 10)],Secuencia (Silencio 2) (Paralelo [Paralelo[Secuencia (Silencio 10) (Nota 60 4), (Nota 40 10)],Secuencia (Silencio 2) (Paralelo[Secuencia (Silencio 10) (Nota 60 4), (Nota 40 10)])])] ~=? canon 2 3 (Paralelo[Secuencia (Silencio 10) (Nota 60 4), (Nota 40 10)]),
  -- Ej (c)
  Secuencia (Nota 60 1) (Secuencia (Nota 60 2) (Nota 60 3)) ~=? secuenciar [Nota 60 1, Nota 60 2, Nota 60 3],
  Secuencia (Nota 60 1) (Secuencia (Nota 60 2) (Secuencia (Nota 60 3) (Nota 60 4))) ~=? secuenciar [Nota 60 1, Nota 60 2, Secuencia (Nota 60 3) (Nota 60 4)],
  Secuencia (Nota 60 1) (Secuencia (Nota 60 2) (Secuencia (Silencio 3) (Paralelo [Nota 60 4,Nota 60 5]))) ~=? secuenciar [Nota 60 1, Nota 60 2, Silencio 3, Paralelo[Nota 60 4, Nota 60 5]]
  ]
testsEj2 = test [
  [] ~=? notasQueSuenan 5 (canonInfinito 5 (Silencio 10)),  
  [60] ~=? notasQueSuenan 0 (canonInfinito 2 (Paralelo [Nota 60 3, Secuencia (Silencio 3) (Nota 62 4)])),
  [62,60] ~=? notasQueSuenan 4 (canonInfinito 2 (Paralelo [Nota 60 3, Secuencia (Silencio 3) (Nota 62 4)]))
  ]
testsEj3 = test [
  -- funcion identidad hecha con foldMelodia
  Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Nota 120 3) (Nota 124 1)) (Nota 128 3)) (Nota 120 1)) (Nota 128 2)) (Nota 120 2)) (Nota 128 4) ~=? foldMelodia (\d -> Silencio d ) (\t d -> Nota t d ) (\rec1 rec2 -> Secuencia rec1 rec2) (\lrec ->  Paralelo lrec) (Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Nota 120 3) (Nota 124 1)) (Nota 128 3)) (Nota 120 1)) (Nota 128 2)) (Nota 120 2)) (Nota 128 4))
  -- Cabe aclarar que tanto la funciones mapMelodia, transportar, duracionToal, cambiarVelocidad e invertir
  -- estan testeadas y pero ademas depende foldMelodia, por eso no hicimos mas tests
  ]
testsEj4 = test [
  -- Ej a
  doremi ~=? mapMelodia (\x-> x) doremi,
  Secuencia (Nota 120 3) (Secuencia (Nota 124 1) (Secuencia (Nota 128 3) (Secuencia (Nota 120 1) (Secuencia (Nota 128 2) (Secuencia (Nota 120 2) (Nota 128 4)))))) ~=? mapMelodia (\x-> x*2) doremi,
  -- Ej b
  Secuencia (Nota 70 3) (Secuencia (Nota 72 1) (Secuencia (Nota 74 3) (Secuencia (Nota 70 1) (Secuencia (Nota 74 2) (Secuencia (Nota 70 2) (Nota 74 4)))))) ~=? transportar 10 doremi,
  -- Ej c
  16 ~=? duracionTotal doremi,
  10 ~=? duracionTotal (Silencio 10),
  10 ~=? duracionTotal (Paralelo[Silencio 10]),
  25 ~=? duracionTotal (Paralelo[Secuencia (Nota 60 10) (Silencio 10), Nota 50 5]),
  -- Ej d
  Secuencia (Nota 60 2) (Secuencia (Nota 62 0) (Secuencia (Nota 64 2) (Secuencia (Nota 60 0) (Secuencia (Nota 64 1) (Secuencia (Nota 60 1) (Nota 64 2)))))) ~=? cambiarVelocidad 0.5 doremi,
  Secuencia (Nota 60 6) (Secuencia (Nota 62 2) (Secuencia (Nota 64 6) (Secuencia (Nota 60 2) (Secuencia (Nota 64 4) (Secuencia (Nota 60 4) (Nota 64 8)))))) ~=? cambiarVelocidad 2 doremi,
  Paralelo[Secuencia (Nota 60 20) (Silencio 20), Nota 50 10] ~=? cambiarVelocidad 2 (Paralelo[Secuencia (Nota 60 10) (Silencio 10), Nota 50 5]),
  -- Ej e
  Paralelo[Secuencia (Silencio 10) (Nota 60 10), Nota 50 5] ~=? invertir (Paralelo[Secuencia (Nota 60 10) (Silencio 10), Nota 50 5]),
  Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Secuencia (Nota 64 4) (Nota 60 2)) (Nota 64 2)) (Nota 60 1)) (Nota 64 3)) (Nota 62 1)) (Nota 60 3) ~=? invertir doremi,
  Secuencia (Nota 10 4) (Secuencia (Secuencia (Nota 10 3) (Nota 10 2)) (Nota 10 1)) ~=? invertir (Secuencia (Secuencia (Nota 10 1) (Secuencia (Nota 10 2) (Nota 10 3))) (Nota 10 4))
  ]
testsEj5 = test [
  [60] ~=? notasQueSuenan 0 doremi,
  [60] ~=? notasQueSuenan 1 doremi,
  [60] ~=? notasQueSuenan 2 doremi,
  [62] ~=? notasQueSuenan 3 doremi,
  [64] ~=? notasQueSuenan 4 doremi,
  [64] ~=? notasQueSuenan 5 doremi,
  [64] ~=? notasQueSuenan 8 doremi,
  [64] ~=? notasQueSuenan 9 doremi,
  [60] ~=? notasQueSuenan 11 doremi,
  [64] ~=? notasQueSuenan 12 doremi,
  [64] ~=? notasQueSuenan 13 doremi,
  [64] ~=? notasQueSuenan 14 doremi,
  [64] ~=? notasQueSuenan 15 doremi,
  [] ~=? notasQueSuenan 16 doremi,
  [60] ~=? notasQueSuenan 6 (Paralelo[Secuencia (Nota 60 10) (Silencio 10), Nota 50 5]),
  [60,65] ~=? notasQueSuenan 7 (Paralelo[Secuencia (Nota 60 10) (Silencio 10), Paralelo[Secuencia (Nota 55 7) (Silencio 10), Nota 65 8]])
  ]

testsEj6 = test [
  -- Ej a
  [] ~=? cambios 1 [] [],
  [Off 1 3,On 1 7,On 1 9] ~=? cambios 1 [1,2,3,4,5] [1,2,7,5,7,4,9],
  [Off 0 1, Off 0 2] ~=? cambios 0 [1,2] [],
  [On 3 5, On 3 6] ~=? cambios 3 [] [5,6], 
  -- Ej b
  [On 0 60,On 1 64,Off 2 60,Off 2 64] ~=? eventosPorNotas funcionAnonima 2,
  [On 0 60,On 1 64,Off 2 60,Off 2 64,On 3 67,Off 4 67] ~=? eventosPorNotas funcionAnonima 3,
  [On 0 60, Off 3 60, On 3 62, Off 4 62, On 4 64, Off 6 64] ~=? eventosPorNotas (funcionLoca doremi) 5,
  [] ~=? eventosPorNotas (\x-> []) 3,
  [On 0 60,On 0 61,Off 1001 60,Off 1001 61] ~=? eventosPorNotas (\x-> [60,61]) 1000,
  [] ~=? eventosPorNotas funcionAnonima2 0,
  [On 1 60, On 1 61, On 1 62, Off 2 60, Off 2 61, Off 2 62] ~=? eventosPorNotas funcionAnonima2 1,
  [On 1 60, On 1 61, On 1 62, Off 2 60, Off 2 62, On 2 63, Off 3 61, Off 3 63] ~=? eventosPorNotas funcionAnonima2 2,
  [On 1 60, On 1 61, On 1 62, Off 2 60, Off 2 62, On 2 63, Off 3 63, Off 4 61] ~=? eventosPorNotas funcionAnonima2 3,
  [On 1 60, On 1 61, On 1 62, Off 2 60, Off 2 62, On 2 63, Off 3 63, On 4 64, Off 5 61, Off 5 64] ~=? eventosPorNotas funcionAnonima2 4,
  [On 1 60, On 1 61, On 1 62, Off 2 60, Off 2 62, On 2 63, Off 3 63, On 4 64, Off 5 61, Off 5 64] ~=? eventosPorNotas funcionAnonima2 5,
  -- ej c TODO:  revisar
  [On 0 60, Off 3 60, On 3 62, Off 4 62, On 4 64, Off 6 64] ~=? eventos doremi 5,
  [On 0 60, On 3 64, On 6 67, Off 7 60, Off 7 64, Off 7 67] ~=? eventos acorde 6,
  [] ~=? eventos (Secuencia (Silencio 1) (Nota 50 5)) 0,
  [On 1 50, Off 2 50] ~=? eventos (Secuencia (Silencio 1) (Nota 50 5)) 1,
  [On 1 50, Off 3 50] ~=? eventos (Secuencia (Silencio 1) (Nota 50 5)) 2
  ]