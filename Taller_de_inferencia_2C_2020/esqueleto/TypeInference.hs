module TypeInference (TypingJudgment, Result(..), inferType, inferNormal, normalize)

where

import Data.List(intersect, union, nub, sort)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Context, AnnotExp, Type)

typeVarsT :: Type -> [Int]
typeVarsT = foldType (:[]) [] [] union id

typeVarsE :: Exp Type -> [Int]
typeVarsE = foldExp (const []) [] id id id [] [] (\r1 r2 r3 ->nub(r1++r2++r3)) (const setAdd) union typeVarsT union (\r1 r2 _ _ r3->nub(r1++r2++r3))
  where setAdd t r = union (typeVarsT t) r

typeVarsC :: Context -> [Int]
typeVarsC c = nub (concatMap (typeVarsT . evalC c) (domainC c))

typeVars :: TypingJudgment -> [Int]
typeVars (c, e, t) = sort $ union (typeVarsC c) (union (typeVarsE e) (typeVarsT t))

normalization :: [Int] -> [Subst]
normalization ns = foldr (\n rec (y:ys) -> extendS n (TVar  y) emptySubst : (rec ys)) (const []) ns [0..]

normalize :: TypingJudgment -> TypingJudgment
normalize j@(c, e, t) = let ss = normalization $ typeVars j in foldl (\(rc, re, rt) s ->(s <.> rc, s <.> re, s <.> rt)) j ss
  
inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s
    
inferNormal :: PlainExp -> Result TypingJudgment
inferNormal e = case infer' e 0 of
    OK (_, tj) -> OK $ normalize tj
    Error s -> Error s


unificacionDeContextos :: Context -> Context -> [UnifGoal] -- [(t1,t2)]
unificacionDeContextos c1 c2 = foldr (\simbolo rec  -> if elem simbolo (domainC c2) 
								then (evalC c1 simbolo, evalC c2 simbolo) : rec else rec ) [](domainC c1)

nuevaVariable :: Int -> Context -> Symbol -> Int
nuevaVariable n c x = if elem x (domainC c) then n else n+1

nuevaTipo :: Int -> Context -> Symbol -> Type
nuevaTipo n c x = if elem x (domainC c) then evalC c x else TVar n

infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

infer' (SuccExp e)    n = case infer' e n of
                            OK (n', (c', e', t')) ->
                              case mgu [(t', TNat)] of
                                UOK subst -> OK (n',
                                                    (
                                                     subst <.> c',
                                                     subst <.> SuccExp e',
                                                     TNat
                                                    )
                                                )
                                UError u1 u2 -> uError u1 u2
                            res@(Error _) -> res
 
-- COMPLETAR DESDE AQUI

infer' ZeroExp                n = OK (n, (emptyContext, ZeroExp, TNat))
infer' (VarExp x)             n = OK(n+1, (extendC emptyContext x (TVar n),VarExp x, TVar n))
infer' (AppExp u v)           n = case infer' u n of
									OK (n_u', (c_u', u', t_u')) ->
										case infer' v n_u' of
											OK (n_v', (c_v', v', t_v')) ->
												case mgu ([(t_u',TFun t_v' (TVar n_v'))] ++ (unificacionDeContextos c_u' c_v')) of
													UOK subst -> OK (n_v'+1,
																		(
																		 joinC [subst <.> c_u',subst <.> c_v'],
																		 subst <.> AppExp u' v',
																		 subst <.> (TVar n_v')
																		)
																	)
													UError u1 u2 -> uError u1 u2
											res@(Error _) -> res
									res@(Error _) -> res								  
infer' (LamExp x _ e)         n = case infer' e n of
									OK (n', (c', e', t')) -> OK (nuevaVariable n' c' x,
																		(
																		 removeC c' x,
																		 LamExp x (nuevaTipo n' c' x) e',
																		 TFun (nuevaTipo n' c' x) t'
																		)
																)
									res@(Error _) -> res
									
-- OPCIONALES

infer' (PredExp e)            n = undefined
infer' (IsZeroExp e)          n = undefined
infer' TrueExp                n = undefined
infer' FalseExp               n = undefined
infer' (IfExp u v w)          n = undefined

-- EXTENSIÓN

infer' (EmptyListExp _)       n = OK (n+1, (emptyContext, EmptyListExp (TVar (n+1)),TList (TVar (n+1))))
infer' (ConsExp u v)          n = case infer' u n of
									OK (n_u', (c_u', u', t_u')) ->
										case infer' v n_u' of
											OK (n_v', (c_v', v', t_v')) ->
												case mgu ([(t_v',TList t_u')] ++ (unificacionDeContextos c_u' c_v')) of
													UOK subst -> OK (n_v'+1,
																		(
																		 joinC [subst <.> c_u',subst <.> c_v'],
																		 subst <.> ConsExp u' v',
																		 subst <.>(TList t_u')
																		)
																	)
													UError u1 u2 -> uError u1 u2
											res@(Error _) -> res
									res@(Error _) -> res
									
infer' (ZipWithExp u v x y w) n =  case infer' u n of 
                                    OK(n_u,(c1,expresionM,rho)) ->
                                      case infer' v n_u of
                                        OK (n_v,(c2,expresionN,phi)) ->
                                          case infer' w n_v of
                                            OK (n_w,(c3,expresionO,sigma)) ->
											  -- Elimino las variables x e y del contexto c3
                                              let c4 = removeC (removeC c3 x) y 
												  -- defino dos variables frescas, que van estar inicializadas
												  -- o por el contexto c3 o por una variable libre
                                                  t' = if elem x (domainC c3) then evalC c3 x else TVar n_w
                                                  t'' = if elem y (domainC c3) then evalC c3 y else TVar (n_w+1)                                                  
                                                  in case mgu ([(TList t',rho),(TList t'',phi)] 
															  -- calculo la lista de tipos a unificar, que viene de la interseccion del contexto c2 y c1
                                                              ++ map (\tipo -> (evalC c1 tipo,evalC c2 tipo))(filter (\variable -> elem variable (domainC c2)) (domainC c1))
															  -- calculo la lista de tipos a unificar, que viene de la interseccion del contexto c4 y c1
                                                              ++ map (\tipo -> (evalC c1 tipo,evalC c4 tipo))(filter (\variable -> elem variable (domainC c4)) (domainC c1))
															  -- calculo la lista de tipos a unificar, que viene de la interseccion del contexto c4 y c2
                                                              ++ map (\tipo -> (evalC c2 tipo,evalC c4 tipo))(filter (\variable -> elem variable (domainC c4)) (domainC c2))
                                                              ) of
                                                      UOK subst -> OK (n_w+2,
                                                                              (
                                                                              joinC [subst <.> c1,subst <.> c2,subst <.> c4],
                                                                              subst <.> ZipWithExp expresionM expresionN x y expresionO,
                                                                              subst <.> TList sigma
                                                                              )
                                                                          )
                                                      UError u1 u2 -> uError u1 u2
                                            res@(Error _) -> res
                                        res@(Error _) -> res
                                    res@(Error _) -> res		
--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
