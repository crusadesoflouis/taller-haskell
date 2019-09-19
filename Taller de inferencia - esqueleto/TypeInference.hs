module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
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


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x)     n = 
    OK ( ( n + 1 ), ( extendC emptyContext x (TVar n), VarExp x, TVar n) )

infer' (ZeroExp)     n = 
    OK ( ( n ), ( emptyContext, ZeroExp, TNat) )

infer' (TrueExp)     n = 
    OK ( ( n ), ( emptyContext, TrueExp, TBool) )

infer' (FalseExp)     n = 
    OK ( ( n ), ( emptyContext, FalseExp, TBool) )    

infer' (PredExp e) n =
    case infer' e n of
        OK ( n', (c', e', t') ) ->
            case mgu [ (t', TNat) ] of 
                UOK subst ->
                    OK ( n', (
                            subst <.> c',
                            subst <.> PredExp e',
                            TNat
                            ) )
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

infer' (SuccExp e) n =
    case infer' e n of
        OK ( n', (c', e', t') ) ->
            case mgu [ (t', TNat) ] of 
                UOK subst ->
                    OK ( n', (
                            subst <.> c',
                            subst <.> SuccExp e',
                            TNat
                            ) )
                UError u1 u2 ->
                    uError u1 u2
        res@(Error _) -> res

infer' (AppExp e1 e2) n =
    case infer' e1 n of
        OK ( n1', (c1', e1', t1') ) ->
            case infer' e2 n1' of
                OK ( n2', (c2', e2', t2') ) ->
                    case mgu ((t1', TFun t2' (TVar (n2'))) : [(evalC c1' a, evalC c2' a) | a <- domainC c1', elem a $ domainC c2' ]) of 
                        UOK subst ->
                            OK ( n2' + 1, (
                                joinC $ (subst <.> c1') : (subst <.> c2') : [],
                                subst <.> AppExp e1' e2',
                                subst <.> TVar (n2')
                                ) )
                        UError u1 u2 ->
                            uError u1 u2
                res2@(Error _) -> res2
        res1@(Error _) -> res1



infer' (LamExp x _ e) n =
    case infer' e n of
        OK ( n', (c', e', t') ) ->
            case elem x (domainC c') of
                True ->
                    OK ( n', (
                            removeC c' x,
                            LamExp x ( evalC c' x) e',
                            TFun ( evalC c' x) t'
                            ) )
                False ->
                    OK ( n'+1, (
                            c',
                            LamExp x (TVar n') e',
                            TFun (TVar n') t' 
                            ) )
        res@(Error _) -> res


--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
