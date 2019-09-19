module Main where

import ParserLC
import PrettyPrintLC
import Text.PrettyPrint
import TypeInference
import Examples

plainExpr :: String -> Doc
plainExpr = ppExpr . parseLC

inferExpr :: String -> Doc
inferExpr = ppTypingResult . inferType . parseLC

testAll :: Doc
testAll = cat (map test definedExprs)

test :: Int -> Doc
test n = pprintTuple (expr n, inferExpr (expr n))

pprintTuple :: (String, Doc) -> Doc
pprintTuple (origExpr, annotExpr) = (text "W(") Text.PrettyPrint.<> text origExpr Text.PrettyPrint.<> (text ") = ") Text.PrettyPrint.<> annotExpr

exprs= cat (map text (map (\n->show n ++ ") " ++ expr n)  definedExprs))

definedExprs :: [Int]
definedExprs= [1..22]

main :: IO Doc
main = return testAll