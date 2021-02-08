{-# LANGUAGE TypeFamilies #-}

module Task5
  ( printScript
  ) where

import Data.Functor.Const (Const (..))
import Task4 (Script (..))

-- | Type for HalyavaScript to Javascript code as String
newtype Printable a = Printable { doPrint :: Int -> String }

-- | Convert HalyavaScript to JavaScript code (printScript example)
printScript
  :: (Printable a)  -- ^ HalyavaScript function to convert
  -> String  -- ^ JavaScript code
printScript script = "function main(){\n" <> addTabs (doPrint script 0) <> "}"

-- | Add tab to block of code
addTabs
  :: String  -- ^ Block of code
  -> String  -- ^ Block of code with tabs
addTabs = unlines . map ("  " <>) . lines

-- | Print expression with operation between
printExpressionWithOp
  :: String  -- ^ Operation
  -> Printable a  -- ^ Expression left
  -> Printable b  -- ^ Expression right
  -> Printable c  -- ^ Return expression
printExpressionWithOp operator exprA exprB =
  Printable $ \i -> doPrint exprA i <> operator <> doPrint exprB i

instance Script Printable where
  type Ref Printable = Const String

  ref @= expr = Printable $ \i -> doPrint ref i <> "=" <> doPrint expr i <> ";\n"
  lineTop # lineBtm = Printable $ \i -> doPrint lineTop i <> doPrint lineBtm i

  (@&&) = printExpressionWithOp "&&"
  (@||) = printExpressionWithOp "||"
  (@==) = printExpressionWithOp "=="
  (@!=) = printExpressionWithOp "!="
  (@<)  = printExpressionWithOp "<"
  (@>)  = printExpressionWithOp ">"
  (@<=) = printExpressionWithOp "<="
  (@>=) = printExpressionWithOp ">="
  (@<>) = printExpressionWithOp "<>"
  (@+)  = printExpressionWithOp "+"
  (@-)  = printExpressionWithOp "-"
  (@*)  = printExpressionWithOp "*"

  hsConst cnst = Printable . const $ show cnst
  hsTake var = Printable $ \i -> "(" <> doPrint var i <> ")"
  hsWithVar var body = Printable $ \i ->
    "var v" <> show i <> "=" <> show var <> ";\n"
    <> doPrint (body $ Printable $ const $ "v" <> show i) (i + 1)

  hsWhile condition body = Printable $ \i ->
    "while (" <> doPrint condition i <> ") {\n"
     <> addTabs (doPrint body i)
     <> "}\n"

  hsIf condition bodyIf bodyElse = Printable $ \i ->
    "if (" <> doPrint condition i <> ") {\n"
    <> addTabs (doPrint bodyIf i)
    <> "} else {\n"
    <> addTabs (doPrint bodyElse i)
    <> "}\n"

  hsReturn var = Printable $ \i -> "return " <> doPrint var i
