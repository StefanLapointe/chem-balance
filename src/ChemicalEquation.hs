module ChemicalEquation (
    Element
  , Formula
  , Equation
  , readEquation
  , showEquation
  , uniqueElements
  , reactantFormulas
  , productFormulas
  , totalElementCount
  , updateFormulaCounts
    ) where

import Text.ParserCombinators.ReadP hiding (count)
import Data.Char (isUpper, isLower, isDigit)
import Data.List (intercalate, nub)

-- In chemistry, a quantity of 1 is not typically written explicitly.

data Count = Count Integer

count :: ReadP Count
count = do
  digits <- munch isDigit
  return $ if null digits then Count 1 else Count (read digits)

showCount :: Count -> String
showCount (Count 1) = ""
showCount (Count c) = show c

-- The name of an element should consist of letters and be capitalized.

data Element = Element String
  deriving (Eq)

element :: ReadP Element
element = do
  upper <- satisfy isUpper
  lowers <- munch isLower
  return $ Element (upper : lowers)

showElement :: Element -> String
showElement (Element e) = e

-- For simplicity, let's assume any formula is a list of atom-count pairs.

data Formula = Formula [(Element, Count)]

elementCount :: ReadP (Element, Count)
elementCount = do
  e <- element
  c <- count
  return (e, c)

formula :: ReadP Formula
formula = do
  elementCounts <- many1 elementCount
  return $ Formula elementCounts

showFormula :: Formula -> String
showFormula (Formula f) = concat $ map showElementCount f
  where
    showElementCount :: (Element, Count) -> String
    showElementCount (e, c) = showElement e ++ showCount c

-- For our purposes, a chemical expression is one side of a chemical equation.

data Expression = Expression [(Count, Formula)]

formulaCount :: ReadP (Count, Formula)
formulaCount = do
  c <- count
  f <- formula
  return (c, f)

expression :: ReadP Expression
expression = do
  formulaCounts <- sepBy1 (formulaCount) (string " + ")
  return $ Expression formulaCounts

showExpression :: Expression -> String
showExpression (Expression e) = intercalate " + " $ map showFormulaCount e
  where
    showFormulaCount :: (Count, Formula) -> String
    showFormulaCount (c, f) = showCount c ++ showFormula f

-- Finally, a chemical equation consists of reactants and products.

data Equation = Equation Expression Expression

equation :: ReadP Equation
equation = do
  reactants <- expression
  _ <- string " -> "
  products <- expression
  eof
  return $ Equation reactants products

showEquation :: Equation -> String
showEquation (Equation reactants products) =
  showExpression reactants ++ " -> " ++ showExpression products

readEquation :: String -> Maybe Equation
readEquation s = case readP_to_S equation s of
  [] -> Nothing
  (e, _) : _ -> Just e

-- Get the list of unique elements that appear in a chemical equation.
uniqueElements :: Equation -> [Element]
uniqueElements = nub . elements

elements :: Equation -> [Element]
elements (Equation (Expression rs) (Expression ps)) =
  concat $ map (formulaElements . snd) (rs ++ ps)
  where
    formulaElements :: Formula -> [Element]
    formulaElements (Formula ecs) = map fst ecs

-- Get the list of reactants in a chemical equation.
reactantFormulas :: Equation -> [Formula]
reactantFormulas (Equation (Expression rs) _) = map snd rs

-- Get the list of products in a chemical equation.
productFormulas :: Equation -> [Formula]
productFormulas (Equation _ (Expression ps)) = map snd ps

-- Get the number of times an element appears in a formula
totalElementCount :: Element -> Formula -> Integer
totalElementCount e (Formula ecs) = totalElementCount' ecs
  where
    totalElementCount' :: [(Element, Count)] -> Integer
    totalElementCount' [] = 0
    totalElementCount' ((e', Count c) : ecs')
      | e == e' = totalElementCount' ecs' + c
      | otherwise = totalElementCount' ecs'

-- Create a new chemical equation by replacing the formula counts.
updateFormulaCounts :: Equation -> [Integer] -> Equation
updateFormulaCounts (Equation (Expression rs) (Expression ps)) cs =
  (Equation (Expression rs') (Expression ps'))
  where
    (rcs, pcs) = splitAt (length rs) cs
    rs' = zipWith updateFormulaCount rs rcs
    ps' = zipWith updateFormulaCount ps pcs
    updateFormulaCount :: (Count, Formula) -> Integer -> (Count, Formula)
    updateFormulaCount (_, f) x = (Count x, f)
