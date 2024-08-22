import ChemicalEquation
import LinearAlgebra
import Data.Ratio
import System.IO

main :: IO ()
main = do
  putStr "Please enter the chemical equation to be balanced: "
  hFlush stdout
  input <- getLine
  putStrLn $ processInput input

processInput :: String -> String
processInput input =
  case readEquation input of
    Nothing -> "Error: Failed to parse input."
    Just equation -> case balance equation of
      Nothing -> "Error: Failed to balance chemical equation."
      Just equation' -> "Balanced equation: " ++ showEquation equation'

balance :: Equation -> Maybe Equation
balance equation = do
  vectorSolution <- solveSystem $ getSystem equation
  let rationalSolution = fromVector vectorSolution
  naturalSolution <- simplifyMaybe rationalSolution
  return $ updateFormulaCounts equation naturalSolution


getSystem :: Equation -> Matrix
getSystem equation =
  toMatrix $ map (getRow reactants products) elements ++ [uniquenessConstraint]
  where
    elements = uniqueElements equation
    reactants = reactantFormulas equation
    products = productFormulas equation
    numFormulas = length $ reactants ++ products
    uniquenessConstraint = [1] ++ (replicate (numFormulas - 1) 0) ++ [1]

getRow :: [Formula] -> [Formula] -> Element -> [Rational]
getRow reactants products element =
  map toRational $ map (* (-1)) reactantPortion ++ productPortion ++ [0]
  where
    reactantPortion = getCounts reactants
    productPortion = getCounts products
    getCounts :: [Formula] -> [Integer]
    getCounts = map $ totalElementCount element

simplifyMaybe :: [Rational] -> Maybe [Integer]
simplifyMaybe rationals =
  let commonDenominator = foldl lcm 1 $ map denominator rationals
      integers = map numerator $ map (* toRational commonDenominator) rationals
      commonDivisor = foldl gcd 1 integers
      integers' = map (`div` commonDivisor) integers
  in
    if all (> 0) integers'
    then Just integers'
    else Nothing
