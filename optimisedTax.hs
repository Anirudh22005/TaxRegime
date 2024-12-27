-- Generalized tax calculation function
calculateTax :: [(Int, Int)] -> Int -> Int
calculateTax brackets salary = go brackets salary 0
  where
    go [] _ tax = tax
    go ((limit, rate):xs) remainingSalary tax
      | remainingSalary > limit = go xs (remainingSalary - limit) (tax + (limit * rate `div` 100))
      | otherwise = tax + (remainingSalary * rate `div` 100)

-- Old tax regime
oldTaxSlabs :: [(Int, Int)]
oldTaxSlabs = [(250000, 5), (500000, 20), (1000000, 30)]

-- New tax regime
newTaxSlabs :: [(Int, Int)]
newTaxSlabs = [(250000, 5), (500000, 10), (750000, 15), (1000000, 20), (1250000, 25), (1500000, 30)]

-- Function to read the input and calculate tax based on the regime
main :: IO ()
main = do
  putStrLn "Choose tax regime (old or new): "
  regime <- getLine
  putStrLn "Enter Your Salary: "
  input <- getLine
  let salary = read input :: Int

  if regime == "old"
    then putStrLn ("The Tax Is: " ++ show (calculateTax oldTaxSlabs salary))
    else putStrLn ("The Tax Is: " ++ show (calculateTax newTaxSlabs salary))

