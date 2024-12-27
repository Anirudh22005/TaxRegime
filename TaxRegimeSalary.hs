oldtax :: Int -> Int
oldtax x
  | x <= 250000 = 0
  | x <= 500000 = (x - 250000) * 5 `div` 100
  | x <= 1000000 = (250000) * 5 `div` 100 + (x - 500000) * 20 `div` 100
  | x > 1000000 = (250000) * 5 `div` 100 + (500000) * 20 `div` 100 + (x - 1000000) * 30 `div` 100

newtax :: Int -> Int
newtax x
  | x <= 250000 = 0
  | x <= 500000 = (x - 250000) * 5 `div` 100
  | x <= 750000 = (250000) * 5 `div` 100 + (x - 500000) * 10 `div` 100
  | x <= 1000000 = (250000) * 5 `div` 100 + (250000) * 10 `div` 100 + (x - 750000) * 15 `div` 100
  | x <= 1250000 = (250000) * 5 `div` 100 + (250000) * 10 `div` 100 + (250000) * 15 `div` 100 + (x - 1000000) * 20 `div` 100
  | x <= 1500000 = (250000) * 5 `div` 100 + (250000) * 10 `div` 100 + (250000) * 15 `div` 100 + (250000) * 20 `div` 100 + (x - 1250000) * 25 `div` 100
  | x > 1500000 = (250000) * 5 `div` 100 + (250000) * 10 `div` 100 + (250000) * 15 `div` 100 + (250000) * 20 `div` 100 + (250000) * 25 `div` 100 + (x - 1500000) * 30 `div` 100

main :: IO ()
main = do
  putStrLn "old or new?"
  regime <- getLine
  putStrLn "Enter Your Salary: "
  input <- getLine
  let salary = read input :: Int
  if regime == "old" 
    then putStrLn ("The Tax Is : " ++ show (oldtax salary))
    else putStrLn ("The Tax Is : " ++ show (newtax salary))

