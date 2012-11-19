import Tsubame.T2Stat

main :: IO()
main = do
  jobs <- t2statAll
  r <- return $ length $ filter (\x -> (jobQueue x == S) && (jobState x == "R")) jobs
  q <- return $ length $ filter (\x -> (jobQueue x == S) && (jobState x == "Q")) jobs
  putStrLn $ show r
  putStrLn $ show q 
  

