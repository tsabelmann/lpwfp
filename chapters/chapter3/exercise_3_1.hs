derivative :: Double -> (Double -> Double) -> Double -> Double
derivative dt f t = ((f $ t + dt/2) - (f $ t - dt/2)) / dt 

-- Original function f
f :: Double -> Double
f x = (1/2) * x^2

-- First derivative of f
f' :: Double -> Double
f' = derivative 10 f

-- Second derivative of f
f'' :: Double -> Double
f'' = derivative 1 f

-- Third derivative of f
f''' :: Double -> Double
f''' = derivative 0.1 f

-- The function f''' is not exactly because of the term 0.1 given to the derivative function.
-- Since 0.1 cannot be expressed exactly, the term 0.1/2 is not exactly as well.
-- Therefore, the resulting function is not as exact as the functions f' and f''.

main = do
    putStrLn $ show $ map f' [1..10]
    putStrLn $ show $ map f'' [1..10]
    putStrLn $ show $ map f''' [1..10]
