import           Fibonacci
import           System.IO

main :: IO ()
main = do
    putStrLn "Fib of what number ?"
    n <- readLn :: IO Int
    putStrLn $ "The answer is " ++ show (fibLinear n)
