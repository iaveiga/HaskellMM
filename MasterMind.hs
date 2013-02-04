import System.IO
import System.Random

swap :: Int -> Int -> [a] -> [a]
swap i j xs | i == j    = xs
            | i > j     = swap j i xs
            | otherwise = swap' i j xs
    where swap'  0 j (x:xs) = let (b,xs') = swap'' x (j-1) xs in b : xs'
          swap'  i j (x:xs) = x : swap' (i-1) (j-1) xs
          swap'' a 0 (x:xs) = (x, a:xs)
          swap'' a j (x:xs) = let (b,xs') = swap'' a (j-1) xs in (b, x:xs')

readAInt :: IO Int
readAInt = readLn

main::IO()
main=do
	putStrLn "MasterMind"
	c <- readAInt
	
	let
		clave = ['D','B','A','F']
		a=c+5
		colours = ['A','B','C','D','E','F']
		cfg = take 4 $ randomRs ('A','F') (mkStdGen 4) :: [Char]
		yatu = swap 1 2 clave
	putStrLn clave
	putStrLn yatu
	putStrLn colours
	putStrLn cfg
	if clave == cfg then putStrLn "Ganaste" else putStrLn "Perdiste"
	print a
	

	