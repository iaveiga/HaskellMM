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
		  

negras :: Eq a => [a]-> [a] -> Int -> Int
negras guess clave iter =
	let i=iter+1 in
	if iter<4 
		then if guess !! iter ==  clave !! iter 
				then 1+negras guess clave i
				else 0+negras guess clave i
	else 0

blanca1 :: Eq a => [a]-> [a] -> Int -> Int -> Int
blanca1 guess clave iter iter2=
	let i=iter+1 in
	if iter<4 
		then if guess !! iter2 ==  clave !! iter && iter/=iter2  
				then 1+blanca1 guess clave i iter2
				else 0+blanca1 guess clave i iter2
	else 0 	

blanca :: Eq a => [a]-> [a] -> Int
blanca guess clave =
	 		blanca1 guess clave 0 0 + blanca1 guess clave 0 1 + blanca1 guess clave 0 2 + blanca1 guess clave 0 3				

					
readAInt :: IO Int
readAInt = readLn

main::IO()
main=do
	putStrLn "MasterMind"
	--c <- readAInt
	
	let
		iter = 3
		clave = ['D','B','A','F']
		--a=c+5
		colours = ['A','B','C','D','E','F']
		cfg = take 4 $ randomRs ('A','F') (mkStdGen 4) :: [Char]
		negra = negras clave yatu 0
		blanc=blanca1 yatu clave 0 0
		blancas=blanca yatu clave
		yatu = swap 1 2 clave
	putStrLn clave
	putStrLn yatu
	putStrLn colours
	putStrLn cfg
	print negra
	print blancas
	print blanc
	
	if clave == cfg then putStrLn "Ganaste" else putStrLn "Perdiste"
	--print a
	
let score b w =
	if b == 4
		then return 4 * (4 + 3) / 2 -1
		else return (((b+w) * (b + w) / 2) + b)
	