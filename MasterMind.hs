import System.IO
import System.Random
import Data.List

swap :: Int -> Int -> [a] -> [a]
swap i j xs | i == j    = xs
            | i > j     = swap j i xs
            | otherwise = swap' i j xs
    where swap'  0 j (x:xs) = let (b,xs') = swap'' x (j-1) xs in b : xs'
          swap'  i j (x:xs) = x : swap' (i-1) (j-1) xs
          swap'' a 0 (x:xs) = (x, a:xs)
          swap'' a j (x:xs) = let (b,xs') = swap'' a (j-1) xs in (b, x:xs')


introducir :: Char -> [Char] -> Int -> [Char]
introducir elem cad pos 
					  | pos==0 =[elem, cad!!1, cad!!2, cad!!3]
				      | pos==1 =[cad!!0, elem, cad!!2, cad!!3]
				      | pos==2 =[cad!!0, cad!!1, elem, cad!!3]
				      | pos==3 =[cad!!0, cad!!1, cad!!2, elem]
					  | otherwise =cad

blacks :: [Char] -> [Char] -> Int -> Int -> [Int] -> [Char]
blacks gues ztas iter neg aleat =
			let 
				array = (introducir (gues!!(aleat!!iter)) ztas (aleat!!iter)) 
				i=iter+1
			in
			if iter < neg then blacks gues array i neg aleat
			else ztas
						



nulas :: [Char] -> [Char] -> Int -> Int -> [Int] -> [Char]
blacks gues ztas aleat =
			let 
				if ztas!!0 == z
				array = (introducir (aleat ztas 0) 
					else if ztas!!1 == z
					array = (introducir (aleat ztas 1) 			
						else if ztas!!2 == z
						array = (introducir (aleat ztas 2) 			
						else if ztas!!3 == z
						array = (introducir (aleat ztas 3) 			
				else array	



respValida :: [Char] -> [Char] -> Bool
respValida resppotencial respanterior
	let score1 = score (resppotencial)
	    score2 = score (respanterior)
	if score1>=score2 
	then True
	else False    




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

score ::  Int -> Int -> Int			
score b w =
	if b == 4
		then 13
		else (((b+w) * (b + w+1)`div`2) + b)


					
readAInt :: IO Int
readAInt = readLn

--potencial :: Int -> Char -> Char -> Char -> Char -> [a] -> Int -> Int -> [a]
--potencial l1 l2 l3 l4 guess iter blacks =		
			--if iter < blacks then
				--let iter=iter+1 in
				--verificacion randomRIO l1 l2 l3 l4 guess iter blacks
			--else
			




main :: IO()
main=do
	putStrLn "MasterMind"
	c <- readAInt
	let
		score b w =
			if b == 4
				then 13
				else ((((b+w) * (b + w+1) / 2) + b))
		iter = 3
		clave = ['D','B','A','F']
		--a=c+5
		colours = ['A','B','C','D','E','F']
		cfg = take 4 $ randomRs ('A','F') (mkStdGen 4) :: [Char]
		ent=take 4 $ randomRs (0,3) (mkStdGen 3) :: [Int]
		entero= ent !! 1
		
		negra = negras clave yatu 0
		blanc=blanca1 yatu clave 0 0
		blancas=blanca yatu clave 
		yatu = swap 1 2 clave 
		def=['Z','Z','Z','Z']
		new=blacks clave def 0 2 ent
	putStrLn clave
	putStrLn yatu
	putStrLn colours
	putStrLn cfg
	print ent
	print negra
	print blancas
	print blanc
	print entero
	putStrLn new
	if clave == cfg then putStrLn "Ganaste" else putStrLn "Perdiste"
	--print a
	
	

let potencial letra1 letra2 letra3 letra4 codigo1 it blacks{
	for i = 0, i<= blacks, i++
		let aleatorio = randomRIO(0,3)
		if aleatorio == 1
			if letra1 != "Z"
				return (potencial (codigo1!!1, letra2, letra3, letra4, codigo1, it, blacks)
			else
				return (potencial (letra1,letra2,letra3,letra4,codigo1,it,blacks)
		else if aleatorio == 2
			if letra2 != "Z"
				return (potencial (letra1,codigo1!!2, letra3, letra4, codigo1, it, blacks)
			else
				return (potencial (letra1,letra2,letra3,letra4,codigo1,it,blacks)
		
		else if aleatorio == 3
			if letra3 != "Z"
				return (potencial (letra1, letra2,codigo1!!3,letra4, codigo1, it, blacks)
			else
				return (potencial (letra1,letra2,letra3,letra4,codigo1,it,blacks)
		
		else if aleatorio == 4
			if letra1 != "Z"
				return (potencial (letra1, letra2, letra3, codigo1!!4, codigo1, it, blacks)
			else
				return (potencial (letra1,letra2,letra3,letra4,codigo1,it,blacks)
				
let potencialw letra1 letra2 letra3 letra4 codigo1 potencialG it whites
	aleatorio <- randomRIO(0,3)
	--ciclo for

	if codigop!!aleatorio == codigo1!!aleatorio
		then potencialw letra1 letra2 letra3 letra4 codigo1 potencialG it whitesciclo for

		else
			if codigop!!0 == "Z"
				then codigop!!0 = codigo1!!aleatorio
				return 
			else if codigop!!1 == "Z"
				then codigop!!1 = codigo1!!aleatorio
				return
			else if codigop!!2 == "Z"
				then codigop!!2 = codigo1!!aleatorio
				return 
			else
				codigop!!3 = codigo1!!aleatorio
				return


	