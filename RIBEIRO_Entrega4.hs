--Aluno: André Gustavo da Rosa Ribeiro
import Data.Char

--1 - Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n.

fatorialn :: Int -> Int
fatorialn n
  | n==0 = 1
  | otherwise = foldr (*) 1 [1..n]

--2 Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de  números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos inteiros listados. 

quadradoReal :: [Float] -> [Float]
quadradoReal lista = map (^2) lista

--3 Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. 

comprimentoPalavras:: [String] -> [Int]
comprimentoPalavras listaPalavras = map length listaPalavras


--4 Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29. 

maiorMultiploDe29 :: Int -> Int
maiorMultiploDe29 n = maximum (filter (\x -> mod x 29 == 0) [0..n])

--5 Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. 

maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum (filter (\y -> mod y n == 0)  [0..100000])

--6 Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12+22+32+42...+𝑛2.

somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^2) [1..n])

-- 7 Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.  

comprimento :: [Int] -> Int
comprimento lista = foldl (+) 0 (map (^0) lista)

--8 Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.

uncurried1 :: (Int, Int) -> Int
uncurried1 (a,b) = a+b

uncurried2 :: (Int, Int) -> Int
uncurried2 (a,b) = a+b-2

curried1 :: Int -> Int -> Int
curried1 a = \b -> a+b

curried2 :: Int -> Int -> Int
curried2 a = \b -> (a+b)




main = do
--Q1
  putStrLn $ "Func.1: entrada: 4 resultado:" ++ show (fatorialn 4)
  putStrLn $ "Func.1: entrada: 0 resultado:" ++ show (fatorialn 0)
  putStrLn ""
--Q2
  putStrLn $ "Func.2: entrada: [1.0, 2.0, 3.0] resultado:" ++ show (quadradoReal [1.0, 2.0, 3.0])
  putStrLn $ "Func.2: entrada: [-3.0, -2.0, -1.0] resultado:" ++ show (quadradoReal [-3.0, -2.0, -1.0])
  putStrLn ""

--Q3
  putStrLn $ "Func.3: entrada: [a, bb, ccc] ; 0 resultado:" ++ show (comprimentoPalavras ["a", "bb", "ccc"])
  putStrLn ""

--Q4
  putStrLn $ "Func.4: entrada: 100000 resultado:" ++ show (maiorMultiploDe29 100000)
  putStrLn ""

--Q5
  putStrLn $ "Func.5: entrada: 17 resultado:" ++ show (maiorMultiploDe 17)
  putStrLn ""

--Q6
  putStrLn $ "Func.6: entrada: 2 resultado:" ++ show (somaQuadrados 2)
  putStrLn $ "Func.6: entrada: 10 resultado:" ++ show (somaQuadrados 10)
  putStrLn ""

--Q7
  putStrLn $ "Func.7: entrada: [1,2,3] resultado:" ++ show (comprimento [1,2,3])
  putStrLn ""

--Q8
  putStrLn $ "Flip 1: entrada: 1 2 resultado:" ++ show (flip (/) 1 2)
  putStrLn $ "Flip 2: entrada: 4 2 resultado:" ++ show (flip (>) 4 2)
  putStrLn $ "ord 1 : entrada: 'a' resultado:" ++ show (ord 'a' )
  putStrLn $ "ord 2 : entrada: 'b resultado:" ++ show (ord 'b' )
  putStrLn $ "Max 1: entrada: 1 ; 2 resultado:" ++ show (max 1 2)
  putStrLn $ "Max 2 : entrada: 4 ; 2 resultado:" ++ show (max 4 2)
  putStrLn $ "Min 1: entrada: 1 ; 2 resultado:" ++ show (min 1 2)
  putStrLn $ "Min 2: entrada: 4 ; 2 resultado:" ++ show (min 4 2)
  putStrLn $ "Curry 1: entrada: 4 ; 2 resultado:" ++ show (curry(uncurried1) 4 2)
  putStrLn $ "Curry 2: entrada: 4 ; 3 resultado:" ++ show (curry(uncurried2) 4 3)
  putStrLn $ "Uncurry 1: entrada: 4 ; 2 resultado:" ++ show (uncurry(curried1) (4,2))
  putStrLn $ "Uncurry 2: entrada: 4 ; 3 resultado:" ++ show (uncurry(curried2) (4,3))
  putStrLn ""
