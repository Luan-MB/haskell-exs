-- Exercicio 1

num_digit :: Integer -> Int
num_digit x = floor ( logBase 10 (fromIntegral x))

inverte_num :: Integer -> Integer
inverte_num x 
    | x < 10 = x
    | otherwise = mod x 10 * 10 ^ num_digit x + inverte_num (div x 10) 

-- Exercicio 3

somatorio_n :: Integer -> Integer
somatorio_n 1 = 1
somatorio_n n = somatorio_n (n-1) + n

-- Exercicio 5

compara_num :: Integer -> Integer -> Integer
compara_num n d
    | n == d = 1
    | otherwise = 0

ocorrencia_dig :: Integer -> Integer -> Integer
ocorrencia_dig n d
    | n < 10 = compara_num n d
    | otherwise = ocorrencia_dig (div n 10) d + compara_num (mod n 10) d

-- Exercicio 6

multip_rec :: Integer -> Integer -> Integer
multip_rec x 1 = x
multip_rec x y = multip_rec x (y-1) + x

-- Exercicio 11

eh_impar :: Integer -> Integer
eh_impar n = mod n 2

calcula_fatorial :: Integer -> Integer -> Integer
calcula_fatorial 1 d = 1
calcula_fatorial n d = n * calcula_fatorial (n-d) d

fatorial_duplo :: Integer -> Integer
fatorial_duplo n
    | (eh_impar n == 0) = 0
    | otherwise = calcula_fatorial n 2

-- Exercicio 12

super_fatorial :: Integer -> Integer
super_fatorial 1 = 1
super_fatorial n = calcula_fatorial n 1 * super_fatorial (n-1)