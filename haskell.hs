-- Exercicio 1

num_digit :: Integer -> Int
num_digit x = floor ( logBase 10 (fromIntegral x))

inverte_num :: Integer -> Integer
inverte_num x 
    | x < 10 = x
    | otherwise = mod x 10 * 10 ^ num_digit x + inverte_num (div x 10) 

-- Exercicio 2

soma_vetor :: [Integer] -> Integer
soma_vetor [] = 0
soma_vetor (a:x) = a + soma_vetor x

-- Exercicio 3

somatorio_n :: Integer -> Integer
somatorio_n 1 = 1
somatorio_n n = somatorio_n (n-1) + n

-- Exercicio 4

inverte_vetor :: [Integer] -> [Integer]
inverte_vetor [] = []
inverte_vetor (a:x) = inverte_vetor x ++ [a]

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

-- Exercicio 7

print_crescente :: Integer -> String
print_crescente 0 = "0"
print_crescente n = print_crescente (n-1) ++ " " ++ show n

-- Exercicio 8

print_decrescente :: Integer -> String
print_decrescente 0 = "0"
print_decrescente n = show n ++ " " ++ print_decrescente(n-1)

-- Exercicio 9

print_par_cresc :: Integer -> String
print_par_cresc 0 = "0"
print_par_cresc n
    | mod n 2 == 1 = show "nao eh par"
    | otherwise = print_par_cresc(n-2) ++ " " ++ show n

-- Exercicio 10

print_par_decresc :: Integer -> String
print_par_decresc 0 = "0"
print_par_decresc n
    | mod n 2 == 1 = show "nao eh par"
    | otherwise = show n ++ " " ++ print_par_decresc(n-2)

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

-- Exercicio 13

menor_vetor :: [Integer] -> Integer
menor_vetor [] = 0
menor_vetor [a] = a
menor_vetor (a:x)
    | a < menor_vetor x = a
    | otherwise = menor_vetor x

-- Exercicio 14

imprime_serie :: Integer -> Integer -> Integer -> String
imprime_serie _ _ 0 = "Incremento invalido"
imprime_serie a b i
    | (b < a && i > 0) || (a < b && i < 0) = ""
    | otherwise = show a ++ " " ++ imprime_serie (a+i) b i

-- Exercicio 15

converte_bin :: Integer -> Integer
converte_bin 0 = 0
converte_bin n = converte_bin (div n 2) * 10 + mod n 2

-- Exercicio 16

soma_sucessiva :: Integer -> Integer -> Integer
soma_sucessiva n 0 = n
soma_sucessiva n m = soma_sucessiva (n+1) (m-1)

-- Exercicio 17

calcula_digitos :: Integer -> Integer
calcula_digitos 0 = 0
calcula_digitos n = calcula_digitos (div n 10) + mod n 10