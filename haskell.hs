-- LISTA 1

-- Exercicio 1

inverte_num :: Integer -> Integer
inverte_num x 
    | x < 10 = x
    | otherwise = mx * 10 ^ num_digit + inverte_num nx
    where 
        nx = div x 10
        mx = mod x 10
        num_digit = floor( logBase 10 (fromIntegral x))

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
converte_bin n = converte_bin (div n 2) * 10 + mod n 2

-- Exercicio 16

soma_sucessiva :: Integer -> Integer -> Integer
soma_sucessiva n 0 = n
soma_sucessiva n m = soma_sucessiva (n+1) (m-1)

-- Exercicio 17

calcula_digitos :: Integer -> Integer
calcula_digitos 0 = 0
calcula_digitos n = calcula_digitos (div n 10) + mod n 10

-- LISTA 2

-- Exercicio 1

inverte_lista :: [Integer] -> [Integer]
inverte_lista [] = []
inverte_lista (a:x) = inverte_lista x ++ [a]

-- Exercicio 2

remove_primeiro :: [Integer] -> [Integer]
remove_primeiro [] = []
remove_primeiro (a:x) = x

-- Exercicio 3

remove_ultimo :: [Integer] -> [Integer]
remove_ultimo [] = []
remove_ultimo [a] = []
remove_ultimo (a:x) = [a] ++ remove_ultimo x

-- Exercicio 4

soma_pares_lista :: [Integer] -> Integer
soma_pares_lista [] = 0
soma_pares_lista (a:x)
    | mod a 2 == 0 = a + soma_pares_lista x
    | otherwise = soma_pares_lista x

-- Exercicio 5

soma_posicao_par :: [Integer] -> Integer
soma_posicao_par [] = 0
soma_posicao_par (a:b:x) = b + soma_posicao_par x

-- Exercicio 6

insere_elemento :: Integer -> [Integer] -> Integer -> [Integer]
insere_elemento a [] _ = [a]
insere_elemento a (b:y) 1 = [a] ++ (b:y)
insere_elemento a (b:y) i = [b] ++ insere_elemento a y (i-1)

-- Exercicio 7

-- Exercicio 9

divide_lista :: [Integer] -> Integer -> [Integer]
divide_lista [] _ = []
divide_lista (a:x) 1 = [a]
divide_lista (a:x) i = [a] ++ divide_lista x (i-1)

-- Exercicio 10

meu_drop :: Int -> [Integer] -> [Integer]
meu_drop _ [] = []
meu_drop 0 (a:x) = (a:x)
meu_drop i (a:x) = meu_drop (i-1) x 

meu_take :: Int -> [Integer] -> [Integer]
meu_take _ [] = []
meu_take 0 (a:x) = []
meu_take j (a:x) = [a] ++ meu_take (j-1) x

intervalo_lista :: [Integer] -> Int -> Int -> [Integer]
intervalo_lista [] _ _ = []
intervalo_lista (a:x) i j = meu_take (j-i+1) (meu_drop (i-1) (a:x))

-- Exercicio 11

rotaciona_lista :: [Integer] -> Int -> [Integer]
rotaciona_lista [] _ = []
rotaciona_lista (a:x) 0 = (a:x)
rotaciona_lista (a:x) i = rotaciona_lista (x ++ [a]) (i-1)

-- Exercicio 12

gera_intervalo :: Integer -> Integer -> [Integer]
gera_intervalo x y 
    | x == y = [y]
    | otherwise = [x] ++ gera_intervalo (x+1) y 

-- PROVA

-- Questão 2

fatorial_d :: Integer -> Integer
fatorial_d 1 = 1
fatorial_d x = x * fatorial_d (x-2)

-- Questão 3

dec_to_bin :: Integer -> Integer
dec_to_bin 0 = 0
dec_to_bin 1 = 1
dec_to_bin x = dec_to_bin nx * 10 + mx
    where 
        mx = mod x 2
        nx = div x 2


