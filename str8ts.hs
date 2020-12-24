--{-# OPTIONS_GHC -Wall #-}
import Debug.Trace


--DADOS
-- defino um data type para representar tiles do grid
data Element = White Int | Black String deriving (Show)
-- defino uma variavel 0 que me ajuda na hora de iniciar funcoes recursivas sobre a matriz
zero = 0

-- defino boolean que começa em False pra ajudar em @find_adjacent_vec 
found = False

-- defino um grid de elementos
{-
grid = [[Black "0", White 0, White 0, Black "1", Black "0", Black "0"],
        [Black "0", White 0, White 0, White 0, White 5, White 1],
        [Black "0", White 0, White 1, White 0, White 0, White 0],
        [White 4, White 0, White 0, White 0, White 0, Black "0"],
        [White 0, White 6, White 5, White 0, White 0, Black "0"],
        [Black "0", Black "0", Black "0", White 0, White 1, Black "4"]]
-}
{-
grid = [[White 3, White 4, Black "0", White 0, White 0, Black "0"],
        [White 0, White 0,Black "1", White 6, White 0, Black"0"],
        [White 0, White 0, White 0, White 0, White 0, Black "0"],
        [Black "0", White 0, White 0, White 0, White 1, White 0],
        [Black "0", White 0, White 3, Black "0", White 0, White 0],
        [Black "0", White 0, White 0,Black "0", White 0, White 0]]
-}

{-
grid = [[Black "0", Black "0", Black "0", White 0, White 0, Black "0"],
        [Black "0", White 2, White 0, White 4, White 0, Black "0"],
        [White 0, White 3, White 0, White 6, White 0, White 4],
        [White 0, White 0, White 0, White 0, White 0, White 0],
        [Black "1", White 0, White 2, White 0, White 0, Black "0"],
        [Black "0", White 0, White 0, Black "0", Black "0", Black "5"]]
-}

grid = [[Black "9", White 6, White 8, White 1, White 0, White 0, White 0, White 4, White 0],
        [White 0, White 0, Black "0", Black "0", White 4, White 0, White 0, Black "7", Black "0"],
        [White 5, Black "0", Black "0", White 0, White 3, White 0, Black "0", Black "0", White 0],
        [White 0, Black "0", White 0, White 0, White 0, White 4, Black "6", White 0, White 0],
        [Black "0", White 0, White 0, White 0, Black "0", White 0, White 0, White 0, Black "0"],
        [White 3, White 0, Black "0", White 0, White 5, White 8, White 0, Black "0", White 0],
        [White 0, Black "0", Black "0", White 0, White 0, White 0, Black "0", Black "5", White 0],
        [Black "0", Black "0", White 5, White 0, White 0, Black "0", Black "4", White 1, White 0],
        [White 0, White 7, White 0, White 0, White 0, White 0, White 0, White 0, Black "1"]]

{-
grid = [[Black "0", White 0, White 0, Black "0", Black "7", White 0, White 0, Black "0", Black "0"],
        [White 0, White 0, White 0, White 0, White 0, White 0, White 0, White 0, White 0],
        [White 0, White 4, Black "0", White 0, White 0, Black "0", Black "0", White 0, White 8],
        [Black "0", White 0, White 0, White 0, Black "0", White 0, White 0, White 0, White 0],
        [Black "0", Black "8", White 2, White 0, White 6, White 0, White 0, Black "1", Black "0"],
        [White 1, White 0, White 0, White 0, Black "0", White 0, White 7, White 0, Black "0"],
        [White 0, White 0, Black "9", Black "0", White 0, White 0, Black "0", White 0, White 6],
        [White 0, White 0, White 0, White 0, White 0, White 0, White 0, White 0, White 0],
        [Black "0", Black "0", White 0, White 9, Black "0", Black "0", White 0, White 0, Black "4"]]
-}

size = length grid


--RETORNO BOOLEANO
-- defino função que retorna se item é numero (white)
--uso: @test_for_blacks, 
is_white::Element->Bool
is_white (White _) = True
is_white (Black _) = False

--compara inteiros e retorna true se deu match
--uso: @test_for_blacks,
compare'::Int->Int->Bool
compare' x n | (x == n) = True
             | otherwise = False

--ACESSO GRID
--retorna linha da grid
get_row::[[Element]]->Int->[Element]
get_row grid x = grid!!x

--retrona coluna da grid
--ex.: get_col zero y
get_col::[[Element]]->Int->Int->[Element]
get_col _ a _ |(a==size)= []  -- retorna lista vazia se tiver iterado sobre tudo
get_col grid i y = (grid!!i!!y):(get_col grid (i+1) y)


--RETORNO INTEIRO
--acho o menor valor do vetor que é maior que 0
menor::[Int]->Int->Int
menor [] m = m
menor (h:b) m | (h/=0 && h<m) = menor b h
              | otherwise = menor b m


--CONVERSAO
--converte string pra inteiro
convert_string_to_int::Element->Int
convert_string_to_int (Black s) = (read s :: Int)
convert_string_to_int (White _) = -1

--elemento pra inteiro
convert_element_to_int::Element->Int
convert_element_to_int (White w) = w
convert_element_to_int (Black _) = -1

--desencapsula elemento black (string)
convert_element_to_string::Element->String
convert_element_to_string (White _) = ""
convert_element_to_string (Black b) = b

--converte inteiro pra elemento
convert_int_to_element::Int->Element
convert_int_to_element s = (White s)

--extractor de monad
fromJust::Maybe a->a
fromJust (Just a) = a

-- metodos pra selecionar um elemento de trio
sel1::(a, b, c)->a
sel1 (a, _, _) = a

sel2::(a, b, c)->b
sel2 (_, b, _)= b

sel3::(a, b, c)->c
sel3 (_, _, c)= c

--converte grid pra printar
convert_grid::[[Element]]->[[String]]
convert_grid [] = []
convert_grid (h:b) = (convert_vet h):(convert_grid b)

convert_vet::[Element]->[String]
convert_vet [] = []
convert_vet (h:b) | (is_white h) = [(show $ convert_element_to_int h)]++(convert_vet b)
                  | otherwise = [("B"++convert_element_to_string h)]++(convert_vet b)

--converte o vetor pra uma string
convert_vet_to_string::[String]->String
convert_vet_to_string [] = ""
convert_vet_to_string (h:b) = h++" "++(convert_vet_to_string b)

convert_grid_to_string::[[String]]->String
convert_grid_to_string [] = ""
convert_grid_to_string (h:b) = (convert_vet_to_string h)++"\n"++(convert_grid_to_string(b))

--TESTES
--testa se o numero é igual ao conteudo de um quadrado preto
test_for_blacks::[Element]->Int->Bool
test_for_blacks [] _ = False 
test_for_blacks (h:b) n | (not (is_white h)) = ((compare' (convert_string_to_int h) n) || (test_for_blacks b n))
                        | otherwise = ((compare' (convert_element_to_int h) n) || (test_for_blacks b n))


-- funcoes pra achar o intervalo que o quadrado pertence 
-- cont = zero, bool = found
--                   v-vetor        v-posicao que estamos testando n (x)                     
find_adjacent_vec::[Element]->Int->Int->Bool->[Int]->[Int]
--                             ^-contador ^-found

find_adjacent_vec vet cont x b out  | (cont>=size) = out
                                    | ((is_white (vet!!cont)) && (cont == x)) = find_adjacent_vec vet (cont+1) x True ((convert_element_to_int (vet!!cont)):out)
                                    | ((is_white (vet!!cont)) && (cont /= x)) = find_adjacent_vec vet (cont+1) x b ((convert_element_to_int (vet!!cont)):out)
                                    | ((not (is_white (vet!!cont))) && b) = out --caso final, retorna vetor resultado
                                    | ((not (is_white (vet!!cont))) && (not b)) = (find_adjacent_vec vet (cont+1) x b [])

--se o valor excedeu 6 ou menor que 1 corrige
correct_values::Int->Int
correct_values x |(x>size) = size
                 |(x<1) = 1
                 |otherwise = x

--acha os intervalos possiveis para n
min_element = 1
max_element = size
find_intervals::[Int]->(Int, Int)
find_intervals [] = (min_element, max_element)
find_intervals vet | (((maximum vet) /= 0)) = (correct_values (((maximum vet) - (length vet)) + 1), correct_values (((menor vet (size+1)) + (length vet)) - 1))
                   | otherwise = (min_element, max_element)

--possible::Int->Int->Int->Bool
--possible x y n = 

--checa se está no range do intervalo encontrado
test_is_in_range::(Int, Int)->(Int, Int)->Int->Bool
test_is_in_range (a, b) (c, d) n |(n>=a && n<=b && n>=c && n<=d) = True
                                 |otherwise = False

--testa se tem um elemento = n no vetor
test_is_repeated::[Int]->Int->Bool
test_is_repeated [] _ = False
test_is_repeated (a:b) n | (a == n) = True
                         |otherwise = test_is_repeated b n

--teste geral, retorna true se é possivel, false se não
test_is_possible::[[Element]]->Int->Int->Int->Bool
test_is_possible grid x y n | ((test_for_blacks (get_row grid x) n)) = False
                            | ((test_for_blacks (get_col grid zero y) n)) = False
                            | (test_is_repeated(find_adjacent_vec (get_row grid x) zero y found []) n) = False
                            | (test_is_repeated(find_adjacent_vec (get_col grid zero y) zero x found []) n) = False
                            | (test_is_in_range(find_intervals (find_adjacent_vec (get_row grid x) zero y found [])) (find_intervals (find_adjacent_vec (get_col grid zero y) zero x found [])) n) = True
                            | otherwise = False


-- FUNCOES PARA RESOLVER STR8TS
-- chamo meu primeiro iterador, se ele retornar em algum momento, solve retorna, se
-- iterator_one retornar True, houve teste de todas as possibilidades em iterator_possibility
-- e elas falharam, logo ativo o backtracking e retorno solve, caso iterator_one retorne
-- False, significa que está tudo certo

--retorna a matriz atualizada
updateMatrix::[[a]]->a->(Int, Int)->[[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

--metodo principal, chama iterador 1 e dependendo da tupla de booleans retornados finaliza ou continua rodando o codigo
--segundo elemento da tupla em todos os metodos indica que o resultado ja foi achado e deve retornar em cascata
-- primeiro boolean define erro(ativa backtracking) segundo boolean de retorno define se acabou a computação
solve::[[Element]]->Maybe (Bool, Bool, [[Element]])
solve grid = do
    --print<- trace ("what is going on") (return True)
    k <- return (iterator_one grid 0) 
    if (snd $ fromJust k)
        then return (False, True, grid)
        else if (fst (fromJust k)) 
            then return (True, False, grid)
            else return(False, False, grid)--n faz nada

--printa resultado da computação, sabe que chegou ao fim quando o 'i' excede o tamanho da matriz entrada
iterator_one::[[Element]]->Int->Maybe (Bool, Bool)
iterator_one grid i = do
    b <- (iterator_two grid i 0)
    --let x = trace(boolToString (fromJust b)) 1
    if (snd b)
        then return (False, True)
        else if (fst b)
            then return (True, False) --erro e nao acabou 
            else if (i+1)<size
                then do
                    print <- (return True)
                    it_one <- iterator_one grid (i+1)
                    return (False, False)
                else do
                    --print final
                    print <- trace (convert_grid_to_string $ convert_grid grid) (return False) --para poder printar minha matriz
                    return(False, True)


--chama iterator 2 denovo, caso nao seja um elemento "White" passa para o proximo quadrado, caso 'j' = size-1 deve retornar para quem chamou, pois
--acabou de iterar rows da linha e não deu erro
iterator_two::[[Element]]->Int->Int->Maybe (Bool, Bool)
iterator_two grid i j = do 
    white <- (return (is_white (grid!!i!!j))) 
    if (white) 
        then do
            let z = convert_element_to_int (grid!!i!!j)
            if (z == 0)
                then do
                    it_pos <- (iterator_possibility grid i j 1)
                    if (snd it_pos)
                        then return (False, True) 
                        else if (fst it_pos)
                            then return (True, False) --erro
                            else if (j < (size-1)) 
                                then do 
                                    it_two <- return (iterator_two grid i (j+1))
                                    if (snd $ fromJust it_two) then return (False, True)
                                        else if (fst $ fromJust it_two) then return (True, False)
                                            else return (False, False)
                                else return (False, False)
                else if (j < (size-1))
                    then do 
                        it_two <- return (iterator_two grid i (j+1))
                        if (snd $ fromJust it_two) then return (False, True)
                            else if (fst $ fromJust it_two) then return (True, False)
                                else return (False, False)
                    else return (False, False)
        else if (j < (size-1))
            then do
                it_two <- return (iterator_two grid i (j+1))
                if (snd $ fromJust it_two) then return (False, True)
                    else if (fst $ fromJust it_two) then return (True, False)
                        else return (False, False)
            else return (False, False)

--chama test_is_possible e 
iterator_possibility::[[Element]]->Int->Int->Int->Maybe (Bool, Bool)
iterator_possibility grid i j n = do
    solve_return <- return (False, False)
    is_possible <- return (test_is_possible grid i j n)
    if (is_possible) 
        then do
            grid_temp <- return (updateMatrix grid (convert_int_to_element n) (i, j))
            solve_return <- (solve grid_temp)
            if (sel2 solve_return) then return (False, True) 
                else do
                    grid_temp <- return (updateMatrix grid (convert_int_to_element 0) (i, j))
                    if n<size
                        then do
                            let it_possible = iterator_possibility grid_temp i j (n+1)
                            if (snd $ fromJust it_possible) then return (False, True)
                                else if (fst $ fromJust it_possible)
                                    then return (True, False)
                                    else return (False, False)
                        else if ((not $ sel1 $ solve_return) && (n == size))
                            then return (True, False)
                            else return (False, False)
        else if ((not is_possible) && n == size)
            then return (True, False)
            else if n<size then do
                let it_possible = iterator_possibility grid i j (n+1)
                if (snd $fromJust it_possible) then return (False, True)
                    else if (fst $ fromJust it_possible)
                        then return (True, False)
                        else return (False, False)
                else if (n == size)
                    then return (True, False)
                    else return (False, False)

print_final::[[String]]->IO ()
print_final [] = print()
print_final (a:b) = do
    print(a)
    print_final b 

main = do
    let x = trace ("call solve") (solve grid)
    --let b = solve
    --print(test_is_possible 4 0 6)

    print_final((convert_grid (sel3 (fromJust x))))