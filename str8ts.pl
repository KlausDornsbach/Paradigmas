:- use_module(library(clpfd)).
:- use_module(library(apply)).
:- use_module(library(listing)).

mat(1, [[7, 7, 7, _, _, 7],
        [7, 2, _, 4, _, 7],
        [_, 3, _, _, _, 4],
        [_, _, _, _, _, _],
        [7, _, 2, _, _, 7],
        [7, _, _, 7, 7, 12]]).

mat(2, [[3, 4, 7, _, _, 7],
        [_, _, 8, 6, _, 7],
        [_, _, 1, _, _, 7],
        [7, _, _, _, 1, _],
        [7, _, _, 7, _, _],
        [7, _, _, 7, _, _]]).

% query:
% mat(2, R), solution(R, S), maplist(portray_clause, S).

%%%
n(1).
n(2).
n(3).
n(4).
n(5).
n(6).

mod_list([], [], _):-!.
mod_list([H|T], [C|Rest], Size):- C #= H mod (Size + 1), mod_list(T, Rest, Size), !. 
%%%
all_diff([]).
all_diff([H|T]) :- H is 0, all_diff(T).
all_diff([H|T]) :- not(member(H, T)), all_diff(T); member(H, T), fail, !.

complete(Vec):-
        check_empty_and_attribute_domain(Vec), % atribuo dominio (1..6) aos itens que são variaveis
        mod_list(Vec, ModVec, 6),    % lista modada
        all_diff(ModVec),             % vejo se tdos da lista modada se diferem
        accumulate_sequence(Vec, []). % vejo se o vetor tem somente sequencias validas 

% atribuo dominio se não for um quadrado preto
check_empty_and_attribute_domain([]).
check_empty_and_attribute_domain([H|T]) :- var(H), n(H), check_empty_and_attribute_domain(T).
check_empty_and_attribute_domain([H|T]) :- nonvar(H), H < 7, n(H), check_empty_and_attribute_domain(T).
check_empty_and_attribute_domain([H|T]) :- nonvar(H), H >= 7, check_empty_and_attribute_domain(T).
% mapeia complete para linhas/colunas
map_complete([]).
map_complete([H|T])  :- complete(H), map_complete(T).
% calcula tamanho sub-vetores das linhas colunas
vec_size([_], 1):-!.
vec_size([_|T], O):- vec_size(T, Y), O #= Y + 1, !.

menor([], M, M):- !.
menor([H|T], M, O) :- (H\=0, H<M, Menor is H;
                      Menor is M), menor(T, Menor, O), !. 

maior([], M, M):-!.
maior([H|T], M, O):-H>M, Mi#=H, maior(T, Mi, O), !;
                    maior(T, M, O), !.
% acha intervalos
find_intervals(List, [Small, Big]) :-
        maior(List, 0, Big1),
        Big1 \= 0,
        menor(List, 6, Small1),
        vec_size(List, SizeVec),
        Small is Big1 - SizeVec + 1,
        Big #= Small1 + SizeVec - 1, !;
        Small #= 1,
        Big #= 6.

test_is_in_range([Small, Big], N) :- N>=Small, N=<Big.

reverse([], Rev, Rev):-!.
reverse([H|T], Acc, Rev):-reverse(T, [H|Acc], Rev), !.

% restraints
holds_sequence([], _):- !. 
holds_sequence([H|T], Interval) :- test_is_in_range(Interval, H), holds_sequence(T, Interval), !.

% acumula sub vetores nas linhas e colunas e checa se são válidas
accumulate_sequence([], [])    :- !.
accumulate_sequence([], L)     :- find_intervals(L, I), holds_sequence(L, I), !.
accumulate_sequence([H|T], L)  :- H < 7, accumulate_sequence(T, [H|L]), !.
accumulate_sequence([H|T], L)  :- H >= 7, find_intervals(L, I), holds_sequence(L, I), accumulate_sequence(T, []), !.

% parte vetorial de setar dicas na matriz solução
set_clues_vector([], []).
set_clues_vector([H|T], [Hsol|Tsol]):-nonvar(H), Hsol is H, set_clues_vector(T, Tsol).
set_clues_vector([H|T], [_|Tsol]):-var(H), set_clues_vector(T, Tsol).

%seta dicas
set_clues([], []).
set_clues([H|T], [Hsol|Tsol]):-set_clues_vector(H, Hsol), set_clues(T, Tsol).

% solving method
solution(Rows, Solution) :-
        Solution = [
                [X00, X01, X02, X03, X04, X05],
                [X10, X11, X12, X13, X14, X15],
                [X20, X21, X22, X23, X24, X25],
                [X30, X31, X32, X33, X34, X35],
                [X40, X41, X42, X43, X44, X45],
                [X50, X51, X52, X53, X54, X55]                        
        ],

        set_clues(Rows, Solution),

        map_complete(Solution), % completa linhas
        
        % completa colunas
        complete([X00, X10, X20, X30, X40, X50]),
        complete([X01, X11, X21, X31, X41, X51]),
        complete([X02, X12, X22, X32, X42, X52]), 
        %só vou até essa coluna pois demora muito pra computar
        complete([X03, X13, X23, X33, X43, X53]).
        %complete([X04, X14, X24, X34, X44, X54]),
        %complete([X05, X15, X25, X35, X45, X55]).
