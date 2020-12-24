camiseta(amarela).
camiseta(azul).
camiseta(branca).
camiseta(verde).
camiseta(vermelha).

nome(anita).
nome(clara).
nome(fernanda).
nome(marieta).
nome(rosana).

produto(celular).
produto(fogao).
produto(geladeira).
produto(tv).
produto(notebook).

desconto(30).
desconto(40).
desconto(50).
desconto(60).
desconto(70).

idade(22).
idade(29).
idade(31).
idade(36).
idade(43).

companhia(irmao).
companhia(marido).
companhia(pai).
companhia(namorado).
companhia(tio).

% está imediatamente a direita de Y
noLadoDireito(X, Y, Lista) :- nextto(Y, X, Lista).

% está imediatamente a esquerda de Y
noLadoEsquerdo(X, Y, Lista) :- nextto(X, Y, Lista).

%X está à ao lado de Y
aoLado(X,Y,Lista) :- nextto(X,Y,Lista);nextto(Y,X,Lista).
                       
%X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), 
                        nth0(IndexY,Lista,Y), 
                        IndexX < IndexY.
                        
%X está à direita de Y (em qualquer posição à direita)
aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista). 

%X está no canto se ele é o primeiro ou o último da lista
noCanto(X,Lista) :- last(Lista,X).
noCanto(X,[X|_]).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

solucao(ListaClientes) :-
    ListaClientes = [
        cliente(Camiseta1, Nome1, Produto1, Desconto1, Idade1, Companhia1),
        cliente(Camiseta2, Nome2, Produto2, Desconto2, Idade2, Companhia2),
        cliente(Camiseta3, Nome3, Produto3, Desconto3, Idade3, Companhia3),
        cliente(Camiseta4, Nome4, Produto4, Desconto4, Idade4, Companhia4),
        cliente(Camiseta5, Nome5, Produto5, Desconto5, Idade5, Companhia5)
    ],

    % cliente camisa azul está na direita da acompanhada pelo irmão e na esquerda da mais nova
    aDireita(cliente(azul, _, _, _, _, _), cliente(_, _, _, _, _, irmao), ListaClientes), 
    aEsquerda(cliente(azul, _, _, _, _, _), cliente(_, _, _, _, 22, _), ListaClientes),

    % cliente de vermelho ao lado da mulher com fogao
    aoLado(cliente(vermelha, _, _, _, _, _), cliente(_, _, fogao, _, _, _), ListaClientes),

    % anita a esquerda da pessoa com 50% de desconto
    noLadoEsquerdo(cliente(_, anita, _, _, _, _), cliente(_, _, _, 50, _, _), ListaClientes),

    % numa ponta pessoa camisa amarela
    noCanto(cliente(amarela, _, _, _, _, _), ListaClientes),

    % pessoa do notebook ta na direita de quem tem 70% de desc. e na esquerda de geladeira
    aDireita(cliente(_, _, notebook, _, _, _), cliente(_, _, _, 70, _, _), ListaClientes),
    aEsquerda(cliente(_, _, notebook, _, _, _), cliente(_, _, geladeira, _, _, _), ListaClientes),

    % pai ao lado celular
    aoLado(cliente(_, _, _, _, _, pai), cliente(_, _, celular, _, _, _), ListaClientes),

    % azul na direita de 43 anos e a esquerda notebook
    aDireita(cliente(azul, _, _, _, _, _), cliente(_, _, _, _, 43, _), ListaClientes),
    aEsquerda(cliente(azul, _, _, _, _, _), cliente(_, _, notebook, _, _, _), ListaClientes),

    % na quarta está cliente de verde
    Camiseta4 = verde,

    % fernanda ao lado de 40% desc.
    aoLado(cliente(_, fernanda, _, _, _, _), cliente(_, _, _, 40, _, _), ListaClientes),

    % c/ namorado ta ao lado de notebook
    aoLado(cliente(_, _, _, _, _, namorado), cliente(_, _, notebook, _, _, _), ListaClientes),

    % 31 anos na direita de 40% desc. e na esquerda de 29 anos
    aDireita(cliente(_, _, _, _, 31, _), cliente(_, _, _, 40, _, _), ListaClientes),
    aEsquerda(cliente(_, _, _, _, 31, _), cliente(_, _, _, _, 29, _), ListaClientes),

    % cliente c/ pai a direita de cli. azul
    aDireita(cliente(_, _, _, _, _, pai), cliente(azul, _, _, _, _, _), ListaClientes),

    % 40% desc. ao lado da acomp. irmao
    aoLado(cliente(_, _, _, 40, _, _), cliente(_, _, _, _, _, irmao), ListaClientes),

    % de branco ao lado 50% desc
    aoLado(cliente(branca, _, _, _, _, _), cliente(_, _, _, 50, _, _), ListaClientes),

    % acomp. pai a direita acomp irmao, e a esquerda acomp. namorado.
    aDireita(cliente(_, _, _, _, _ , pai), cliente(_, _, _, _, _ , irmao), ListaClientes),
    aEsquerda(cliente(_, _, _, _, _ ,pai), cliente(_, _, _, _, _ , namorado), ListaClientes),

    % marieta tem 31 anos
    member(cliente(_, marieta, _, _, 31, _), ListaClientes),

    % anita ao lado de azul
    aoLado(cliente(_, anita, _, _, _, _), cliente(azul, _, _, _, _, _), ListaClientes),

    % 30% desc.  a direita 70% e a esquerda acompanhado do pai
    aDireita(cliente(_, _, _, 30, _, _), cliente(_, _, _, 70, _, _), ListaClientes),
    aEsquerda(cliente(_, _, _, 30, _, _), cliente(_, _, _, _, _, pai), ListaClientes),

    % acomp. tio ao lado de verde
    aoLado(cliente(_, _, _, _, _, tio), cliente(verde, _, _, _, _, _), ListaClientes),

    Nome5 = clara,

    % 22 anos a direita azul e esquerda 31 anos
    aDireita(cliente(_, _, _, _, 22, _), cliente(azul, _, _, _, _, _), ListaClientes),
    aEsquerda(cliente(_, _, _, _, 22, _), cliente(_, _, _, _, 31, _), ListaClientes),

    % branca no lado direito acomp. marido
    noLadoDireito(cliente(branca, _, _, _, _, _), cliente(_, _, _, _, _, marido), ListaClientes),

    camiseta(Camiseta1), camiseta(Camiseta2), camiseta(Camiseta3),
    camiseta(Camiseta4), camiseta(Camiseta5),
    todosDiferentes([Camiseta1, Camiseta2, Camiseta3, Camiseta4, Camiseta5]),

    nome(Nome1), nome(Nome2), nome(Nome3), 
    nome(Nome4), nome(Nome5),
    todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),

    produto(Produto1), produto(Produto2), produto(Produto3), 
    produto(Produto4), produto(Produto5),
    todosDiferentes([Produto1, Produto2, Produto3, Produto4, Produto5]),

    desconto(Desconto1), desconto(Desconto2), desconto(Desconto3),
    desconto(Desconto4), desconto(Desconto5),
    todosDiferentes([Desconto1, Desconto2, Desconto3, Desconto4, Desconto5]),
    
    idade(Idade1), idade(Idade2), idade(Idade3),
    idade(Idade4), idade(Idade5),
    todosDiferentes([Idade1, Idade2, Idade3, Idade4, Idade5]),

    companhia(Companhia1), companhia(Companhia2), companhia(Companhia3),
    companhia(Companhia4), companhia(Companhia5),
    todosDiferentes([Companhia1, Companhia2, Companhia3, Companhia4, Companhia5]).

% SOLUCAO
% 1 ?- solucao(Sol).
% Sol = [cliente(vermelha, fernanda, tv, 70, 43, irmao), cliente(azul, rosana, fogao, 40, 36, marido), cliente(branca, anita, celular, 30, 22, tio), cliente(verde, marieta, notebook, 50, 31, pai), cliente(amarela, clara, geladeira, 60, 29, namorado)] ;
% false.






