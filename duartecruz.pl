% ist1110181 Duarte Cruz
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ["puzzlesAcampar.pl"]. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo

%Funcao que recebe uma coordenada e devolve a vizinhanca (imediatamente acima, imediatamente a esquerda, 
%imediatamente a direita e imediatamente abaixo da coordenada (L, C))
vizinhanca((L,C),Vizinhanca):-
    L1 is L-1,
    L2 is L+1,
    C1 is C-1,
    C2 is C+1,
    Vizinhanca = [(L1, C), (L, C1), (L, C2), (L2, C)].


%Funcao que recebe uma coordenada e devolve a vizinhanca alargada (vizinhanca, atraves da funcao vizinhanca e as posicoes nas diagonais)
vizinhancaAlargada((L,C),VizinhancaAlargada):-
    vizinhanca((L,C), Vizinhanca),
    L1 is L-1,
    L2 is L+1,
    C1 is C-1,
    C2 is C+1,
    append([(L1, C1), (L2, C1), (L1, C2), (L2, C2)], Vizinhanca,VizinhancaAlargadaNaoOrdenada),
    ordenar(VizinhancaAlargadaNaoOrdenada,VizinhancaAlargada).


% Funcao auxiliar para ordenar a lista de coordenadas de acordo com a ordem de leitura do jogo (cima,esquerda,direita,baixo)
% Neste caso a lista que recebe e vazia, logo a ordenada sera tambem vazia
ordenar([],Ordenada):-
    Ordenada = [].

% Neste caso a lista que recebe ja nao e vazia
ordenar(NaoOrdenada,Ordenada):-
    sort(NaoOrdenada,Ordenada).

% Funcao para obter a coordenada do canto inferior direito do tabuleiro
coordenadamaxima(Tabuleiro,CoordenadaMaxima):-
    length(Tabuleiro,L),
    CoordenadaMaxima = [L,L]. 

% Funcao para obter uma lista com todas as celulas do tabuleiro. Esta funcao utiliza a funcao auxiliar coordenadamaxima
% para saber qual a linha e coluna maxima do tabuleiro e a funcao auxiliar celulaslinhasaux que retorna uma lista nao ordenada
% de todas as celulas do tabuleiro. Utiliza ainda a funcao auxiliar ordenar para ordenar a lista recebida anteriormente 
todasCelulas(Tabuleiro, TodasCelulas):-
    coordenadamaxima(Tabuleiro,CoordenadaMaxima),
    nth0(0, CoordenadaMaxima, Lmax),
    nth0(1, CoordenadaMaxima, Cmax),
    celulaslinhasaux(1,1,Lmax,Cmax,TodasCelulasLinhas),
    ordenar(TodasCelulasLinhas, TodasCelulas).

% Funcao auxiliar que retorna todas as celulas do tabuleiro que utiliza por sua vez outra funcao auxiliar
% chamada celulaslinhas 
% quando chega a ultima linha do tabuleiro para a recursao
celulaslinhasaux(L,C,Lmax,Cmax,CelulasUltimaLinha):-
    L==Lmax,
    celulaslinhas(Lmax,C,Lmax,Cmax,CelulasUltimaLinha).

% muda para a linha seguinte quando acabamos de ver as celulas todas da linha pretendida
celulaslinhasaux(L,C,Lmax,Cmax,CelulasTodas):-
    L<Lmax,
    celulaslinhas(L,C,Lmax,Cmax,CelulasLinha),
    L1 is L+1,
    celulaslinhasaux(L1,C,Lmax,Cmax,Resto),
    append(CelulasLinha,Resto,CelulasTodas).

% Funcao auxiliar para obter as celulas todas de uma linha do tabuleiro 
% Neste caso quando chega a ultima celula da linha
celulaslinhas(L,C,_,Cmax,UltimaCelulaLinha):-
    C==Cmax,
    UltimaCelulaLinha = [(L,C)].

% Neste caso adiciona as celulas de todas as colunas (menos a ultima) de uma linha  
celulaslinhas(L,C,Lmax,Cmax,LinhaCompleta):-
    C<Cmax,
    C1 is C+1,
    celulaslinhas(L,C1,Lmax,Cmax,Resto),
    append([(L,C)],Resto,LinhaCompleta).

% Funcao auxiliar que determina qual o objeto presente na celula
objetoPresente(Celula, Objeto) :-
    Objeto = Celula.

% Funcao que retorna todas as celulas do tabuleiro que possuem um determinado objeto. Utiliza
% a funcao auxiliar coordenada maxima para ver qual a coordenada maxima do tabuleiro e a funcao
% correspondente ao caso (espacosVazios -> quando estamos a procura de celulas sem objeto, 
% todasCelulasAux -> quando estamos a procura de espacos preenchidos). Utiliza ainda a funcao auxiliar ordenar
% para ordenar a lista de acordo com a ordem de leitura do territorio
% Neste caso, para quando estamos a procura de espacos vazios
todasCelulas(Tabuleiro,TodasCelulas, TipoObjeto) :-
    coordenadamaxima(Tabuleiro, CoordenadaMaxima),
    nth0(0,CoordenadaMaxima,LinhaMax),
    nth0(1,CoordenadaMaxima,ColunaMax),
    TipoObjeto \== t,
    TipoObjeto \== r,
    TipoObjeto \== a,
    espacosVazios(Tabuleiro, LinhaMax, ColunaMax,[],TodasCelulasNaoOrdenadas),
    ordenar(TodasCelulasNaoOrdenadas,TodasCelulas).

% Neste caso para quando esta a procura de espacos preenchidos
todasCelulas(Tabuleiro,TodasCelulas, TipoObjeto) :-
    coordenadamaxima(Tabuleiro, CoordenadaMaxima),
    nth0(0,CoordenadaMaxima,LinhaMax),
    nth0(1,CoordenadaMaxima,ColunaMax),
    todasCelulasAux(Tabuleiro, LinhaMax, ColunaMax, TipoObjeto,[],TodasCelulasNaoOrdenadas),
    ordenar(TodasCelulasNaoOrdenadas,TodasCelulas).

% Funcao auxiliar que verifica se a celula contem o objeto pretendido 
% Quando ja estamos na Linha zero e porque ja vimos todo o tabuleiro, logo para-se a recursao
todasCelulasAux(_, Linha, _, _,X,R) :-
    Linha == 0, 
    R=X.
    
% Quando ja estamos na coluna zero e porque ja vimos a linha toda, logo muda-se de linha
todasCelulasAux(Tabuleiro, Linha, Coluna, TipoObjeto,TodasCelulasLinhasVistas,Resultado) :-
    Coluna == 0,
    coordenadamaxima(Tabuleiro, CoordenadaMaxima),
    nth0(1,CoordenadaMaxima,ColunaMax),
    Linha1 is Linha - 1,
    todasCelulasAux(Tabuleiro, Linha1, ColunaMax, TipoObjeto, TodasCelulasLinhasVistas,Resultado).

% Neste caso contem o objeto, logo adicionamos ao grupo das celulas que tambem tem o objeto e passamos para a proxima celula da linha 
todasCelulasAux(Tabuleiro, Linha, Coluna, TipoObjeto, TodasCelulasUmaLinha,Resultado) :-
    nth1(Linha, Tabuleiro, LinhaASerVista),
    nth1(Coluna, LinhaASerVista, Celula),
    objetoPresente(Celula, ObjetoPresente),
    ObjetoPresente == TipoObjeto,
    append([(Linha,Coluna)],TodasCelulasUmaLinha,TodasCelulasLinha),
    C1 is Coluna-1,
    todasCelulasAux(Tabuleiro, Linha, C1, TipoObjeto, TodasCelulasLinha,Resultado).

% Neste caso nao contem o objeto, logo ignoramos e passamos para a proxima celula da linha 
todasCelulasAux(Tabuleiro, Linha, Coluna, TipoObjeto, TodasCelulasUmaLinha,Resultado) :-
    nth1(Linha, Tabuleiro, LinhaASerVista),
    nth1(Coluna, LinhaASerVista, Celula),
    objetoPresente(Celula, ObjetoPresente),
    ObjetoPresente \== TipoObjeto,
    C1 is Coluna-1,
    todasCelulasAux(Tabuleiro, Linha, C1, TipoObjeto, TodasCelulasUmaLinha,Resultado).

% Funcao auxiliar que verifica se a celula corresponde a um espaco vazio 
% Quando ja estamos na Linha zero e porque ja vimos todo o tabuleiro, logo para-se a recursao
espacosVazios(_, Linha, _, _,_) :-
    Linha == 0, 
    !.

% Quando ja estamos na coluna zero e porque ja vimos a linha toda, logo muda-se de linha
espacosVazios(Tabuleiro, Linha, Coluna,TodasCelulasLinhasVistas,Resultado) :-
    Coluna == 0,
    coordenadamaxima(Tabuleiro, CoordenadaMaxima),
    nth0(1,CoordenadaMaxima,ColunaMax),
    Linha1 is Linha - 1,
    espacosVazios(Tabuleiro, Linha1, ColunaMax, TodasCelulasLinhasVistas,Resultado).

% Neste caso a celula corresponde a um espaco vazio, logo adicionamos ao grupo das celulas que tambem o sao 
% e passamos para a proxima celula da linha 
espacosVazios(Tabuleiro,Linha,Coluna,CelulasVaziasLinha,Resultado):-
    nth1(Linha, Tabuleiro, LinhaASerVista),
    nth1(Coluna, LinhaASerVista, Celula),
    objetoPresente(Celula, ObjetoPresente),
    ObjetoPresente \== t,
    ObjetoPresente \== r,
    ObjetoPresente \== a,
    append([(Linha,Coluna)],CelulasVaziasLinha,CelulasTodasVaziasLinha),
    C1 is Coluna-1,
    espacosVazios(Tabuleiro, Linha, C1, CelulasTodasVaziasLinha,Resto),
    append(Resto,CelulasTodasVaziasLinha,Resultado).

% Neste caso a celula contem um objeto, logo ignoramos e passamos a proxima celula da linha
espacosVazios(Tabuleiro,Linha,Coluna,CelulasVaziasLinha,Resultado):-
    nth1(Linha, Tabuleiro, LinhaASerVista),
    nth1(Coluna, LinhaASerVista, Celula),
    objetoPresente(Celula, ObjetoPresente),
    (ObjetoPresente == t;ObjetoPresente == r;ObjetoPresente == a),
    C1 is Coluna-1,
    espacosVazios(Tabuleiro, Linha, C1, CelulasVaziasLinha,Resultado).

% Funcao que conta a quantidade do objeto/espaco vazio, por linha e por coluna do tabuleiro
% Utiliza a funcao todasCelulas para saber quais sao as celulas que contem o objeto/espaco vazio,
% a funcao auxiliar contarLinhas para contar a quantidade por linha, a funcao ordenarporColunas para
% dar transpose ao tabuleiro e a funcao contarColunas para contar a quantidade por coluna.
% No caso de haver celulas que contem o objeto pretendido
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, TipoObjeto):-
    todasCelulas(Tabuleiro,ListaTodasCelulas, TipoObjeto),
    ListaTodasCelulas \== [],
    coordenadamaxima(Tabuleiro,[LinhaMax,_]),
    findall(N, between(1, LinhaMax, N), ListaTodasLinhas),
    contarLinhas(ListaTodasCelulas,ListaTodasLinhas,0,[],ContagemLinhas),
    ordenarporColunas(ListaTodasCelulas,ListaOrdenadaporColunas),
    contarColunas(ListaOrdenadaporColunas,ListaTodasLinhas,0,[],ContagemColunas).

% No caso de nao haver em nenhuma celula o objeto que foi procurado
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, TipoObjeto):-      
    todasCelulas(Tabuleiro,ListaTodasCelulas, TipoObjeto),
    coordenadamaxima(Tabuleiro,[LinhaMax,_]),
    ListaTodasCelulas == [],
    length(ContagemLinhas,LinhaMax),
    findall(0, member(_, ContagemLinhas), ContagemLinhas),
    length(ContagemColunas,LinhaMax),
    findall(0, member(_, ContagemColunas), ContagemColunas).

% Funcao que conta a quantidade de um determinado objeto/espaco vazio por linha
% Ja nao ha mais celulas com o objeto e nem mais linhas por verificar, logo ja podemos parar de procurar
contarLinhas([],[],_,ListaContagem,ContagemLinhas):-
    ContagemLinhas=ListaContagem.

% Quando ja nao ha mais celulas com o objeto/espaco vazio, mas ainda faltam linhas por verificar
contarLinhas([],[_|OutrasLinhas],Contagem,ListaContagem,ContagemLinhas):-
    append(ListaContagem,[Contagem],ListaContagemNova),
    contarLinhas([],OutrasLinhas,0,ListaContagemNova,ContagemLinhas).

% Quando a celula esta na linha que estamos a verificar, adicionamos 1 valor a contagem e passamos a proxima celula, mantendo-nos na mesma linha
contarLinhas([(Linha,_)|RestantesCelulasComObjeto],[LinhaAtual|OutrasLinhas],Contagem,ListaContagem,ContagemLinhas):-
    Linha==LinhaAtual,
    ContagemNova is Contagem+1,
    contarLinhas(RestantesCelulasComObjeto,[LinhaAtual|OutrasLinhas],ContagemNova,ListaContagem,ContagemLinhas).

% Quando a celula esta na linha seguinte a que estamos a verificar, adicionamos o valor da contagem da linha que estamos a verificar a lista de 
% valores e passamos para a proxima linha, sendo que reiniciamos tambem o valor de contagem para o zero
contarLinhas([(Linha,_)|RestantesCelulasComObjeto],[LinhaAtual|OutrasLinhas],Contagem,ListaContagem,ContagemLinhas):-
    Linha>LinhaAtual,
    append(ListaContagem,[Contagem],ListaContagemNova),
    contarLinhas([(Linha,_)|RestantesCelulasComObjeto],OutrasLinhas,0,ListaContagemNova,ContagemLinhas).

% Funcao que conta a quantidade de um determinado objeto/espaco vazio por coluna
% Ja nao ha mais celulas com o objeto e nem mais colunas por verificar, logo ja podemos parar de procurar
contarColunas([],[],_,ListaContagem,ContagemColunas):-
    ContagemColunas=ListaContagem.

% Quando ja nao ha mais celulas com o objeto/espaco vazio, mas ainda faltam colunas por verificar
contarColunas([],[_|OutrasColunas],Contagem,ListaContagem,ContagemColunas):-
    append(ListaContagem,[Contagem],ListaContagemNova),
    contarColunas([],OutrasColunas,0,ListaContagemNova,ContagemColunas).
    
% Quando a celula esta na coluna que estamos a verificar, adicionamos 1 valor a contagem e passamos a proxima celula, mantendo-nos na mesma coluna
contarColunas([(_,Coluna)|RestantesCelulasComObjeto],[ColunaAtual|OutrasColunas],Contagem,ListaContagem,ContagemColunas):-
    Coluna==ColunaAtual,
    ContagemNova is Contagem+1,
    contarColunas(RestantesCelulasComObjeto,[ColunaAtual|OutrasColunas],ContagemNova,ListaContagem,ContagemColunas).

% Quando a celula esta na coluna seguinte a que estamos a verificar, adicionamos o valor da contagem da coluna que estamos a verificar a lista de 
% valores e passamos para a proxima coluna, sendo que reiniciamos tambem o valor de contagem para o zero
contarColunas([(_,Coluna)|RestantesCelulasComObjeto],[ColunaAtual|OutrasColunas],Contagem,ListaContagem,ContagemColunas):-
    Coluna>ColunaAtual,
    append(ListaContagem,[Contagem],ListaContagemNova),
    contarColunas([(_,Coluna)|RestantesCelulasComObjeto],OutrasColunas,0,ListaContagemNova,ContagemColunas).

% Faz a mesma coisa que o transpose/2, no entanto este facilitou-me a visualizacao da matriz apos a alteracao. 
%Ou seja, troca as colunas para as linhas e as linhas para as colunas
ordenarporColunas(ListaOriginal,ListaOrdenada):-
    sort(2,@=<,ListaOriginal,ListaOrdenada).

% Funcao que verifica se a celula corresponde a espaco vazio. Utiliza a funcao coordenadamaxima para ajudar na verificacao de a coordenada
% pertencer ao tabuleiro
% Neste caso a coordenada pertence ao tabuleiro, logo retorna True apenas se a celula corresponder a um espaco vazio
celulaVazia(Tabuleiro,(Linha,Coluna)):-
    coordenadamaxima(Tabuleiro,[LinhaMax,ColunaMax]),
    Linha =< LinhaMax,
    Coluna =< ColunaMax,
    Linha>0,
    Coluna>0,
    nth1(Linha,Tabuleiro,LinhaCompleta),
    nth1(Coluna,LinhaCompleta,Celula),
    Celula \== a,
    Celula \== t.

% Neste caso a coordenada nao pertence ao tabuleiro, logo retorna True
celulaVazia(Tabuleiro,(Linha,Coluna)):-
    coordenadamaxima(Tabuleiro,[LinhaMax,ColunaMax]),
    (Linha<1; Coluna<1; LinhaMax<Linha; ColunaMax<Coluna).

% Funcao que insere um determinado objeto numa determinada celula. Utiliza a funcao auxiliar insereObjeto
insereObjectoCelula(Tabuleiro,Objeto,(Linha,Coluna)):-
    insereObjeto(Tabuleiro,Objeto,(Linha,Coluna),1,1,[],TabuleiroNovo),
    Tabuleiro = TabuleiroNovo.

% Funcao auxiliar que insere um objeto na celula quando esta e um espaco vazio.
%Quando ja estamos na linha pretendida, vamos para outra funcao auxiliar denominada insereObjetoLinhaCerta
% Neste caso nao estamos na linha que e suposto alterar, logo nao se altera nada e passamos a proxima linha
insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaAtual,ColunaAtual,JaVisto,Resultado):-
    coordenadamaxima(Tabuleiro,[LinhaMax,_]),
    LinhaAtual =< LinhaMax,
    LinhaAtual \== Linha,
    nth1(LinhaAtual,Tabuleiro,LinhaToda),
    append(JaVisto,[LinhaToda],NovoTabuleiro),
    LinhaNova is LinhaAtual+1,
    insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaNova,ColunaAtual,NovoTabuleiro,Resultado).

% Este caso e para quando ja verificamos celulas, logo a variavel JaVisto nao esta vazia
insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaAtual,ColunaAtual,JaVisto,Resultado):-
    LinhaAtual == Linha,
    JaVisto \== [],
    coordenadamaxima(Tabuleiro,[_,ColunaMax]),
    nth1(LinhaAtual,Tabuleiro,LinhaToda),
    insereObjetoLinhaCerta(LinhaToda,Tenda,Coluna,ColunaAtual,ColunaMax,[],LinhaAtualizada),
    append(JaVisto,[LinhaAtualizada],NovoTabuleiro),
    LinhaNova is LinhaAtual+1,
    insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaNova,ColunaAtual,NovoTabuleiro,Resultado).

% A mesma coisa de cima, mas neste caso e para quando ainda nao verificamos nada, logo a variavel JaVisto esta vazia
insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaAtual,ColunaAtual,JaVisto,Resultado):-
    LinhaAtual == Linha,
    JaVisto == [],
    coordenadamaxima(Tabuleiro,[_,ColunaMax]),
    nth1(LinhaAtual,Tabuleiro,LinhaToda),
    insereObjetoLinhaCerta(LinhaToda,Tenda,Coluna,ColunaAtual,ColunaMax,[],LinhaAtualizada),
    append([],[LinhaAtualizada],NovoTabuleiro),
    LinhaNova is LinhaAtual+1,
    insereObjeto(Tabuleiro,Tenda,(Linha,Coluna),LinhaNova,ColunaAtual,NovoTabuleiro,Resultado).

% Para quando ja passamos pela linha, logo podemos terminar a procura
insereObjeto(Tabuleiro,_,_,LinhaAtual,_,JaVisto,Resultado):-
    coordenadamaxima(Tabuleiro,[LinhaMax,_]),
    LinhaAtual > LinhaMax,
    Resultado = JaVisto.

% Para quando ja estamos na linha pretendida e agora encontramo-nos na celula pretendida.
% Este caso e para quando a celula nao tem nenhum objeto, pois so assim conseguimos colocar la algo
insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaAtual,ColunaMax,LinhaAteAgora,LinhaNova):-
    Coluna==ColunaAtual,
    nth1(ColunaAtual,LinhaAntes,Celula),
    Celula \== a,
    append(LinhaAteAgora,[Objeto],LinhaAtualizada),
    ColunaNova is ColunaAtual +1,
    insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaNova,ColunaMax,LinhaAtualizada,LinhaNova).

% O mesmo de cima, no entanto este caso e para quando ja se encontra algo na celula, logo nao se coloca nada na celula
insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaAtual,ColunaMax,LinhaAteAgora,LinhaNova):-  
    Coluna==ColunaAtual,
    append(LinhaAteAgora,[a],LinhaAtualizada),
    ColunaNova is ColunaAtual +1,
    insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaNova,ColunaMax,LinhaAtualizada,LinhaNova).

% Para quando ja estamos na linha certa mas na coluna errada, pelo que vamos adicionando as celulas a uma linha nova que depois ira substituir a antiga no tabuleiro
insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaAtual,ColunaMax,LinhaAteAgora,LinhaNova):-
    ColunaAtual\==Coluna,
    ColunaAtual =< ColunaMax,
    nth1(ColunaAtual,LinhaAntes,Celula),
    append(LinhaAteAgora,[Celula],LinhaAtual),
    ColunaNova is ColunaAtual + 1,
    insereObjetoLinhaCerta(LinhaAntes,Objeto,Coluna,ColunaNova,ColunaMax,LinhaAtual,LinhaNova).

% Para quando ja vimos a linha pretendida toda, pelo que ja temos a linha atualizada
insereObjetoLinhaCerta(_,_,_,ColunaAtual,ColunaMax,LinhaAteAgora,LinhaNova):-
    ColunaAtual > ColunaMax,
    LinhaNova = LinhaAteAgora.

% Para inserir um determinado objeto entre celulas (inclusive)
% Este caso e para se as posicoes forem da mesma linha, que ira para a funcao auxiliar entrePosicoesAuxLinhas
insereObjectoEntrePosicoes(Tabuleiro,Objeto,(Linha,C1),(L2,C2)):-               
    Linha==L2,
    C1 \== C2,
    entrePosicoesAuxLinhas(Tabuleiro,Objeto,(Linha,C1),(Linha,C2),TabuleiroFinal),
    Tabuleiro = TabuleiroFinal.

% Este caso e para se as posicoes forem da mesma coluna, que ira para a funcao auxiliar entrePosicoesAuxColunas
insereObjectoEntrePosicoes(Tabuleiro,Objeto,(L1,Coluna),(L2,C2)):-               
    L1 \== L2,
    Coluna==C2,
    entrePosicoesAuxColunas(Tabuleiro,Objeto,(L1,Coluna),(L2,C2),TabuleiroFinal),
    Tabuleiro = TabuleiroFinal.

% Funcao auxiliar para quando queremos adicionar objetos entre celulas da mesma linha
% Neste caso verifica que ainda nao passamos pelo limite estabelecido e so adiciona se a celula estiver vazia 
% (esta verificacao e a posterior colocacao do objeto acontece atraves da funcao insereObjectoCelula)
entrePosicoesAuxLinhas(Tabuleiro,Objeto,(Linha,CAtual),(Linha,C2),TabuleiroFinal):-
    Diferenca is C2-CAtual,
    Diferenca>=0,
    insereObjectoCelula(Tabuleiro,Objeto,(Linha,CAtual)),
    ColunaNova is CAtual+1,
    entrePosicoesAuxLinhas(Tabuleiro,Objeto,(Linha,ColunaNova),(Linha,C2),TabuleiroFinal).

% Neste caso verifica que ja passamos pelo limite estabelecido, pelo que ja temos o tabuleiro atualizado
entrePosicoesAuxLinhas(TabuleiroAtual,_,(_,CAtual),(_,C2),TabuleiroFinal):-
    Diferenca is C2-CAtual,
    Diferenca<0,
    TabuleiroFinal = TabuleiroAtual.

% Funcao auxiliar para quando queremos adicionar objetos entre celulas da mesma coluna
% Neste caso verifica que ainda nao passamos pelo limite estabelecido e so adiciona se a celula estiver vazia 
% (esta verificacao e a posterior colocacao do objeto acontece atraves da funcao insereObjectoCelula)
entrePosicoesAuxColunas(Tabuleiro,Objeto,(LAtual,Coluna),(L2,Coluna),TabuleiroFinal):-
    Diferenca is L2-LAtual,
    Diferenca>=0,
    insereObjectoCelula(Tabuleiro,Objeto,(LAtual,Coluna)),
    LinhaNova is LAtual+1,
    entrePosicoesAuxColunas(Tabuleiro,Objeto,(LinhaNova,Coluna),(L2,Coluna),TabuleiroFinal).

% Neste caso verifica que ja passamos pelo limite estabelecido, pelo que ja temos o tabuleiro atualizado
entrePosicoesAuxColunas(TabuleiroAtual,_,(LAtual,_),(L2,_),TabuleiroFinal):-
    Diferenca is L2-LAtual,
    Diferenca<0,
    TabuleiroFinal = TabuleiroAtual.

% Funcao que verifica se ja ha o maximo de tendas nas linhas e nas colunas. Caso haja, entao sabemos que
% so pode haver relva nas restantes celulas da mesma linha/coluna, pelo que esta funcao a adiciona.
relva((Tabuleiro,TendasPorLinha,TendasPorColuna)):-
    coordenadamaxima(Tabuleiro,[LinhaMax,ColunaMax]),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    tendasPorLinhaAux(Tabuleiro,TendasPorLinha,1,LinhaMax,ContagemLinhas,TabuleiroAposLinhas),
    tendasPorColunaAux(TabuleiroAposLinhas,TendasPorColuna,1,ColunaMax,ContagemColunas,TabuleiroFinal),                            
    Tabuleiro = TabuleiroFinal.

% Funcao auxiliar que verifica se ja se atingiu o limite de tendas nalguma das linhas
% Neste caso verifica que ja se atingiu o numero, pelo que sabemos que podemos adicionar relva nos espacos vazios
% desta linha, pelo que utiliza a funcao insereObjectoEntrePosicoes para o realizar
tendasPorLinhaAux(Tabuleiro,LimiteTendasPorLinha,Linha,LinhaMax,ContagemLinhas,TabuleiroAposLinhas):-
    Linha =< LinhaMax,
    nth1(Linha,LimiteTendasPorLinha,Limite),
    nth1(Linha,ContagemLinhas,Numero),
    Limite==Numero,
    insereObjectoEntrePosicoes(Tabuleiro,r,(Linha,1),(Linha,LinhaMax)),
    LinhaNova is Linha+1,
    tendasPorLinhaAux(Tabuleiro,LimiteTendasPorLinha,LinhaNova,LinhaMax,ContagemLinhas,TabuleiroAposLinhas).

% Neste caso verifica que a linha ainda nao atingiu o numero limite de tendas, pelo que nao podemos assumir
% que os restantes espacos sao relva, logo nao se altera a linha
tendasPorLinhaAux(Tabuleiro,LimiteTendasPorLinha,Linha,LinhaMax,ContagemLinhas,TabuleiroAposLinhas):-
    Linha =< LinhaMax,
    nth1(Linha,LimiteTendasPorLinha,Limite),
    nth1(Linha,ContagemLinhas,Numero),
    Limite\==Numero,
    LinhaNova is Linha+1,
    tendasPorLinhaAux(Tabuleiro,LimiteTendasPorLinha,LinhaNova,LinhaMax,ContagemLinhas,TabuleiroAposLinhas).

% Neste caso ja verificamos todas as linhas, pelo que ja temos um tabuleiro atualizado, que tanto pode ter
% alteracoes (caso alguma linha tenha atingido o limite) ou nao (caso nenhuma linha tenha atingido o limite)
tendasPorLinhaAux(Tabuleiro,_,Linha,LinhaMax,_,TabuleiroAposLinhas):-
    Linha > LinhaMax,
    TabuleiroAposLinhas = Tabuleiro.

% Funcao auxiliar que verifica se ja se atingiu o limite de tendas nalguma das colunas
% Neste caso verifica que ja se atingiu o numero, pelo que sabemos que podemos adicionar relva nos espacos vazios
% desta coluna, pelo que utiliza a funcao insereObjectoEntrePosicoes para o realizar
tendasPorColunaAux(Tabuleiro,LimiteTendasPorColuna,Coluna,ColunaMax,ContagemColunas,TabuleiroFinal):-
    Coluna =< ColunaMax,
    nth1(Coluna,LimiteTendasPorColuna,Limite),
    nth1(Coluna,ContagemColunas,Numero),
    Limite==Numero,
    insereObjectoEntrePosicoes(Tabuleiro,r,(1,Coluna),(ColunaMax,Coluna)),
    ColunaNova is Coluna+1,
    tendasPorColunaAux(Tabuleiro,LimiteTendasPorColuna,ColunaNova,ColunaMax,ContagemColunas,TabuleiroFinal).

% Neste caso verifica que a coluna ainda nao atingiu o numero limite de tendas, pelo que nao podemos assumir
% que os restantes espacos sao relva, logo nao se altera a coluna
tendasPorColunaAux(Tabuleiro,LimiteTendasPorColuna,Coluna,ColunaMax,ContagemColunas,TabuleiroFinal):-
    Coluna =< ColunaMax,
    nth1(Coluna,LimiteTendasPorColuna,Limite),
    nth1(Coluna,ContagemColunas,Numero),
    Limite\==Numero,
    ColunaNova is Coluna+1,
    tendasPorColunaAux(Tabuleiro,LimiteTendasPorColuna,ColunaNova,ColunaMax,ContagemColunas,TabuleiroFinal).

% Neste caso ja verificamos todas as colunas, pelo que ja temos um tabuleiro atualizado, que tanto pode ter
% alteracoes (caso alguma coluna tenha atingido o limite) ou nao (caso nenhuma coluna tenha atingido o limite)
tendasPorColunaAux(Tabuleiro,_,Coluna,ColunaMax,_,TabuleiroFinal):-
    Coluna > ColunaMax,
    TabuleiroFinal = Tabuleiro.

% Funcao que verifica se ha posicoes que nao se pode por tendas,por nao estarem na vizinhanca de nenhuma arvore
% e coloca relva nessas posicoes (caso nao estejam ocupadas)
% Esta funcao utiliza a funcao todasCelulas para obter uma lista com celulas que possuem arvores e obtem as 
% suas vizinhancas (unicas celulas onde se pode colocar tendas) atraves da funcao auxiliar celulasPermitidas.
% A partir destas vizinhancas, verifica quais sao as celulas do tabuleiro que nao se encontram nas vizinhancas
% atraves da funcao inacessiveisAux e verifica quais e que sao os espacos vazios. Assim, estes espacos vazios so 
% poderao possuir relva.
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro,TodasCelulasArvore, a),
    celulasPermitidas(TodasCelulasArvore,[],CelulasPermitidas),
    todasCelulas(Tabuleiro,TodasCelulas),
    inacessiveisAux(Tabuleiro,CelulasPermitidas,TodasCelulas,TabuleiroFinal),
    Tabuleiro=TabuleiroFinal.

% Funcao auxiliar que obtem as vizinhancas das arvores
% Neste caso, vai verificando 1 a 1 e adiciona as suas vizinhancas a variavel JaVistas
celulasPermitidas([Arvore|RestantesArvores],CelulasPermitidas,TodasPermitidas):-
    vizinhanca(Arvore,Vizinhanca),
    append(Vizinhanca,CelulasPermitidas,JaVistas),
    celulasPermitidas(RestantesArvores,JaVistas,TodasPermitidas).

% Neste caso, quando nao ha mais arvores para verificar a sua vizinhanca, sabe-se que a variavel
% Permitidas corresponde a todas as vizinhancas de arvores
celulasPermitidas([],Permitidas,Resultado):-
    ordenar(Permitidas,Resultado).

% Verifica quais as celulas do tabuleiro que pertencem a vizinhanca de uma arvore e adiciona relva caso
% sejam espacos vazios
% Neste caso verifica que a celula nao pertence a vizinhanca
inacessiveisAux(Tabuleiro,CelulasPermitidas,[Celula|RestoCelulasTabuleiro],TabuleiroFinal):-
    member(Celula,CelulasPermitidas),
    inacessiveisAux(Tabuleiro,CelulasPermitidas,RestoCelulasTabuleiro,TabuleiroFinal).

% Neste caso verifica que a celula pertence a vizinhanca e utiliza a funcao insereObjectoCelula
% para adicionar relva a celula, caso seja vazia
inacessiveisAux(Tabuleiro,CelulasPermitidas,[Celula|RestoCelulasTabuleiro],TabuleiroFinal):-
    \+ member(Celula,CelulasPermitidas),
    insereObjectoCelula(Tabuleiro,r,Celula),
    inacessiveisAux(Tabuleiro,CelulasPermitidas,RestoCelulasTabuleiro,TabuleiroFinal).

% Neste caso ja nao ha mais celulas por verificar, pelo que ja temos o tabuleiro final
inacessiveisAux(Tabuleiro,_,[],TabuleiroFinal):-
    TabuleiroFinal=Tabuleiro.

% Funcao que verifica se ha o mesmo numero de espacos vazios (atraves da funcao calculaObjectosTabuleiro)
%e de limite de tendas por linha (atraves da funcao aproveitaAuxLinha) e coluna (atraves da funcao aproveitaAuxColuna).
% Caso seja igual, sabemos que todos os espacos livres correspondem a tendas, pelo que as podemos colocar.
aproveita((Tabuleiro,TendasPorLinha,TendasPorColuna)):-
    coordenadamaxima(Tabuleiro,[LinhaMax,_]),
    calculaObjectosTabuleiro(Tabuleiro,CLinhas,CColunas,_),
    aproveitaAuxLinha(Tabuleiro,TendasPorLinha,1,LinhaMax,CLinhas,TabuleiroAposLinhas),
    aproveitaAuxColuna(TabuleiroAposLinhas,TendasPorColuna,1,LinhaMax,CColunas,TabuleiroFinal),
    Tabuleiro=TabuleiroFinal.

% Funcao auxiliar que verifica se o limite de tendas de uma determinada linha corresponde ao numero de espacos vazios dessa
% mesma linha.
% Em caso afirmativo, coloca-as atraves da funcao insereObjectoEntrePosicoes.
% Neste caso o numero de espacos vazios corresponde ao limite de tendas da linha, pelo que coloca as tendas e muda para a 
% proxima linha
aproveitaAuxLinha(Tabuleiro,TendasPorLinha,LinhaAtual,LinhaMax,CLinhas,TabuleiroAposLinhas):-
    LinhaAtual =< LinhaMax,
    nth1(LinhaAtual,TendasPorLinha,LimiteLinha),
    nth1(LinhaAtual,CLinhas,EspacosVazios),
    EspacosVazios \==0,
    LimiteLinha == EspacosVazios,
    insereObjectoEntrePosicoes(Tabuleiro,t,(LinhaAtual,1),(LinhaAtual,LinhaMax)),
    LinhaNova is LinhaAtual+1,
    aproveitaAuxLinha(Tabuleiro,TendasPorLinha,LinhaNova,LinhaMax,CLinhas,TabuleiroAposLinhas).

% Neste caso o numero de espacos vazios ou e zero ou nao corresponde ao limite de tendas da linha,
% pelo que nao colocamos nenhuma tenda e passamos para a proxima linha
aproveitaAuxLinha(Tabuleiro,TendasPorLinha,LinhaAtual,LinhaMax,CLinhas,TabuleiroAposLinhas):-
    LinhaAtual =< LinhaMax,
    nth1(LinhaAtual,TendasPorLinha,LimiteLinha),
    nth1(LinhaAtual,CLinhas,EspacosVazios),
    (LimiteLinha \== EspacosVazios;EspacosVazios ==0),
    LinhaNova is LinhaAtual+1,
    aproveitaAuxLinha(Tabuleiro,TendasPorLinha,LinhaNova,LinhaMax,CLinhas,TabuleiroAposLinhas).

% Neste caso ja verificamos as linhas todas, pelo que ja temos o tabuleiro final, que tanto pode estar alterado
% ou pode estar igual
aproveitaAuxLinha(Tabuleiro,_,LinhaAtual,LinhaMax,_,TabuleiroAposLinhas):-
    LinhaAtual > LinhaMax,
    TabuleiroAposLinhas=Tabuleiro.

% Funcao auxiliar que verifica se o limite de tendas de uma determinada coluna corresponde ao numero de espacos vazios dessa 
% mesma coluna.
% Em caso afirmativo, coloca-as atraves da funcao insereObjectoEntrePosicoes.
% Neste caso o numero de espacos vazios corresponde ao limite de tendas da coluna, pelo que coloca as tendas e muda para a 
% proxima coluna
aproveitaAuxColuna(Tabuleiro,TendasPorColuna,ColunaAtual,ColunaMax,CColunas,TabuleiroFinal):-
    ColunaAtual =< ColunaMax,
    nth1(ColunaAtual,TendasPorColuna,LimiteColuna),
    nth1(ColunaAtual,CColunas,EspacosVazios),
    EspacosVazios \==0,
    LimiteColuna == EspacosVazios,
    insereObjectoEntrePosicoes(Tabuleiro,t,(1,Coluna),(ColunaMax,Coluna)),
    ColunaNova is ColunaAtual+1,
    aproveitaAuxColuna(Tabuleiro,TendasPorColuna,ColunaNova,ColunaMax,CColunas,TabuleiroFinal).

% Neste caso o numero de espacos vazios ou e zero ou nao corresponde ao limite de tendas da coluna,
% pelo que nao colocamos nenhuma tenda e passamos para a proxima coluna
aproveitaAuxColuna(Tabuleiro,TendasPorColuna,ColunaAtual,ColunaMax,CColunas,TabuleiroFinal):-
    ColunaAtual =< ColunaMax,
    nth1(ColunaAtual,TendasPorColuna,LimiteColuna),
    nth1(ColunaAtual,CColunas,EspacosVazios),
    (LimiteColuna \== EspacosVazios;EspacosVazios ==0),
    ColunaNova is ColunaAtual+1,
    aproveitaAuxColuna(Tabuleiro,TendasPorColuna,ColunaNova,ColunaMax,CColunas,TabuleiroFinal).


% Neste caso ja verificamos as colunas todas, pelo que ja temos o tabuleiro final, que tanto pode estar alterado
% ou pode estar igual
aproveitaAuxColuna(Tabuleiro,_,ColunaAtual,ColunaMax,_,TabuleiroFinal):-
    ColunaAtual > ColunaMax,
    TabuleiroFinal=Tabuleiro.

% Funcao que coloca relva nos espacos vazios da vizinhanca alargada de uma tenda, pois nao se pode colocar la
% nenhuma outra tenda. A funcao utiliza a funcao todasCelulas para receber uma lista de todas as celulas que 
% possuem uma tenda e utiliza a funcao auxiliar limpaVizinhancasAux para colocar a relva
limpaVizinhancas((Tabuleiro,_,_)):-
    todasCelulas(Tabuleiro,CelulascomTendas,t),
    limpaVizinhancasAux(Tabuleiro,CelulascomTendas,TabuleiroApos),
    Tabuleiro=TabuleiroApos.

% Funcao auxiliar que coloca relva na vizinhanca alargada de uma tenda.
% Neste caso ja verificamos todas as celulas com tenda, pelo que ja temos o tabuleiro final.
limpaVizinhancasAux(Tabuleiro,[],TabuleiroFinal):-
    TabuleiroFinal=Tabuleiro.

% Neste caso obtem a vizinhanca alargada atraves da funcao vizinhanca e coloca relva atraves da funcao
% auxiliar coloca relva
limpaVizinhancasAux(Tabuleiro,[(LinhaAtual|ColunaAtual)|RestoCelulas],Resultado):-
    vizinhanca((LinhaAtual|ColunaAtual),Vizinhanca),
    colocaRelva(Tabuleiro,Vizinhanca,TabuleiroApos),
    limpaVizinhancasAux(TabuleiroApos,RestoCelulas,Resultado).

% Funcao auxiliar que coloca relva nas celulas pretendidas.
% Neste caso ja colocamos em todas as celulas, pelo que ja temos o tabuleiro final.
colocaRelva(Tabuleiro,[],TabuleiroApos):-
    Tabuleiro=TabuleiroApos.

% Neste caso utiliza a funcao insereObjectoCelula para colocar relva na celula pretendida e passa para a proxima
colocaRelva(Tabuleiro,[(LinhaAtual|ColunaAtual)|RestoCelulas],TabuleiroApos):-
    insereObjectoCelula(Tabuleiro,(LinhaAtual|ColunaAtual),r),
    colocaRelva(Tabuleiro,RestoCelulas,TabuleiroApos).
