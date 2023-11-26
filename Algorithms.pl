%===========================================================================================================
% Ligações entre edificios

liga(a,b).
liga(b,c).
liga(b,d).
liga(c,d).


%===========================================================================================================
% Pisos dos edificios

pisos(a,[a1,a2]).
pisos(b,[b1,b2,b3]).
pisos(c,[c0,c1,c2,c3]).
pisos(d,[d0,d1,d2]).

%===========================================================================================================
% Elevador de cada edificio

elevador(a,[a1,a2]).
elevador(b,[b1,b2,b3]).
elevador(c,[c0,c1,c2,c3]).
elevador(d,[d0,d1,d2]).

%===========================================================================================================
% Ligações entre pisos

corredor(c,d,c1,d1).

corredor(a,b,a2,b2).
corredor(b,c,b2,c2).
corredor(b,d,b2,d2).
corredor(c,d,c2,d2).

corredor(b,c,b3,c3).


%===========================================================================================================
% Pontos de Acesso

ponto_acesso(a101,a1,cel(2,2)).
ponto_acesso(elev(a1, a2),a1,cel(7,7)).

ponto_acesso(elev(a1, a2),a2,cel(2,2)).
ponto_acesso(cor(a2, b2),a2,cel(7,7)).

ponto_acesso(cor(a2, b2),b2,cel(2,2)).
ponto_acesso(cor(b2, c2),b2,cel(7,7)).

ponto_acesso(cor(b2, c2),c2,cel(2,2)).
ponto_acesso(cor(c2, d2),c2,cel(7,7)).

ponto_acesso(cor(c2, d2),d2,cel(2,2)).
ponto_acesso(d201,d2,cel(7,7)).

% A
%pa(APF,a1,0,1).
%pa(Beng,a1,0,1).
%pa(K1,a1,0,1).
%pa(K2,a1,0,1).
%pa(R1,a1,0,1).
%pa(R2,a1,0,1).
%pa(E,a1,0,1).
%
%pa(A201,a2,0,1).
%pa(A202,a2,0,1).
%pa(A203,a2,0,1).
%pa(A204,a2,0,1).
%pa(A205,a2,0,1).
%pa(A206,a2,0,1).
%pa(A207,a2,0,1).
%pa(A208,a2,0,1).
%pa(A209,a2,0,1).
%pa(b2,a2,0,1).
%pa(E,a2,0,1).
%
%% B
%
%pa(B101,b1,0,1).
%pa(B102,b1,0,1).
%pa(B103,b1,0,1).
%pa(B104,b1,0,1).
%pa(B105,b1,0,1).
%pa(B106,b1,0,1).
%pa(B106B,b1,0,1).
%pa(B107,b1,0,1).
%pa(E,b1,0,1).
%
%
%% C
%
%pa(C101,c0,0,1).
%pa(C102,c0,0,1).
%pa(C103,c0,0,1).
%pa(C104,c0,0,1).
%pa(C105,c0,0,1).
%pa(C106,c0,0,1).
%pa(C108,c0,0,1).
%pa(C110,c0,0,1).
%pa(E,c0,0,1).
%
%pa(D201,c1,0,1).
%pa(D202,c1,0,1).
%pa(D203,c1,0,1).
%pa(D204,c1,0,1).
%pa(D205,c1,0,1).
%pa(D206,c1,0,1).
%pa(D207,c1,0,1).
%pa(D209,c1,0,1).
%pa(E,c1,0,1).
%pa(d1,c1,0,1).
%
%
%
%% D
%
%pa(D101,d0,0,1).
%pa(D102,d0,0,1).
%pa(D103,d0,0,1).
%pa(D104,d0,0,1).
%pa(D105,d0,0,1).
%pa(D106,d0,0,1).
%pa(D108,d0,0,1).
%
%
%pa(D201,d1,0,1).
%pa(D202,d1,0,1).
%pa(D203,d1,0,1).
%pa(D204,d1,0,1).
%pa(D205,d1,0,1).
%pa(D206,d1,0,1).
%pa(E,d1,0,1).
%pa(c1,d1,0,1).


%===========================================================================================================
% Mapa de piso

:-dynamic ligacel/2.

%coluna :1,2,3,4,5,6,7,8
%linha 1:1,1,1,1,1,1,1,1
%linha 2:0,0,0,0,0,0,0,1
%linha 3:0,0,0,0,0,0,0,1
%linha 4:0,0,0,0,0,0,0,1
%linha 5:1,1,1,1,0,0,0,1
%linha 6:1,1,1,1,0,0,0,1
%linha 7:1,1,1,1,0,0,0,1
%
%
%
%m(col,lin,valor,Piso)
m(1,1,1,a1).
m(2,1,1,a1).
m(3,1,1,a1).
m(4,1,1,a1).
m(5,1,1,a1).
m(6,1,1,a1).
m(7,1,1,a1).
m(8,1,1,a1).

m(1,2,0,a1).
m(2,2,0,a1).
m(3,2,0,a1).
m(4,2,0,a1).
m(5,2,0,a1).
m(6,2,0,a1).
m(7,2,0,a1).
m(8,2,1,a1).

m(1,3,0,a1).
m(2,3,0,a1).
m(3,3,0,a1).
m(4,3,0,a1).
m(5,3,0,a1).
m(6,3,0,a1).
m(7,3,0,a1).
m(8,3,1,a1).

m(1,4,0,a1).
m(2,4,0,a1).
m(3,4,0,a1).
m(4,4,0,a1).
m(5,4,0,a1).
m(6,4,0,a1).
m(7,4,0,a1).
m(8,4,1,a1).

m(1,5,1,a1).
m(2,5,1,a1).
m(3,5,1,a1).
m(4,5,1,a1).
m(5,5,0,a1).
m(6,5,0,a1).
m(7,5,0,a1).
m(8,5,1,a1).

m(1,6,1,a1).
m(2,6,1,a1).
m(3,6,1,a1).
m(4,6,1,a1).
m(5,6,0,a1).
m(6,6,0,a1).
m(7,6,0,a1).
m(8,6,1,a1).

m(1,7,1,a1).
m(2,7,1,a1).
m(3,7,1,a1).
m(4,7,1,a1).
m(5,7,0,a1).
m(6,7,0,a1).
m(7,7,0,a1).
m(8,7,1,a1).

m(1,1,1,a2).
m(2,1,1,a2).
m(3,1,1,a2).
m(4,1,1,a2).
m(5,1,1,a2).
m(6,1,1,a2).
m(7,1,1,a2).
m(8,1,1,a2).

m(1,2,0,a2).
m(2,2,0,a2).
m(3,2,0,a2).
m(4,2,0,a2).
m(5,2,0,a2).
m(6,2,0,a2).
m(7,2,0,a2).
m(8,2,1,a2).

m(1,3,0,a2).
m(2,3,0,a2).
m(3,3,0,a2).
m(4,3,0,a2).
m(5,3,0,a2).
m(6,3,0,a2).
m(7,3,0,a2).
m(8,3,1,a2).

m(1,4,0,a2).
m(2,4,0,a2).
m(3,4,0,a2).
m(4,4,0,a2).
m(5,4,0,a2).
m(6,4,0,a2).
m(7,4,0,a2).
m(8,4,1,a2).

m(1,5,1,a2).
m(2,5,1,a2).
m(3,5,1,a2).
m(4,5,1,a2).
m(5,5,0,a2).
m(6,5,0,a2).
m(7,5,0,a2).
m(8,5,1,a2).

m(1,6,1,a2).
m(2,6,1,a2).
m(3,6,1,a2).
m(4,6,1,a2).
m(5,6,0,a2).
m(6,6,0,a2).
m(7,6,0,a2).
m(8,6,1,a2).

m(1,7,1,a2).
m(2,7,1,a2).
m(3,7,1,a2).
m(4,7,1,a2).
m(5,7,0,a2).
m(6,7,0,a2).
m(7,7,0,a2).
m(8,7,1,a2).

m(1,1,1,b2).
m(2,1,1,b2).
m(3,1,1,b2).
m(4,1,1,b2).
m(5,1,1,b2).
m(6,1,1,b2).
m(7,1,1,b2).
m(8,1,1,b2).

m(1,2,0,b2).
m(2,2,0,b2).
m(3,2,0,b2).
m(4,2,0,b2).
m(5,2,0,b2).
m(6,2,0,b2).
m(7,2,0,b2).
m(8,2,1,b2).

m(1,3,0,b2).
m(2,3,0,b2).
m(3,3,0,b2).
m(4,3,0,b2).
m(5,3,0,b2).
m(6,3,0,b2).
m(7,3,0,b2).
m(8,3,1,b2).

m(1,4,0,b2).
m(2,4,0,b2).
m(3,4,0,b2).
m(4,4,0,b2).
m(5,4,0,b2).
m(6,4,0,b2).
m(7,4,0,b2).
m(8,4,1,b2).

m(1,5,1,b2).
m(2,5,1,b2).
m(3,5,1,b2).
m(4,5,1,b2).
m(5,5,0,b2).
m(6,5,0,b2).
m(7,5,0,b2).
m(8,5,1,b2).

m(1,6,1,b2).
m(2,6,1,b2).
m(3,6,1,b2).
m(4,6,1,b2).
m(5,6,0,b2).
m(6,6,0,b2).
m(7,6,0,b2).
m(8,6,1,b2).

m(1,7,1,b2).
m(2,7,1,b2).
m(3,7,1,b2).
m(4,7,1,b2).
m(5,7,0,b2).
m(6,7,0,b2).
m(7,7,0,b2).
m(8,7,1,b2).

m(1,1,1,c2).
m(2,1,1,c2).
m(3,1,1,c2).
m(4,1,1,c2).
m(5,1,1,c2).
m(6,1,1,c2).
m(7,1,1,c2).
m(8,1,1,c2).

m(1,2,0,c2).
m(2,2,0,c2).
m(3,2,0,c2).
m(4,2,0,c2).
m(5,2,0,c2).
m(6,2,0,c2).
m(7,2,0,c2).
m(8,2,1,c2).

m(1,3,0,c2).
m(2,3,0,c2).
m(3,3,0,c2).
m(4,3,0,c2).
m(5,3,0,c2).
m(6,3,0,c2).
m(7,3,0,c2).
m(8,3,1,c2).

m(1,4,0,c2).
m(2,4,0,c2).
m(3,4,0,c2).
m(4,4,0,c2).
m(5,4,0,c2).
m(6,4,0,c2).
m(7,4,0,c2).
m(8,4,1,c2).

m(1,5,1,c2).
m(2,5,1,c2).
m(3,5,1,c2).
m(4,5,1,c2).
m(5,5,0,c2).
m(6,5,0,c2).
m(7,5,0,c2).
m(8,5,1,c2).

m(1,6,1,c2).
m(2,6,1,c2).
m(3,6,1,c2).
m(4,6,1,c2).
m(5,6,0,c2).
m(6,6,0,c2).
m(7,6,0,c2).
m(8,6,1,c2).

m(1,7,1,c2).
m(2,7,1,c2).
m(3,7,1,c2).
m(4,7,1,c2).
m(5,7,0,c2).
m(6,7,0,c2).
m(7,7,0,c2).
m(8,7,1,c2).

m(1,1,1,d2).
m(2,1,1,d2).
m(3,1,1,d2).
m(4,1,1,d2).
m(5,1,1,d2).
m(6,1,1,d2).
m(7,1,1,d2).
m(8,1,1,d2).

m(1,2,0,d2).
m(2,2,0,d2).
m(3,2,0,d2).
m(4,2,0,d2).
m(5,2,0,d2).
m(6,2,0,d2).
m(7,2,0,d2).
m(8,2,1,d2).

m(1,3,0,d2).
m(2,3,0,d2).
m(3,3,0,d2).
m(4,3,0,d2).
m(5,3,0,d2).
m(6,3,0,d2).
m(7,3,0,d2).
m(8,3,1,d2).

m(1,4,0,d2).
m(2,4,0,d2).
m(3,4,0,d2).
m(4,4,0,d2).
m(5,4,0,d2).
m(6,4,0,d2).
m(7,4,0,d2).
m(8,4,1,d2).

m(1,5,1,d2).
m(2,5,1,d2).
m(3,5,1,d2).
m(4,5,1,d2).
m(5,5,0,d2).
m(6,5,0,d2).
m(7,5,0,d2).
m(8,5,1,d2).

m(1,6,1,d2).
m(2,6,1,d2).
m(3,6,1,d2).
m(4,6,1,d2).
m(5,6,0,d2).
m(6,6,0,d2).
m(7,6,0,d2).
m(8,6,1,d2).

m(1,7,1,d2).
m(2,7,1,d2).
m(3,7,1,d2).
m(4,7,1,d2).
m(5,7,0,d2).
m(6,7,0,d2).
m(7,7,0,d2).
m(8,7,1,d2).



%===========================================================================================================
% Encontrar um caminho entre dois edificios

caminho_edificios(Start,End,Path):-
	caminho_edificios(Start,End,[Start],Path).
	

caminho_edificios(End,End,List,Path):- !,
	reverse(List,Path).

caminho_edificios(Current,End,List,Path):-
	(liga(Current,Next);liga(Next,Current)),
	\+ member(Next,List),
	caminho_edificios(Next,End,[Next|List],Path).

%===========================================================================================================
% Encontrar um caminho entre pisos de edificios usando corredores e elevadores

caminho_pisos(PisoOr,PisoDest,LEdCam,LLig,LPsCam):-
	pisos(EdOr,LPisosOr),
	member(PisoOr,LPisosOr),
	pisos(EdDest,LPisosDest),
	member(PisoDest,LPisosDest),
	caminho_edificios(EdOr,EdDest,LEdCam),!,
	segue_pisos(PisoOr,PisoDest,LEdCam,LLig,LpsCamAux),
	append([PisoOr],LpsCamAux,LPsCam).
	
segue_pisos(PisoDest,PisoDest,_,[],[]).

segue_pisos(PisoDest1,PisoDest,[EdDest],[elev(PisoDest1,PisoDest)],[PisoDest|ListaPisos]):-
	PisoDest\==PisoDest1,
	elevador(EdDest,LPisos),
	member(PisoDest1,LPisos),
	member(PisoDest,LPisos).
	
segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[cor(PisoAct,PisoSeg)|LOutrasLig],[PisoSeg|ListaPisos]):-
	(corredor(EdAct,EdSeg,PisoAct,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct)),
	segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig, ListaPisos).
	
segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[elev(PisoAct,PisoAct1),cor(PisoAct1,PisoSeg)|LOutrasLig],[PisoAct1, PisoSeg | ListaPisos]):-
	(corredor(EdAct,EdSeg,PisoAct1,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct1)),
	PisoAct1\==PisoAct,
	elevador(EdAct,LPisos),
	member(PisoAct,LPisos),
	member(PisoAct1,LPisos),
	segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig,ListaPisos).
	
%===========================================================================================================	
% Escolher o caminho que envolve menos utilizacoes de elevadores e em caso de iguadade menos utilizacao de corredores

melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor,LPsCam):-
	findall(LLig,caminho_pisos(PisoOr,PisoDest,_,LLig,LPsCam),LLLig),
	menos_elevadores(LLLig,LLigMelhor,_,_),
	extract_floor_sequence(LLigMelhor,LPsCam).

menos_elevadores([LLig],LLig,NElev,NCor):-
	conta(LLig,NElev,NCor).

menos_elevadores([LLig|OutrosLLig],LLigR,NElevR,NCorR):-
	menos_elevadores(OutrosLLig,LLigM,NElev,NCor),
	conta(LLig,NElev1,NCor1),
	(((NElev1<NElev;(NElev1==NElev,NCor1<NCor)),!,
		NElevR is NElev1, NCorR is NCor1,LLigR=LLig);
		(NElevR is NElev,NCorR is NCor,LLigR=LLigM)).

conta([],0,0).

conta([elev(_,_)|L],NElev,NCor):- conta(L,NElevL,NCor),NElev is NElevL+1.
conta([cor(_,_)|L],NElev,NCor):-conta(L,NElev,NCorL),NCor is NCorL+1.

%===========================================================================================================
% 

extract_floor_sequence([], []).
extract_floor_sequence([elev(From, To) | Rest], [From, To | Result]) :-
    extract_floor_sequence(Rest, Result).
extract_floor_sequence([cor(_, To) | Rest], [To | Result]) :-
    extract_floor_sequence(Rest, Result).

%===========================================================================================================
% Cria o grafo de ligacoes entre celulas (celulas com 0 estão ligadas a celulas com 0) == cria_grafo(8,7).
% m(col, lin, valor, piso) => m(0, 0, 0, A1)...
% node(Id, Col, Lin, Valor, Piso)...
% edge(Id1, Id2, Custo, Piso)...

cria_grafo(_,0,_):-!.
cria_grafo(Col,Lin,Piso):-
  cria_grafo_lin(Col,Lin,Piso),
  Lin1 is Lin-1,
  cria_grafo(Col,Lin1,Piso).


cria_grafo_lin(0,_,_):-!.

cria_grafo_lin(Col,Lin,Piso):-
  ((corr_pos(_, Col, Lin, Piso),
  node(Id1, Col, Lin, _, Piso))
  ;
  (elev_pos(_, Col, Lin, Piso),
  node(Id1, Col, Lin, _, Piso))
  ;
  (ponto_acesso(_, Col, Lin, Piso),
  node(Id1, Col, Lin, _, Piso))
  ;
  node(Id1,Col,Lin,0,Piso)),
  !,
  ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
  ((node(Id2,ColS,Lin,0,Piso), assertz(edge(Id1, Id2, 1, Piso));true)), % Verifca à direita.
  ((node(Id3,ColA,Lin,0,Piso), assertz(edge(Id1, Id3, 1, Piso));true)), % Verifca à esquerda.
  ((node(Id4,Col,LinS,0,Piso), assertz(edge(Id1, Id4, 1, Piso));true)), % Verifica abaixo.
  ((node(Id5,Col,LinA,0,Piso), assertz(edge(Id1, Id5, 1, Piso));true)), % Verifica acima.
  C is sqrt(2),
  ((node(Id6,ColS,LinA,0,Piso), assertz(edge(Id1, Id6, C, Piso));true)), % Verifica diagonal superior direita.
  ((node(Id7,ColA,LinA,0,Piso), assertz(edge(Id1, Id7, C, Piso));true)), % Verifica diagonal superior esquerda.
  ((node(Id8,ColS,LinS,0,Piso), assertz(edge(Id1, Id8, C, Piso));true)), % Verifica diagonal inferior direita.
  ((node(Id9,ColA,LinS,0,Piso), assertz(edge(Id1, Id9, C, Piso));true)), % Verifica diagonal inferior esquerda.
  
  Col1 is Col-1,
  cria_grafo_lin(Col1,Lin,Piso),!.

cria_grafo_lin(Col,Lin,Piso):-
  Col1 is Col-1,cria_grafo_lin(Col1,Lin,Piso).

%===========================================================================================================
% A-star.

aStar(Orig,Dest,Cam,Custo,Piso):-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo,Piso).

% Se for preciso apenas o melhor caminho, colocar cut a seguir ao reverse.
aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo,Piso):-
	reverse([Dest|T],Cam),!.

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo,Piso):-
	LA=[Act|_],
	findall((CEX,CaX,[X|LA]),
		(Dest\==Act,edge(Act,X,CustoX,Piso),\+ member(X,LA),
		CaX is CustoX + Ca, estimativa(X,Dest,EstX,Piso),
		CEX is CaX +EstX),Novos),
	append(Outros,Novos,Todos),
	sort(Todos,TodosOrd),
	aStar2(Dest,TodosOrd,Cam,Custo,Piso).

% substituir a chamada edge(Act,X,CustoX)
% por (edge(Act,X,CustoX);edge(X,Act,CustoX))
% se quiser ligacoes bidirecionais

estimativa(Nodo1,Nodo2,Estimativa,Piso):-
	node(Nodo1,X1,Y1,_,Piso),
	node(Nodo2,X2,Y2,_,Piso),
	Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).
%===========================================================================================================
% caminho_completo


	
%===========================================================================================================
% RobotPath 

robot_path(P1,P2, LLigMelhor, LPisMelhor, List) :-
	ponto_acesso(P1,PisoOr,Orig),
	ponto_acesso(P2,PisoDest,Dest),
	melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor, LPisMelhor),
	caminho_completo(P1,P2, LLigMelhor, LPisMelhor, List).


