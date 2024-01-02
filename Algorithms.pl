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
ponto_acesso(a102,a1,cel(4,4)).

ponto_acesso(elev(a1, a2),a1,cel(6,6)).

ponto_acesso(elev(a1, a2),a2,cel(6,6)).
ponto_acesso(cor(a2, b2),a2,cel(1,4)).

ponto_acesso(cor(a2, b2),b2,cel(8,4)).
ponto_acesso(cor(b2, c2),b2,cel(7,7)).

ponto_acesso(cor(b2, c2),c2,cel(7,1)).
ponto_acesso(cor(c2, d2),c2,cel(7,7)).

ponto_acesso(cor(c2, d2),d2,cel(7,1)).
ponto_acesso(d201,d2,cel(1,4)).

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


:-dynamic node/4. % node(Id, Col, Lin, Valor, Piso)...
:-dynamic edge/4. % edge(Id1, Id2, Custo, Piso)...
:-dynamic cel/2. % cel(col, lin)...
:-dynamic ponto_acesso/3. % ponto_acesso(ponto, piso, cel(col, lin))...
:-dynamic lens/3. % lens(piso, col, lin)...



%coluna :1,2,3,4,5,6,7,8
%linha 1:1,1,1,1,1,1,1,1
%linha 2:0,0,0,0,0,0,0,1
%linha 3:0,0,0,0,0,0,0,1
%linha 4:0,0,0,0,0,0,0,1
%linha 5:1,1,1,1,0,0,0,1
%linha 6:1,1,1,1,0,0,0,1
%linha 7:1,1,1,1,0,0,0,1
%

%m(col,lin,valor,Piso)
node(1,1,1,a1).
node(2,1,1,a1).
node(3,1,1,a1).
node(4,1,1,a1).
node(5,1,1,a1).
node(6,1,1,a1).
node(7,1,1,a1).
node(8,1,1,a1).

node(1,2,0,a1).
node(2,2,0,a1).
node(3,2,0,a1).
node(4,2,0,a1).
node(5,2,0,a1).
node(6,2,0,a1).
node(7,2,0,a1).
node(8,2,1,a1).

node(1,3,0,a1).
node(2,3,0,a1).
node(3,3,1,a1).
node(4,3,0,a1).
node(5,3,0,a1).
node(6,3,0,a1).
node(7,3,0,a1).
node(8,3,1,a1).

node(1,4,0,a1).
node(2,4,0,a1).
node(3,4,0,a1).
node(4,4,0,a1).
node(5,4,0,a1).
node(6,4,0,a1).
node(7,4,0,a1).
node(8,4,1,a1).

node(1,5,1,a1).
node(2,5,1,a1).
node(3,5,1,a1).
node(4,5,1,a1).
node(5,5,0,a1).
node(6,5,0,a1).
node(7,5,0,a1).
node(8,5,1,a1).

node(1,6,1,a1).
node(2,6,1,a1).
node(3,6,1,a1).
node(4,6,1,a1).
node(5,6,0,a1).
node(6,6,0,a1).
node(7,6,0,a1).
node(8,6,1,a1).

node(1,7,1,a1).
node(2,7,1,a1).
node(3,7,1,a1).
node(4,7,1,a1).
node(5,7,1,a1).
node(6,7,1,a1).
node(7,7,1,a1).
node(8,7,1,a1).

node(1,1,1,a2).
node(2,1,1,a2).
node(3,1,1,a2).
node(4,1,1,a2).
node(5,1,1,a2).
node(6,1,1,a2).
node(7,1,1,a2).
node(8,1,1,a2).

node(1,2,0,a2).
node(2,2,0,a2).
node(3,2,0,a2).
node(4,2,0,a2).
node(5,2,0,a2).
node(6,2,0,a2).
node(7,2,0,a2).
node(8,2,1,a2).

node(1,3,0,a2).
node(2,3,0,a2).
node(3,3,0,a2).
node(4,3,0,a2).
node(5,3,0,a2).
node(6,3,0,a2).
node(7,3,0,a2).
node(8,3,1,a2).

node(1,4,0,a2).
node(2,4,0,a2).
node(3,4,0,a2).
node(4,4,0,a2).
node(5,4,0,a2).
node(6,4,0,a2).
node(7,4,0,a2).
node(8,4,1,a2).

node(1,5,1,a2).
node(2,5,0,a2).
node(3,5,0,a2).
node(4,5,0,a2).
node(5,5,0,a2).
node(6,5,0,a2).
node(7,5,0,a2).
node(8,5,1,a2).

node(1,6,1,a2).
node(2,6,0,a2).
node(3,6,0,a2).
node(4,6,0,a2).
node(5,6,0,a2).
node(6,6,0,a2).
node(7,6,0,a2).
node(8,6,1,a2).

node(1,7,1,a2).
node(2,7,1,a2).
node(3,7,1,a2).
node(4,7,1,a2).
node(5,7,1,a2).
node(6,7,1,a2).
node(7,7,1,a2).
node(8,7,1,a2).

node(1,1,1,b2).
node(2,1,1,b2).
node(3,1,1,b2).
node(4,1,1,b2).
node(5,1,1,b2).
node(6,1,1,b2).
node(7,1,1,b2).
node(8,1,1,b2).

node(1,2,1,b2).
node(2,2,0,b2).
node(3,2,0,b2).
node(4,2,0,b2).
node(5,2,0,b2).
node(6,2,0,b2).
node(7,2,0,b2).
node(8,2,0,b2).

node(1,3,1,b2).
node(2,3,0,b2).
node(3,3,0,b2).
node(4,3,0,b2).
node(5,3,0,b2).
node(6,3,0,b2).
node(7,3,0,b2).
node(8,3,0,b2).

node(1,4,1,b2).
node(2,4,0,b2).
node(3,4,0,b2).
node(4,4,0,b2).
node(5,4,0,b2).
node(6,4,0,b2).
node(7,4,0,b2).
node(8,4,0,b2).

node(1,5,1,b2).
node(2,5,1,b2).
node(3,5,1,b2).
node(4,5,1,b2).
node(5,5,0,b2).
node(6,5,0,b2).
node(7,5,0,b2).
node(8,5,1,b2).

node(1,6,1,b2).
node(2,6,1,b2).
node(3,6,1,b2).
node(4,6,1,b2).
node(5,6,0,b2).
node(6,6,0,b2).
node(7,6,0,b2).
node(8,6,1,b2).

node(1,7,1,b2).
node(2,7,1,b2).
node(3,7,1,b2).
node(4,7,1,b2).
node(5,7,0,b2).
node(6,7,0,b2).
node(7,7,0,b2).
node(8,7,1,b2).

node(1,1,1,c2).
node(2,1,1,c2).
node(3,1,1,c2).
node(4,1,1,c2).
node(5,1,0,c2).
node(6,1,0,c2).
node(7,1,0,c2).
node(8,1,1,c2).

node(1,2,0,c2).
node(2,2,0,c2).
node(3,2,0,c2).
node(4,2,0,c2).
node(5,2,0,c2).
node(6,2,0,c2).
node(7,2,0,c2).
node(8,2,1,c2).

node(1,3,0,c2).
node(2,3,0,c2).
node(3,3,0,c2).
node(4,3,0,c2).
node(5,3,0,c2).
node(6,3,0,c2).
node(7,3,0,c2).
node(8,3,1,c2).

node(1,4,0,c2).
node(2,4,0,c2).
node(3,4,0,c2).
node(4,4,0,c2).
node(5,4,0,c2).
node(6,4,0,c2).
node(7,4,0,c2).
node(8,4,1,c2).

node(1,5,1,c2).
node(2,5,1,c2).
node(3,5,1,c2).
node(4,5,1,c2).
node(5,5,0,c2).
node(6,5,0,c2).
node(7,5,0,c2).
node(8,5,1,c2).

node(1,6,1,c2).
node(2,6,1,c2).
node(3,6,1,c2).
node(4,6,1,c2).
node(5,6,0,c2).
node(6,6,0,c2).
node(7,6,0,c2).
node(8,6,1,c2).

node(1,7,1,c2).
node(2,7,1,c2).
node(3,7,1,c2).
node(4,7,1,c2).
node(5,7,0,c2).
node(6,7,0,c2).
node(7,7,0,c2).
node(8,7,1,c2).

node(1,1,1,d2).
node(2,1,1,d2).
node(3,1,1,d2).
node(4,1,1,d2).
node(5,1,0,d2).
node(6,1,0,d2).
node(7,1,0,d2).
node(8,1,1,d2).

node(1,2,0,d2).
node(2,2,0,d2).
node(3,2,0,d2).
node(4,2,0,d2).
node(5,2,1,d2).
node(6,2,0,d2).
node(7,2,0,d2).
node(8,2,1,d2).

node(1,3,1,d2).
node(2,3,1,d2).
node(3,3,1,d2).
node(4,3,1,d2).
node(5,3,0,d2).
node(6,3,0,d2).
node(7,3,0,d2).
node(8,3,1,d2).

node(1,4,0,d2).
node(2,4,0,d2).
node(3,4,0,d2).
node(4,4,0,d2).
node(5,4,0,d2).
node(6,4,0,d2).
node(7,4,0,d2).
node(8,4,1,d2).

node(1,5,1,d2).
node(2,5,1,d2).
node(3,5,1,d2).
node(4,5,1,d2).
node(5,5,0,d2).
node(6,5,0,d2).
node(7,5,0,d2).
node(8,5,1,d2).

node(1,6,1,d2).
node(2,6,1,d2).
node(3,6,1,d2).
node(4,6,1,d2).
node(5,6,0,d2).
node(6,6,0,d2).
node(7,6,0,d2).
node(8,6,1,d2).

node(1,7,1,d2).
node(2,7,1,d2).
node(3,7,1,d2).
node(4,7,1,d2).
node(5,7,0,d2).
node(6,7,0,d2).
node(7,7,0,d2).
node(8,7,1,d2).






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
% Extrair a sequencia de pisos de um caminho

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
  ((ponto_acesso(_,Piso,cel(Col, Lin)),
  node(Col, Lin, _, Piso))
  ;
  node(Col,Lin,0,Piso)),
  !,
  ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
  ((node(ColS,LinS,0,Piso),node(Col,LinS,0,Piso),node(ColS,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColS,LinS) ,1, Piso));true)), % Verifca à direita.
  ((node(ColA,LinS,0,Piso),node(Col,LinA,0,Piso),node(ColA,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColA,LinS) , 1, Piso));true)), % Verifca à esquerda.
  ((node(ColS,LinA,0,Piso),node(Col,LinS,0,Piso),node(ColS,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColS,LinA) , 1, Piso));true)), % Verifica abaixo.
  ((node(ColA,LinA,0,Piso),node(Col,LinA,0,Piso),node(ColA,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColS,LinS) , 1, Piso));true)), % Verifica acima.
  C is sqrt(2),
  ((node(ColS,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColS,Lin) , C, Piso));true)), % Verifica diagonal superior direita.
  ((node(ColA,Lin,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,ColA,Lin) , C, Piso));true)), % Verifica diagonal superior esquerda.
  ((node(Col,LinS,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,Col,LinS) , C, Piso));true)), % Verifica diagonal inferior direita.
  ((node(Col,LinA,0,Piso), assertz(edge(lens(Piso,Col,Lin),lens(Piso,Col,LinA) , C, Piso));true)), % Verifica diagonal inferior esquerda.
  Col1 is Col-1,
  cria_grafo_lin(Col1,Lin,Piso),!.

cria_grafo_lin(Col,Lin,Piso):-
  Col1 is Col-1,cria_grafo_lin(Col1,Lin,Piso).

%===========================================================================================================
% A-star.

% - aStar(Orig,Dest,Cam,Custo,Piso)
aStar(Orig,Dest,Cam,Custo,Piso):-
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo,Piso).

% Se for preciso apenas o melhor caminho, colocar cut a seguir ao reverse.
aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo,Piso):-
	reverse([Dest|T],Cam),!.

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo,Piso):-
	LA=[Act|_],
	findall((CEX,CaX,[XH|LA]),
		(Dest\==Act,
		Act = cel(XX, YY),
		ActH = lens(Piso, XX, YY),

		X = lens(XXX, YYY, ZZZ),
		XH = cel(YYY, ZZZ),

		(edge(ActH,X,CustoX,Piso);edge(X,ActH,CustoX,Piso)),\+ member(X,LA),
		CaX is CustoX + Ca,
		estimativa(XH,Dest,EstX,Piso),
		CEX is CaX +EstX),Novos),
	append(Outros,Novos,Todos),
	sort(Todos,TodosOrd),
	aStar2(Dest,TodosOrd,Cam,Custo,Piso).

% substituir a chamada edge(Act,X,CustoX)
% por (edge(Act,X,CustoX);edge(X,Act,CustoX))
% se quiser ligacoes bidirecionais

estimativa(cel(X1,Y1),cel(X2,Y2),Estimativa,Piso):-
	Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).
%===========================================================================================================
% caminho_completo

is_elevador(Point) :-
    ponto_acesso(Point, _, ElevadorType),
    ElevadorType = elev(_, _).


caminho_completo(Point, Point, [], [Points]).

caminho_completo(Point, Point2, [],[Path1]):-
	ponto_acesso(Point,PisoOr,Orig),
	ponto_acesso(Point2,PisoDest,Dest),
	 (PisoOr = PisoDest ->
			write('==========================='), nl,
			write('Point1: '), write(Point), nl,
			write('PisoOr: '), write(PisoOr), nl,
			write('Orig: '), write(Orig), nl,
			write('PointX: '), write(Point2), nl,
			write('PisoDest: '), write(PisoDest), nl,
			write('Dest: '), write(Dest), nl,
			write('Same floor!'), nl,
			write('==========================='), nl,
            aStar(Orig, Dest, Path1, Custo, PisoOr)
			%Path1 = [a], Custo = 0,
            %caminho_completo(Point2, Point2, [], Path1)
        ;
			(ponto_acesso(Point,PisoDest,New)->
			write('==========================='), nl,
			write('Point1: '), write(Point), nl,
			write('PisoOr: '), write(PisoDest), nl,
			write('Orig: '), write(New), nl,
			write('PointX: '), write(Point2), nl,
			write('PisoDest: '), write(PisoDest), nl,
			write('Dest: '), write(Dest), nl,
			write('Same floor!'), nl,
			write('==========================='), nl,
			%Path1 = [b], Custo = 1,
			aStar(New, Dest, Path1, Custo, PisoDest)
            %caminho_completo(Point2, Point2, [], Path1)
			);
            Path1 = [C], Custo = 1
			%caminho_completo(Point2, Point2, [], Path1)
        ).


caminho_completo(Point1, Point2, [PointX|T], [Path1|RestPath]) :-
	ponto_acesso(Point1,PisoOr,Orig),
	ponto_acesso(PointX,PisoDest,Dest),
	(is_elevador(Point1), is_elevador(PointX) ->
		caminho_completo(PointX, Point2, T, RestPath)	
    ;
        (PisoOr = PisoDest ->
			write('==========================='), nl,
			write('Point1: '), write(Point1), nl,
			write('PisoOr: '), write(PisoOr), nl,
			write('Orig: '), write(Orig), nl,
			write('PointX: '), write(PointX), nl,
			write('PisoDest: '), write(PisoDest), nl,
			write('Dest: '), write(Dest), nl,
			write('Same floor!'), nl,
			write('==========================='), nl,
            aStar(Orig, Dest, Path1, Custo, PisoOr),
			%Path1 = [a], Custo = 0,
            caminho_completo(PointX, Point2, T, RestPath)
        ;
			(ponto_acesso(Point1,PisoDest,New)->
			write('==========================='), nl,
			write('Point1: '), write(Point1), nl,
			write('PisoOr: '), write(PisoDest), nl,
			write('Orig: '), write(New), nl,
			write('PointX: '), write(PointX), nl,
			write('PisoDest: '), write(PisoDest), nl,
			write('Dest: '), write(Dest), nl,
			write('Same floor!'), nl,
			write('==========================='), nl,
			%Path1 = [b], Custo = 1,
			aStar(New, Dest, Path1, Custo, PisoDest),
            caminho_completo(PointX, Point2, T, RestPath)
			);
            Path1 = [C], Custo = 1,
			caminho_completo(PointX, Point2, T, RestPath)
        )
    ).
	
%===========================================================================================================
% RobotPath -> caminho entre dois pontos de acesso

robot_path(P1,P2, LLigMelhor, LPisMelhor, List) :-
	ponto_acesso(P1,PisoOr,Orig),
	ponto_acesso(P2,PisoDest,Dest),
	melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor, LPisMelhor),
	caminho_completo(P1,P2, LLigMelhor, List).

%===========================================================================================================
% Predicate to print all edges
print_all_edges(Graph) :-
    edge(Start, End, Weight, Graph),
    format('Edge: ~w -> ~w (Weight: ~w)~n', [Start, End, Weight]),
    fail. % This predicate will fail intentionally to stop backtracking after printing all edges.


%===========================================================================================================
% Algoritmo genetico

:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.


tarefa(t1,2).	% tarefa(Id,TempoProcessamento).
tarefa(t2,4).
tarefa(t3,1).
tarefa(t4,3).
tarefa(t5,3).

entre_tarefas(t1,t2,3).	% tarefa(Id, IdTarefa1, IdTarefa2, TempoProcessamento).
entre_tarefas(t1,t3,1).
entre_tarefas(t1,t4,6).
entre_tarefas(t1,t5,4).

entre_tarefas(t2,t1,1).
entre_tarefas(t2,t3,6).
entre_tarefas(t2,t4,9).
entre_tarefas(t2,t5,3).

entre_tarefas(t3,t1,2).
entre_tarefas(t3,t2,5).
entre_tarefas(t3,t4,1).
entre_tarefas(t3,t5,5).

entre_tarefas(t4,t1,3).
entre_tarefas(t4,t2,2).
entre_tarefas(t4,t3,1).
entre_tarefas(t4,t5,7).

entre_tarefas(t5,t1,2).
entre_tarefas(t5,t2,5).
entre_tarefas(t5,t3,1).
entre_tarefas(t5,t4,8).

estabilizacao_solucao(75).

% tarefas(NTarefas).
tarefas(5).

% parameteriza��o
inicializa:-
	%write('Numero de novas Geracoes: '),read(NG), 
	
	NG is 100,
	(retract(geracoes(_));true), asserta(geracoes(NG)),

	%write('Dimensao da Populacao: '),read(DP), 
	DP is 5,
	(retract(populacao(_));true), asserta(populacao(DP)),

	%write('Probabilidade de Cruzamento (%):'), 
	%read(P1), PC is P1/100,  
	PC is 70/100,
	(retract(prob_cruzamento(_));true),asserta(prob_cruzamento(PC)),

	%write('Probabilidade de Mutacao (%):'), 
	%read(P2), PM is P2/100, 
	PM is 2/100,
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).


gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	%trace,
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	PopOrd = [FS|_],
	gera_geracao(0,NG,PopOrd,[FS,FS,0]).

gera_populacao(Pop):-
	populacao(TamPop),
	tarefas(NumT),
	findall(Tarefa,tarefa(Tarefa,_),ListaTarefas),
	gera_populacao(TamPop,ListaTarefas,NumT,Pop).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
	gera_individuo(ListaTarefas,NumT,Ind),
	not(member(Ind,Resto)).
gera_populacao(TamPop,ListaTarefas,NumT,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,L).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	Seq = [T|Resto],
	avalia(Resto,T,V).

avalia([],_,0).
avalia([T|Resto],Anterior,Total):-
	avalia(Resto,T,Total1),
	entre_tarefas(Anterior, T, Dur),
	Total is Total1+Dur.

ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).

% Numero de geracoes maximo atingido.
gera_geracao(G,G,Pop,_):-!,
	write('Gera��o '), write(G), write(':'), nl, write(Pop), nl,
	write('Numero de geracoes maximo atingido!').

% Numero de estabilizacao maximo atingido.
gera_geracao(N,_,Pop,[_,_,X]):-
	estabilizacao_solucao(X),
	!,
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	%Pop = [Lista*Tempo|_],
	%(retractall(bto(_,_)),!;true),
	%asserta(bto(Lista, Tempo)).
	write('Estabilizacao maxima atingida!').
	% asserta(melhor_individuo()).

gera_geracao(N,G,Pop,[SolAct,SolAct,X]):-
	X2 is X+1,
	gera_geracao2(N,G,Pop,[SolAct,SolAct,X2]).

gera_geracao(N,G,Pop,[SolAnt,SolAct,_]):-
	gera_geracao2(N,G,Pop,[SolAnt,SolAct,1]).

gera_geracao2(N, G, Pop, [_, SolAct, X]):-
	%trace,
	%write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	cruzamento(Pop,NPop1,1),
	NPop1 = [NSol|_],
	mutacao(NPop1,NPop,1),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	N1 is N+1,
	gera_geracao(N1,G,NPopOrd,[SolAct, NSol, X]).

gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


% Passar sempre o melhor indivíduo + fazer cruzamentos aleatórios e nao sucessivos (1º com 2º, 3º com 4º...)
cruzamento([],[],_).

%cruzamento([Ind*_],[Ind],_).

% Predicado intermédio que vai garantir que:
%  1 - O melhor indíviduo é passado para a próxima geracao;
%  2 - Nao sao cruzados pares consecutivos (1º e 2º, 3º e 4º...).
% Como?
%  1 - Se se tratar do primeiro índice, ou seja, indivíduo com melhor prestacao, vai ser diretamente passado para a próxima geracao;
%  2 - É feito um shuffle à lista (excepto o melhor indivíduo).
cruzamento([Ind1*_|Resto], [Ind1|Resto1], 1):-
	!,
	random_permutation(Resto, RestoBaralhado),
	cruzamento(RestoBaralhado, Resto1, 2).

cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1],Index):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!,
        cruzar(Ind1,Ind2,P1,P2,NInd1),
	  cruzar(Ind2,Ind1,P1,P2,NInd2))
	;
	(NInd1=Ind1,NInd2=Ind2)),
	Index2 is Index+1,
	cruzamento(Resto,Resto1,Index2).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tarefas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tarefas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tarefas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[],_).

mutacao([Ind1|Rest],[Ind1|Rest1], 1):-
	!,
	mutacao(Rest, Rest1, 2).

mutacao([Ind|Rest],[NInd|Rest1], V):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	V2 is V+1,
	mutacao(Rest,Rest1,V2).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).
