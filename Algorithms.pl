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
%m(col,lin,valor)
m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).

m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).

m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).

m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).

m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).

m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).

m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1).


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
	menos_elevadores(LLLig,LLigMelhor,_,_).

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
% Cria o grafo de ligacoes entre celulas (celulas com 0 estão ligadas a celulas com 0) == cria_grafo(8,7).

cria_grafo(_,0):-!.
cria_grafo(Col,Lin):-cria_grafo_lin(Col,Lin),Lin1 is Lin-1,cria_grafo(Col,Lin1).


cria_grafo_lin(0,_):-!.
cria_grafo_lin(Col,Lin):-m(Col,Lin,0),!,ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
    ((m(ColS,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
    ((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
    ((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
    ((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
    Col1 is Col-1,
    cria_grafo_lin(Col1,Lin).
cria_grafo_lin(Col,Lin):-Col1 is Col-1,cria_grafo_lin(Col1,Lin).

%===========================================================================================================
% Deep First Search (DFS) == dfs(cel(1,3),cel(6,7),L).

dfs(Orig,Dest,Cam):-
	dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam):-
	reverse(LA,Cam).

dfs2(Act,Dest,LA,Cam):-
	ligacel(Act,X),
        \+ member(X,LA),
	dfs2(X,Dest,[X|LA],Cam).

%===========================================================================================================
% All Deep First Search (DFS) == all_dfs(cel(1,3),cel(6,7),L).

all_dfs(Orig,Dest,LCam):-
	findall(Cam,dfs(Orig,Dest,Cam),LCam).

%===========================================================================================================
% Best Deep First Search (DFS) == better_dfs(cel(1,3),cel(6,7),L).

better_dfs(Orig,Dest,Cam):-
	all_dfs(Orig,Dest,LCam),
	shortlist(LCam,Cam,_).


shortlist([L],L,N):-!,length(L,N).

shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1),
				length(L,NL),
			((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).


%===========================================================================================================
% Breadth First Search (BFS) == bfs(cel(1,3),cel(6,7),L).

bfs(Orig,Dest,Cam):-bfs2(Dest,[[Orig]],Cam).

bfs2(Dest,[[Dest|T]|_],Cam):-
	reverse([Dest|T],Cam).

bfs2(Dest,[LA|Outros],Cam):-
	LA=[Act|_],
	findall([X|LA],
		(Dest\==Act,ligacel(Act,X),\+ member(X,LA)),
		Novos),
	append(Outros,Novos,Todos),
	bfs2(Dest,Todos,Cam).


%===========================================================================================================
% RobotPath 

%robotPath(Orig, PisoOr, Dest, PisoDest, Number, Cam) :-
%    (Number = 1 ->
%        caminho_pisos(PisoOr,PisoDest,LEdCam,LLig),
%		
%
%    ; 
%        melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor),
%
%
%    ).
	

