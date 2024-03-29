# US 1430 - seja gerado um plano de atendimento das tarefas indicadas que indique uma sequência de execução das tarefas que seja criada através de um Algoritmo Genético

## 1. Context

* First time that this task is developed.
* This task comes in the context of Sprint C.

## 2. Requirements

**Dependencies:**
- **No dependencies**

## 3. Analysis
**Analyzing this User Story we understand that:**

* Using a genetic algorithm, it will be generated a sequence of execution of tasks, considering the time it takes to go from the end point of a task to the start point of the next.

* The base code of the algorithm is provided and is intended to adapt it to how solution, as well as optimize it in the following fields:
    - Ensure that the best individual of the current population goes to the next population generated;
    - Avoid that the crossing sequence always happens between the first and second elements of the population, next between third and fourth, and so on.
    - Apply a selection method not purely elitist.
    - Apply a different termination condition. In our case, it will be used the number of generations and the solution stabilization.



## 4. Implementation

```prolog
:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic estabilizacao_solucao/1.
:-dynamic melhor_sequencia/1.

% parameterizacao
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

	% Estabilizacao da solucao
	estabilizacao_solucao(75).

gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
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
	write('Geracao '), write(G), write(':'), nl, write(Pop), nl,
	Pop = [Lista*Tempo|_],
	(retractall(melhor_sequencia(_,_)),!;true),
	asserta(melhor_sequencia(Lista, Tempo)),
	write('Numero maximo de geracoes atingido!').

% Numero de estabilizacao maximo atingido.
gera_geracao(N,_,Pop,[_,_,X]):-
	estabilizacao_solucao(X),
	!,
	write('Geracao '), write(N), write(':'), nl, write(Pop), nl,
	Pop = [Lista*Tempo|_],
	(retractall(melhor_sequencia(_,_)),!;true),
	asserta(melhor_sequencia(Lista, Tempo)).
	write('Estabilizacao maxima atingida!').

gera_geracao(N,G,Pop,[SolAct,SolAct,X]):-
	X2 is X+1,
	gera_geracao2(N,G,Pop,[SolAct,SolAct,X2]).

gera_geracao(N,G,Pop,[SolAnt,SolAct,_]):-
	gera_geracao2(N,G,Pop,[SolAnt,SolAct,1]).

gera_geracao2(N, G, Pop, [_, SolAct, X]):-
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

cruzamento([],[],_).

%cruzamento([Ind*_],[Ind],_).
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
```

## 5. Observations
No observations.