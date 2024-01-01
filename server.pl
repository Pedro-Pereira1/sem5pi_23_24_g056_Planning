% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)). 
:-set_setting(http:cors, [*]).
:-set_prolog_flag(answer_write_options,[max_depth(0)]).
:-set_prolog_flag(report_error,true).
:-set_prolog_flag(unknown,error). 


% Dynamic Predicates
:-dynamic building/1. % building(A)...
:-dynamic pisos/2. % pisos(A, [1,2,3])...
:-dynamic corredor/4. % passageway(A, B, 2, 2)...
:-dynamic elevador/2. % elevator(A, [1,2,3])...
:-dynamic room/3. % room(Name, A, 1)...
:-dynamic robot/3. % robot(Code, Type, OperationStatus(true or false))...
:-dynamic robotType/3. % robotType(TypeID, AvailableTasks[])...
:-dynamic elev/2. % elev(a1, a2), elev(a2, a3)...
:-dynamic cor/2. % cor(a1, b1), cor(a2, b2)...
:-dynamic liga/2. % ligacel(building1, building2)...
:-dynamic cel/2. % cel(x, y) => cel(1, 3)...
:-dynamic node/4. % node(Id, Col, Lin, Valor, Piso)...
:-dynamic edge/4. % edge(Id1, Id2, Custo, Piso)...
:-dynamic ponto_acesso/3. % ponto_acesso(Ponto, Floor, Cel)...
:-dynamic lens/3. % lens(piso, col, lin)...

% Criacao de servidor HTTP no porto 'Port'					
% Gerir servidor
startServer(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stopServer:-
    retract(port(Port)),
    http_stop_server(Port,_).

:- http_handler('/shortestPath', get_path, []).

get_path(Request):-
    cors_enable(Request, [methods([get])]),
    %get_data(),
    http_parameters(Request, [origem(Origem, []), destino(Destino, [])]),
    atom_string(Origem, Or),
    atom_string(Destino, Dest),
    robot_path(Or, Dest, LLigMelhor, LPisMelhor, CamF),

    cells_lists_to_json_object(CamF, LPisMelhor, LLigMelhor, JsonObject),

    reply_json(JsonObject, [json_object(dict)]),
    !.

% Converte a estrutura cel para uma lista
cel_to_list(cel(X, Y), [X, Y]).

% Converte uma lista de células para uma lista de listas
list_to_list([], []).
list_to_list([H|T], [LH|LT]) :-
    cel_to_list(H, LH),
    list_to_list(T, LT).

% Converte uma lista de listas de células para uma lista de listas
lists_to_lists([], []).
lists_to_lists([H|T], [LH|LT]) :-
    list_to_list(H, LH),
    lists_to_lists(T, LT).

cells_lists_to_json_object([], _, _, JsonObject):- 
    JsonObject = json([cells=[], floorIds=[], accPoints=[]]).
cells_lists_to_json_object(CellsLists, [], [], JsonObject):- 
    lists_to_lists(CellsLists, List),
    JsonObject = json([cells=List, floorIds=[], accPoints=[]]).
cells_lists_to_json_object(CellsLists, FloorIds, ElevsAndCors, JsonObject) :-
    lists_to_lists(CellsLists, List),
    convert_elevs_and_cors(ElevsAndCors, ElevsAndCorsList),
    JsonObject = json([cells=List, floorIds=FloorIds, accPoints=ElevsAndCorsList]).

convert_elevs_and_cors([], []).
convert_elevs_and_cors([elev(X,Y)|T], [[X,elev,Y]|ET]) :-
    convert_elevs_and_cors(T, ET).
convert_elevs_and_cors([cor(X,Y)|T], [[X,cor,Y]|ET]) :-
    convert_elevs_and_cors(T, ET).

get_data():-
    get_buildings(),
    get_elevators(),
    get_rooms(),
    get_floors(),
    get_passageways(),
    get_maps(),
    get_robots(),
    get_robot_types(),
    !.

% Buildings ========================================================
get_buildings():-
    delete_buildings(),
    http_open('http://localhost:4000/api/buildings/listAllBuildings', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_pass_building(Status, ResJSON).

handle_http_status_pass_building(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    create_building(ResObj).

handle_http_status_pass_building(400, _).
    
create_building([]).
create_building([Building|T]):-
    assertz(building(Building.buildingCode)),
    create_building(T).

delete_buildings():-
    retractall(building(_)).

% ==================================================================

% Floors ==================================================================
get_floors() :-
    delete_floors(),
    findall(X, building(X), Buildings),
    process_buildings(Buildings),
    !.

process_buildings([]).
process_buildings([Building|RestBuildings]):-
    get_building_floors(Building),
    process_buildings(RestBuildings).

get_building_floors(Building):-
    atom_concat('http://localhost:4000/api/floors/listAllFloors/', Building, Url),
    http_open(Url, ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_floors(Status, ResJSON, Building).

handle_http_status_floors(200, ResJSON, Building) :-
    json_read_dict(ResJSON, ResObj),
    extrai_pisos(ResObj, Pisos),
    create_floor(Building, Pisos).

handle_http_status_floors(400, _, _).

extrai_pisos([], []):-!.
extrai_pisos([H|T], [H.floorId|L]):-
    extrai_pisos(T, L).

create_floor(_, []):-!.
create_floor(Building, Floors):-
    assertz(pisos(Building, Floors)).

delete_floors():-
    retractall(pisos(_,_)).

% ==================================================================

% Floor Maps ===========================================================
get_maps() :-
    delete_maps(),
    findall(X, building(X), Buildings),
    process_buildings_map(Buildings),
    !.

process_buildings_map([]):-!.
process_buildings_map([Building|RestBuildings]):-
    elevador(Building, ElevFloors),
    create_elev(ElevFloors),
    get_building_floors_maps(Building),
    process_buildings_map(RestBuildings).

get_building_floors_maps(Building):-
    atom_concat('http://localhost:4000/api/floors/listAllFloors/', Building, Url),
    http_open(Url, ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status(Status, ResJSON).

handle_http_status(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    create_map(ResObj).

handle_http_status(400, _, _).

create_map([]):-!.
create_map([Floor|T]):-
    process_map(Floor.floorMap, Floor.floorId),
    create_map(T).

process_map(MapObj, FloorId):-
    create_nodes(MapObj.map, FloorId, 0, 0, MapObj),
    matrix_dimensions(MapObj.map, Rows, Cols),
    cria_grafo(Cols, Rows, FloorId).

create_nodes([], _, _, _, _):-!.
create_nodes([Array|Restantes], Piso, Col, Lin, MapObj):-
    create_line(Array, Piso, Col, Lin, MapObj),
    Col2 is 0,
    Lin2 is Lin+1,
    create_nodes(Restantes, Piso, Col2, Lin2, MapObj).

create_line([], _, _, _, _):-!.
create_line([Valor|Restantes], Piso, Col, Lin, MapObj):-
    put_value(Valor, Piso, Col, Lin, MapObj),
    Col2 is Col+1,
    create_line(Restantes, Piso, Col2, Lin, MapObj).

put_value(0, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)).

put_value(8, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(9, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(10, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(11, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(12, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_cor(Piso, Col, Lin, MapObj).

put_value(13, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_cor(Piso, Col, Lin, MapObj).

put_value(14, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 0, Piso)),
    process_pontos_acesso_elev(Piso, Col, Lin).

put_value(_, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)).

process_pontos_acesso_room(FloorId, Col, Lin, MapObj):-
    get_door_by_coords(MapObj.doorsCoords, MapObj.rooms, Col, Lin, RoomName),
    assertz(ponto_acesso(RoomName, FloorId, cel(Col, Lin))).

get_door_by_coords([], [], _, _, _):-!.
get_door_by_coords([],_ , _, _, _):-!.
get_door_by_coords(_, [], _, _, _):-!.
get_door_by_coords([[X, Y]|T], [RoomName|L], Col, Lin, TheRoom):-
    (X == Col, Y == Lin -> TheRoom = RoomName; get_door_by_coords(T, L, Col, Lin, TheRoom)).


process_pontos_acesso_cor(FloorId, Col, Lin, MapObj):-
    http_open('http://localhost:4000/api/passageways/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_pass(Status, ResJSON, FloorId, Col, Lin, MapObj).

handle_pass(200, ResJSON, FloorId, Col, Lin, MapObj) :-
    json_read_dict(ResJSON, Passageways),
    create_pontos_acesso_cor(Col, Lin, FloorId, Passageways, MapObj.passagewaysCoords).

handle_pass(400, _).

create_pontos_acesso_cor(_, _, _, [], _):-!.
create_pontos_acesso_cor(_, _, _, _, []):-!.
create_pontos_acesso_cor(Col, Lin, FloorId, Passageways, PassCoords):-
    get_passId_by_coords(PassCoords, Col, Lin, PassId),
    get_floor2_of_pass(Passageways, PassId, FloorId, Floor2),
    findall(cor(X, Y), (cor(X, Y), ((X == FloorId , Y == Floor2);(X == Floor2 , Y == FloorId))), Cor),
    assert_each_ponto_acesso(Cor, FloorId, cel(Col, Lin)).

assert_each_ponto_acesso([], _, _).
assert_each_ponto_acesso([X|T], FloorId, Location):-
    \+ ponto_acesso(X, FloorId, _),
    assertz(ponto_acesso(X, FloorId, Location)).


get_passId_by_coords([], _, _, _):-!.
get_passId_by_coords([[Id, X, Y, X1, Y1]|T], Col, Lin, PassId):-
    (((X == Col, Y == Lin);(X1 == Col, Y1 == Lin))-> PassId = Id; get_passId_by_coords(T, Col, Lin, PassId)).

get_floor2_of_pass([], _,_, _):-!.
get_floor2_of_pass([Pass|T], PassId, Floor1, Floor2):-
        (((Pass.passagewayId == PassId, Pass.floor1Id == Floor1) -> Floor2 = Pass.floor2Id;(Pass.passagewayId == PassId, Pass.floor2Id == Floor1) -> Floor2 = Pass.floor1Id); get_floor2_of_pass(T, PassId, Floor1, Floor2)).
    

process_pontos_acesso_elev(FloorId, Col, Lin):-
    findall(elev(X, Y), (elev(X, Y), (X == FloorId ; Y == FloorId)), Elevs),
    create_pontos_acesso_elev(Elevs, Col, Lin, FloorId).

create_pontos_acesso_elev([], _, _, _):-!.
create_pontos_acesso_elev([Elev|Rest], Col, Lin, FloorId):-
    assertz(ponto_acesso(Elev, FloorId, cel(Col, Lin))),
    create_pontos_acesso_elev(Rest, Col, Lin, FloorId).

create_elev([]):-!.
create_elev([Floor|Rest]):-
    create_elev2(Rest, Floor).

create_elev2([], _):-!.
create_elev2([Floor|Rest], Prev):-
    assertz(elev(Prev, Floor)),
    create_elev2(Rest, Floor).

% Base case: an empty list has 0 rows and 0 columns.
matrix_dimensions([], 0, 0).

% Case for a non-empty list.
matrix_dimensions([Row|Rows], NumRows, NumCols) :-
    % Count the number of rows in the remaining matrix.
    length(Rows, RemainingRows),
    
    % Count the number of columns in the current row.
    length(Row, NumColumns),
    
    % The total number of rows is the remaining rows plus 1 (current row).
    NumRows is RemainingRows + 1,
    
    % Set the number of columns.
    NumCols is NumColumns.

delete_maps():-
    retractall(node( _, _, _, _)),
    retractall(edge(_, _, _, _, _)),
    retractall(lens(_, _, _)),
    retractall(elev(_, _)),
    retractall(ponto_acesso(_, _, _)).

% ==================================================================

% Passageways =======================================================
get_passageways():-
    delete_passageways(),
    http_open('http://localhost:4000/api/passageways/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_pass(Status, ResJSON),
    !.

handle_http_status_pass(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    process_passageways(ResObj).


handle_http_status_pass(400, _).

findBuildingByFloorId(FloorId, Building):-
    pisos(Building, Floors),
    member(FloorId, Floors).

process_passageways([]).
process_passageways([Passageway|T]) :-
    findBuildingByFloorId(Passageway.floor1Id, Building1),
    findBuildingByFloorId(Passageway.floor2Id, Building2),
    create_passageway(Passageway, Building1, Building2),
    process_passageways(T). 

create_passageway(Passageway, Building1, Building2):-
    assertz(corredor(Building1, Building2, Passageway.floor1Id, Passageway.floor2Id)),
    (clause(liga(Building1, Building2), true)
    ->  % Fact already exists, do nothing
        true
    ;   % Fact doesn't exist, assert it
        assertz(liga(Building1, Building2))
    ),
    assertz(cor(Passageway.floor1Id, Passageway.floor2Id)).

delete_passageways():-
    retractall(cor(_, _)),
    retractall(liga(_, _)),
    retractall(corredor(_,_,_,_)).

% ==================================================================

% Elevators =========================================================
get_elevators():-
    delete_elevators(),
    http_open('http://localhost:4000/api/elevators/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_elev(Status, ResJSON).

handle_http_status_elev(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    create_elevator(ResObj).

handle_http_status_elev(400, _).

create_elevator([]).
create_elevator([Elevator|T]):-
    assertz(elevador(Elevator.buildingCode, Elevator.floorsId)),
    assert_elev(Elevator.floorsId),
    create_elevator(T).

assert_elev([]).
assert_elev([Floor|T]):-
    assertz(elev(Floor, T)),
    assert_elev(T).

delete_elevators():-
    retractall(elevador(_,_)).

% ==================================================================

% Rooms =========================================================
get_rooms():-
    delete_rooms(),
    findall(X, building(X), Buildings),
    process_buildings2(Buildings),
    !.

process_buildings2([]).
process_buildings2([Building|RestBuildings]) :-
    get_building_rooms(Building),
    process_buildings2(RestBuildings).

get_building_rooms(Building):-
    atom_concat('http://localhost:4000/api/rooms/listAllInBuilding/', Building, Url),
    http_open(Url, ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status1(Status, ResJSON, Building).

handle_http_status1(200, ResJSON, Building) :-
    json_read_dict(ResJSON, ResObj),
    create_room(Building, ResObj).

handle_http_status1(400, _, _).

create_room(_, []).
create_room(Building, [Room|T]):-
    assertz(room(Room.roomName, Building, Room.floorId)),
    create_room(Building, T).

delete_rooms():-
    retractall(room(_,_,_)).

% ==================================================================

% Robots ===========================================================
get_robots():-
    delete_robots(),
    http_open('http://localhost:4000/api/robots/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_robots(Status, ResJSON).

handle_http_status_robots(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    create_robot(ResObj).

handle_http_status_robots(400, _).

create_robot([]).
create_robot([Robot|T]):-
    assertz(robot(Robot.code, Robot.type, Robot.operationStatus)),
    create_robot(T).

delete_robots():-
    retractall(robot(_,_,_)).

% ==================================================================

% Robot Type ==========================================================
get_robot_types():-
    delete_robotType(),
    http_open('http://localhost:4000/api/robotTypes/listAllRobotTypes', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_robots_types(Status, ResJSON).

handle_http_status_robots_types(200, ResJSON) :-
    json_read_dict(ResJSON, ResObj),
    create_robotType(ResObj).

handle_http_status_robots_types(400, _).

create_robotType([]).
create_robotType([RobotType|T]):-
    assertz(robotType(RobotType.robotTypeID, RobotType.availableTasks)),
    create_robotType(T).

delete_robotType():-
    retractall(robotType(_,_)).

%===========================================================================================================
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
    aStar2(Dest,[(_,0,[Orig])],Cam,Custo,Piso),
    %write('Caminho'),write(Cam),nl.

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

count_nodes():-
    node(Col, Lin, Val, Floor),
    format('Node: (~w, ~w, ~w, ~w)~n', [Col, Lin, Val, Floor]),
    fail. % This predicate will fail intentionally to stop backtracking after printing all edges..

write_nodes(Floor):-
    findall(node(X, Y, Z, Floor), node(X, Y, Z, Floor), Nodes),
    write('Length: '), length(Nodes, Length), write(Length), nl,
    write(Nodes), nl.
