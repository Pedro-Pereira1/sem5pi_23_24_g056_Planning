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

% Dynamic Predicates
:-dynamic building/1. % building(A)...
:-dynamic pisos/2. % pisos(A, [1,2,3])...
:-dynamic corredor/4. % passageway(A, B, 2, 2)...
:-dynamic elevador/2. % elevator(A, [1,2,3])...
:-dynamic room/3. % room(Name, A, 1)...
:-dynamic robot/3. % robot(Code, Type, OperationStatus(true or false))...
:-dynamic robotType/3. % robotType(TypeID, AvailableTasks[])...
:-dynamic elev/2. % elev(a1, a2), elev(a2, a3)...
:-dynamic corr/2. % corr(a1, b1), corr(a2, b2)...

:-dynamic liga/2. % ligacel(building1, building2)...

:-dynamic cel/2. % cel(x, y) => cel(1, 3)...
:-dynamic liga/2. % liga(EdificioA, EdificioB)...
:-dynamic node/4. % node(Id, Col, Lin, Valor, Piso)...
:-dynamic edge/4. % edge(Id1, Id2, Custo, Piso)...
:-dynamic elev_pos/4. % elev_pos(Id, Col, Lin, Piso)...
:-dynamic corr_pos/4. % corr_pos(Id, Col, Lin, Piso)...
:-dynamic ponto_acesso/3. % ponto_acesso(Col, Lin, Piso)...



% Criacao de servidor HTTP no porto 'Port'					
% Gerir servidor
startServer(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stopServer:-
    retract(port(Port)),
    http_stop_server(Port,_).

get_data():-
    get_buildings(),
    get_elevators(),
    get_rooms(),
    get_floors(),
    get_passageways(),
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

% Floors ===========================================================
get_floors() :-
    delete_floors(),
    delete_maps(),
    findall(X, building(X), Buildings),
    process_buildings(Buildings),
    !.

process_buildings([]).
process_buildings([Building|RestBuildings]):-
    elevador(Building, ElevFloors),
    create_elev(ElevFloors),
    get_building_floors(Building),
    process_buildings(RestBuildings).

get_building_floors(Building):-
    atom_concat('http://localhost:4000/api/floors/listAllFloors/', Building, Url),
    http_open(Url, ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status(Status, ResJSON, Building).

handle_http_status(200, ResJSON, Building) :-
    json_read_dict(ResJSON, ResObj),
    extrai_pisos(ResObj, Pisos),
    create_floor(Building, Pisos),
    create_map(ResObj).

handle_http_status(400, _, _).

extrai_pisos([], []):-!.
extrai_pisos([H|T], [H.floorId|L]):-
    extrai_pisos(T, L).

create_floor(_, []):-!.
create_floor(Building, Floors):-
    assertz(pisos(Building, Floors)).

create_map([]):-!.
create_map([Floor|T]):-
    process_map(Floor.floorMap, Floor.floorId),
    create_map(T).

process_map(MapObj, FloorId):-
    create_nodes(MapObj.map, FloorId, 0, 0, MapObj).

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
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(9, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(10, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(11, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_room(Piso, Col, Lin, MapObj).

put_value(12, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_corr(Piso, Col, Lin, MapObj).

put_value(13, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_corr(Piso, Col, Lin, MapObj).

put_value(14, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)),
    process_pontos_acesso_elev(Piso, Col, Lin).

put_value(_, Piso, Col, Lin, MapObj):-
    assertz(node(Col, Lin, 1, Piso)).

process_pontos_acesso_room(FloorId, Col, Lin, MapObj):-
    get_door_by_coords(MapObj.doorsCoords, MapObj.rooms, Col, Lin, RoomName),
    assertz(ponto_acesso(RoomName, FloorId, cel(Col, Lin))).

get_door_by_coords([], [], _, _, _):-!.
get_door_by_coords([[X, Y]|T], [RoomName|L], Col, Lin, TheRoom):-
    (X == Col, Y == Lin -> TheRoom = RoomName; get_door_by_coords(T, L, Col, Lin, TheRoom)).


process_pontos_acesso_corr(FloorId, Col, Lin, MapObj):-
    http_open('http://localhost:4000/api/passageways/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_pass(Status, ResJSON, FloorId, Col, Lin, MapObj).

handle_pass(200, ResJSON, FloorId, Col, Lin, MapObj) :-
    json_read_dict(ResJSON, Passageways),
    create_pontos_acesso_corr(Col, Lin, FloorId, Passageways, MapObj.passagewaysCoords).

handle_pass(400, _).

create_pontos_acesso_corr(_, _, _, [], _):-!.
create_pontos_acesso_corr(_, _, _, _, []):-!.
create_pontos_acesso_corr(Col, Lin, FloorId, Passageways, PassCoords):-
    get_passId_by_coords(PassCoords, Col, Lin, PassId),
    get_floor2_of_pass(Passageways, PassId, FloorId, Floor2),
    write('Floor1: '),writeln(FloorId),
    write('Floor2: '),writeln(Floor2),
    findall(corr(X, Y), (corr(X, Y), ((X == FloorId , Y == Floor2);(X == Floor2 , Y == FloorId))), Corr),
    write('Coor: '),writeln(Corr),
    assert_each_ponto_acesso(Corr, FloorId, cel(Col, Lin)),
    create_pontos_acesso_corr(Rest, Col, Lin, FloorId, T).

assert_each_ponto_acesso([], _, _).
assert_each_ponto_acesso([X|T], FloorId, Location):-
    assertz(ponto_acesso(X, FloorId, Location)),
    assert_each_ponto_acesso(T, FloorId, Location).


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

delete_maps():-
    retractall(node(_, _, _, _, _)),
    retractall(edge(_, _, _, _, _)),
    retractall(elev(_, _)),
    retractall(ponto_acesso(_, _, _)).

delete_floors():-
    retractall(pisos(_,_)).

% ==================================================================

% Passageways =======================================================
get_passageways():-
    delete_passageways(),
    http_open('http://localhost:4000/api/passageways/listAll', ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status_pass(Status, ResJSON).

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
    assertz(corr(Passageway.floor1Id, Passageway.floor2Id)).

delete_passageways():-
    retractall(corr(_, _)),
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

