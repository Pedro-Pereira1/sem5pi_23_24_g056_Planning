% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_cors)).
:- use_module(library(date)).
:- use_module(library(random)).

% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

% Dynamic Predicates
:-dynamic building/1. % building(A)...
:-dynamic floor/2. % floor(A, 1)...
:-dynamic passageway/5. % passageway(1, A, 2, B, 2)...
:-dynamic elevator/3. % elevator(1, A, [1,2,3])...
:-dynamic room/3. % room(Name, A, 1)...
:-dynamic robot/3. % robot(Code, Type, OperationStatus(true or false))...
:-dynamic robotType/3. % robotType(TypeID, AvailableTasks[])...

:-dynamic m/4. % m(col, lin, value, floor) => m(0, 0, 0, 1)...
:-dynamic ligacel/3. % ligacel(cel1, cel2, floor) => ligacel(cel(1,3), cel(2,3), 1)...

:-dynamic cel/2. % cel(x, y) => cel(1, 3)...
:-dynamic liga/2. % liga(EdificioA, EdificioB)...
:-dynamic node/5. % node(Id, Col, Lin, Valor, Piso)...
:-dynamic edge/4. % edge(Id1, Id2, Custo, Piso)...
:-dynamic elev_pos/4. % elev_pos(Id, Col, Lin, Piso)...
:-dynamic corr_pos/4. % corr_pos(Id, Col, Lin, Piso)...
:-dynamic ponto_acesso/4. % ponto_acesso(Id, Col, Lin, Piso)...



% Criacao de servidor HTTP no porto 'Port'					
% Gerir servidor
startServer(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(port(Port)).

stopServer:-
    retract(port(Port)),
    http_stop_server(Port,_).


% Buildings ========================================================
get_buildings():-
    delete_buildings(),
    http_open('http://localhost:4000/api/buildings/listAllBuildings', ResJSON, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(ResJSON, ResObj),
    atts_building(ResObj, ResVal),
    create_building(ResVal).

    %get_floors(ResObj).

atts_building([], []).
atts_building([H|T], [H.buildingCode|L]):-
    atts_building(T, L).
    
create_building([]).
create_building([C|T]):-
    assertz(building(C)),
    create_building(T).

delete_buildings():-
    retractall(building(_)).
% ==================================================================

% Floors ===========================================================
get_floors() :-
    delete_floors(),
    findall(X, building(X), Buildings),
    process_buildings(Buildings),
    !.

process_buildings([]).
process_buildings([Building|RestBuildings]) :-
    get_building_floors(Building),
    process_buildings(RestBuildings).

get_building_floors(Building):-
    atom_concat('http://localhost:4000/api/floors/listAllFloors/', Building, Url),
    http_open(Url, ResJSON, [cert_verify_hook(cert_accept_any), status_code(Status)]),
    handle_http_status(Status, ResJSON, Building).

handle_http_status(200, ResJSON, Building) :-
    json_read_dict(ResJSON, ResObj),
    create_floor(Building, ResObj).

handle_http_status(_, _, _).

create_floor(_, []).
create_floor(Building, [Floor|T]):-
    assertz(floor(Building, Floor.floorId)),
    create_floor(Building, T).

create_floor_map(_, []).
create_floor_map([Floor|T]):-
    assertz(floorMap(Floor.floorId, Floor.floorMap)),
    create_floor_map(Building, T).

delete_floors():-
    retractall(floor(_,_)).

% ==================================================================

% Passageways =======================================================
get_passageways():-
    delete_passageways(),
    http_open('http://localhost:4000/api/passageways/listAll', ResJSON, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(ResJSON, ResObj),
    process_passageways(ResObj).
    
findBuildingByFloorId(FloorId, Building):-
    floor(Building, FloorId).

process_passageways([]).
process_passageways([Passageway|T]) :-
    findBuildingByFloorId(Passageway.floor1Id, Building1),
    findBuildingByFloorId(Passageway.floor2Id, Building2),
    create_passageway(Passageway, Building1, Building2),
    process_passageways(T).

create_passageway(Passageway, Building1, Building2):-
    assertz(passageway(Passageway.passagewayId, Building1, Passageway.floor1Id, Building2, Passageway.floor2Id)).

delete_passageways():-
    retractall(passageway(_,_,_,_,_)).

% ==================================================================

% Elevators =========================================================
get_elevators():-
    delete_elevators(),
    http_open('http://localhost:4000/api/elevators/listAll', ResJSON, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(ResJSON, ResObj),
    create_elevator(ResObj).

create_elevator([]).
create_elevator([Elevator|T]):-
    assertz(elevator(Elevator.elevatorId, Elevator.buildingCode, Elevator.floorsId)),
    create_elevator(T).

delete_elevators():-
    retractall(elevators(_,_,_,_,_)).

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

handle_http_status1(_, _, _).

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
    http_open('http://localhost:4000/api/robots/listAll', ResJSON, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(ResJSON, ResObj),
    create_robot(ResObj).

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
    http_open('http://localhost:4000/api/robotTypes/listAllRobotTypes', ResJSON, [cert_verify_hook(cert_accept_any)]),
    json_read_dict(ResJSON, ResObj),
    create_robotType(ResObj).

create_robotType([]).
create_robotType([RobotType|T]):-
    assertz(robotType(RobotType.robotTypeID, RobotType.availableTasks)),
    create_robotType(T).

delete_robotType():-
    retractall(robotType(_,_)).

% Floor Maps ==================================================================
create_graph(_,0):-!.
create_graph(Col,Lin):-
    create_graph_lin(Col,Lin),
    Lin1 is Lin-1,
    create_graph(Col,Lin1).

create_graph_lin(0,_):-!.
create_graph_lin(Col,Lin):-
    m(Col,Lin,0),
    !,
    ColS is Col+1, 
    ColA is Col-1, 
    LinS is Lin+1,
    LinA is Lin-1,
    ((m(ColS,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
    ((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
    ((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
    ((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
    Col1 is Col-1,
    create_graph_lin(Col1,Lin).

create_graph_lin(Col,Lin):-Col1 is Col-1,create_graph_lin(Col1,Lin).