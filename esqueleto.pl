%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%%AUX

%% posValida(+Pos, +T).
posValida(pos(F,C),T) :- F >= 0, C >= 0, numFilas(T,F1), F =< F1, numCol(T,C1), C =< C1.

%%allOflenghth(+Lista, +Longitud).
allOflenghth([],_).
allOflenghth([X|L],C) :- length(X,C), allOflenghth(L,C).

%%numFilas(+Tablero, ?Longitud).
numFilas(T,X) :- length(T,X).

%%numFilas(+Tablero, ?Longitud).
numCol(T,X) :- last(T,L), length(L,X).

%%posValidaYLibre(+Pos, +Tablero).
posValidaYLibre(P,T) :- posValida(P,T),casilleroLibre(P,T).

%%casilleroLibre(+Pos, +Tablero).
%%No chekea si la posicion es valida, se deberia hacer previamente.
casilleroLibre(pos(F,C),T) :- nth0(F,T,F1), nth0(C,F1,X), var(X).

%%movValido(+Pos, +Tablero, +Camino).
movValido(P,T,X) :- posValidaYLibre(P,T),not(member(P,X)).

%%llegueEn(?Pos, ?Pasos).
:- dynamic llegueEn/2.

%%tardoMasQueAntes(+Pos, +Pasos).
tardoMasQueAntes(P,L) :- llegueEn(P,X), X < L . 

%%tablero(+Nombre, ?Tablero).
%%Ejemplos
tablero(ej5x5, T) :-tablero(5, 5, T),ocupar(pos(1, 1), T),ocupar(pos(1, 2), T).
tablero(chorizo2x4, T) :- tablero(2, 4, T).
tablero(chorizo4x2, T) :- tablero(4, 2, T).
tablero(chorizo2x5obstaculo, T) :- tablero(2, 4, T), ocupar(pos(1, 3), T).
tablero(chorizo5x2obstaculo, T) :- tablero(5, 2, T), ocupar(pos(3, 0), T).
tablero(libre2x2, T) :- tablero(2, 2, T).
tablero(libre3x3, T) :- tablero(3, 3, T).
tablero(dona3x3, T) :- tablero(3, 3, T), ocupar(pos(1, 1), T).
tablero(cueva6x6, T) :- tablero(6, 6, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T), ocupar(pos(1, 3), T), ocupar(pos(1, 4), T), ocupar(pos(2, 4), T), ocupar(pos(3, 4), T), ocupar(pos(4, 4), T), ocupar(pos(4, 3), T),ocupar(pos(4, 2), T), ocupar(pos(4, 1), T).
tablero(zigzag5x5restringido, T) :- tablero(5, 6, T), ocupar(pos(1, 1), T), ocupar(pos(0, 2), T), ocupar(pos(3, 1), T), ocupar(pos(3, 2), T), ocupar(pos(2, 3), T), ocupar(pos(1, 4), T), ocupar(pos(3, 5), T), ocupar(pos(4, 5), T), ocupar(pos(3, 4), T).
tablero(zigzag5x5, T) :- tablero(5, 6, T), ocupar(pos(1, 1), T), ocupar(pos(0, 2), T), ocupar(pos(3, 1), T), ocupar(pos(3, 2), T), ocupar(pos(2, 3), T), ocupar(pos(1, 4), T).
tablero(chorizo6x3, T) :- tablero(6, 3, T), ocupar(pos(1, 1), T), ocupar(pos(2, 1), T),ocupar(pos(3, 1), T),ocupar(pos(4, 1), T). %tablero(chorizo6x3, T)


%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(F,C,T) :- length(T,F), allOflenghth(T,C).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(F,C),T) :- nth0(F,T,F1), nth0(C,F1,ocupada).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(F,C),T,V) :- posValida(pos(F,C),T), vecinoAux(pos(F,C),T,V).

%%vecinoAux(+Inicio, +Tablero, ?Vecino).
vecinoAux(pos(F,C),_,pos(F1,C)) :- F > 0, F1 is F-1.
vecinoAux(pos(F,C),_,pos(F,C1)) :- C > 0, C1 is C-1.
vecinoAux(pos(F,C),T,pos(F1,C)) :- numFilas(T,NF), F < NF, F1 is F+1.
vecinoAux(pos(F,C),T,pos(F,C1)) :- numCol(T,NC), C < NC, C1 is C+1.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F,C),T,V) :- vecino(pos(F,C),T,V), casilleroLibre(V,T).


%%vecinoEnOrden(+Inicio, +Fin, -Vecino).
vecinoEnOrden(pos(FS,CS),pos(FE,CE),V) :- X is FS - FE, Y is CS - CE, vecinoEnOrdenAux(pos(FS,CS),X,Y,V).


%%vecinoEnOrdenAux(+Inicio, +X,+Y, -Vecino).
vecinoEnOrdenAux(pos(FS,CS),X,_,pos(NS,CS)) :- X < 0, NS is FS +1.
vecinoEnOrdenAux(pos(FS,CS),X,_,pos(NS,CS)) :- X > 0, NS is FS -1.
vecinoEnOrdenAux(pos(FS,CS),_,Y,pos(FS,NS)) :- Y < 0, NS is CS +1.
vecinoEnOrdenAux(pos(FS,CS),_,Y,pos(FS,NS)) :- Y > 0, NS is CS -1.

vecinoEnOrdenAux(pos(FS,CS),X,_,pos(NS,CS)) :- X =< 0, NS is FS -1.
vecinoEnOrdenAux(pos(FS,CS),X,_,pos(NS,CS)) :- X >= 0, NS is FS +1.
vecinoEnOrdenAux(pos(FS,CS),_,Y,pos(FS,NS)) :- Y =< 0, NS is CS -1.
vecinoEnOrdenAux(pos(FS,CS),_,Y,pos(FS,NS)) :- Y >= 0, NS is CS +1.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
camino(S,F,T,C) :- posValidaYLibre(S,T), posValidaYLibre(F,T),armarCamino(F,S,T,C,[]).

%%ArmarCamino(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino(pos(SX,SY),pos(SX,SY),_,[pos(SX,SY) | X],X).
armarCamino(S,F,T,C,X) :- vecinoLibre(S,T,V), not(member(V,X)), armarCamino(V,F,T,C,[S|X]).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(S,F,T,N) :- aggregate_all(count, camino(S,F,T,_), N2), N is N2.

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud. NO HACE ESTO!!! es una heuristica
%% que trata pero no es perfecta
%%Descripcion de la funcion: La funcion se fija que los dos extremos del camino esten libres y sean posiciones validas (que esten
%%en el rango del tablero), luego arma el camino. La heuristica hace que primero se busque en direccion al estado final, esto lo hace
%%primero en el eje 'x' y luego en el eje 'y'. Por ejemplo, si el casillero final esta a la derecha y abajo del casillero actual donde
%%estoy y el casillero de la derecha esta disponible, voy a la derecha. Si no puedo ir a la derecha voy abajo, si no puedo ir abajo voy a
%%la izquierda, y si no arriba (en ese orden de prioridades, este criterio se ve en la funcion vecinoEnOrden). De esta manera siempre se
%%intenta tomar el camino mas directo posible al casillero final.
camino2(S,F,T,C) :- posValidaYLibre(S,T), posValidaYLibre(F,T), armarCamino2(F,S,T,C,[]).

%%ArmarCamino2(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino2(pos(SX,SY),pos(SX,SY),_,[pos(SX,SY) | X],X).
armarCamino2(S,F,T,C,X) :- vecinoEnOrden(S,F,V), movValido(V,T,X),armarCamino2(V,F,T,C,[S|X]).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.

%% Descripcion de la funcion: En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
%% La idea de lo que hacemos en ejercicio es similar a la que hacemos en camino2 en cuanto la eleccion de por donde ir cuando se esta
%% armando el camino. La diferencia esta en que reducimos el espacio de busqueda de la siguiente manera: Cuando llegamos a algun casillero
%% del tablero mientras estamos construyendo el camino, nos fijamos que no haya otro camino donde hayamos llegado en menos pasos a ese
%% mismo casillero. De ser asi, descartamos el camino que estabamos construyendo, hacemos esto porque siempre va a ser mejor tomar el otro
%% camino hasta ese casillero, ya que llegamos en menos pasos. Para realizar esto usamos assertz para ir guardando en cuantos pasos
%% llegamos a los casilleros.
camino3(S,F,T,C) :-  retractall(llegueEn(_,_)),posValida(S,T), posValida(F,T), assertz(llegueEn(F,0)), armarCamino3(F,S,T,C,[]).

%%ArmarCamino3(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino3(pos(SX,SY),pos(SX,SY),_,[pos(SX,SY) | X],X).
armarCamino3(S,F,T,C,X) :- vecinoEnOrden(S,F,V), movValido(V,T,X), length(X,L1), L is L1+1, not(tardoMasQueAntes(V,L)), retractall(llegueEn(V,_)), assertz(llegueEn(V,L)), armarCamino3(V,F,T,C,[S|X]).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.

%% Aclaracion: En este ejercicio usamos la funcion camino hecha en el ejercicio 5. Lo que agregamos es que se fije que los casilleros
%% inicial y final sean validos en ambos tableros antes de empezar a buscar caminos. De esta manera si para alguno de los dos tableros
%% no es valida alguna de esas dos posiciones, sabemos que no existe un camino y podemos terminar inmediatamente
caminoDual(S,F,T1,T2,C) :- posValidaYLibre(S,T2),posValidaYLibre(F,T2),camino(S,F,T1,C),camino(S,F,T2,C).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%i
test(all) :- test(introductorios), test(caminos).

test(introductorios) :- test(tablero), test(ocupar), test(vecino), test(vecino2), test(vecinoLibre).
test(caminos) :- test(camino1), test(camino2), test(camino3), test(caminoDual). 
test(camino1) :- test(camino1_1), test(camino1_2), test(cantidadCamino1_1), test(cantidadCamino1_2), test(cantidadCamino1_3).
test(camino2) :- test(camino2_1), test(camino2_2), test(cantidadCamino2_1), test(cantidadCamino2_2), test(cantidadCamino2_3).
test(camino3) :- test(camino3_1), test(camino3_2), test(cantidadCamino3_1), test(cantidadCamino3_2), test(cantidadCamino3_3).
test(caminoDual) :- test(caminoDual_1), test(caminoDual_2).

%%introductorios
test(tablero) :- tablero(2,2,T), length(T, L), L is 2, nth0(1,T,F1), length(F1, 2).
test(ocupar) :- tablero(libre3x3, T), ocupar(pos(1,1), T), nth0(1,T,F1), nth0(1,F1,RES), ground(RES).
test(vecino) :- tablero(libre3x3, T), vecino(pos(0,0), T, V), V = pos(1,0).
test(vecino2) :- tablero(libre3x3, T), vecino(pos(0,0), T, V), V = pos(0,1).
test(vecinoLibre) :- tablero(libre3x3, T), ocupar(pos(0,1), T), vecinoLibre(pos(0,0), T, V),  V = pos(1,0).

%%Camino1
test(camino1_1) :- tablero(libre2x2, T), camino(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(0,1), pos(1,1)].
test(camino1_2) :- tablero(libre2x2, T), camino(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(1,0), pos(1,1)].

test(cantidadCamino1_1) :- tablero(libre3x3, T), cantidadDeCaminos(pos(0,0),pos(2,2),T,N), N = 12.
test(cantidadCamino1_2) :- tablero(chorizo2x4, T), cantidadDeCaminos(pos(1,2),pos(1,3),T,N), N = 4.
test(cantidadCamino1_3) :- tablero(chorizo5x2obstaculo, T), cantidadDeCaminos(pos(2,0),pos(4,0),T,N), N = 3.

%%Camino2
test(camino2_1) :- tablero(libre2x2, T), camino2(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(0,1), pos(1,1)].
test(camino2_2) :- tablero(libre2x2, T), camino2(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(1,0), pos(1,1)].

test(cantidadCamino2_1) :- tablero(libre3x3, T), cantidadDeCaminosConCamino2(pos(0,0),pos(2,2),T,N), N = 12.
test(cantidadCamino2_2) :- tablero(chorizo2x4, T), cantidadDeCaminosConCamino2(pos(1,2),pos(1,3),T,N), N = 4.
test(cantidadCamino2_3) :- tablero(chorizo5x2obstaculo, T), cantidadDeCaminosConCamino2(pos(2,0),pos(4,0),T,N), N = 3.

%%Camino3
test(camino3_1) :- tablero(libre2x2, T), camino3(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(0,1), pos(1,1)].
test(camino3_2) :- tablero(libre2x2, T), camino3(pos(0,0),pos(1,1),T,C), C = [pos(0,0), pos(1,0), pos(1,1)].

test(cantidadCamino3_1) :- tablero(libre3x3, T), cantidadDeCaminosConCamino3(pos(0,0),pos(2,2),T,N), N = 6.
test(cantidadCamino3_2) :- tablero(chorizo2x4, T), cantidadDeCaminosConCamino3(pos(1,2),pos(1,3),T,N), N = 1.
test(cantidadCamino3_3) :- tablero(chorizo5x2obstaculo, T), cantidadDeCaminosConCamino3(pos(2,0),pos(4,0),T,N), N = 1.

test(caminoDual_1) :- tablero(libre3x3, T1), tablero(libre2x2, T2), cantidadDeCaminosConDual(pos(0,0),pos(1,1),T1,T2,N), N = 2.
test(caminoDual_2) :- tablero(libre3x3, T1), tablero(libre2x2, T2), not(caminoDual(pos(0,0),pos(2,2),T1,T2,_)).

%%Solo para uso de test
cantidadDeCaminosConCamino2(S,F,T,N) :- aggregate_all(count, camino2(S,F,T,_), N2), N is N2.
cantidadDeCaminosConCamino3(S,F,T,N) :- aggregate_all(count, camino3(S,F,T,_), N2), N is N2.
cantidadDeCaminosConDual(S,F,T1,T2,N) :- aggregate_all(count, caminoDual(S,F,T1,T2,_), N2), N is N2.
