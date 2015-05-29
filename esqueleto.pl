%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%%AUX
posValida(pos(f,c),T) :- f >= 0, c >= 0, numFilas(T,F), f =< F, numCol(T,C), c =< C.

allOflenghth([],c).
allOflenghth([x|L],c) :- length(x,c), allOflenghth(L,c).

numFilas(T,X) :- length(T,X).

numCol(T,X) :- last(T,L), length(L,X).

%%No chekea si la posicion es valida, se deberia hacer previamente.
casilleroLibre(pos(f,c),T) :- nth0(f,T,F), nth0(c,F,X), var(X).

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(f,c,L) :- length(L,f), allOflenghth(L,c).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(f,c),T) :- nth0(f,T,F), nth0(c,F,X), X is ocupada.

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(f,c),T,V) :- posValida(pos(f,c),T), vecinoAux(pos(f,c),T,V).

vecinoAux(pos(f,c),T,V) :- f > 0, F is f-1,V is pos(F,c).
vecinoAux(pos(f,c),T,V) :- c > 0, C is c-1,V is pos(f,C).
vecinoAux(pos(f,c),T,V) :- numFilas(T,F), f < F, F1 is f+1,V is pos(F1,c).
vecinoAux(pos(f,c),T,V) :- numCol(T,C), c < C, C1 is c+1,V is pos(f,C1).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(f,c),T,V) :- vecino(pos(f,c),T,V), casilleroLibre(V,T).

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

%%Checkeamos posValida o no vale la pena?
camino(S,F,T,C) :- posValida(S,T), posValida(F,T), X is [], armarCamino(S,F,T,C,X).

%%ArmarCamino(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
%% TODO VALE ARMAR LA LISTA DE ESA FORMA?
armarCamino(pos(sx,sy),pos(fx,fy),T,C,X) :- posValida(pos(sx,sy),T), sx = fx, sy = fy, C is [pos(fx,fy) | X].
armarCamino(S,F,T,C,X) :- posValida(S,T), vecinoLibre(S,T,V), not(member(V,X)), armarCamino(V,F,T,C,[S|X]).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(S,F,T,N) :- ground(N), aggregate_all(contador, camino(S,F,T,C), N2), N = N2.
cantidadDeCaminos(S,F,T,N) :- var(N), aggregate_all(contador, camino(S,F,T,C), N).

%% Creo que se puede unificar en:
%% cantidadDeCaminos(S,F,T,N) :- aggregate_all(contador, camino(S,F,T,C), N2), N is N2.
%% pero no estoy seguro

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.
camino2(S,F,T,C) :- posValida(S,T), posValida(F,T), X is [], armarCamino2(S,F,T,C,X).

%%ArmarCamino2(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- sx = fx, sy = fy, C is [pos(fx,fy) | X].
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- sx < fx, V is pos(sx+1,sy), continuar(pos(sx,sy),pos(fx,fy),T,C,X,V).
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- sx > fx, V is pos(sx-1,sy), continuar(pos(sx,sy),pos(fx,fy),T,C,X,V).
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- sx = fx, sy < fy, V is pos(sx,sy+1), continuar(pos(sx,sy),pos(fx,fy),T,C,X,V).
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- sx = fx, sy > fy, V is pos(sx,sy-1), continuar(pos(sx,sy),pos(fx,fy),T,C,X,V).
%%continua con camino (el original)
armarCamino2(pos(sx,sy),pos(fx,fy),T,C,X) :- camino(pos(sx,sy),pos(fx,fy),T,C2), append(X,C2,C).

%%continuar(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial, +Vecino)
continuar(S,F,T,C,X,V) :- casilleroLibre(V,T), not(member(V,X)), armarCamino(V,F,T,C,[S|X]).

%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.
camino3(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_,_,_,_,_).
