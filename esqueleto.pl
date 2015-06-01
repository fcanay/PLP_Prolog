%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%%AUX
posValida(pos(F,C),T) :- F >= 0, C >= 0, numFilas(T,F1), F =< F1, numCol(T,C1), C =< C1.

allOflenghth([],_).
allOflenghth([X|L],C) :- length(X,C), allOflenghth(L,C).

numFilas(T,X) :- length(T,X).

numCol(T,X) :- last(T,L), length(L,X).

movValido(P,T,X) :- posValida(P,T),casilleroLibre(P,T),not(member(P,X)).

%%Ejemplos
tablero(ej5x5, T) :-tablero(5, 5, T),ocupar(pos(1, 1), T),ocupar(pos(1, 2), T).
tablero(chorizo2x4, T) :- tablero(2, 4, T).
tablero(chorizo4x2, T) :- tablero(4, 2, T).
tablero(chorizo2x5obstaculo, T) :- tablero(2, 4, T), ocupar(pos(1, 3), T).
tablero(chorizo5x2obstaculo, T) :- tablero(5, 2, T), ocupar(pos(3, 0), T).
tablero(libre2x2, T) :- tablero(2, 2, T).
tablero(libre3x3, T) :- tablero(3, 3, T).
tablero(dona3x3, T) :- tablero(3, 3, T), ocupar(pos(1, 1), T).
tablero(cueva5x5, T) :- tablero(5, 5, T), ocupar(pos(1, 1), T), ocupar(pos(1, 2), T), ocupar(pos(1, 3), T), ocupar(pos(2, 3), T), ocupar(pos(3, 1), T), ocupar(pos(3, 2), T), ocupar(pos(3, 3), T).
tablero(zigzag5x5restringido, T) :- tablero(5, 6, T), ocupar(pos(1, 1), T), ocupar(pos(0, 2), T), ocupar(pos(3, 1), T), ocupar(pos(3, 2), T), ocupar(pos(2, 3), T), ocupar(pos(1, 4), T), ocupar(pos(3, 5), T), ocupar(pos(4, 5), T), ocupar(pos(3, 4), T).
tablero(zigzag5x5, T) :- tablero(5, 6, T), ocupar(pos(1, 1), T), ocupar(pos(0, 2), T), ocupar(pos(3, 1), T), ocupar(pos(3, 2), T), ocupar(pos(2, 3), T), ocupar(pos(1, 4), T).


%%No chekea si la posicion es valida, se deberia hacer previamente.
casilleroLibre(pos(F,C),T) :- nth0(F,T,F1), nth0(C,F1,X), var(X).

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

vecinoAux(pos(F,C),_,pos(F1,C)) :- F > 0, F1 is F-1.
vecinoAux(pos(F,C),_,pos(F,C1)) :- C > 0, C1 is C-1.
vecinoAux(pos(F,C),T,pos(F1,C)) :- numFilas(T,NF), F < NF, F1 is F+1.
vecinoAux(pos(F,C),T,pos(F,C1)) :- numCol(T,NC), C < NC, C1 is C+1.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(pos(F,C),T,V) :- vecino(pos(F,C),T,V), casilleroLibre(V,T).

vecinoEnOrden(pos(FS,CS),pos(FE,CE),T,V) :- X is FS - FE, Y is CS - CE,vecinoEnOrdenAux(pos(FS,CS),X,Y,T,V).
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(NS,CS)) :- X < 0, NS is FS +1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(NS,CS)) :- X > 0, NS is FS -1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(FS,NS)) :- Y < 0, NS is CS +1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(FS,NS)) :- Y > 0, NS is CS -1.

vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(NS,CS)) :- X =< 0, NS is FS -1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(NS,CS)) :- X >= 0, NS is FS +1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(FS,NS)) :- Y =< 0, NS is CS -1.
vecinoEnOrdenAux(pos(FS,CS),X,Y,T,pos(FS,NS)) :- Y >= 0, NS is CS +1.


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

%%Checkeamos posValida o no vale la pena? No vale la pena, se chekea en vecinosLibres. Santi: Estas seguro? Me parece que va este eh
%%camino(pos(FS,CS),pos(FF,CF),T,C) :- posValida(pos(FS,CS),T), posValida(pos(FF,CF),T), armarCamino(pos(FS,CS),pos(FF,CF),T,C,[]).
%%Sino camino(pos(100,100), pos(100,100), [], C) da [pos(100,100)]
camino(S,F,T,C) :- armarCamino(F,S,T,C,[]).

%%ArmarCamino(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino(pos(SX,SY),pos(SX,SY),T,[pos(SX,SY) | X],X).
armarCamino(S,F,T,C,X) :- vecinoLibre(S,T,V), not(member(V,X)), armarCamino(V,F,T,C,[S|X]).

%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(S,F,T,N) :- ground(N), aggregate_all(contador, camino(S,F,T,_), N2), N = N2.
cantidadDeCaminos(S,F,T,N) :- var(N), aggregate_all(contador, camino(S,F,T,_), N).

%% Creo que se puede unificar en:
%% cantidadDeCaminos(S,F,T,N) :- aggregate_all(contador, camino(S,F,T,C), N2), N is N2.
%% pero no estoy seguro

%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud. NO HACE ESTO!!! es una heuristica
%% que trata pero no es perfecta
camino2(S,F,T,C) :- posValida(S,T), posValida(F,T),armarCamino2F2(F,S,T,C,[]).

%%VERSION SANTI
%%armarCamino2S(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)
armarCamino2S(pos(SX,SY),pos(FX,FY),_,C,X) :- SX = FX, SY = FY, C is [pos(FX,FY) | X].
armarCamino2S(pos(SX,SY),pos(FX,FY),T,C,X) :- SX < FX, V is pos(SX+1,SY), continuar(pos(SX,SY),pos(FX,FY),T,C,X,V).
armarCamino2S(pos(SX,SY),pos(FX,FY),T,C,X) :- SX > FX, V is pos(SX-1,SY), continuar(pos(SX,SY),pos(FX,FY),T,C,X,V).
armarCamino2S(pos(SX,SY),pos(FX,FY),T,C,X) :- SX = FX, SY < FY, V is pos(SX,SY+1), continuar(pos(SX,SY),pos(FX,FY),T,C,X,V).
armarCamino2S(pos(SX,SY),pos(FX,FY),T,C,X) :- SX = FX, SY > FY, V is pos(SX,SY-1), continuar(pos(SX,SY),pos(FX,FY),T,C,X,V).
%%continua con algun vecino (como el original)
armarCamino2S(pos(SX,SY),pos(FX,FY),T,C,X) :- vecinoLibre(S,T,V), not(member(V,X)), armarCamino2S(V,F,T,C,[S|X]).

%%continuar(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial, +Vecino)
continuar(S,F,T,C,X,V) :- casilleroLibre(V,T), not(member(V,X)), armarCamino2S(V,F,T,C,[S|X]).

%%No se si se puede hacer asi, o hay que hacer "X is [], armarCamino2(S,F,T,C,X)."
%%camino2(S,F,T,C) :- posValida(S,T), posValida(F,T), armarCamino2(F,S,T,C,[]).

%%ArmarCamino2(+Start, +Finish, +Tablero, -CaminoFinal, +CaminoParcial)

%% VERSION FEDE
%%Caso base, llegue al nodo final
armarCamino2F(pos(FX,FY),pos(FX,FY),_,[pos(FX,FY) | X],X).

%%Caso inductivo, entro primero hacia la direccion q convenga segun distancia Manhattan.
%%Si alguna coordenada es igual entre S y F entrare en ambos casos de esa coordenada aqui
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SX =< FX, SY \= FY, NX is SX+1,movValido(pos(NX,SY),T,X), armarCamino2F(pos(NX,SY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SX >= FX, SY \= FY, NX is SX-1,movValido(pos(NX,SY),T,X), armarCamino2F(pos(NX,SY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SY =< FY, SX \= FX, NY is SY+1,movValido(pos(SX,NY),T,X), armarCamino2F(pos(SX,NY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SY >= FY, SX \= FX, NY is SY-1,movValido(pos(SX,NY),T,X), armarCamino2F(pos(SX,NY),pos(FX,FY),T,C,[pos(SX,SY)|X]).

%%Por ultima voy hacia el lado apuesto de F, ya que tengo q generar todos los casos igual.
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SX > FX, SY \= FY,NX is SX+1,movValido(pos(NX,SY),T,X), armarCamino2F(pos(NX,SY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SX < FX, SY \= FY,NX is SX-1,movValido(pos(NX,SY),T,X), armarCamino2F(pos(NX,SY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SY > FY, SX \= FX,NY is SY+1,movValido(pos(SX,NY),T,X), armarCamino2F(pos(SX,NY),pos(FX,FY),T,C,[pos(SX,SY)|X]).
armarCamino2F(pos(SX,SY),pos(FX,FY),T,C,X) :- SY < FY, SX \= FX,NY is SY-1,movValido(pos(SX,NY),T,X), armarCamino2F(pos(SX,NY),pos(FX,FY),T,C,[pos(SX,SY)|X]).


armarCamino2F2(pos(SX,SY),pos(SX,SY),T,[pos(SX,SY) | X],X).
armarCamino2F2(S,F,T,C,X) :- vecinoEnOrden(S,F,T,V), movValido(V,T,X),armarCamino2F2(V,F,T,C,[S|X]).





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
