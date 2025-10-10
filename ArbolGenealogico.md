# Arbol Genealogico
### Abuelos
*	Filiberto
*	Eugenia
*	Mela
*	Eustacio
### Tios
* Luna
*	Tavo
*	Gabriel
*	Luis
*	Catalina
*	Fatima
*	Martha
### Primos
*	LuisEd
*	Alan 
*	Santy
*	Andrea
*	Tavito
*	barbara
### Papas
*	Sol 
*	Baltazar
### Hermano
* BaltazarJr

## Codigo
```
(defparameter *familia*
 '((masculino
      (filiberto
         (rol abuelo)
         (estatura baja)
         (pelo castano)
         (conyuge eugenia)
         (hijos (sol luna gabriel tavo luis)))
      (eustacio
         (rol abuelo)
         (conyuge mela)
         (estatura baja)
         (pelo negro)
         (hijos (baltazar catalina)))
      (tavo
         (rol tio)
         (estatura alto)
         (pelo negro)
         (conyuge fatima)
         (hijos (tavito)))
      (gabriel
         (rol tio)
         (conyuge no)
         (estatura alta)
         (pelo negro)
         (hijos (no)))
    (luis
         (rol tio)
         (estatura baja)
         (pelo negro)
         (conyuge martha)
         (hijos (alan luisEd)))
      (luisEd
         (rol primo)
         (conyuge no)
         (estatura baja)
         (pelo pelirojo)
         (hijos (santiago barbara)))
    (alan
         (rol primo)
         (estatura baja)
         (pelo negro)
         (conyuge no)
         (hijos (no)))
      (santiago
         (rol primo)
         (conyuge no)
         (estatura alto)
         (pelo pelirojo)
         (hijos (no)))
    (tavito
         (rol sobrino)
         (estatura baja)
         (pelo castano)
         (conyuge no)
         (hijos (no)))
      (baltazar
         (rol padre)
         (conyuge sol)
         (estatura alto)
         (pelo negro)
         (hijos (adrian baltazarJr)))
        (adrian
         (rol hijo)
         (estatura alto)
         (pelo negro)
         (conyuge no)
         (hijos (no)))
      (baltazarJr
         (rol hermano)
         (conyuge no)
         (estatura alto)
         (pelo negro)
         (hijos (no)))
      )
   (femenino
      (eugenia
         (rol abue)
         (estatura baja)
         (pelo castano)
         (conyuge filiberto)
         (hijos (sol tavo gabriel luis luna)))
      (mela
         (rol abue)
         (estatura baja)
         (pelo negro)
         (conyuge eustacio)
         (hijos (baltazar catalina)))
       (luna
         (rol tia)
         (estatura alta)
         (pelo castano)
         (conyuge no)
         (hijos (andrea)))
      (catalina
         (rol tia)
         (estatura baja)
         (pelo pelirojo)
         (conyuge no)
         (hijos (no)))
      (fatima
         (rol tia)
         (estatura baja)
         (pelo negro)
         (conyuge tavo)
         (hijos (tavito)))
      (martha
         (rol tia)
         (estatura baja)
         (pelo castano)
         (conyuge luis)
         (hijos (alan luisEd)))
       (andrea
         (rol prima)
         (estatura alta)
         (pelo guero)
         (conyuge no)
         (hijos (no)))
      (barbara
         (rol prima)
         (estatura baja)
         (pelo pelirojo)
         (conyuge no)
         (hijos (no)))
      )))

```
## Arbol Genealogico en SWIProlog
```
hombre(filiberto).
hombre(eustacio).
hombre(tavo).
hombre(gabriel).
hombre(luis).
hombre(luisEd).
hombre(alan).
hombre(santiago).
hombre(tavito).
hombre(baltazar).
hombre(adrian).
hombre(baltazarJr).

mujer(eugenia).
mujer(mela).
mujer(luna).
mujer(catalina).
mujer(fatima).
mujer(martha).
mujer(andrea).
mujer(barbara).
mujer(sol).

padre(filiberto, sol).
padre(filiberto, luna).
padre(filiberto, gabriel).
padre(filiberto, tavo).
padre(filiberto, luis).
padre(eustacio, baltazar).
padre(eustacio, catalina).
padre(luis, alan).
padre(luis, luisEd).
padre(luisEd, santiago).
padre(luisEd, barbara).
padre(tavo, tavito).
padre(baltazar, adrian).
padre(baltazar, baltazarJr).

madre(eugenia, sol).
madre(eugenia, luna).
madre(eugenia, gabriel).
madre(eugenia, tavo).
madre(eugenia, luis).
madre(mela, baltazar).
madre(mela, catalina).
madre(martha, alan).
madre(martha, luisEd).
madre(fatima, tavito).
madre(sol, adrian).
madre(sol, baltazarJr).
madre(luna, andrea).

% REGLAS DERIVADAS (CORREGIDAS Y ROBUSTAS)
hijo_o_hija(Hijo, Padre_o_Madre) :-
    padre(Padre_o_Madre, Hijo) ; 
    madre(Padre_o_Madre, Hijo).

% 1. Hermano: Regla original que encuentra hermanos completos (duplica, pero es lógicamente correcta)
hermano(P1, P2):-
    (padre(P, P1), padre(P, P2) ; madre(M, P1), madre(M, P2)),
    P1 \= P2.

% 2. Abuelo: Encuentra a un progenitor intermedio (P_o_M) que es hijo de Abuelo
abuelo(Abuelo, Nieto):-
    hombre(Abuelo),
    (padre(P_o_M, Nieto) ; madre(P_o_M, Nieto)), % P_o_M es padre/madre del Nieto
    padre(Abuelo, P_o_M). % Abuelo es padre de P_o_M (único camino por padre)

% 3. Abuela: Encuentra a un progenitor intermedio (P_o_M) que es hijo de Abuela
abuela(Abuela, Nieto):-
    mujer(Abuela),
    (padre(P_o_M, Nieto) ; madre(P_o_M, Nieto)),
    madre(Abuela, P_o_M). % Abuela es madre de P_o_M (único camino por madre)

% 4. Tío/Tía: Buscamos al progenitor del sobrino (P_o_M) y luego a sus hermanos (Tio/Tia)
tio(Tio, Sobrino):-
    hombre(Tio),
    (padre(P_o_M, Sobrino) ; madre(P_o_M, Sobrino)), % Progenitor del Sobrino
    hermano(Tio, P_o_M). % Tío es hermano de P_o_M

tia(Tia, Sobrino):-
    mujer(Tia),
    (padre(P_o_M, Sobrino) ; madre(P_o_M, Sobrino)),
    hermano(Tia, P_o_M).

% 5. Primo: Se basa en la regla hermano (P_o_M)
primo(P1, P2):-
    hijo_o_hija(P1, Parent1), % Parent1 es padre/madre de P1
    hijo_o_hija(P2, Parent2), % Parent2 es padre/madre de P2
    hermano(Parent1, Parent2), % Parent1 y Parent2 son hermanos
    P1 \= P2.
```
