#  Práctica: Listas en Prolog
.. 1. Objetivo

.. 2. Instrucciones generales

.. 3. Ejercicio 1: Cabeza y cola de una lista

.. 4. Ejercicio 2: Verificar si un elemento pertenece a una lista

.. 5. Ejercicio 3: Calcular la longitud de una lista

.. 6. Ejercicio 4: Concatenar dos listas

.. 7. Ejercicio 5: Invertir una lista

.. 8. Ejercicio 6: Obtener el último elemento

.. 9. Ejercicio 7: Sumar los elementos de una lista numérica

.. 10. Ejercicio 8: Eliminar un elemento de una lista

.. 11. Ejercicio 9: Duplicar los elementos de una lista

.. 12. Ejercicio 10: Intercalar dos listas


## Práctica: Listas en Prolog
════════════════════════════

## Objetivo
────────────

  Aprender a manipular listas en Prolog mediante ejercicios básicos de
  definición, acceso, recursión y construcción de predicados.


## Instrucciones generales
───────────────────────────


### 1.3 Ejercicio 1: Cabeza y cola de una lista
───────────────────────────────────────────

  Define un predicado que permita obtener la cabeza y la cola de una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- cabeza_y_cola([a,b,c,d], C, T).
  
  └────
  ```
cabeza_y_cola([C | R], C, R).
  ```
Prueba de escritorio

Llamada: cabeza_y_cola([a,b,c,d], C, T).

Unificación con la cláusula: C = a, R = [b,c,d].

Resultado final: C = a, T = [b,c,d].

### 1.4 Ejercicio 2: Verificar si un elemento pertenece a una lista
───────────────────────────────────────────────────────────────

  Crea un predicado `pertenece/2` que determine si un elemento se encuentra en una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- pertenece(b, [a,b,c]).
  
  └────
  ```
  pertenece(X, [X|_]).
  pertenece(X, [_|T]) :- pertenece(X, T).
  
  ```
Prueba de escritorio

Llamada: pertenece(b, [a,b,c])

Primera cláusula falla (a = b). Segunda cláusula: se llama pertenece(b, [b,c]).

Ahora primera cláusula unifica y retorna true.

### 1.5 Ejercicio 3: Calcular la longitud de una lista
──────────────────────────────────────────────────

  Define un predicado `longitud/2` que devuelva la cantidad de elementos de una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- longitud([a,b,c,d], N).
  
  └────
  ```
  longitud([], 0).
  longitud([_|T], N) :-
      longitud(T, N1),
      N is N1 + 1.
  ```
Prueba de escritorio

Llamadas recursivas: longitud([a,b,c,d],N) -> longitud([b,c,d],N1) -> ... -> longitud([],0).

Subida: N1=0 -> N=1 -> N=2 -> N=3 -> N=4.

### 1.6 Ejercicio 4: Concatenar dos listas
──────────────────────────────────────
  Crea el predicado `concatenar/3` que una dos listas.  
  
  Consulta:
  
  ┌────
  
  │ ?- concatenar([1,2], [3,4], R).
  
  └────
  ```
  concatenar([], L, L).
  concatenar([H|T], L2, [H|R]) :-
      concatenar(T, L2, R).
  ```
Prueba de escritorio

concatenar([1,2],[3,4],R) produce R = [1|R2] con R2 = concatenar([2],[3,4],R2) ...

Resultado final: [1,2,3,4].


### 1.7 Ejercicio 5: Invertir una lista
───────────────────────────────────

  Escribe un predicado `invertir/2` que invierta el orden de los elementos.  
  
  Consulta:
  
  ┌────
  
  │ ?- invertir([a,b,c,d], R).
  
  └────
  ```
  invertir(L, R) :- invertir_acc(L, [], R).
  
  invertir_acc([], Acc, Acc).
  invertir_acc([H|T], Acc, R) :-
      invertir_acc(T, [H|Acc], R).
  ```
Prueba de escritorio 

invertir_acc([a,b,c,d],[],R) ->
invertir_acc([b,c,d],[a],R) -> ... -> invertir_acc([], [d,c,b,a], R) => R = [d,c,b,a].

### 1.8 Ejercicio 6: Obtener el último elemento
───────────────────────────────────────────

  Crea el predicado `ultimo/2` que retorne el último elemento de una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- ultimo([a,b,c,d], X).
  
  └────
  ```
  ultimo([X], X).
  ultimo([_|T], X) :- ultimo(T, X).
  ```
Prueba de escritorio

Recorrer la lista removiendo cabeza hasta que quede un elemento [d], entonces X = d.

### 1.9 Ejercicio 7: Sumar los elementos de una lista numérica
──────────────────────────────────────────────────────────
  Define un predicado `suma_lista/2` que calcule la suma de los elementos de una lista de números.  
  
  Consulta:
  
  ┌────
  
  │ ?- suma_lista([2,4,6,8], S).
  
  └────
  ```
  suma_lista([], 0).
  suma_lista([H|T], S) :-
      suma_lista(T, S1),
      S is H + S1.
  ```
Prueba de escritorio 

suma_lista([2,4,6,8],S) -> suma_lista([4,6,8],S1) ... -> suma_lista([],0).

Subida: 0+8=8, 8+6=14, 14+4=18, 18+2=20.

### 1.10 Ejercicio 8: Eliminar un elemento de una lista
───────────────────────────────────────────────────

  Crea un predicado `eliminar/3` que elimine la primera aparición de un elemento en una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- eliminar(c, [a,b,c,d,c], R).
  
    └────
  ```
  eliminar(_, [], []).
  eliminar(X, [X|T], T) :- !.   % elimina la primera aparición y corta (opcional)
  eliminar(X, [H|T], [H|R]) :-
      eliminar(X, T, R).
  ```
Prueba de escritorio 

Recorre hasta encontrar c: al encontrarla devuelve la cola sin esa cabeza; por ! no busca más versiones.

### 1.11 Ejercicio 9: Duplicar los elementos de una lista
─────────────────────────────────────────────────────

  Escribe un predicado `duplicar/2` que duplique cada elemento de una lista. 
  
  Consulta:
  
  ┌────
  
  │ ?- duplicar([a,b,c], R).
  
  └────
  ```
  duplicar([], []).
  duplicar([H|T], [H,H|R]) :-
      duplicar(T, R).
  ```
Prueba de escritorio

Para H=a produce [a,a|R1], luego duplica resto -> concatenación final [a,a,b,b,c,c].

### 1.12 Ejercicio 10: Intercalar dos listas
────────────────────────────────────────

  Crea un predicado `intercalar/3` que mezcle los elementos de dos listas alternándolos.  
  
  Consulta:
  
  ┌────
  
  │ ?- intercalar([1,3,5], [2,4,6], R).
  
  └────
  ```
  intercalar([], [], []).
  intercalar([H1|T1], [], [H1|R]) :-
      intercalar(T1, [], R).
  intercalar([], [H2|T2], [H2|R]) :-
      intercalar([], T2, R).
  intercalar([H1|T1], [H2|T2], [H1,H2|R]) :-
      intercalar(T1, T2, R).
  ```
Prueba de escritorio 

Paso 1: toma 1 y 2 -> [1,2|R1], luego intercalar([3,5],[4,6],R1) ...

Resultado final: [1,2,3,4,5,6].
