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


### 1.4 Ejercicio 2: Verificar si un elemento pertenece a una lista
───────────────────────────────────────────────────────────────

  Crea un predicado `pertenece/2` que determine si un elemento se encuentra en una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- pertenece(b, [a,b,c]).
  
  └────


### 1.5 Ejercicio 3: Calcular la longitud de una lista
──────────────────────────────────────────────────

  Define un predicado `longitud/2` que devuelva la cantidad de elementos de una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- longitud([a,b,c,d], N).
  
  └────


### 1.6 Ejercicio 4: Concatenar dos listas
──────────────────────────────────────
  Crea el predicado `concatenar/3` que una dos listas.  
  
  Consulta:
  
  ┌────
  
  │ ?- concatenar([1,2], [3,4], R).
  
  └────


### 1.7 Ejercicio 5: Invertir una lista
───────────────────────────────────

  Escribe un predicado `invertir/2` que invierta el orden de los elementos.  
  
  Consulta:
  
  ┌────
  
  │ ?- invertir([a,b,c,d], R).
  
  └────


### 1.8 Ejercicio 6: Obtener el último elemento
───────────────────────────────────────────

  Crea el predicado `ultimo/2` que retorne el último elemento de una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- ultimo([a,b,c,d], X).
  
  └────


### 1.9 Ejercicio 7: Sumar los elementos de una lista numérica
──────────────────────────────────────────────────────────
  Define un predicado `suma_lista/2` que calcule la suma de los elementos de una lista de números.  
  
  Consulta:
  
  ┌────
  
  │ ?- suma_lista([2,4,6,8], S).
  
  └────


### 1.10 Ejercicio 8: Eliminar un elemento de una lista
───────────────────────────────────────────────────

  Crea un predicado `eliminar/3` que elimine la primera aparición de un elemento en una lista.  
  
  Consulta:
  
  ┌────
  
  │ ?- eliminar(c, [a,b,c,d,c], R).
  
  └────


### 1.11 Ejercicio 9: Duplicar los elementos de una lista
─────────────────────────────────────────────────────

  Escribe un predicado `duplicar/2` que duplique cada elemento de una lista. 
  
  Consulta:
  
  ┌────
  
  │ ?- duplicar([a,b,c], R).
  
  └────


### 1.12 Ejercicio 10: Intercalar dos listas
────────────────────────────────────────

  Crea un predicado `intercalar/3` que mezcle los elementos de dos listas alternándolos.  
  
  Consulta:
  
  ┌────
  
  │ ?- intercalar([1,3,5], [2,4,6], R).
  
  └────
