

##  Predicados de Consulta y Pertenencia

*Estos predicados se usan para revisar qué hay dentro de una lista o en qué posición.*

### `member/2`

  * **Sintaxis:** `member(Elemento, Lista)`
  * **Descripción:** Es verdadero si `Elemento` es un miembro (pertenece) a la `Lista`. Es el predicado de "pertenencia" por excelencia.
  * **Ejemplo:**
    ```prolog
    ?- member(b, [a, b, c]).
    true.

    ?- member(X, [1, 2]).
    X = 1 ;
    X = 2.
    ```

### `nth0/3`

  * **Sintaxis:** `nth0(Index, Lista, Elemento)`
  * **Descripción:** Es verdadero si `Elemento` está en la `Lista` en la posición `Index`, usando **indexación basada en cero** (el primer elemento es el 0).
  * **Ejemplo:**
    ```prolog
    ?- nth0(1, [a, b, c], Elem).
    Elem = b.

    ?- nth0(I, [a, b], b).
    I = 1.
    ```

### `nth1/3`

  * **Sintaxis:** `nth1(Index, Lista, Elemento)`
  * **Descripción:** Idéntico a `nth0/3`, pero usa **indexación basada en uno** (el primer elemento es el 1).
  * **Ejemplo:**
    ```prolog
    ?- nth1(1, [a, b, c], Elem).
    Elem = a.
    ```

### `last/2`

  * **Sintaxis:** `last(Lista, UltimoElemento)`
  * **Descripción:** Es verdadero si `UltimoElemento` es el último elemento de la `Lista`.
  * **Ejemplo:**
    ```prolog
    ?- last([a, b, c], E).
    E = c.
    ```

-----

##  Predicados de Medición y Estructura

*Estos predicados te dan información sobre la lista en sí, como su tamaño.*

### `length/2`

  * **Sintaxis:** `length(Lista, Longitud)`
  * **Descripción:** Es verdadero si `Longitud` es el número de elementos en la `Lista`.
  * **Ejemplo:**
    ```prolog
    ?- length([a, b, c], L).
    L = 3.

    ?- length(MiLista, 2).
    MiLista = [_, _]. % Crea una lista de 2 variables
    ```

### `flatten/2`

  * **Sintaxis:** `flatten(ListaAnidada, ListaPlana)`
  * **Descripción:** "Aplana" una lista que contiene otras listas, creando una sola lista con todos los elementos.
  * **Ejemplo:**
    ```prolog
    ?- flatten([a, [b, c], [d, [e]]], R).
    R = [a, b, c, d, e].
    ```

-----

##  Predicados de Manipulación y Transformación

*Estos predicados crean listas nuevas a partir de listas existentes.*

### `append/3`

  * **Sintaxis:** `append(Lista1, Lista2, ListaResultado)`
  * **Descripción:** Es verdadero si `ListaResultado` es la concatenación (unión) de `Lista1` y `Lista2`. Es el predicado más versátil.
  * **Ejemplo:**
    ```prolog
    % Unir listas
    ?- append([a, b], [c, d], R).
    R = [a, b, c, d].

    % Partir listas
    ?- append(Inicio, Fin, [1, 2, 3]).
    Inicio = [], Fin = [1, 2, 3] ;
    Inicio = [1], Fin = [2, 3] ;
    Inicio = [1, 2], Fin = [3] ;
    Inicio = [1, 2, 3], Fin = [].
    ```

### `reverse/2`

  * **Sintaxis:** `reverse(ListaOriginal, ListaInvertida)`
  * **Descripción:** Invierte el orden de los elementos en la lista.
  * **Ejemplo:**
    ```prolog
    ?- reverse([1, 2, 3], R).
    R = [3, 2, 1].
    ```

### `delete/3`

  * **Sintaxis:** `delete(Lista, Elemento, ListaResultado)`
  * **Descripción:** Elimina **todas** las ocurrencias de `Elemento` de la `Lista` para crear `ListaResultado`.
  * **Ejemplo:**
    ```prolog
    ?- delete([a, b, a, c], a, R).
    R = [b, c].
    ```

### `select/3`

  * **Sintaxis:** `select(Elemento, Lista, Resto)`
  * **Descripción:** Similar a `delete/3`, pero solo elimina **una** ocurrencia a la vez. Es útil para generar permutaciones. Si hay duplicados, puede dar múltiples soluciones.
  * **Ejemplo:**
    ```prolog
    ?- select(b, [a, b, c, b], R).
    R = [a, c, b] ;  % Eliminó la primera 'b'
    R = [a, b, c].   % Eliminó la segunda 'b'
    ```

-----

##  Predicados de Ordenamiento y Filtrado

*Estos predicados ordenan listas o seleccionan subconjuntos de ellas.*

### `sort/2`

  * **Sintaxis:** `sort(Lista, ListaOrdenada)`
  * **Descripción:** Ordena la `Lista` y **elimina duplicados**.
  * **Ejemplo:**
    ```prolog
    ?- sort([c, a, f, a, b], R).
    R = [a, b, c, f]. % Solo queda una 'a'
    ```

### `msort/2`

  * **Sintaxis:** `msort(Lista, ListaOrdenada)`
  * **Descripción:** Ordena la `Lista` pero **mantiene los duplicados**. (Viene de "Merge Sort").
  * **Ejemplo:**
    ```prolog
    ?- msort([c, a, f, a, b], R).
    R = [a, a, b, c, f]. % Mantiene ambas 'a'
    ```

### `include/3`

  * **Sintaxis:** `include(Objetivo, Lista, SubLista)`
  * **Descripción:** Crea una `SubLista` que contiene solo los elementos de la `Lista` que cumplen con el `Objetivo` (que es otro predicado).
  * **Ejemplo:** (Requiere un predicado auxiliar)
    ```prolog
    es_par(N) :- N mod 2 =:= 0.

    ?- include(es_par, [1, 2, 3, 4], Pares).
    Pares = [2, 4].
    ```

### `exclude/3`

  * **Sintaxis:** `exclude(Objetivo, Lista, SubLista)`
  * **Descripción:** Es lo opuesto a `include/3`. Crea una `SubLista` con los elementos que **no** cumplen el `Objetivo`.
  * **Ejemplo:**
    ```prolog
    es_par(N) :- N mod 2 =:= 0.

    ?- exclude(es_par, [1, 2, 3, 4], Impares).
    Impares = [1, 3].
    ```

-----

##  Predicados Numéricos para Listas

*Estos son comunes en muchas implementaciones de Prolog (como SWI-Prolog) para operaciones matemáticas.*

### `sum_list/2`

  * **Sintaxis:** `sum_list(ListaNumeros, Suma)`
  * **Descripción:** Es verdadero si `Suma` es la suma de todos los números en `ListaNumeros`.
  * **Ejemplo:**
    ```prolog
    ?- sum_list([1, 2, 3], S).
    S = 6.
    ```

### `max_list/2`

  * **Sintaxis:** `max_list(ListaNumeros, Maximo)`
  * **Descripción:** Es verdadero si `Maximo` es el elemento más grande de la `ListaNumeros`.
  * **Ejemplo:**
    ```prolog
    ?- max_list([5, 1, 9, 3], M).
    M = 9.
    ```

### `min_list/2`

  * **Sintaxis:** `min_list(ListaNumeros, Minimo)`
  * **Descripción:** Es verdadero si `Minimo` es el elemento más pequeño de la `ListaNumeros`.
  * **Ejemplo:**
    ```prolog
    ?- min_list([5, 1, 9, 3], M).
    M = 1.
    ```
