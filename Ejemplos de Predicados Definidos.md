# 10 ejemplos de predicados definidos
### 1. `append/3`
* **Descripción:** Es el predicado más famoso para listas. Concatena `Lista1` y `Lista2` para formar `ListaResultado`. También puede usarse "al revés" para descomponer listas.
* **Ejemplos:**
    * `?- append([1, 2], [3, 4], L).`
    * `L = [1, 2, 3, 4].`
    *
    * `?- append(Inicio, [c, d], [a, b, c, d]).`
    * `Inicio = [a, b].`

### 2. `member/2`
* **Descripción:** Verifica si un `Elemento` es miembro de una `Lista`. También puede usarse para generar (en backtracking) todos los elementos de una lista.
* **Ejemplos:**
    * `?- member(c, [a, b, c]).`
    * `true.`
    *
    * `?- member(X, [1, 2, 3]).`
    * `X = 1 ;`
    * `X = 2 ;`
    * `X = 3.`

### 3. `select/3`
* **Descripción:** Es como `member/2`, pero "saca" el `Elemento` de la `Lista` y te devuelve el `Resto` de la lista sin ese elemento. Es muy útil para hacer permutaciones o "tomar" un elemento.
* **Ejemplos:**
    * `?- select(b, [a, b, c, b], Resto).`
    * `Resto = [a, c, b] ;` (Encuentra la primera 'b')
    * `Resto = [a, b, c].` (Encuentra la segunda 'b')

### 4. `nth0/3`
* **Descripción:** Relaciona un `Índice` (basado en 0) con un `Elemento` dentro de una `Lista`. `nth1/3` hace lo mismo pero con índices basados en 1.
* **Ejemplos:**
    * `?- nth0(2, [pan, leche, huevos], Elem).`
    * `Elem = huevos.` (Índice 0: pan, Índice 1: leche, Índice 2: huevos)
    *
    * `?- nth0(Indice, [a, b, c], b).`
    * `Indice = 1.`

### 5. `length/2`
* **Descripción:** Relaciona una `Lista` con su `Longitud` (un número entero).
* **Ejemplos:**
    * `?- length([a, b, c], N).`
    * `N = 3.`
    *
    * `?- length(L, 3).` (Uso generativo)
    * `L = [_G1, _G2, _G3].` (Crea una lista de 3 variables anónimas)

### 6. `reverse/2`
* **Descripción:** Invierte el orden de los elementos en `Lista1` y los pone en `ListaInvertida`.
* **Ejemplo:**
    * `?- reverse([1, 2, 3, 4], L).`
    * `L = [4, 3, 2, 1].`

### 7. `sort/2`
* **Descripción:** Ordena los elementos de `ListaDesordenada` y produce `ListaOrdenada`. **Importante:** `sort/2` también elimina elementos duplicados. Si no quieres eliminar duplicados, usa `msort/2`.
* **Ejemplo:**
    * `?- sort([3, 1, 4, 1, 5, 9, 2], L).`
    * `L = [1, 2, 3, 4, 5, 9].`

### 8. `findall/3`
* **Descripción:** Es un meta-predicado para recolectar resultados. Encuentra todas las soluciones posibles para un `Objetivo` (un predicado) y las guarda en una `Lista`.
* **Ejemplo:** (Asumiendo que tienes estos hechos: `numero(1). numero(2). numero(3).`)
    * `?- findall(X, numero(X), Todos).`
    * `Todos = [1, 2, 3].`

### 9. `is/2`
* **Descripción:** Es el operador fundamental para la aritmética. Evalúa la `ExpresiónAritmética` de la derecha y unifica el `Resultado` (numérico) con la variable de la izquierda. No es lo mismo que `=` (unificación).
* **Ejemplo:**
    * `?- X is 5 + 2.`
    * `X = 7.`
    *
    * `?- Y = 5 + 2.` (¡Diferente!)
    * `Y = 5+2.` (Unifica con la estructura, no con el resultado)

### 10. `atom/1`
* **Descripción:** Es un predicado de chequeo de tipo. Tiene éxito si `Termino` es un átomo (una constante de texto, no un número, no una variable, no una lista).
* **Ejemplos:**
    * `?- atom(hola).`
    * `true.`
    *
    * `?- atom(123).`
    * `false.`
    *
    * `?- atom('Esto es un atomo').`
    * `true.`
    *
    * `?- atom(X).` (Si X no está instanciada)
    * `false.`
