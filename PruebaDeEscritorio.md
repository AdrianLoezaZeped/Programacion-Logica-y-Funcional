# Prueba de Escritorio
## Codigo

```
replace0([I|Index], Input, N, Resp, R):-
    nth0(I, Input, Atom),
    length(Index, M), M > 0,
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R), !.
```
### Datos para la Prueba de Escritorio

Para realizar la prueba, asignaremos valores a las variables, simulando una ejecución real del chatbot Eliza procesando una frase.

  * **`Input` (Lo que escribe el usuario):** `['tengo', 'sueño', 'mucho']`
  * **`[I|Index]` (Índices a reemplazar):** `[1, 2]` (Queremos tomar "sueño" y "mucho").
  * **`N` (Contador de placeholders):** `1`
  * **`Resp` (Plantilla seleccionada):** `['por', 'que', 1, 'tan', 2]`

-----

### 1\. Prueba de Escritorio 


**Llamada Inicial:** `replace0([1, 2], ['tengo', 'sueño', 'mucho'], 1, ['por', 'que', 1, 'tan', 2], R).`

| Línea | Código | Estado de las Variables | Descripción de lo que ocurre |
| :--- | :--- | :--- | :--- |
| **1** | `replace0([I\|Index], ...)` | `I` = 1<br>`Index` = `[2]` | **Unificación:** Se separa la cabeza de la lista (`1`) del resto (`[2]`). |
| **2** | `nth0(I, Input, Atom)` | `I` = 1<br>`Input` = `['tengo', 'sueño', '...']`<br>`Atom` = `'sueño'` | Busca en la posición 1 de la lista Input y guarda la palabra en `Atom`. |
| **3** | `length(Index, M)` | `Index` = `[2]`<br>`M` = 1 | Calcula la longitud de la lista restante (`[2]`). El resultado es 1. |
| **4** | `M > 0` | `1 > 0` | **Evalúa VERDADERO**. Como hay elementos pendientes en la cola, continúa. |
| **5** | `select(N, Resp, Atom, R1)` | `N` = 1<br>`Resp` = `[..., 1, ..., 2]`<br>`Atom` = `'sueño'` | Busca el número `1` en la plantilla y lo cambia por `'sueño'`.<br>**Resultado R1:** `['por', 'que', 'sueño', 'tan', 2]` |
| **6** | `N1 is N + 1` | `N` = 1<br>`N1` = 2 | Suma 1 al contador. Ahora buscará el placeholder número 2. |
| **7** | `replace0(Index, ...)` | **Llamada Recursiva** | Se llama de nuevo a la regla con los nuevos valores: `replace0([2], Input, 2, R1, R)`. |
| **---** | **Segunda Vuelta (Iteración)** | **Entrada a la recursión** | ------------------------------------------------------------ |
| **8** | `replace0([I\|Index], ...)` | `I` = 2<br>`Index` = `[]` (vacío) | **Unificación:** Se separa la cabeza (`2`) del resto (lista vacía). |
| **9** | `nth0(I, Input, Atom)` | `I` = 2<br>`Atom` = `'mucho'` | Busca en la posición 2 del Input. Encuentra `'mucho'`. |
| **10** | `length(Index, M)` | `Index` = `[]`<br>`M` = 0 | Calcula la longitud de la lista restante (que ahora está vacía). El resultado es 0. |
| **11** | `M > 0` | `0 > 0` | **Evalúa FALSO**. |
| **12** | **Resultado** | **FAIL (Fallo)** | Al no cumplirse la condición `M > 0`, la regla se detiene y retorna **Falso** en este punto (no devuelve valor en `R`). |

-----

### 2\. Funcionamiento

**Objetivo General:**
Este predicado intenta recorrer una lista de índices numéricos (`[I|Index]`), extraer las palabras correspondientes de una lista de entrada (`Input`), e insertarlas secuencialmente en una plantilla de respuesta (`Resp`) reemplazando los números `1`, `2`, `3`, etc.

**Análisis por Línea:**

1.  **`nth0(I, Input, Atom)`**:

      * Es un "extractor". Utiliza el número `I` (que viene de tu lista de índices) para ir a la lista `Input` (lo que dijo el usuario) y "pescar" la palabra exacta que está en esa posición. Esa palabra se guarda temporalmente en la variable `Atom`.

2.  **`length(Index, M), M > 0`**:

      * Es un "control de flujo" o "guardian".
      * `length` cuenta cuántos índices quedan pendientes en la cola (`Index`).
      * `M > 0` verifica que **aún queden elementos por procesar después del actual**.
      * *Lógica:* Esta línea fuerza a que el código solo se ejecute si NO estamos en el último elemento de la lista.

3.  **`select(N, Resp, Atom, R1)`**:

      * Es el "sustituto". Busca el número entero `N` (empezando por 1) dentro de la lista `Resp`. Cuando lo encuentra, lo quita y pone en su lugar el valor de `Atom` (la palabra que extrajimos antes). El resultado parcial se guarda en `R1`.

4.  **`N1 is N + 1`**:

      * Es el "contador". Prepara el siguiente número a buscar. Si acabamos de reemplazar el `1`, ahora preparará el `2` para la siguiente vuelta.

5.  **`replace0(Index, Input, N1, R1, R), !.`**:

      * Es el "bucle" (Recursividad). Vuelve a llamar a la función para repetir el proceso con el resto de la lista.
      * El símbolo `!` (cut) sirve para optimizar: le dice a Prolog "si llegaste hasta aquí y tuviste éxito, no intentes buscar otras formas de resolver esto, quédate con este camino".
