## Diferencia entre `if`, `cond`, `when` y `unless` 

Estas son todas estructuras de control condicional en Lisp, pero se usan en situaciones diferentes.

  * **`if`**: Es la estructura condicional más simple. Evalúa una condición: si es verdadera, ejecuta una acción; opcionalmente, si es falsa, puede ejecutar otra.

      * **Estructura**: `(if <condición> <acción-si-verdadero> [acción-si-falso])`
      * **Ejemplo**: `(if (> x 0) "Positivo" "No positivo")`

  * **`cond`**: Se usa cuando tienes múltiples condiciones que evaluar en secuencia. Es el equivalente a un "if-elseif-else" en otros lenguajes.

      * **Estructura**: `(cond (<condición1> <acción1>) (<condición2> <acción2>) ... (t <acción-por-defecto>))`
      * **Funcionamiento**: Evalúa cada condición en orden. Cuando encuentra la primera que es verdadera, ejecuta su acción correspondiente y se detiene. La cláusula `(t ...)` es para el caso por defecto (como un "else").

  * **`when`**: Es como un `if` pero sin la parte del "else". Si la condición es verdadera, ejecuta una o más acciones. Si es falsa, no hace nada y devuelve `NIL`.

      * **Estructura**: `(when <condición> <acción1> <acción2> ...)`
      * **Uso**: Muy útil cuando solo te interesa el caso verdadero y necesitas ejecutar varias expresiones.

  * **`unless`**: Es lo opuesto a `when`. Si la condición es falsa (`NIL`), ejecuta una o más acciones. Si es verdadera, no hace nada y devuelve `NIL`.

      * **Estructura**: `(unless <condición> <acción1> <acción2> ...)`
      * **Uso**: Perfecto para ejecutar código solo si una condición *no* se cumple.

## ¿Qué devuelven `car` y `cdr`?

`car` y `cdr` son las funciones fundamentales para acceder a los elementos de una lista.

  * **`car`**: Devuelve el **primer elemento** de una lista. Su nombre viene del acrónimo "Contents of the Address Register".

      * Ejemplo: `(car '(a b c))` devuelve `a`.

  * **`cdr`**: Devuelve **el resto de la lista** después del primer elemento. Es decir, una lista nueva sin el primer elemento. Su nombre viene de "Contents of the Decrement Register".

      * Ejemplo: `(cdr '(a b c))` devuelve `(b c)`.

**Combinaciones**: Para obtener elementos intermedios, puedes combinarlas. Por ejemplo, para obtener el segundo elemento ('b'), necesitas el *primer elemento* (`car`) del *resto de la lista* (`cdr`).

  * `(car (cdr '(a b c)))` -\> `(car '(b c))` -\> `b`.
  * Lisp tiene atajos para esto: `(cadr '(a b c))` es lo mismo que lo anterior. De igual forma, `(caddr lista)` te da el tercer elemento, `(cddr lista)` te da el resto de la lista a partir del tercer elemento, y así sucesivamente.

## Ejercicio 1: N-ésimo elemento con car/cdr 

[cite\_start]**Objetivo**: Crear una función `(n-esimo n lista)` que devuelva el elemento en la posición `n` usando solo `car` y `cdr`[cite: 9].

**Lógica**:
La forma más sencilla de hacerlo es con recursividad:

1.  **Caso Base**: Si `n` es 1, significa que queremos el primer elemento de la lista actual. Simplemente devolvemos `(car lista)`.
2.  **Caso Recursivo**: Si `n` es mayor que 1, significa que aún no hemos llegado al elemento deseado. Nos "movemos" un paso hacia la derecha en la lista llamando de nuevo a la función con `n-1` y con el resto de la lista `(cdr lista)`.

**Código**:

```lisp
(defun n-esimo (n lista)
  (if (= n 1)
      (car lista)
      (n-esimo (- n 1) (cdr lista))))
```
**Ejemplo explicado**: `(n-esimo 3 '(a b c d e))`

1.  `n=3`, no es 1. Se llama a `(n-esimo 2 '(b c d e))`.
2.  `n=2`, no es 1. Se llama a `(n-esimo 1 '(c d e))`.
3.  `n=1`. Se cumple el caso base. Se devuelve `(car '(c d e))`, que es `c`.

## Ejercicio 2: Filtrar positivos con when 

**Objetivo**: Crear una función `(filtra-positivos lista)` que devuelva solo los números positivos [cite: 12][cite\_start], usando `when`

**Lógica**:
La forma más idiomática en Common Lisp es usar `loop`.

1.  Recorremos cada `numero` en la `lista`.
2.  Para cada `numero`, usamos `when` para comprobar si es mayor que cero `(> numero 0)`.
3.  Si la condición es verdadera, la cláusula `collect` de `loop` lo añade a una nueva lista que se construye internamente.
4.  Al final, `loop` devuelve la lista recolectada.

**Código**:

```lisp
(defun filtra-positivos (lista)
  (loop for numero in lista
        when (> numero 0)
        collect numero))
```

**Ejemplo explicado**: `(filtra-positivos '(-2 0 3 -5 7))`

  * `numero` es -2. `(> -2 0)` es falso. No se hace nada.
  * `numero` es 0. `(> 0 0)` es falso. No se hace nada.
  * `numero` es 3. `(> 3 0)` es verdadero. `when` se activa y se recolecta `3`.
  * `numero` es -5. `(> -5 0)` es falso. No se hace nada.
  * `numero` es 7. `(> 7 0)` es verdadero. `when` se activa y se recolecta `7`.
  * El bucle termina y devuelve la lista de elementos recolectados: `(3 7)`.

## Ejercicio 3: Clasificación con cond 

**Objetivo**: Crear una función `(clasifica-numero n)` que devuelva una cadena de texto según el rango en el que se encuentre `n`.

**Lógica**:
Esta situación es perfecta para `cond`, ya que tenemos múltiples casos excluyentes.

1.  La primera cláusula comprueba si `n < 0`.
2.  La segunda, si `n = 0`.
3.  Las siguientes comprueban los rangos usando `and` para delimitar el inferior y el superior (ej: `(and (>= n 1) (<= n 10))`).
4.  La última comprueba si `n > 100`.

**Código**:

```lisp
(defun clasifica-numero (n)
  (cond
    ((< n 0) "Negativo")
    ((= n 0) "Cero")
    ((<= 1 n 10) "Pequeño")
    ((<= 11 n 100) "Mediano")
    ((> n 100) "Grande")))
```

*(Nota: En Lisp se puede escribir `(<= 11 n 100)` como atajo para `(and (>= n 11) (<= n 100))`)*

**Ejemplo explicado**: `(clasifica-numero 57)`

  * `(< 57 0)` es falso.
  * `(= 57 0)` es falso.
  * `(<= 1 57 10)` es falso.
  * `(<= 11 57 100)` es verdadero. `cond` devuelve `"Mediano"` y termina la evaluación.

## Ejercicio 4: Suma de pares con unless

**Objetivo**: Crear `(suma-pares lista)` que sume solo los números pares, ignorando los impares con `unless`.

**Lógica**:
De nuevo, `loop` es una excelente opción.

1.  Recorremos cada `numero` en la `lista`.
2.  La condición para sumar es que el número sea par. La instrucción dice "ignore los impares usando `unless`". Un número es impar si `(oddp numero)` es verdadero. Entonces, `(unless (oddp numero) ...)` significa "a menos que el número sea impar...", o en otras palabras, "si el número es par...".
3.  La cláusula `sum` de `loop` se encarga de acumular el valor de `numero` en un total.

**Código**:

```lisp
(defun suma-pares (lista)
  (loop for numero in lista
        unless (oddp numero) ; Se ejecuta solo si el número NO es impar
        sum numero))
```

**Ejemplo explicado**: `(suma-pares '(1 2 3 4 5 6))`

  * `numero` es 1. `(oddp 1)` es verdadero. `unless` no hace nada.
  * `numero` es 2. `(oddp 2)` es falso. `unless` ejecuta la acción `sum`, el total es 2.
  * `numero` es 3. `(oddp 3)` es verdadero. `unless` no hace nada.
  * `numero` es 4. `(oddp 4)` es falso. `unless` ejecuta `sum`, el total es 2 + 4 = 6.
  * `numero` es 5. `(oddp 5)` es verdadero. `unless` no hace nada.
  * `numero` es 6. `(oddp 6)` es falso. `unless` ejecuta `sum`, el total es 6 + 6 = 12.
  * El `loop` termina y devuelve el total acumulado: `12`.

## Ejercicio 5: Procesamiento de listas con car y cdr

**Objetivo**: Crear `(procesa-lista lista)` que examine la lista o su primer elemento y devuelva un mensaje específico.

**Lógica**:
Usaremos `cond` para evaluar los casos en el orden de prioridad que se especifica.

1.  **Primer caso**: ¿La lista está vacía? Usamos `(null lista)`.
2.  **Segundo caso**: ¿El primer elemento (`car`) es un número mayor a 50?. Es importante comprobar primero que sea un número con `(numberp (car lista))` para evitar errores si el primer elemento no es numérico.
3.  **Tercer caso**: ¿El primer elemento es una sublista? Usamos `(listp (car lista))`.
4.  **Caso final**: Si ninguno de los anteriores se cumple, es el "Caso general". Usamos `t` para el caso por defecto.

**Código**:

```lisp
(defun procesa-lista (lista)
  (cond
    ((null lista) "Lista vacía")
    ((and (numberp (car lista)) (> (car lista) 50)) "Grande")
    ((listp (car lista)) "Sublista detectada")
    (t "Caso general")))
```
