# Ejecicios en CLISP
Por medio del defun se crea una funcion que lleva el nombre y realiza la suma de dos numero 
## Suma de dos numeros
```
(defun suma (a b) //Se crea la funcion llamada suma con dos variables con a y b 
  (+ a b)) //Se realiza la funcion en preorden 
```
<img width="331" height="49" alt="image" src="https://github.com/user-attachments/assets/18d6ccd8-2bd0-4e00-a80e-7aa51c442d02" />

## Factorial 
Se realiza la funcion de calcular un factorial
``` 
(defun factorial (x) //Se crea la funcion llamada factorial en la cual va recibir un valor en el lugar de x
  (if (= x 0)  //Se realiza la comparacion para saber si el valor de x tiene el valor de 0
      1   //Si se cumple la condicion anterior el valor de X se convierte en 1
      (* x (factorial (- x 1))))) // Por medio de la recursividad se realiza la operacion para calcular un factorial por, poniendo la operacion en Preorden
```
<img width="401" height="59" alt="image" src="https://github.com/user-attachments/assets/1eb07eee-6b68-4b75-9ef1-498dfc036640" />

## AreaCuadrado
Realiza el calculo de un cuadrado
```
(defun areacuadro (a b) //Se llama a la funcion areacuadrado y se designan dos valores a ingresar en la terminal
  (* a b)) //Se realiza la multiplicacion de por en preorden
```
<img width="334" height="56" alt="image" src="https://github.com/user-attachments/assets/2925cb83-01f5-48be-9c76-8ddde2360f92" />

## Fibonacci
Se calcula el valor de fibonacci de un numero 
```
(defun fibonacci (x) //Se crea la funcion fibonacci con espera de un valor 
  (if (<= x 1) //Se realiza la comparacion de cuando x sea igual o menor
      x // Una vez que se cumpla se convertira el valor en x
      (+ (fibonacci (- x 1)) (fibonacci (- x 2))))) //Se realizan las operaciones a x-1 y x-2
```
Realize un ejemplo de como seria de otra a la vista en clases
```
(defun fibo (x) //Se crea la funcion fibo con espera de un valor
  (if (< x 2) //Entra a un ciclo if en donde se compara el valor de x que sea menor a dos
      1 //Una vez que se cumpla se convierte el valor de x a 1
      (+ (fibo (- x 1)) (fibo (- x 2))))) //realiza una operacion en la cual llama dos veces a la funcion fibo y le resta 1 y 2
```
<img width="495" height="52" alt="image" src="https://github.com/user-attachments/assets/881afaa8-e023-48f6-92ce-6dd74c11dfdc" />

## Division por Restas
Realiza la division por medio solo restas te da el resultado de la operacion y el residuo
```
(defun division-por-restas (a b) //Se crea una funcion llamada division-por-restas que espera a 2 valores en la terminal
  (if (< a b) //Se realiza la comparacion entre el valor a y b
      (list 0 a)   // caso base: no se puede restar mas
      (let ((res (division-por-restas (- a b) b))) //Guarda el resultado de manera temporal para seguir haciendo las operaciones
        (list (+ 1 (first res)) (second res))))) //crea una lista con el valor del resultado y el residuo
```
<img width="447" height="52" alt="image" src="https://github.com/user-attachments/assets/5ee409e6-e5b7-4f95-8e0c-d6d0614be0cc" />

## Multiplicacion por sumas
```
(defun multiplicacion (a b) //Se crea una funcion llamada multiplicacion
  (if (= b 0)//Se realiza la comparacion para saber si b es igual a 0
      0 //Si se cumple el valor de b se convierte en 0
      (+ a (multiplicacion a (- b 1))))) // Se realiza las sumas y de los datos y una resta en con el valor de b - 1 y se le suma el valor de a
```
<img width="467" height="64" alt="image" src="https://github.com/user-attachments/assets/a7e93d2b-d688-4ea5-82ff-89d1d91829ad" />


## CDR, CAR, CADR
Una lista en Lisp está formada por celdas (cons), cada una con:

car → devuelve el primer elemento.

cdr → devuelve la cola de la lista (todos menos el primero).

Se pueden combinar hasta 4 niveles de car y cdr:

cadr = (car (cdr L)) → segundo elemento.

caddr = (car (cdr (cdr L))) → tercer elemento.

cadddr= (car (cdr (cdr (cdr L)))) → cuarto elemento.

cdddr = (cdr (cdr (cdr L))) → la cola desde el 4to.

Solo existen combinaciones hasta 4 letras (ej. cadddr), no más.

###  ( a b c ( d 1) ( c x ) m n)

(D 1)

<img width="468" height="50" alt="Captura de pantalla 2025-09-10 094803" src="https://github.com/user-attachments/assets/cc33bbb8-e2fc-459d-9a4c-7bc0dd90cb7d" />

X

<img width="730" height="51" alt="Captura de pantalla 2025-09-10 102751" src="https://github.com/user-attachments/assets/1224c2c9-6772-4a2f-9635-ff21198fb187" />

D

<img width="694" height="54" alt="Captura de pantalla 2025-09-10 102923" src="https://github.com/user-attachments/assets/5f5c1c2e-f750-4bb2-87b0-d067495b9d2b" />

### ((( a b c (d) e f ((g h)) i j t)
T

<img width="1464" height="56" alt="Captura de pantalla 2025-09-10 105148" src="https://github.com/user-attachments/assets/8603c845-8565-4946-8cf6-060dacd1af4f" />

G

<img width="1289" height="53" alt="Captura de pantalla 2025-09-10 110351" src="https://github.com/user-attachments/assets/b243f2aa-ad3c-4473-a088-42a9534454cf" />

(D)

<img width="995" height="55" alt="Captura de pantalla 2025-09-10 110550" src="https://github.com/user-attachments/assets/7378b613-dabd-4e72-8ec1-41ea226e3bf0" />

B

<img width="823" height="52" alt="Captura de pantalla 2025-09-10 110735" src="https://github.com/user-attachments/assets/285fb9dd-146c-4582-82d0-1109ad320d05" />

### ((( a b c d )) 1 (2) 3 (4 5) ((6 (7) 8))
B

<img width="886" height="44" alt="image" src="https://github.com/user-attachments/assets/537ba8c1-3d54-4523-9456-93583183fdc0" />

D

<img width="1018" height="49" alt="image" src="https://github.com/user-attachments/assets/fac00e86-e8c6-4b6c-8bba-7ac12b3c7f3b" />

(2)

<img width="826" height="54" alt="Captura de pantalla 2025-09-10 114107" src="https://github.com/user-attachments/assets/d0b17535-32ec-4ff1-bb9a-512247a51c29" />

(7)

<img width="1188" height="53" alt="Captura de pantalla 2025-09-10 113627" src="https://github.com/user-attachments/assets/d4ee10a1-7909-4cd3-8e81-0a98bfb0c8b1" />

5

<img width="1110" height="56" alt="Captura de pantalla 2025-09-10 114436" src="https://github.com/user-attachments/assets/daeeba88-9d42-419a-9afe-37f7285b81f4" />

6

<img width="1060" height="59" alt="Captura de pantalla 2025-09-10 114522" src="https://github.com/user-attachments/assets/75831d04-b281-467d-8e5d-fed87a1936d7" />

Perfecto 🙌 te armo el apunte con los conceptos y la función `holamundo` en **CLISP** usando tanto `printc` como `format`.

---

## Printc  y Format
**`printc`**

   * Evalúa y muestra cadenas y expresiones.
   * Puede parecer que imprime dos veces por la **doble evaluación**.
   * Ejemplo:

     ```lisp
     (printc "Hola") ;; imprime Hola y devuelve "Hola"
     ```

**`format`**

   * Maneja salidas de texto con formato.
   * `(format nil "texto")` → devuelve la cadena.
   * `(format t "texto")` → imprime en pantalla.
   * Ejemplo:

     ```lisp
     (format t "La suma es: ~a~%" (+ 2 3))
     ;; La suma es: 5
     ```


```lisp
;; Con printc
(defun holamundo-printc ()
  (printc "Hola mundo"))

;; Con format
(defun holamundo-format ()
  (format t "Hola mundo~%"))
```

* `printc` → imprime una cadena y también devuelve la cadena (doble evaluación).
* `format` → controla mejor la salida, `t` significa que imprime en pantalla, y `~%` es un salto de línea.

---

## Comparacion entre UNLESS, IF y WHEN

**`unless`**

   * Sintaxis: `(unless condicion expresiones...)`
   * Significa *“a menos que”*.
   * Ejecuta el cuerpo solo si la condición es **falsa**.
   * Ejemplo:

     ```lisp
     (unless (= 1 2)
       (print "Esto se imprime porque 1 no es igual a 2"))
     ```
**`when`**

   * Sintaxis: `(when condicion expresiones...)`
   * Ejecuta el cuerpo solo si la condición es **verdadera**.
   * Tiene un **`progn` implícito** (se pueden poner varias expresiones).
   * Ejemplo:

     ```lisp
     (when (> 5 3)
       (print "5 es mayor que 3")
       (print "Se ejecutó el when"))
     ```

   **`if`**

   * Necesita **dos ramas**: la verdadera y la falsa.
   * Ejemplo:

     ```lisp
     (if (= 2 2)
         (print "Es verdadero")
         (print "Es falso"))
     ```

```
(defun evalua(a b)
  (when (< a b)
    (format t "Evaluacion del when ~%")
    (format t "A < B")
  )
  
  (unless (< a b))
  (format t "Evaluacion del unless ~%")
    (format t "A > B")
)

(defun evalua(a b)
  (if (< a b)
  (progn ;Se puede cargar mas de dos declaraciones(cargar un bloque con mas lineas)
  (format t "Evaluacion del if ~%")
  (format t "A < B")
  )
  (format nill "B > A")
  )
)
```


## progn

   * Agrupa varias expresiones y devuelve **el valor de la última**.
   * Ejemplo:

     ```
     (progn
       (print "Primero")
       (print "Segundo")
       (+ 2 3)) ; => 5
    (defun evalua(a b)
      (if (< a b)
        (progn ;Se puede cargar mas de dos declaraciones(cargar un bloque con mas lineas)
        (format t "Evaluacion del if ~%")
        (format t "A < B")
        )
        (format nill "B > A")
      )
    )

    



## assoc

   * Busca un par `(clave . valor)` en una lista asociativa (alist).
   * Ejemplo:

     ```lisp
     (assoc 'a '((a . 1) (b . 2) (c . 3)))
     ;; => (A . 1)
     ```

## cond

* Sirve para evaluar **varias condiciones** (como `if – else if – else`).
* Cada condición se evalúa en orden, y cuando una es **verdadera**, ejecuta su bloque.
* La palabra clave **`t`** se usa como "caso por defecto" (otherwise).

**Sintaxis:**

```lisp
(cond
  (condicion1 expresion1 ...)
  (condicion2 expresion2 ...)
  ...
  (t expresion-por-defecto))
```

**Ejemplo:**

```lisp
(cond
  ((> x 0) (print "Positivo"))
  ((= x 0) (print "Cero"))
  (t       (print "Negativo")))
```

---

### case

* Se usa para comparar un **valor fijo** contra varias opciones (similar a `switch-case` en otros lenguajes).
* Cada opción es un **literal o símbolo**.
* La palabra clave **`otherwise`** funciona como "caso por defecto".

**Sintaxis:**

```lisp
(case valor
  (opcion1 expresion1 ...)
  (opcion2 expresion2 ...)
  ...
  (otherwise expresion-por-defecto))
```

**Ejemplo:**

```lisp
(case x
  (1 (print "Uno"))
  (2 (print "Dos"))
  (3 (print "Tres"))
  (otherwise (print "Otro número")))
```

---

* **`cond`** → evalúa **condiciones lógicas** (útil para rangos, comparaciones, etc.).
* **`case`** → compara un **único valor** contra varias opciones fijas.
En CLISP, `mapcar`, `oddp` y `evenp` son funciones muy útiles para trabajar con listas y números. A continuación te explico el funcionamiento de cada una con ejemplos.

-----

## mapcar

La función **`mapcar`** es una herramienta poderosa para el procesamiento de listas. Su propósito es aplicar una función a cada elemento de una o más listas y devolver una nueva lista con los resultados.

**Sintaxis:**

```lisp
(mapcar <función> <lista-1> <lista-2> ... <lista-n>)
```

**Funcionamiento:**

1.  **Aplica una función:** Toma una función como su primer argumento.
2.  **Recorre las listas:** Aplica esa función a los elementos de las listas que se le pasan como argumentos. Si se proporciona más de una lista, la función tomará el primer elemento de cada lista como sus argumentos, luego el segundo de cada una, y así sucesivamente.
3.  **Se detiene con la lista más corta:** El proceso se detiene cuando se llega al final de la lista más corta.
4.  **Devuelve una nueva lista:** Retorna una lista nueva que contiene los resultados de cada aplicación de la función.

**Ejemplos:**

  * **Aplicar una función a una sola lista:** Incrementar en uno cada elemento de una lista.

    ```lisp
    (mapcar #'1+ '(1 2 3 4 5))
    ```

    **Resultado:** `(2 3 4 5 6)`
    *Aquí, `#'1+` es la función que suma 1 a su argumento.*

  * **Aplicar una función a múltiples listas:** Sumar los elementos correspondientes de dos listas.

    ```lisp
    (mapcar #'+ '(1 2 3) '(10 20 30))
    ```

    **Resultado:** `(11 22 33)`
    *En la primera pasada, calcula `(+ 1 10)`, en la segunda `(+ 2 20)`, y así sucesivamente.*

  * **Uso con una función `lambda` (anónima):** Obtener el cuadrado de cada número en una lista.

    ```lisp
    (mapcar #'(lambda (x) (* x x)) '(1 2 3 4))
    ```

    **Resultado:** `(1 4 9 16)`

-----

### \#\# `oddp` y `evenp`

Estas dos funciones son predicados, lo que significa que devuelven un valor booleano: `T` (verdadero) o `NIL` (falso). Se utilizan para determinar si un número entero es impar o par.

### `oddp`

La función **`oddp`** comprueba si un número entero es **impar**.

**Sintaxis:**

```lisp
(oddp <número-entero>)
```

**Funcionamiento:**

  * Devuelve `T` si el número es impar.
  * Devuelve `NIL` si el número es par.

**Ejemplos:**

```lisp
(oddp 3)   ; Devuelve T
(oddp 10)  ; Devuelve NIL
(oddp -7)  ; Devuelve T
```

### `evenp`

La función **`evenp`** comprueba si un número entero es **par**.

**Sintaxis:**

```lisp
(evenp <número-entero>)
```

**Funcionamiento:**

  * Devuelve `T` si el número es par.
  * Devuelve `NIL` si el número es impar.

**Ejemplos:**

```lisp
(evenp 4)   ; Devuelve T
(evenp 9)   ; Devuelve NIL
(evenp -2)  ; Devuelve T
```

-----

### \#\# Combinando las funciones

Puedes usar `oddp` o `evenp` junto con otras funciones como `remove-if` o `remove-if-not` para filtrar listas. Por ejemplo, para obtener solo los números impares de una lista:

```lisp
(remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9))
```

**Resultado:** `(1 3 5 7 9)`

Y para obtener solo los números pares:

```lisp
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9))
```

**Resultado:** `(2 4 6 8)`

## Funciones de Volumen y de Area 
Utilizando las siguientes funciones: printc , readline format t,Setq,setf ,let , read

### Se define pi para usarlo en los cálculos que lo requieran.
(defconstant +pi+ pi)

### 1. Área de un Cuadrado
```
(defun area-cuadrado ()
  (princ "Ingrese el lado del cuadrado: ")
  (let ((lado (read)))
    (format t "El área del cuadrado es: ~a unidades cuadradas.~%" (* lado lado))))
```
### 2. Área de un Rectángulo
```
(defun area-rectangulo ()
  (let (base altura)
    (princ "Ingrese la base del rectángulo: ")
    (setf base (read))
    (princ "Ingrese la altura del rectángulo: ")
    (setf altura (read))
    (format t "El área del rectángulo es: ~a unidades cuadradas.~%" (* base altura))))
```
### 3. Área de un Círculo
```
(defun area-circulo ()
  (princ "Ingrese el radio del círculo: ")
  (let* ((radio (read))
         (area (* +pi+ radio radio)))
    (format t "El área del círculo es: ~f unidades cuadradas.~%" area)))
```
### 4. Área de un Triángulo (con base y altura)
```
(defun area-triangulo ()
  (princ "Ingrese la base del triángulo: ")
  (let ((base (read)))
    (princ "Ingrese la altura del triángulo: ")
    (let ((altura (read)))
      (format t "El área del triángulo es: ~a unidades cuadradas.~%" (/ (* base altura) 2)))))
```
### 5. Área de un Trapecio
```
(defun area-trapecio ()
  (let (base-mayor base-menor altura)
    (princ "Ingrese la base mayor: ")
    (setf base-mayor (read))
    (princ "Ingrese la base menor: ")
    (setf base-menor (read))
    (princ "Ingrese la altura: ")
    (setf altura (read))
    (format t "El área del trapecio es: ~a unidades cuadradas.~%" (* (/ (+ base-mayor base-menor) 2) altura))))
```
### 6. Área de un Rombo
```
(defun area-rombo ()
  (princ "Ingrese la diagonal mayor: ")
  (let ((D (read)))
    (princ "Ingrese la diagonal menor: ")
    (let ((d (read)))
      (format t "El área del rombo es: ~a unidades cuadradas.~%" (/ (* D d) 2)))))
```
### 7. Área de un Elipse
```
(defun area-elipse ()
  (let (semi-eje-a semi-eje-b)
    (princ "Ingrese el semi-eje mayor (a): ")
    (setf semi-eje-a (read))
    (princ "Ingrese el semi-eje menor (b): ")
    (setf semi-eje-b (read))
    (format t "El área de la elipse es: ~f unidades cuadradas.~%" (* +pi+ semi-eje-a semi-eje-b))))
```
### 8. Área de un Polígono Regular
```
(defun area-poligono-regular ()
  (princ "Ingrese el número de lados: ")
  (let ((n (read)))
    (princ "Ingrese la longitud de un lado: ")
    (let ((lado (read)))
      (princ "Ingrese la apotema: ")
      (let ((apotema (read)))
        (let* ((perimetro (* n lado))
               (area (/ (* perimetro apotema) 2)))
          (format t "El área del polígono regular es: ~a unidades cuadradas.~%" area))))))
```
### 9. Área de un Paralelogramo
```
(defun area-paralelogramo ()
  (let (base altura)
    (princ "Ingrese la base del paralelogramo: ")
    (setf base (read))
    (princ "Ingrese la altura del paralelogramo: ")
    (setf altura (read))
    (format t "El área del paralelogramo es: ~a unidades cuadradas.~%" (* base altura))))
```
### 10. Área de la superficie de una Esfera
```
(defun area-superficie-esfera ()
  (princ "Ingrese el radio de la esfera: ")
  (let* ((radio (read))
         (area (* 4 +pi+ (expt radio 2))))
    (format t "El área de la superficie de la esfera es: ~f unidades cuadradas.~%" area)))
```

### 1. Volumen de un Cubo
```
(defun volumen-cubo ()
  (princ "Ingrese la longitud de la arista del cubo: ")
  (let ((arista (read)))
    (setf (symbol-value 'volumen) (* arista arista arista))
    (format t "El volumen del cubo es: ~a unidades cúbicas.~%" volumen)))
```
### 2. Volumen de un Paralelepípedo (o Cuboide)
```
(defun volumen-paralelepipedo ()
  (let (largo ancho alto)
    (princ "Ingrese el largo: ")
    (setf largo (read))
    (princ "Ingrese el ancho: ")
    (setf ancho (read))
    (princ "Ingrese el alto: ")
    (setf alto (read))
    (format t "El volumen es: ~a unidades cúbicas.~%" (* largo ancho alto))))
```
### 3. Volumen de una Esfera
```
(defun volumen-esfera ()
  (princ "Ingrese el radio de la esfera: ")
  (let* ((radio (read))
         (volumen (* (/ 4.0 3.0) +pi+ (expt radio 3))))
    (format t "El volumen de la esfera es: ~f unidades cúbicas.~%" volumen)))
```
### 4. Volumen de un Cilindro
```
(defun volumen-cilindro ()
  (princ "Ingrese el radio de la base del cilindro: ")
  (let ((radio (read)))
    (princ "Ingrese la altura del cilindro: ")
    (let ((altura (read)))
      (format t "El volumen del cilindro es: ~f unidades cúbicas.~%" (* +pi+ (expt radio 2) altura)))))
```
### 5. Volumen de un Cono
```
(defun volumen-cono ()
  (let (radio altura)
    (princ "Ingrese el radio de la base del cono: ")
    (setf radio (read))
    (princ "Ingrese la altura del cono: ")
    (setf altura (read))
    (let ((volumen (/ (* +pi+ (expt radio 2) altura) 3.0)))
      (format t "El volumen del cono es: ~f unidades cúbicas.~%" volumen))))
```
### 6. Volumen de una Pirámide de base cuadrada
```
(defun volumen-piramide-cuadrada ()
  (princ "Ingrese la longitud del lado de la base: ")
  (let ((lado-base (read)))
    (princ "Ingrese la altura de la pirámide: ")
    (let ((altura (read)))
      (format t "El volumen de la pirámide es: ~a unidades cúbicas.~%" (/ (* (expt lado-base 2) altura) 3)))))
```
### 7. Volumen de un Prisma Rectangular
```
(defun volumen-prisma-rectangular ()
  (princ "Ingrese el área de la base: ")
  (let ((area-base (read)))
    (princ "Ingrese la altura del prisma: ")
    (let ((altura (read)))
      (format t "El volumen del prisma es: ~a unidades cúbicas.~%" (* area-base altura)))))
```
### 8. Volumen de un Elipsoide
```
(defun volumen-elipsoide ()
  (let (a b c)
    (princ "Ingrese el semi-eje a: ")
    (setf a (read))
    (princ "Ingrese el semi-eje b: ")
    (setf b (read))
    (princ "Ingrese el semi-eje c: ")
    (setf c (read))
    (format t "El volumen del elipsoide es: ~f unidades cúbicas.~%" (* (/ 4.0 3.0) +pi+ a b c))))
```
### 9. Volumen de un Toroide (Torus)
```
(defun volumen-toroide ()
  (princ "Ingrese el radio mayor (R, desde el centro del toroide al centro del tubo): ")
  (let ((R (read)))
    (princ "Ingrese el radio menor (r, el radio del tubo): ")
    (let ((r (read)))
      (format t "El volumen del toroide es: ~f unidades cúbicas.~%" (* 2 (expt +pi+ 2) R (expt r 2))))))
```
### 10. Volumen de un Tetraedro Regular
```
(defun volumen-tetraedro-regular ()
  (princ "Ingrese la longitud de la arista (a): ")
  (let* ((a (read))
         (volumen (/ (expt a 3) (* 6 (sqrt 2)))))
    (format t "El volumen del tetraedro regular es: ~f unidades cúbicas.~%" volumen)))
```
