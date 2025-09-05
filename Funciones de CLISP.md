## 1. Funciones Fundamentales de Listas

### 1.1 CAR - Primer Elemento

**Función:** Extrae el primer elemento de una lista (considerada fundamental en Lisp)

**Sintaxis:** `(car lista)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de CAR
(car '(1 2 3 4))        ; => 1
(car '((a b) c d))      ; => (A B)
(car '())               ; => NIL
(car nil)               ; => NIL

;; Uso práctico
(defun primer-elemento (lista)
  "Retorna el primer elemento de una lista"
  (car lista))

(primer-elemento '(manzana pera uva))  ; => MANZANA
```

### 1.2 CDR - Resto de la Lista

**Función:** Obtiene todos los elementos excepto el primero (fundamental en Lisp)

**Sintaxis:** `(cdr lista)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de CDR
(cdr '(1 2 3 4))        ; => (2 3 4)
(cdr '((a b) c d))      ; => (C D)
(cdr '(solo))           ; => NIL
(cdr '())               ; => NIL

;; Uso práctico
(defun todos-menos-primero (lista)
  "Retorna todos los elementos excepto el primero"
  (cdr lista))

(todos-menos-primero '(do re mi fa))  ; => (RE MI FA)
```

### 1.3 CONS - Construcción de Listas

**Función:** Construye listas uniendo elementos (fundamental para construcción de listas)

**Sintaxis:** `(cons elemento lista)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de CONS
(cons 1 '(2 3 4))           ; => (1 2 3 4)
(cons 'a '(b c))            ; => (A B C)
(cons '(1 2) '(3 4))        ; => ((1 2) 3 4)
(cons 'x nil)               ; => (X)

;; Construcción paso a paso
(cons 1 (cons 2 (cons 3 nil)))  ; => (1 2 3)

;; Función práctica
(defun agregar-al-inicio (elemento lista)
  "Agrega un elemento al inicio de una lista"
  (cons elemento lista))

(agregar-al-inicio 'nuevo '(elemento lista))  ; => (NUEVO ELEMENTO LISTA)
```

### 1.4 LIST - Creación de Listas

**Función:** Crea una nueva lista con los elementos proporcionados

**Sintaxis:** `(list &rest elementos)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de LIST
(list 1 2 3 4)              ; => (1 2 3 4)
(list 'a 'b 'c)             ; => (A B C)
(list (+ 2 3) (* 4 5))      ; => (5 20)
(list)                      ; => NIL

;; Lista mixta
(list 1 'dos "tres" 4.0)    ; => (1 DOS "tres" 4.0)

;; Función práctica
(defun crear-lista-numeros (desde hasta)
  "Crea una lista de números consecutivos"
  (if (> desde hasta)
      nil
      (cons desde (crear-lista-numeros (1+ desde) hasta))))

(crear-lista-numeros 1 5)   ; => (1 2 3 4 5)
```

### 1.5 LENGTH - Longitud de Lista

**Función:** Calcula el número de elementos en una lista

**Sintaxis:** `(length secuencia)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de LENGTH
(length '(1 2 3 4))         ; => 4
(length '())                ; => 0
(length "Hola")             ; => 4 (también funciona con strings)
(length #(1 2 3))           ; => 3 (también con arrays)

;; Función práctica
(defun lista-vacia-p (lista)
  "Verifica si una lista está vacía"
  (= (length lista) 0))

(lista-vacia-p '())         ; => T
(lista-vacia-p '(1 2 3))    ; => NIL
```

### 1.6 APPEND - Concatenación de Listas

**Función:** Une múltiples listas en una sola

**Sintaxis:** `(append &rest listas)`

**Ejemplos:**
```lisp
;; Ejemplos básicos de APPEND
(append '(1 2) '(3 4))          ; => (1 2 3 4)
(append '(a) '(b c) '(d e f))   ; => (A B C D E F)
(append '() '(1 2 3))           ; => (1 2 3)
(append '(1 2 3) '())           ; => (1 2 3)

;; Función práctica
(defun combinar-listas (&rest listas)
  "Combina múltiples listas en una"
  (apply #'append listas))

(combinar-listas '(1 2) '(3 4) '(5 6))  ; => (1 2 3 4 5 6)
```

## 2. Funciones de Definición

### 2.1 DEFUN - Definición de Funciones

**Función:** Define nuevas funciones

**Sintaxis:** `(defun nombre (parámetros) "documentación" cuerpo)`

**Ejemplos:**
```lisp
;; Función simple
(defun saludar (nombre)
  "Saluda a una persona"
  (format t "Hola, ~A!~%" nombre))

(saludar "Juan")  ; Imprime: Hola, Juan!

;; Función con múltiples parámetros
(defun suma (a b)
  "Suma dos números"
  (+ a b))

(suma 5 3)  ; => 8

;; Función recursiva
(defun factorial (n)
  "Calcula el factorial de un número"
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(factorial 5)  ; => 120

;; Función con parámetros opcionales
(defun potencia (base &optional (exponente 2))
  "Eleva base a exponente (default 2)"
  (expt base exponente))

(potencia 3)    ; => 9
(potencia 2 4)  ; => 16
```

### 2.2 DEFVAR - Variables Globales

**Función:** Define variables globales

**Sintaxis:** `(defvar nombre [valor inicial] [documentación])`

**Ejemplos:**
```lisp
;; Variables globales simples
(defvar *contador* 0 "Contador global")
(defvar *lista-usuarios* '() "Lista de usuarios del sistema")
(defvar *configuracion* '((debug . t) (log-level . 3)))

;; Uso de variables globales
(defun incrementar-contador ()
  "Incrementa el contador global"
  (incf *contador*))

(incrementar-contador)  ; *contador* ahora es 1
(incrementar-contador)  ; *contador* ahora es 2

;; Función que usa configuración global
(defun obtener-config (clave)
  "Obtiene un valor de configuración"
  (cdr (assoc clave *configuracion*)))

(obtener-config 'debug)     ; => T
(obtener-config 'log-level) ; => 3
```

### 2.3 DEFPARAMETER - Parámetros Globales

**Función:** Define parámetros globales (siempre reinicializados)

**Sintaxis:** `(defparameter nombre valor [documentación])`

**Ejemplos:**
```lisp
;; Parámetros del sistema
(defparameter *version* "1.0.0" "Versión del sistema")
(defparameter *debug-mode* t "Modo de depuración activo")
(defparameter *max-conexiones* 100 "Máximo número de conexiones")

;; Función que usa parámetros
(defun mostrar-info-sistema ()
  "Muestra información del sistema"
  (format t "Sistema versión: ~A~%" *version*)
  (format t "Debug activo: ~A~%" *debug-mode*)
  (format t "Max conexiones: ~A~%" *max-conexiones*))

(mostrar-info-sistema)
```

## 3. Funciones de Asignación y Variables

### 3.1 SETF - Asignación General

**Función:** Asigna valores a variables y lugares

**Sintaxis:** `(setf lugar valor [lugar valor...])`

**Ejemplos:**
```lisp
;; Asignación simple
(defvar *x*)
(setf *x* 42)
*x*  ; => 42

;; Múltiples asignaciones
(defvar *a* nil)
(defvar *b* nil)
(setf *a* 10 *b* 20)
(list *a* *b*)  ; => (10 20)

;; Asignación a elementos de lista
(defvar *lista* '(1 2 3 4))
(setf (car *lista*) 'primero)
*lista*  ; => (PRIMERO 2 3 4)

(setf (cdr *lista*) '(segundo tercero))
*lista*  ; => (PRIMERO SEGUNDO TERCERO)

;; Función práctica
(defun actualizar-lista (lista posicion nuevo-valor)
  "Actualiza un elemento en una posición específica"
  (setf (nth posicion lista) nuevo-valor)
  lista)
```

### 3.2 LET - Variables Locales

**Función:** Crea un ámbito local con variables temporales

**Sintaxis:** `(let ((var valor)...) cuerpo)`

**Ejemplos:**
```lisp
;; Variables locales simples
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30

;; Variables locales complejas
(defun calcular-area-circulo (radio)
  "Calcula el área de un círculo"
  (let ((pi 3.14159)
        (radio-cuadrado (* radio radio)))
    (* pi radio-cuadrado)))

(calcular-area-circulo 5)  ; => 78.53975

;; LET anidado
(let ((x 1))
  (let ((y 2))
    (let ((z 3))
      (+ x y z))))  ; => 6

;; Función con variables temporales
(defun procesar-datos (datos)
  "Procesa una lista de datos"
  (let ((longitud (length datos))
        (suma (apply #'+ datos))
        (maximo (apply #'max datos)))
    (list :longitud longitud
          :suma suma
          :promedio (/ suma longitud)
          :maximo maximo)))

(procesar-datos '(1 2 3 4 5))
; => (:LONGITUD 5 :SUMA 15 :PROMEDIO 3 :MAXIMO 5)
```

### 3.3 LET* - Variables Locales Secuenciales

**Función:** Como LET, pero las variables pueden usar valores de variables anteriores

**Sintaxis:** `(let* ((var valor)...) cuerpo)`

**Ejemplos:**
```lisp
;; Variables dependientes
(let* ((x 5)
       (y (* x 2))
       (z (+ x y)))
  z)  ; => 15

;; Cálculo paso a paso
(defun calcular-descuento (precio porcentaje)
  "Calcula precio con descuento"
  (let* ((descuento (/ (* precio porcentaje) 100))
         (precio-final (- precio descuento))
         (impuesto (* precio-final 0.15))
         (total (+ precio-final impuesto)))
    (list :precio-original precio
          :descuento descuento
          :precio-con-descuento precio-final
          :impuesto impuesto
          :total total)))

(calcular-descuento 100 10)
```

## 4. Funciones Aritméticas

### 4.1 Operaciones Básicas

**Función:** Operaciones aritméticas fundamentales

**Ejemplos:**
```lisp
;; Suma
(+ 1 2 3 4)         ; => 10
(+ 1.5 2.5)         ; => 4.0
(apply #'+ '(1 2 3 4))  ; => 10

;; Resta
(- 10 3)            ; => 7
(- 10 2 1)          ; => 7
(- 5)               ; => -5 (negación)

;; Multiplicación
(* 2 3 4)           ; => 24
(* 1.5 2)           ; => 3.0

;; División
(/ 10 2)            ; => 5
(/ 10 3)            ; => 10/3 (fracción)
(/ 10.0 3)          ; => 3.3333333

;; Función calculadora simple
(defun calculadora (operacion a b)
  "Calculadora simple"
  (case operacion
    (+ (+ a b))
    (- (- a b))
    (* (* a b))
    (/ (/ a b))
    (otherwise "Operación no válida")))

(calculadora '+ 5 3)  ; => 8
(calculadora '* 4 6)  ; => 24
```

### 4.2 Funciones Matemáticas Avanzadas

**Ejemplos:**
```lisp
;; Exponenciación
(expt 2 3)          ; => 8
(expt 9 0.5)        ; => 3.0 (raíz cuadrada)

;; Raíz cuadrada
(sqrt 16)           ; => 4.0
(sqrt 2)            ; => 1.4142135

;; Valor absoluto
(abs -5)            ; => 5
(abs 3.14)          ; => 3.14

;; Módulo
(mod 7 3)           ; => 1
(mod 10 4)          ; => 2

;; Funciones trigonométricas
(sin 0)             ; => 0.0
(cos 0)             ; => 1.0
(tan (/ pi 4))      ; => 1.0

;; Función matemática compleja
(defun distancia-euclidiana (x1 y1 x2 y2)
  "Calcula la distancia euclidiana entre dos puntos"
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(distancia-euclidiana 0 0 3 4)  ; => 5.0
```

### 4.3 Funciones de Comparación

**Ejemplos:**
```lisp
;; Comparaciones numéricas
(= 5 5)             ; => T
(= 5 6)             ; => NIL
(/= 5 6)            ; => T (diferente)

(< 3 5)             ; => T
(> 5 3)             ; => T
(<= 5 5)            ; => T
(>= 5 5)            ; => T

;; Múltiples comparaciones
(< 1 2 3 4)         ; => T
(> 4 3 2 1)         ; => T

;; Función de validación
(defun numero-en-rango-p (numero minimo maximo)
  "Verifica si un número está en un rango"
  (and (>= numero minimo) (<= numero maximo)))

(numero-en-rango-p 5 1 10)      ; => T
(numero-en-rango-p 15 1 10)     ; => NIL
```

## 5. Funciones de Control de Flujo

### 5.1 IF - Condicional Simple

**Función:** Ejecuta código basándose en una condición

**Sintaxis:** `(if condición entonces [sino])`

**Ejemplos:**
```lisp
;; IF simple
(if (> 5 3)
    "Cinco es mayor que tres"
    "Esto no debería ocurrir")  ; => "Cinco es mayor que tres"

;; IF con una sola rama
(if (= 1 1) "Verdadero")  ; => "Verdadero"

;; Función con IF
(defun signo (numero)
  "Determina el signo de un número"
  (if (> numero 0)
      "positivo"
      (if (< numero 0)
          "negativo"
          "cero")))

(signo 5)   ; => "positivo"
(signo -3)  ; => "negativo"
(signo 0)   ; => "cero"
```

### 5.2 WHEN - Condicional de Una Rama

**Función:** Ejecuta múltiples expresiones si la condición es verdadera

**Sintaxis:** `(when condición expresiones...)`

**Ejemplos:**
```lisp
;; WHEN simple
(when (> 5 3)
  (print "Cinco es mayor que tres")
  (print "Esta línea también se ejecuta")
  "Valor de retorno")

;; Función con WHEN
(defun procesar-positivo (numero)
  "Procesa un número solo si es positivo"
  (when (> numero 0)
    (format t "Procesando número positivo: ~A~%" numero)
    (* numero 2)))

(procesar-positivo 5)   ; Imprime y retorna 10
(procesar-positivo -3)  ; Retorna NIL
```

### 5.3 UNLESS - Condicional Negativa

**Función:** Ejecuta código si la condición es falsa

**Sintaxis:** `(unless condición expresiones...)`

**Ejemplos:**
```lisp
;; UNLESS simple
(unless (< 5 3)
  "Cinco NO es menor que tres")  ; => "Cinco NO es menor que tres"

;; Función con UNLESS
(defun verificar-lista-no-vacia (lista)
  "Procesa lista solo si no está vacía"
  (unless (null lista)
    (format t "La lista tiene ~A elementos~%" (length lista))
    (first lista)))

(verificar-lista-no-vacia '(a b c))  ; => A
(verificar-lista-no-vacia '())       ; => NIL
```

### 5.4 COND - Condicional Múltiple

**Función:** Maneja múltiples condiciones

**Sintaxis:** `(cond (condición1 expresiones...) (condición2 expresiones...) ...)`

**Ejemplos:**
```lisp
;; COND básico
(defun clasificar-numero (n)
  "Clasifica un número"
  (cond ((> n 0) "positivo")
        ((< n 0) "negativo")
        ((= n 0) "cero")
        (t "no es un número")))

(clasificar-numero 5)   ; => "positivo"
(clasificar-numero -2)  ; => "negativo"
(clasificar-numero 0)   ; => "cero"

;; COND complejo
(defun calificar-nota (nota)
  "Convierte nota numérica a letra"
  (cond ((>= nota 90) (values "A" "Excelente"))
        ((>= nota 80) (values "B" "Bueno"))
        ((>= nota 70) (values "C" "Regular"))
        ((>= nota 60) (values "D" "Insuficiente"))
        (t (values "F" "Reprobado"))))

(calificar-nota 85)  ; => "B", "Bueno"
```

### 5.5 CASE - Selección por Valor

**Función:** Selecciona código basándose en el valor de una expresión

**Sintaxis:** `(case expresión (valor1 código...) (valor2 código...) ...)`

**Ejemplos:**
```lisp
;; CASE básico
(defun dia-semana (numero)
  "Retorna el nombre del día de la semana"
  (case numero
    (1 "Lunes")
    (2 "Martes") 
    (3 "Miércoles")
    (4 "Jueves")
    (5 "Viernes")
    ((6 7) "Fin de semana")
    (otherwise "Día inválido")))

(dia-semana 1)  ; => "Lunes"
(dia-semana 6)  ; => "Fin de semana"
(dia-semana 8)  ; => "Día inválido"

;; CASE para operaciones
(defun operacion (op a b)
  "Ejecuta operaciones básicas"
  (case op
    (+ (+ a b))
    (- (- a b))
    (* (* a b))
    (/ (if (/= b 0) (/ a b) "División por cero"))
    (otherwise "Operación no soportada")))

(operacion '+ 5 3)  ; => 8
(operacion '/ 10 2) ; => 5
```

## 6. Funciones de Bucles

### 6.1 DOTIMES - Bucle Numérico

**Función:** Ejecuta código un número específico de veces

**Sintaxis:** `(dotimes (variable límite [resultado]) cuerpo...)`

**Ejemplos:**
```lisp
;; DOTIMES simple
(dotimes (i 5)
  (format t "Iteración ~A~%" i))
; Imprime: Iteración 0, 1, 2, 3, 4

;; DOTIMES con resultado
(dotimes (i 5 "Completado")
  (print i))  ; => "Completado"

;; Función usando DOTIMES
(defun suma-hasta (n)
  "Suma números desde 0 hasta n-1"
  (let ((suma 0))
    (dotimes (i n suma)
      (incf suma i))))

(suma-hasta 5)  ; => 10 (0+1+2+3+4)

;; Crear lista con DOTIMES
(defun lista-cuadrados (n)
  "Crea lista de cuadrados hasta n"
  (let ((resultado '()))
    (dotimes (i n (reverse resultado))
      (push (* i i) resultado))))

(lista-cuadrados 5)  ; => (0 1 4 9 16)
```

### 6.2 DOLIST - Bucle sobre Lista

**Función:** Itera sobre cada elemento de una lista

**Sintaxis:** `(dolist (variable lista [resultado]) cuerpo...)`

**Ejemplos:**
```lisp
;; DOLIST simple
(dolist (item '(a b c d))
  (print item))
; Imprime: A B C D

;; DOLIST con resultado
(dolist (x '(1 2 3) "Procesado")
  (format t "Número: ~A~%" x))  ; => "Procesado"

;; Función usando DOLIST
(defun sumar-lista (lista)
  "Suma todos los elementos de una lista"
  (let ((suma 0))
    (dolist (elemento lista suma)
      (incf suma elemento))))

(sumar-lista '(1 2 3 4 5))  ; => 15

;; Buscar elemento
(defun buscar-elemento (elemento lista)
  "Busca un elemento en una lista"
  (dolist (item lista nil)
    (when (equal item elemento)
      (return t))))

(buscar-elemento 'c '(a b c d))  ; => T
(buscar-elemento 'x '(a b c d))  ; => NIL
```

### 6.3 LOOP - Bucle General

**Función:** Macro de bucle muy poderosa y flexible

**Ejemplos:**
```lisp
;; LOOP básico infinito (con RETURN)
(let ((contador 0))
  (loop
    (when (>= contador 3) (return "Terminado"))
    (format t "Contador: ~A~%" contador)
    (incf contador)))

;; LOOP con FOR
(loop for i from 1 to 5
      collect (* i i))  ; => (1 4 9 16 25)

;; LOOP con IN
(loop for elemento in '(a b c d)
      collect (list elemento (symbol-name elemento)))
; => ((A "A") (B "B") (C "C") (D "D"))

;; LOOP complejo
(defun estadisticas-lista (lista)
  "Calcula estadísticas de una lista numérica"
  (loop for numero in lista
        count numero into total
        sum numero into suma
        maximize numero into maximo
        minimize numero into minimo
        finally (return (list :total total
                             :suma suma
                             :promedio (/ suma total)
                             :maximo maximo
                             :minimo minimo))))

(estadisticas-lista '(1 5 3 9 2))
; => (:TOTAL 5 :SUMA 20 :PROMEDIO 4 :MAXIMO 9 :MINIMO 1)
```

## 7. Funciones de Predicados (Pruebas)

### 7.1 Predicados de Tipos

**Función:** Verifican el tipo de un objeto

**Ejemplos:**
```lisp
;; Predicados básicos de tipo
(numberp 42)        ; => T
(numberp "42")      ; => NIL
(stringp "hola")    ; => T
(symbolp 'simbolo)  ; => T
(listp '(1 2 3))    ; => T
(null nil)          ; => T
(null '())          ; => T

;; Función de validación
(defun validar-entrada (valor tipo)
  "Valida que un valor sea del tipo esperado"
  (case tipo
    (numero (numberp valor))
    (cadena (stringp valor))
    (simbolo (symbolp valor))
    (lista (listp valor))
    (otherwise nil)))

(validar-entrada 42 'numero)        ; => T
(validar-entrada "hola" 'cadena)    ; => T
(validar-entrada 'x 'numero)        ; => NIL
```

### 7.2 Predicados de Comparación

**Ejemplos:**
```lisp
;; Comparaciones de igualdad
(eq 'a 'a)          ; => T (misma identidad)
(eql 3 3)           ; => T (mismo valor y tipo)
(equal '(1 2) '(1 2))  ; => T (misma estructura)
(equalp "Hello" "HELLO")  ; => T (igualdad generosa)

;; Función de comparación
(defun comparar-objetos (obj1 obj2)
  "Compara dos objetos con diferentes niveles"
  (cond ((eq obj1 obj2) "Identidad (EQ)")
        ((eql obj1 obj2) "Valor y tipo (EQL)")
        ((equal obj1 obj2) "Estructura (EQUAL)")
        ((equalp obj1 obj2) "Igualdad generosa (EQUALP)")
        (t "Diferentes")))

(comparar-objetos 'a 'a)            ; => "Identidad (EQ)"
(comparar-objetos 3 3)              ; => "Valor y tipo (EQL)"
(comparar-objetos '(1 2) '(1 2))    ; => "Estructura (EQUAL)"
```

### 7.3 Predicados Lógicos

**Ejemplos:**
```lisp
;; Operadores lógicos
(and t t t)         ; => T
(and t nil t)       ; => NIL
(or nil nil t)      ; => T
(or nil nil nil)    ; => NIL
(not t)             ; => NIL
(not nil)           ; => T

;; Función con lógica compleja
(defun validar-usuario (nombre edad email)
  "Valida los datos de un usuario"
  (and (stringp nombre)
       (> (length nombre) 0)
       (numberp edad)
       (>= edad 18)
       (stringp email)
       (find #\@ email)))

(validar-usuario "Juan" 25 "juan@email.com")    ; => #\@
(validar-usuario "" 25 "juan@email.com")        ; => NIL
(validar-usuario "Juan" 15 "juan@email.com")    ; => NIL
```

## 8. Funciones de Entrada/Salida

### 8.1 PRINT y PRIN1

**Función:** Imprime objetos en forma legible

**Ejemplos:**
```lisp
;; PRINT básico
(print "Hola mundo")    ;
