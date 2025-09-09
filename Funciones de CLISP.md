# Funciones de CLISP

### 1.1 CAR - Primer Elemento
**Función:** Extrae el primer elemento de una lista
```lisp
(car '(1 2 3 4))        ; => 1
(car '((a b) c d))      ; => (A B)
(car '())               ; => NIL
```

### 1.2 CDR - Resto de la Lista
**Función:** Obtiene todos los elementos excepto el primero
```lisp
(cdr '(1 2 3 4))        ; => (2 3 4)
(cdr '(solo))           ; => NIL
```

### 1.3 CONS - Construcción de Listas
**Función:** Construye listas uniendo elementos
```lisp
(cons 1 '(2 3 4))       ; => (1 2 3 4)
(cons 'a '(b c))        ; => (A B C)
```

### 1.4 LIST - Creación de Listas
**Función:** Crea una nueva lista con los elementos dados
```lisp
(list 1 2 3 4)          ; => (1 2 3 4)
(list 'a 'b 'c)         ; => (A B C)
```

### 1.5 LENGTH - Longitud de Lista
**Función:** Calcula el número de elementos
```lisp
(length '(1 2 3 4))     ; => 4
(length '())            ; => 0
```

### 1.6 APPEND - Concatenación de Listas
**Función:** Une múltiples listas en una sola
```lisp
(append '(1 2) '(3 4))          ; => (1 2 3 4)
(append '(a) '(b c) '(d e f))   ; => (A B C D E F)
```

### 1.7 REVERSE - Invertir Lista
**Función:** Invierte el orden de los elementos
```lisp
(reverse '(1 2 3 4))    ; => (4 3 2 1)
(reverse "hola")        ; => "aloh"
```

### 1.8 NTH - Elemento en Posición
**Función:** Obtiene el elemento en la posición n
```lisp
(nth 0 '(a b c d))      ; => A
(nth 2 '(a b c d))      ; => C
(nth 5 '(a b c))        ; => NIL
```

## 2. Funciones de Definición (4 funciones)

### 2.1 DEFUN - Definición de Funciones
**Función:** Define nuevas funciones
```lisp
(defun suma (a b)
  "Suma dos números"
  (+ a b))

(suma 5 3)  ; => 8
```

### 2.2 DEFVAR - Variables Globales
**Función:** Define variables globales (no reinicializa si ya existe)
```lisp
(defvar *contador* 0 "Contador global")
(defvar *lista-usuarios* '())
```

### 2.3 DEFPARAMETER - Parámetros Globales
**Función:** Define parámetros globales (siempre reinicializa)
```lisp
(defparameter *version* "1.0.0" "Versión del sistema")
(defparameter *debug-mode* t)
```

### 2.4 DEFCONSTANT - Constantes
**Función:** Define constantes inmutables
```lisp
(defconstant +pi+ 3.14159 "Valor de Pi")
(defconstant +max-size+ 100)
```

## 3. Funciones de Variables y Asignación (4 funciones)

### 3.1 SETF - Asignación General
**Función:** Asigna valores a variables y lugares
```lisp
(defvar *x*)
(setf *x* 42)           ; *x* ahora es 42
(setf *a* 10 *b* 20)    ; múltiples asignaciones
```

### 3.2 LET - Variables Locales
**Función:** Crea ámbito local con variables temporales
```lisp
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

### 3.3 LET* - Variables Locales Secuenciales
**Función:** Variables locales que pueden depender de anteriores
```lisp
(let* ((x 5)
       (y (* x 2))
       (z (+ x y)))
  z)  ; => 15
```

### 3.4 INCF - Incrementar Variable
**Función:** Incrementa el valor de una variable
```lisp
(defvar *contador* 5)
(incf *contador*)       ; *contador* ahora es 6
(incf *contador* 3)     ; *contador* ahora es 9
```

## 4. Funciones Aritméticas (8 funciones)

### 4.1 + - Suma
**Función:** Suma números
```lisp
(+ 1 2 3 4)         ; => 10
(+ 1.5 2.5)         ; => 4.0
```

### 4.2 - - Resta
**Función:** Resta números
```lisp
(- 10 3)            ; => 7
(- 10 2 1)          ; => 7
(- 5)               ; => -5 (negación)
```

### 4.3 * - Multiplicación
**Función:** Multiplica números
```lisp
(* 2 3 4)           ; => 24
(* 1.5 2)           ; => 3.0
```

### 4.4 / - División
**Función:** Divide números
```lisp
(/ 10 2)            ; => 5
(/ 10 3)            ; => 10/3
(/ 10.0 3)          ; => 3.3333333
```

### 4.5 EXPT - Exponenciación
**Función:** Eleva un número a una potencia
```lisp
(expt 2 3)          ; => 8
(expt 9 0.5)        ; => 3.0 (raíz cuadrada)
```

### 4.6 SQRT - Raíz Cuadrada
**Función:** Calcula la raíz cuadrada
```lisp
(sqrt 16)           ; => 4.0
(sqrt 2)            ; => 1.4142135
```

### 4.7 ABS - Valor Absoluto
**Función:** Retorna el valor absoluto
```lisp
(abs -5)            ; => 5
(abs 3.14)          ; => 3.14
```

### 4.8 MOD - Módulo
**Función:** Calcula el resto de una división
```lisp
(mod 7 3)           ; => 1
(mod 10 4)          ; => 2
```

## 5. Funciones de Comparación (6 funciones)

### 5.1 = - Igualdad Numérica
**Función:** Compara igualdad de números
```lisp
(= 5 5)             ; => T
(= 5 6)             ; => NIL
(= 5 5.0)           ; => T
```

### 5.2 < - Menor Que
**Función:** Verifica si es menor
```lisp
(< 3 5)             ; => T
(< 1 2 3 4)         ; => T (orden ascendente)
```

### 5.3 > - Mayor Que
**Función:** Verifica si es mayor
```lisp
(> 5 3)             ; => T
(> 4 3 2 1)         ; => T (orden descendente)
```

### 5.4 <= - Menor o Igual
**Función:** Verifica si es menor o igual
```lisp
(<= 5 5)            ; => T
(<= 3 5 7)          ; => T
```

### 5.5 >= - Mayor o Igual
**Función:** Verifica si es mayor o igual
```lisp
(>= 5 5)            ; => T
(>= 7 5 3)          ; => T
```

### 5.6 /= - Diferente
**Función:** Verifica si son diferentes
```lisp
(/= 5 6)            ; => T
(/= 5 5)            ; => NIL
```

## 6. Funciones de Control de Flujo (5 funciones)

### 6.1 IF - Condicional Simple
**Función:** Ejecuta código basándose en condición
```lisp
(if (> 5 3)
    "Cinco es mayor"
    "Esto no ocurre")   ; => "Cinco es mayor"
```

### 6.2 WHEN - Condicional de Una Rama
**Función:** Ejecuta múltiples expresiones si es verdadero
```lisp
(when (> 5 3)
  (print "Es verdadero")
  "Valor retornado")
```

### 6.3 UNLESS - Condicional Negativa
**Función:** Ejecuta código si la condición es falsa
```lisp
(unless (< 5 3)
  "Cinco NO es menor")  ; => "Cinco NO es menor"
```

### 6.4 COND - Condicional Múltiple
**Función:** Maneja múltiples condiciones
```lisp
(cond ((> x 0) "positivo")
      ((< x 0) "negativo")
      (t "cero"))
```

### 6.5 CASE - Selección por Valor
**Función:** Selecciona código basándose en valor
```lisp
(case dia
  (1 "Lunes")
  (2 "Martes")
  ((6 7) "Fin de semana")
  (otherwise "Día inválido"))
```

## 7. Funciones de Bucles (3 funciones)

### 7.1 DOTIMES - Bucle Numérico
**Función:** Ejecuta código n veces
```lisp
(dotimes (i 5)
  (format t "Iteración ~A~%" i))
```

### 7.2 DOLIST - Bucle sobre Lista
**Función:** Itera sobre cada elemento de una lista
```lisp
(dolist (item '(a b c d))
  (print item))
```

### 7.3 LOOP - Bucle General
**Función:** Macro de bucle muy poderosa
```lisp
(loop for i from 1 to 5
      collect (* i i))  ; => (1 4 9 16 25)
```

## 8. Funciones de Predicados (6 funciones)

### 8.1 NULL - Verificar Nulo
**Función:** Verifica si es NIL
```lisp
(null nil)          ; => T
(null '())          ; => T
(null '(1 2))       ; => NIL
```

### 8.2 NUMBERP - Es Número
**Función:** Verifica si es un número
```lisp
(numberp 42)        ; => T
(numberp "42")      ; => NIL
```

### 8.3 STRINGP - Es Cadena
**Función:** Verifica si es una cadena
```lisp
(stringp "hola")    ; => T
(stringp 'hola)     ; => NIL
```

### 8.4 SYMBOLP - Es Símbolo
**Función:** Verifica si es un símbolo
```lisp
(symbolp 'simbolo)  ; => T
(symbolp "simbolo") ; => NIL
```

### 8.5 LISTP - Es Lista
**Función:** Verifica si es una lista
```lisp
(listp '(1 2 3))    ; => T
(listp nil)         ; => T
(listp 42)          ; => NIL
```

### 8.6 ATOM - Es Átomo
**Función:** Verifica si es un átomo (no lista)
```lisp
(atom 42)           ; => T
(atom 'simbolo)     ; => T
(atom '(1 2))       ; => NIL
```

## 9. Funciones Lógicas (3 funciones)

### 9.1 AND - Y Lógico
**Función:** Retorna T si todos son verdaderos
```lisp
(and t t t)         ; => T
(and t nil t)       ; => NIL
```

### 9.2 OR - O Lógico
**Función:** Retorna T si alguno es verdadero
```lisp
(or nil nil t)      ; => T
(or nil nil nil)    ; => NIL
```

### 9.3 NOT - Negación Lógica
**Función:** Invierte el valor lógico
```lisp
(not t)             ; => NIL
(not nil)           ; => T
```

## 10. Funciones de Entrada/Salida (4 funciones)

### 10.1 PRINT - Imprimir Objeto
**Función:** Imprime objeto en forma legible
```lisp
(print "Hola mundo")    ; Imprime: "Hola mundo"
(print '(1 2 3))        ; Imprime: (1 2 3)
```

### 10.2 FORMAT - Formato de Salida
**Función:** Imprime con formato específico
```lisp
(format t "Hola ~A~%" "Juan")       ; Imprime: Hola Juan
(format t "Número: ~D~%" 42)        ; Imprime: Número: 42
```

### 10.3 READ - Leer Entrada
**Función:** Lee una expresión desde entrada estándar
```lisp
(read)              ; Espera entrada del usuario
```

### 10.4 READ-LINE - Leer Línea
**Función:** Lee una línea completa como cadena
```lisp
(read-line)         ; Lee línea completa
```

## 11. Funciones Adicionales de Listas (4 funciones)

### 11.1 FIRST, SECOND, etc. - Accesores de Lista
**Función:** Acceden a elementos específicos
```lisp
(first '(a b c d))      ; => A (equivale a CAR)
(second '(a b c d))     ; => B
(third '(a b c d))      ; => C
```

### 11.2 LAST - Último Elemento
**Función:** Retorna el último cons de la lista
```lisp
(last '(1 2 3 4))       ; => (4)
(car (last '(1 2 3 4))) ; => 4
```

### 11.3 MEMBER - Buscar Miembro
**Función:** Busca un elemento en la lista
```lisp
(member 'b '(a b c d))  ; => (B C D)
(member 'x '(a b c d))  ; => NIL
```

### 11.4 REMOVE - Remover Elementos
**Función:** Remueve elementos de la lista
```lisp
(remove 'b '(a b c b d))    ; => (A C D)
(remove 2 '(1 2 3 2 4))     ; => (1 3 4)
```
