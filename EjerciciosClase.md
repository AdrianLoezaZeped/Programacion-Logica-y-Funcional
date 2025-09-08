# Ejecicios en CLISP
## Suma de dos numeros
```
(defun suma (a b)
  (+ a b))
```
## Factorial 
```
(defun factorial (x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))
```
## AreaCuadrado
```
(defun areacuadro (a b)
  (* a b))
```
## Fibonacci
```
(defun fibonacci (x)
  (if (<= x 1)
      x
      (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))
```
Realize un ejemplo de como seria de otra a la vista en clases
```
(defun fibo (x)
  (if (< x 2)
      1
      (+ (fibo (- x 1)) (fibo (- x 2)))))
```
## Division por Restas
```
(defun division-por-restas (a b)
  (if (< a b)
      (list 0 a)   ; caso base: no se puede restar mas
      (let ((res (division-por-restas (- a b) b)))
        (list (+ 1 (first res)) (second res)))))
```
## Multiplicacion por sumas
```
(defun multiplicacion (a b)
  (if (= b 0)
      0
      (+ a (multiplicacion a (- b 1)))))
```
