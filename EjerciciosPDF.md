# Problemas con CAR y CDR
 Lista: (abcde)→Extraer d 
```
(cadddr '(a b c d e))
```
 Lista: ((12)(34)(56))→Extraer 5
```
(car (car (cdr (cdr '((1 2) (3 4) (5 6))))))
```
 Lista: ((ab)(cd)(ef))→Extraer e
```
(car (car (cdr (cdr '((a b) (c d) (e f))))))
```
 Lista: ((xy)((pq)(rs))(zw))→Extraer z
 ```
(car(car(cdr(cdr '((xy)((pq)(rs))(zw))))))
 ```
 Lista: ((1(23))(4(56)))→Extraer 6
 ```
(car(cdr(car(cdr(car(cdr '((1(23))(4(56)))))))))
 ```
 Lista: (((ab)c)de)→Extraer c
```
(car(cdr(car '(((ab)c)de))
```
 Lista: (((12)3)((45)6))→Extraer 6
 ```
(car(cdr(car(cdr '(((12)3)((45)6))))))
```
 Lista: ((p(q(rs)))tu)→Extraer (rs)
 ```
(car(cdr(car(cdr(car'((p(q(rs)))tu))))))
```
 Lista: (((a)b)(c(de))f)→Extraer d
 ```
(car(car(cdr(car(cdr'(((a)b)(c(de))f))))))
```
 Lista: ((1(2(34)))(56))→Extraer 3
 ```
(car(cdr(car(cdr(car(car '((1(2(34)))(56))))))))
```
 Lista: (((x)(y))((z)(w)))→Extraer (w)
 ```
(car(cdr(car(cdr'(((x)(y))((z)(w)))))))
```
 Lista: ((a(b(cd)))(ef))→Extraer c
 ```
(car(car(cdr(car(cdr(car'((a(b(cd)))(ef))))))))
```
 Lista: ((1(2(3(45))))(67))→Extraer 4
 ```
(car (cadr (cadr (cadr (car '((1 (2 (3 (4 5)))) (6 7)))))))
```
 Lista: (((ab)c)((de)f)((gh) i))→Extraer g
 ```
(car(car(car(cdr(cdr'(((ab)c)((de)f)((gh) i)))
```
 Lista: (((xy)(zw))((pq)(rs)))→Extraer r
 ```
(car(car(cdr(car(cdr'(((xy)(zw))((pq)(rs))))))))))
```
 Lista: ((1(2(3(4(56)))))(78))→Extraer 5
 ```
(car (car (cdr (car (cdr (car (cdr (car (cdr (car '((1 (2 (3 (4 (5 6))))) (7 8))))))))))))
```
 Lista: ((a(b(c(de))))(fg))→Extraer d
```
(car(car(cdr(car(cdr(car(cdr(car'((a(b(c(de))))(fg))))))))))
 ```
 Lista: (((12)(34))((56)(78)))→Extraer 7
 ```
(car(car(cdr(car(cdr'(((12)(34))((56)(78))))))))
  ```
Lista: ((x(y(z(wv)))))→Extraer w
```
  (car(car(cdr(car(cdr(car(cdr(car'((x(y(z(wv))))))))))))))
```
Lista: (((abc)(def))((ghi)(jkl)))→Extraer j
  ```
  (car(car(cdr(car(cdr'(((abc)(def))((ghi)(jkl))))))))
  ```

# Ejercicios de Lisp (Uso restringido de funciones)
## 2.1 Ejercicio 1
 Dada una lista de pares clave-valor, usar ‘assoc‘ para obtener el valor de la clave ‘’edad‘. Lista de ejemplo:
 
 ```(setq persona '((nombre . "Ana") (edad . 23) (ciudad . "Morelia")))```
 
 Pregunta: ¿cómo obtener la edad con ‘assoc‘, ‘cdr‘ y ‘car‘?

 ## 2.2 Ejercicio 2
 Usar ‘if‘ para escribir una función que diga si el primer elemento de una lista es un número positivo o no. Ejemplo:
 ```
     (mi-funcion '(5 3 2)) ; => "positivo"
     (mi-funcion '(-2 1 4)) ; => "no positivo"
```
 ## 2.3 Ejercicio 3
 Definir una función que recorra una lista de números con ‘mapcar‘ y devuelva una nueva lista que contenga solo el doble de los números pares. Restricción:
 usar ‘if‘ dentro de ‘mapcar‘.
 
 ## 2.4 Ejercicio 4
 Usar ‘cond‘ para hacer una función que reciba un símbolo que puede ser ‘rojo‘, ‘azul‘ o ‘verde‘ y regrese un mensaje:
 • rojo → "Color cálido"
 • azul → "Color frío"
 • verde → "Color neutro"
 • cualquier otro → "Color desconocido"
## 2.5 Ejercicio 5
 Escribir una función que use ‘case‘ para clasificar un día de la semana (‘lunes‘,‘martes‘, ...):
 • lunes a viernes → "día laboral"
 • sábado, domingo → "fin de semana"
 
## 2.6 Ejercicio 6
 Definir una función que reciba una lista y con ‘when‘ imprima el primer elemento si la lista no está vacía.
 
## 2.7 Ejercicio 7
 Definir una función que reciba una lista y con ‘unless‘ imprima "lista vacía"
 cuando la lista no tenga elementos.
## 2.8 Ejercicio 8
 Dada una lista de listas, usar ‘mapcar‘, ‘car‘ y ‘cdr‘ para obtener una nueva lista con los primeros elementos de cada sublista. Ejemplo:
 (mi-funcion '((1 2) (3 4) (5 6))) ; => (1 3 5)
 ## 2.9 Ejercicio 9
 Dada una lista de asociación:
 ```
 (setq alumnos '((juan . 8) (maria . 10) (ana . 9)))
 ```
 Escribir una función que, dado un nombre, devuelva "Aprobado" si la calificación es >= 8, o "Reprobado" en caso contrario. Usar ‘assoc‘, ‘cdr‘ y ‘if‘.
 
 ## 2.10 Ejercicio 10
 Definir una función que use ‘cond‘ para evaluar una lista de números y de volver:
 
 • "vacía" si no hay elementos
 
 • "un solo elemento" si la lista tiene uno
 
 • "larga" si tiene más de uno.
