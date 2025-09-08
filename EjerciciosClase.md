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


