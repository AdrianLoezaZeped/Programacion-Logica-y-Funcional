# Ejercicios de tarea
Realizar los siguientes algoritmos proporcionados por el docente
### Problema 1
Que calcule el sueldo que le corresponde al trabajador de una
empresa que cobra 40.000 euros anuales, el programa debe realizar los
cálculos en función de los siguientes criterios:

- Si lleva más de 10 años en la empresa se le aplica un aumento del 10%.
- Si lleva menos de 10 años pero más que 5 se le aplica un aumento del
  7%.
- Si lleva menos de 5 años pero más que 3 se le aplica un aumento del
  5%.
- Si lleva menos de 3 años se le aplica un aumento del 3%.()

```
(defun sueldo (x) ;Se crea la funcion con nombre de sueldo
  (cond ;Llamo la funcion cond
    ((and (>= x 3) (<= x 5)) (format nil "Su sueldo seria de $42,000")) ;Procedo a realizar todas las condicionales a cumplir 
    ((and (>= x 5) (<= x 9)) (format nil "Su sueldo seria de $42,800"))
    ((>= x 10) (format nil "Su sueldo seria de $44,000"))
    ((< x 3) (format nil "Su sueldo seria de $41,200"))
    (t (format nil "No válido"))))
    ;Basado a los años que lleves en la impresa se van a ingresar y va comparar todas las condiciones y va imprimir un mensaje por medio de "format nill"
```
<img width="336" height="249" alt="image" src="https://github.com/user-attachments/assets/67f14465-723c-4b4d-a4f8-3249e464fe89" />

Pruebas realizadas en la terminal de Clisp en donde cumple con todas la condiciones indicadas
### Problema 2
Hacer un algortimo que tome el peso en libras de una cantidad de
ropa a lavar en una lavadora y nos devuelva el nivel dependiendo del
peso; además nos informe la cantidad de litros de agua que
necesitamos. Se sabe que con más de 30 libras la lavadora no funcionara
ya que es demasiado peso. Si la ropa pesa 22 ó más libras, el nivel será
de máximo; si pesa 15 ó más nivel será de alto; si pesa 8 ó más será un
nivel medio o de lo contrario el nivel será minimo
```
(defun pesoLavadora (x) ;Se crea la funcion llamada pesoLavadora
  (cond ;Llamo la funcion cond
    ((and (>= x 22) (<= x 30)) (format nil "Es el nivel maximo de agua")) ;Procedo a realizar todas las condicionales a cumplir 
    ((and (>= x 15) (<= x 22)) (format nil "Es el nivel alto de agua"))
    ((and (>= x 8) (<= x 15)) (format nil "El nivel medio del agua"))
    ((and (>= x 0) (<= x 8)) (format nil "Es el nivel mas bajo del agua"))
    (t (format nil "Pasa del limite permitido por la lavadora"))))
    ;Basado en el peso ingresa en libras, te va indicar el nivel de agua que necesitas para lavar la ropa
```
<img width="518" height="252" alt="image" src="https://github.com/user-attachments/assets/4fc56810-c26d-463d-9380-8d9e567fed4d" />

Pruebas realizadas en la terminal de Clisp en donde cumple con todas la condiciones indicadas
### Problema 3
Martha va a realizar su fiesta de quince años. Por lo cual ha
invitado a una gran cantidad de personas. Pero también ha decidido
algunas reglas: Que todas las personas con edades mayores a los quince
años, sólo pueden entrar si traen regalos; que jóvenes con los quince
años cumplidos entra totalmente gratis pero los de menos de quince años
no pueden entrar a la fiesta. Hacer un algoritmo donde se tome la edad
de una persona y que requisito de los anteriores le toca cumplir si
quiere entrar.

```
(defun fiestaMartha (x) ;Se crea la funcion con el nombre de fiestaMartha
  (cond ;Llamo la funcion cond
    ((> x 15) (format nil "Debes llevar regalo")) ;Procedo a realizar todas las condicionales a cumplir 
    ((= x 15) (format nil "Puede entrar gratis"))
    ((< x 15) (format nil "No puede entrar"))
    (t (format nil "Ingrese un valor correcto")))
    ;Basado en tu edad ingresada la funcion te indicara los requisitos para asistir a la fiesta o en respectivo caso si puedes asistir o no
```
<img width="345" height="144" alt="image" src="https://github.com/user-attachments/assets/1ab564ec-cf53-4840-ad4a-48b8e2d89848" />

Pruebas realizadas en la terminal de Clisp en donde cumple con todas la condiciones indicadas
