# Problema de Juguete
Los problemas de juguete o problemas de juguete (toy problems) en programación lógica son ejemplos sencillos y de baja complejidad, diseñados para ilustrar y practicar los conceptos básicos del paradigma de la programación lógica, como las cláusulas, las consultas, las uniones y el backtracking. Se utilizan para entender cómo el motor de inferencia de un lenguaje lógico deduce soluciones a partir de un conjunto de hechos y reglas, y no son problemas complejos que requieren un análisis profundo o algoritmos sofisticados. 

## Asertijo 1
Cuatro personas necesitan cruzar un puente de noche con un sola
linterna. El puente es frágil y solo puede soportar a dos personas a
la vez. Cada persona tarda diferentes tiempos en cruzar (1, 2 5 y 10
minutos).  Cuando dos personas cruzan, lo hacen al ritmo del más lento
¿Cómo pueden todos cruzar el puente en 17 minutos?

* Primero, cruzan juntas la persona de 1 minuto y la de 2 minutos.
* Luego, la de 1 minuto regresa con la linterna.
* En seguida, la persona de 5 minutos y la de 10 minutos cruzan.
* La de 2 minutos que ya había cruzado regresa con la linterna. 
* Por último, vuelven a cruzar la persona de 1 minuto y la de 2 minutos. 
En total: 2+1+10+2+2 = 17 minutos.
### Formato de Matriz

| Paso | Lado Izq. | Lado Der. | Linterna | Movimiento    | Valor | Acumulado |
| ---- | --------- | --------- | -------- | ------------- | ----- | --------- |
| 0    | 1,2,5,10  | –         | I        | Inicio        | –     | 0         |
| 1    | 5,10      | 1,2       | D        | Cruzan 1 y 2  | 2     | 2         |
| 2    | 1,5,10    | 2         | I        | Regresa 1     | 1     | 3         |
| 3    | 1         | 2,5,10    | D        | Cruzan 5 y 10 | 10    | 13        |
| 4    | 1,2       | 5,10      | I        | Regresa 2     | 2     | 15        |
| 5    | –         | 1,2,5,10  | D        | Cruzan 1 y 2  | 2     | 17        |


## Asertijo 2
En una calle hay cinco casas, cada una de un color distinto.  En cada
casa vive una persona de distinta nacionalidad.  Cada dueño bebe un
único tipo de bebida, fuma una sola marca de cigarrillos y tiene una
mascota diferente a sus vecinos.  A partir de las 15 pistas
presentadas a continuación, la consigna que hay que responder es:
"¿Quién es el dueño del pez? el aleman".

El británico vive en la casa roja.
El sueco tiene un perro como mascota.
El danés toma té.
-El noruego vive en la primera casa.
El alemán fuma Prince.
La casa verde está inmediatamente a la izquierda de la blanca.
El dueño de la casa verde bebe café.
El propietario que fuma Pall Mall cría pájaros.
El dueño de la casa amarilla fuma Dunhill.
El hombre que vive en la casa del centro bebe leche.
El vecino que fuma Blends vive al lado del que tiene un gato.
El hombre que tiene un caballo vive al lado del que fuma Dunhill.
El propietario que fuma Bluemaster toma cerveza.
El vecino que fuma Blends vive al lado del que toma agua.
-El noruego vive al lado de la casa azul.
### Formato de Matriz

| Casa | Color    | Nacionalidad | Bebida  | Cigarro    | Mascota |
| ---- | -------- | ------------ | ------- | ---------- | ------- |
| 1    | Amarilla | Noruego      | Agua    | Dunhill    | Gato    |
| 2    | Azul     | Danés        | Té      | Blends     | Caballo |
| 3    | Roja     | Británico    | Leche   | Pall Mall  | Pájaros |
| 4    | Verde    | Alemán       | Café    | Prince     | Pez     |
| 5    | Blanca   | Sueco        | Cerveza | Bluemaster | Perro   |

## Asertijo 3 
Hay que cruzar un granjero, pollo, coyote y un maiz pero no se pueden quedar el pollo con el coyote, ni el pollo con el maiz
* Primero cruzas al granjero con el pollo
* Despues cruzas el granjero con el coyote y regresas al pollo 
* Despues cruzas el maiz 
* Despues cruzas el pollo

### Formato de Matriz
| Paso | Lado Izq. | Lado Der. | Barca    | Movimiento    |
| ---- | --------- | --------- | -------- | ------------- |
| 0    | g,p,c,m   | –         | I        | Inicio        |
| 1    | c,m       | g,p       | D        | Cruzan g y p  |
| 2    | g,c,m     | p         | I        | Regresa g     |
| 3    | m         | g,p,c     | D        | Cruzan g y c  |
| 4    | g,p,m     | c         | I        | Regresa g y p |
| 5    | p         | g,c,m     | D        | Cruzan g y m  |
| 6    | p , g     | c,m       | I        | Regresa g     |
| 7    | -         | g,c,m,p   | D        | Cruzan g y p  |

Para resolver en lisp se utiliza "Espacios de Estados" se desglosa todas las combinaciones para determinar si tienen nodos terminales o si se puede desarollar todavia mas, para poder llegar a todas las soluciones posibles, eso se utiliza la mayor parte de los programas de programacion logicas y funcional se realiza el desarollo hasta llegar el estado final
por medio de "assoc, car, cdr" se utilizan los espacios de solucion para poder recorrer todo el arbol generado 




