# Logica de primer nivel

La LPO es un sistema formal diseñado para razonar sobre objetos, sus propiedades y las relaciones entre ellos, utilizando cuantificadores.

## Elementos de la Lógica de Primer Orden

### 1. Variables de Individuo ($\mathbf{x, y, z}$...)
Símbolos que se utilizan para representar **cualquier elemento no especificado** dentro del **dominio del discurso**. Son fundamentales para la generalización y el uso de cuantificadores.

* **Ejemplo:** En la fórmula $\forall x\ (\text{Humano}(x) \rightarrow \text{Mortal}(x))$, la variable $x$ puede tomar como valor a Sócrates, Platón o cualquier otro individuo en el dominio.

### 2. Constantes de Individuo ($\mathbf{a, b, c}$...)
Símbolos que representan un **objeto particular y específico** del dominio del discurso.

* **Ejemplo:** **Sócrates**, **Madrid**, **3**. En una fórmula se escribiría $\text{Mortal}(\text{Sócrates})$.

### 3. Cuantificadores
Símbolos utilizados para expresar la **cantidad de objetos** en el dominio que satisfacen una propiedad o relación.

* **Cuantificador Universal ($\mathbf{\forall}$):** Se lee "para todo", "para cada" o "todos". Indica que una propiedad se cumple para **todos** los elementos del dominio.
    * **Ejemplo:** $\forall x\ (\text{Humano}(x) \rightarrow \text{Mortal}(x))$ (Todos los humanos son mortales).

* **Cuantificador Existencial ($\mathbf{\exists}$):** Se lee "existe al menos uno" o "alguno". Indica que una propiedad se cumple para **uno o más** elementos del dominio.
    * **Ejemplo:** $\exists x\ (\text{Planeta}(x) \land \text{Habitado}(x))$ (Existe al menos un planeta que está habitado).

### 4. Propiedades (Predicados Unarios)
Símbolos de **predicado** que representan una característica o cualidad de **un solo individuo** en el dominio. Tienen aridad 1.

* **Ejemplo:** **Verde($x$)**, **Primo($y$)**, **Es-Un-Perro(Max)**. Formalmente, $\text{Verde}(x)$.

### 5. Relaciones (Predicados Poliádicos)
Símbolos de **predicado** que representan un vínculo o conexión entre **dos o más individuos** en el dominio. Tienen aridad $\ge 2$.

* **Ejemplo:** **Mayor-Que($x, y$)**, **Hijo-De(Juan, Pedro)**. Formalmente, $\text{Mayor-Que}(5, 3)$.

### 6. Dominio del Discurso (o Universo de Discurso)
El **conjunto no vacío** de todos los objetos sobre los cuales se interpretan las fórmulas de la LPO. Todas las constantes de individuo, y los valores que pueden tomar las variables, deben pertenecer a este conjunto.

* **Ejemplo:** Si el dominio es el conjunto de los **números naturales** ($\mathbb{N}$), la fórmula $\text{Mayor-Que}(5, 3)$ es verdadera, y la variable $x$ en $\forall x\ (\text{Par}(x))$ solo se refiere a números naturales. Si el dominio fuera el conjunto de las personas, esas fórmulas no tendrían sentido en ese contexto.

## Ejercicio de clase
Variables de individuo constantes de individuo, cuantificador,
propiedades, relaciones, dominio del discurso

• La Tierra es un planeta.

Concepto de LPO	Elemento de la Frase	Tipo de Uso

Dominio del Discurso	Objetos celestes.	El grupo de cosas al que pertenece "La Tierra".

Constante de Individuo	La Tierra.	El objeto específico y con nombre propio.

Propiedades	Es un planeta.	La cualidad o característica que se le atribuye a "La Tierra".

Variables/Cuantificadores/Relaciones	Ninguno.	No hay generalización ("todos") ni relación con otro objeto.


• La Luna no es un planeta.

Dominio del Discurso	Objetos celestes.	El grupo de cosas al que pertenece "La Luna".

Constante de Individuo	La Luna.	El objeto específico y con nombre propio.

Propiedades	Es un planeta.	La cualidad que se está negando para "La Luna".

Conectiva Lógica	No (la negación).	El operador que invierte el valor de verdad de la afirmación.

Variables/Cuantificadores/Relaciones	Ninguno.	No hay generalización ni relación con otros objetos.


• La Luna es un satélite.

Dominio del Discurso	Objetos celestes.	El grupo de cosas al que pertenece "La Luna".

Constante de Individuo	La Luna.	El objeto específico y con nombre propio.

Propiedades	Es un satelite.	

Variables/Cuantificadores/Relaciones	Ninguno.	No hay generalización ni relación con otros objetos.


• La Tierra gira alrededor del Sol.

Dominio del Discurso	Objetos celestes..

Constante de Individuo	La tierra, Sol.	

Propiedades	Es gira alrededor.	

Variables/Cuantificadores/Relaciones	Niguna.	


• Todo planeta es un satélite.

Dominio del Discurso	Objetos celestes..

Constante de Individuo	Ninguna.

Variable de Individuo X

Propiedades	Es un satelite y un planeta.	

Cuantificadores Todo

Variables/Relaciones	Niguna.	


• Todo planeta gira alrededor del Sol.

Dominio del Discurso	Objetos celestes..

Constante de Individuo	Sol.

Variable de Individuo X

Propiedades	Es planeta .	

Cuantificadores Todo

Relacion Gira alrededor

Variables	Niguna.	


• Algún planeta gira alrededor de la Luna.

Dominio del Discurso	Objetos celestes..

Constante de Individuo	Luna.

Variable de Individuo X

Propiedades	Es planeta.	

Cuantificadores Existencial

Relacion Gira alrededor de


• Hay por lo menos un satélite.

Dominio del Discurso	Objetos celestes..

Variable de Individuo X

Propiedades	Es satelite.	

Cuantificadores Existencial


• Todos los perros del vecindario muerden a algún cartero.

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perros del Vecindario.

Variable de Individuo X

Propiedades	Muerden.	

Cuantificadores universal Todo

Cuantificadores existenciales muerden al cartero

Relacion muerden a

Constantes de individuo Ninguna


• Hay un cartero al que lo muerden todos los perros

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perros del Vecindario.

Propiedades	Es cartero y perro.	

Cuantificadores universal Todo los perros

Cuantificadores existenciales Hay un cartero

Constantes de individuo Ninguna


• Todos los carteros son mordidos por algún perro

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perros del Vecindario.

Variable de Individuo X

Propiedades	Muerden.	

Cuantificadores universal Todo los perros

Cuantificadores existenciales Algun perro

Relacion es mordido por(o muerde a)


• Hay un perro que muerde a todos los carteros

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perro.

Propiedades	Es cartero , es perro.	

Cuantificadores universal Todos los carteros

Cuantificadores existenciales Hay un perro

Relacion es mordido por(o muerde a)


• Todos los perros que asustan a algún cartero, lo muerden

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perro.

Propiedades	Es cartero, es perro.	

Cuantificadores universal Todos los perros

Cuantificadores existenciales asustan algun cartero

Relacion Asustar, muerden a 


• Hay un perro que muerde a todos los perros que muerden a algún cartero

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Cartero.

Variables de Individuo	Perro.

Propiedades	Es cartero, es perro, morder algun cartero.	

Cuantificadores universal Muerde a todos los perros 

Cuantificadores existenciales Hay un perro, muerden a algun cartero

Relacion morder a algun cartero


• Hay un solo perro que se muerde a sí mismo

Dominio del Discurso	Seres del Vecindario..

Variables de Individuo	Perro.

Propiedades	Es perro

Cuantificadores existenciales Hay un perro solo perro

Relacion Muerde a

