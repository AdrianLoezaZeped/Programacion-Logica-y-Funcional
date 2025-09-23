# Arbol Genealogico
### Abuelos
*	Filiberto
*	Eugenia
*	Mela
*	Eustacio
### Tios
* Luna
*	Tavo
*	Gabriel
*	Luis
*	Catalina
*	Fatima
*	Martha
### Primos
*	LuisEd
*	Alan 
*	Santy
*	Andrea
*	Tavito
*	barbara
### Papas
*	Sol 
*	Baltazar
### Hermano
* BaltazarJr

## Codigo
```
(defparameter *familia*
 '((masculino
      (filiberto
         (rol abuelo)
         (estatura baja)
         (pelo castano)
         (conyuge eugenia)
         (hijos (sol luna gabriel tavo luis)))
      (eustacio
         (rol abuelo)
         (conyuge mela)
         (estatura baja)
         (pelo negro)
         (hijos (baltazar catalina)))
      (tavo
         (rol tio)
         (estatura alto)
         (pelo negro)
         (conyuge fatima)
         (hijos (tavito)))
      (gabriel
         (rol tio)
         (conyuge no)
         (estatura alta)
         (pelo negro)
         (hijos (no)))
    (luis
         (rol tio)
         (estatura baja)
         (pelo negro)
         (conyuge martha)
         (hijos (alan luisEd)))
      (luisEd
         (rol primo)
         (conyuge no)
         (estatura baja)
         (pelo pelirojo)
         (hijos (santiago barbara)))
    (alan
         (rol primo)
         (estatura baja)
         (pelo negro)
         (conyuge no)
         (hijos (no)))
      (santiago
         (rol primo)
         (conyuge no)
         (estatura alto)
         (pelo pelirojo)
         (hijos (no)))
    (tavito
         (rol sobrino)
         (estatura baja)
         (pelo castano)
         (conyuge no)
         (hijos (no)))
      (baltazar
         (rol padre)
         (conyuge sol)
         (estatura alto)
         (pelo negro)
         (hijos (adrian baltazarJr)))
        (adrian
         (rol hijo)
         (estatura alto)
         (pelo negro)
         (conyuge no)
         (hijos (no)))
      (baltazarJr
         (rol hermano)
         (conyuge no)
         (estatura alto)
         (pelo negro)
         (hijos (no)))
      )
   (femenino
      (eugenia
         (rol abue)
         (estatura baja)
         (pelo castano)
         (conyuge filiberto)
         (hijos (sol tavo gabriel luis luna)))
      (mela
         (rol abue)
         (estatura baja)
         (pelo negro)
         (conyuge eustacio)
         (hijos (baltazar catalina)))
       (luna
         (rol tia)
         (estatura alta)
         (pelo castano)
         (conyuge no)
         (hijos (andrea)))
      (catalina
         (rol tia)
         (estatura baja)
         (pelo pelirojo)
         (conyuge no)
         (hijos (no)))
      (fatima
         (rol tia)
         (estatura baja)
         (pelo negro)
         (conyuge tavo)
         (hijos (tavito)))
      (martha
         (rol tia)
         (estatura baja)
         (pelo castano)
         (conyuge luis)
         (hijos (alan luisEd)))
       (andrea
         (rol prima)
         (estatura alta)
         (pelo guero)
         (conyuge no)
         (hijos (no)))
      (barbara
         (rol prima)
         (estatura baja)
         (pelo pelirojo)
         (conyuge no)
         (hijos (no)))
      )))

```
