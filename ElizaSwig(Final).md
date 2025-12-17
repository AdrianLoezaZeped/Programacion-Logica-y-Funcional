# Eliza en SwigProlog
Este proyecto implementa un chatbot tipo **ELIZA** usando **Prolog**.  
El sistema se basa en reglas y plantillas para interpretar frases del usuario y generar respuestas.

El chatbot puede:
- Mantener una conversación básica
- Registrar un paciente
- Guardar síntomas mencionados
- Inferir posibles enfermedades
- Calcular probabilidad, severidad y riesgo
- Generar un reporte médico completo
- Responder consultas sobre una base de familia y una base de carros

---

## Flujo general de Eliza

1. El sistema muestra un saludo.
2. El usuario escribe una frase.
3. La entrada se normaliza (minúsculas y sin signos).
4. Se compara la frase con una lista de **plantillas**.
5. Si hay coincidencia, se ejecuta la acción correspondiente.
6. El chatbot responde y espera otra entrada.
7. El ciclo se repite hasta que el usuario escriba `adios` o `bye`.

---

## Normalización de entrada

Para evitar errores al comparar texto, la entrada del usuario se limpia de la siguiente forma:

* Se convierten todas las palabras a minúsculas
* Se eliminan signos de puntuación como `. , ? ! ; :`

Ejemplo:

Entrada del usuario:

```
Hola, MI Nombre es Juan.
```

Entrada normalizada:

```
[hola, mi, nombre, es, juan]
```

Esto permite que el chatbot compare frases de manera consistente.

---

## Motor de patrones (matching)

El chatbot utiliza un sistema de **plantillas (templates)** para reconocer intenciones del usuario.

Cada plantilla tiene la forma:

```prolog
template(Patron, Accion, Indices).
```

* **Patrón**: estructura de la frase esperada
* **Acción**: bandera que indica qué debe hacer el sistema
* **Índices**: posiciones de palabras importantes dentro de la frase

Ejemplo:

```prolog
template([mi, nombre, es, s(_)], [flagSetPaciente], [3]).
```

Esta plantilla detecta frases como:

```
mi nombre es juan
```

---

## Comodines en las plantillas

El uso de `s(_)` permite aceptar cualquier palabra en esa posición.

Ejemplo:

```prolog
template([tengo, s(_)], [flagDiagnostico], [1]).
```

Acepta frases como:

* `tengo fiebre`
* `tengo congestion`
* `tengo estornudos`

---

## Generación de respuestas (replace0)

El predicado `replace0/5` es el encargado de:

* Extraer palabras importantes de la entrada
* Consultar la base de conocimiento
* Generar la respuesta final
* Guardar información en memoria si es necesario

Aquí se implementa la lógica principal del chatbot.

---

## Manejo de memoria

El sistema recuerda información usando dos mecanismos:

### Paciente actual

Se guarda en una variable global:

```prolog
nb_setval(paciente_actual, Nombre)
```

Esto permite que los síntomas se asocien al paciente correcto.

### Síntomas del paciente

Los síntomas se guardan dinámicamente:

```prolog
:- dynamic sintoma/2.
```

Ejemplo:

```prolog
sintoma(juan, fiebre).
```

Los síntomas se van acumulando conforme avanza la conversación.

---

## Base médica

La base médica se compone de hechos como:

```prolog
tiene_sintoma(gripe, fiebre).
tratamiento(gripe, 'Reposo e hidratacion').
```

A partir de estos hechos, el sistema puede:

* Proponer enfermedades posibles
* Calcular probabilidad
* Determinar severidad
* Evaluar nivel de riesgo
* Generar recomendaciones

---

## Diagnóstico básico

Una enfermedad se considera posible si el paciente presenta al menos uno de sus síntomas.

Esto permite mostrar opciones tempranas aunque no haya todos los síntomas confirmados.

---

## Probabilidad y severidad

* **Probabilidad**: porcentaje de síntomas confirmados
* **Severidad**:

  * 1 síntoma → Leve
  * 2 síntomas → Moderada
  * 3 o más → Severa

---

## Árbol de decisión

El sistema incluye un árbol de decisión médico simple que prioriza síntomas clave como:

* fiebre
* rigidez de cuello
* dolor de cabeza severo
* estornudos
* ardor

Esto permite obtener un diagnóstico más directo cuando los síntomas son claros.

---

## Reporte completo

El usuario puede solicitar un reporte completo con frases como:

```
dame un reporte completo
```

El reporte incluye:

* Paciente
* Síntomas confirmados
* Enfermedades posibles
* Probabilidad
* Severidad
* Nivel de riesgo
* Tratamiento
* Recomendación

---

## Base de familia

Esta base está construida a partir de **hechos simples**, como quién es padre, madre, esposo o esposa de otra persona, y **reglas**, que permiten inferir relaciones más complejas.

### Hechos familiares

Los hechos definen información directa, por ejemplo:

- Cuantos hijos tiene filiberto
- Adrian es hijo de sol
- Quien es abuelo de barbara

Ejemplo conceptual:
```
padrede(filiberto, sol).
madrede(eugenia, sol).
```
---

## Base de carros
El sistema también incluye una base de información sobre automóviles, utilizada para responder preguntas generales sobre distintos modelos de carros.

A diferencia del módulo médico, esta base no realiza inferencias complejas, sino que consulta datos ya conocidos.
### Ejemplo de preguntas

* cuanto cuesta un toyota_corolla
* donde se fabrica el ford_mustang
* que motor tiene el ferrari_488
* cuantos caballos tiene el bmw_x3
### Ejemplo de la informacion de los automoviles
```
precio(toyota_corolla, 25).
potencia(toyota_corolla, 169).
```

# Codigo
```
% =========================================================
% CONFIGURACION GENERAL
% =========================================================

:- dynamic sintoma/2.
% "sintoma/2" es dinámico porque el programa va a agregar síntomas en tiempo de ejecución
% Ejemplo de lo que se guarda: sintoma(juan, fiebre).

:- discontiguous replace0/5.
% Permite que las reglas de replace0/5 estén separadas en distintas partes del archivo
% (por ejemplo: unas para medicina, otras para familia, otras para carros)

% =========================================================
% MAIN (INICIO DEL CHATBOT)
% =========================================================

eliza :-
    nb_setval(paciente_actual, desconocido),
    % Guarda en memoria el nombre del paciente actual (inicia como "desconocido")

    writeln('hola, mi nombre es eliza tu chatbot.'),
    % Mensaje inicial del bot

    writeln('ingresa tu consulta (solo minusculas, sin punto al final):'),
    % Instrucción para que el usuario escriba en un formato simple

    readln(Raw),
    % Lee lo que escribe el usuario como lista de palabras/tokens

    normalizar_input(Raw, Input),
    % Convierte a minúsculas y elimina signos para facilitar el matching

    eliza(Input), !.
    % Entra al ciclo principal del chat con la entrada ya normalizada

% =========================================================
% SALIDA DEL PROGRAMA
% =========================================================

eliza(Input) :-
    ( Input == [adios]
    ; Input == [bye]
    ),
    % Si el usuario escribe "adios" o "bye", se termina la conversación

    writeln('adios. espero poder verte ayudado.'),
    % Mensaje final

    !.
    % Corta para que no siga buscando más reglas

% =========================================================
% CICLO PRINCIPAL DE CONVERSACION
% =========================================================

eliza(Input) :-
    template(Stim, Resp, IndStim),
    % Selecciona un template (patrón) de los disponibles:
    % Stim = patrón que debe coincidir
    % Resp = acción/bandera (ej. flagDiagnostico)
    % IndStim = posiciones importantes dentro del input

    match(Stim, Input),
    % Verifica si la entrada del usuario coincide con el patrón del template

    replace0(IndStim, Input, 0, Resp, R),
    % Genera la respuesta final (y puede guardar datos) según la bandera detectada

    ( R \= [] -> writeln(R) ; true ),
    % Imprime la respuesta si no está vacía

    readln(Raw1),
    % Lee la siguiente frase del usuario

    normalizar_input(Raw1, Input1),
    % Normaliza la entrada nuevamente

    eliza(Input1), !.
    % Repite el ciclo del chatbot con la nueva entrada

% =========================================================
% NORMALIZACION (LIMPIAR ENTRADAS)
% =========================================================

limpiar_token(T, T2) :-
    atom(T),
    % Se asegura de trabajar solo con átomos (palabras)

    downcase_atom(T, L),
    % Convierte la palabra a minúsculas

    \+ member(L, ['.', ',', '?', '!', ';', ':']),
    % Si el token es puntuación, no se considera

    T2 = L, !.
    % Regresa el token limpio

limpiar_token(T, T2) :-
    atom(T),
    downcase_atom(T, T2), !.
    % Si es palabra normal, solo la convierte a minúsculas

limpiar_token(T, T).
% Si no es átomo, lo deja igual

normalizar_input(In, Out) :-
    findall(X,
        ( member(T, In),
          % Recorre cada token de la entrada

          limpiar_token(T, X),
          % Limpia cada token

          atom(X),
          \+ member(X, ['.', ',', '?', '!', ';', ':'])
          % Quita signos sueltos por seguridad
        ),
        Out).
    % Devuelve la lista final de palabras normalizadas

% =========================================================
% MATCH (COMPARACION SIMPLE POR PLANTILLAS)
% =========================================================

match([], []).
% Patrón vacío coincide con lista vacía

match([], _) :- true.
% Patrón vacío también se considera coincidencia (permite flexibilidad)

match([S|Stim], [I|Input]) :-
    atom(S),
    % Si el elemento del patrón es una palabra fija...

    S == I,
    % ...debe ser exactamente igual a la palabra del usuario

    match(Stim, Input), !.
    % Continúa con el resto de la lista

match([S|Stim], [_|Input]) :-
    \+ atom(S),
    % Si no es átomo, es un comodín como s(_)

    match(Stim, Input), !.
    % Acepta cualquier palabra en esa posición

% =========================================================
% TEMPLATES (INTENCIONES)
% =========================================================

% Registro del paciente (guarda el nombre)
template([hola, mi, nombre, es, s(_)], [flagSetPaciente], [4]).
template([mi, nombre, es, s(_)],      [flagSetPaciente], [3]).
template([soy, s(_)],                 [flagSetPaciente], [1]).

% Diagnóstico básico: detecta síntomas y los guarda
template([tengo, s(_), y, s(_)], [flagDiagnostico], [1, 3]).
template([tengo, s(_)],         [flagDiagnostico], [1]).
template([siento, s(_), y, s(_)], [flagDiagnostico], [1, 3]).
template([siento, s(_)],          [flagDiagnostico], [1]).

% Actividad 2: buscar un síntoma exclusivo
template([diagnostico, exclusivo], [flagExclusivo], []).
template([sintoma, exclusivo],     [flagExclusivo], []).

% Tratamientos y síntomas de una enfermedad
template([cual, es, el, tratamiento, para, s(_)], [flagTratamiento], [5]).
template([como, se, trata, la, s(_)],             [flagTratamiento], [4]).
template([cuales, son, los, sintomas, de, s(_)],  [flagSintomas],    [5]).
template([que, sintomas, tiene, la, s(_)],        [flagSintomas],    [4]).

% Actividad 3: probabilidad
template([que, probabilidad, tengo, de, tener, s(_)], [flagProbabilidad], [5]).
template([cual, es, la, probabilidad, de, s(_)],      [flagProbabilidad], [5]).

% Actividad 4: preventivo
template([estoy, en, riesgo, de, s(_)], [flagPreventivo], [4]).
template([puedo, tener, s(_)],          [flagPreventivo], [2]).

% Actividad 5: enfermedades similares
template([que, enfermedades, son, similares, a, s(_)], [flagSimilares], [5]).
template([enfermedades, similares, a, s(_)],           [flagSimilares], [3]).

% Actividad 6: síntomas contradictorios
template([tengo, sintomas, contradictorios], [flagContradictorios], []).
template([hay, sintomas, contradictorios],   [flagContradictorios], []).

% Actividad 7: diagnóstico por árbol de decisión
template([diagnostico, por, arbol], [flagArbol], []).
template([usar, arbol],            [flagArbol], []).

% Actividad 8: riesgo
template([cual, es, mi, nivel, de, riesgo], [flagRiesgo], []).
template([nivel, de, riesgo],               [flagRiesgo], []).

% Severidad
template([que, tan, grave, es, mi, condicion], [flagSeveridad], []).

% Actividad 9: tratamiento combinado
template([tratamiento, combinado], [flagTratamientoCombinado], []).
template([dame, tratamientos],     [flagTratamientoCombinado], []).

% Actividad 10: recomendación
template([dame, recomendacion], [flagRecomendacion], []).
template([recomendacion],       [flagRecomendacion], []).

% Actividad 11: diagnosticar y tratar en un paso
template([diagnosticar, y, tratar], [flagDiagTratar], []).
template([diagnostico, y, tratamiento], [flagDiagTratar], []).

% Actividad 12: reporte completo
template([dame, un, reporte, completo], [flagReporteBonito], []).
template([quiero, un, diagnostico, completo], [flagReporteBonito], []).
template([reporte], [flagReporteBonito], []).

% Plantillas para familia (relaciones y preguntas)
% (El bot usa la misma lógica de templates + replace0 para responder con la base familiar)
template([s(_), es, hijo, de, s(_)],    [flagFamilia, hijo],    [0,4]).
template([s(_), es, hija, de, s(_)],    [flagFamilia, hijo],    [0,4]).
template([s(_), es, padre, de, s(_)],   [flagFamilia, padre],   [0,4]).
template([s(_), es, madre, de, s(_)],   [flagFamilia, madre],   [0,4]).
template([s(_), es, hermano, de, s(_)], [flagFamilia, hermano], [0,4]).
template([s(_), es, hermana, de, s(_)], [flagFamilia, hermano], [0,4]).
template([s(_), es, abuelo, de, s(_)],  [flagFamilia, abuelo],  [0,4]).
template([s(_), es, abuela, de, s(_)],  [flagFamilia, abuelo],  [0,4]).
template([s(_), es, tio, de, s(_)],     [flagFamilia, tio],     [0,4]).
template([s(_), es, tia, de, s(_)],     [flagFamilia, tio],     [0,4]).
template([s(_), es, primo, de, s(_)],   [flagFamilia, primo],   [0,4]).
template([s(_), es, prima, de, s(_)],   [flagFamilia, primo],   [0,4]).
template([s(_), es, nieto, de, s(_)],   [flagFamilia, nieto],   [0,4]).
template([s(_), es, nieta, de, s(_)],   [flagFamilia, nieto],   [0,4]).
template([s(_), es, sobrino, de, s(_)], [flagFamilia, sobrino], [0,4]).
template([s(_), es, sobrina, de, s(_)], [flagFamilia, sobrino], [0,4]).
template([s(_), es, esposo, de, s(_)],  [flagFamilia, esposo],  [0,4]).
template([s(_), es, esposa, de, s(_)],  [flagFamilia, esposo],  [0,4]).
template([s(_), es, cunado, de, s(_)],  [flagFamilia, cunado],  [0,4]).
template([s(_), es, cunada, de, s(_)],  [flagFamilia, cunado],  [0,4]).

template([quien, es, el, padre, de, s(_)], [flagFamilia, quien_padre], [5]).
template([quien, es, la, madre, de, s(_)], [flagFamilia, quien_madre], [5]).
template([quienes, son, los, hijos, de, s(_)], [flagFamilia, quien_hijos], [5]).
template([quienes, son, los, hermanos, de, s(_)], [flagFamilia, quien_hermanos], [5]).
template([quienes, son, los, abuelos, de, s(_)], [flagFamilia, quien_abuelos], [5]).

template([cuantos, hijos, tiene, s(_)], [flagFamilia, cuantos_hijos], [3]).
template([cuantos, hermanos, tiene, s(_)], [flagFamilia, cuantos_hermanos], [3]).

% Plantillas para carros (consultas a la base de autos)
template([cual, es, la, mejor, marca, de, carros], [flagCarros, mejor_marca], []).
template([cual, es, el, carro, mas, rapido, del, mundo], [flagCarros, mas_rapido], []).
template([cuanto, cuesta, un, s(_)], [flagCarros, precio], [3]).
template([donde, se, fabrica, el, s(_)], [flagCarros, fabrica], [4]).
template([cuantos, caballos, tiene, el, s(_)], [flagCarros, potencia], [4]).
template([que, motor, tiene, el, s(_)], [flagCarros, motor], [4]).
template([cuanto, consume, el, s(_)], [flagCarros, consumo], [3]).
template([que, tan, seguro, es, el, s(_)], [flagCarros, seguridad], [6]).

% Saludos simples
template([hola|_],    [hola, como, estas, tu, '?'], []).
template([buendia|_], [buen, dia, como, estas, tu, '?'], []).
template([como, estas, tu],     [yo, estoy, bien, gracias, por, preguntar, '.'], []).
template([como, estas, tu, '?'],[yo, estoy, bien, gracias, por, preguntar, '.'], []).

% Catch-all: si nada coincide, responde esto
template(_, [please, explain, a, little, more, '.'], []).

% =========================================================
% BASE MEDICA
% =========================================================
% Aquí se guardan hechos de síntomas por enfermedad y tratamientos

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).

tiene_sintoma(gonorrea, ardor).
tiene_sintoma(gonorrea, secrecion).
tiene_sintoma(gonorrea, dolor_testiculos).
tiene_sintoma(gonorrea, dolor_pelvico).
tiene_sintoma(gonorrea, sangrado).
tiene_sintoma(gonorrea, picazon_rectal).
tiene_sintoma(gonorrea, dolor_garganta).
tiene_sintoma(gonorrea, dolor_ojos).

tiene_sintoma(diabetes_gestacional, sed).
tiene_sintoma(diabetes_gestacional, orina_frecuente).
tiene_sintoma(diabetes_gestacional, fatiga).
tiene_sintoma(diabetes_gestacional, vision_borrosa).
tiene_sintoma(diabetes_gestacional, infecciones).
tiene_sintoma(diabetes_gestacional, nauseas).

tiene_sintoma(cancer_pie, lunar_cambia).
tiene_sintoma(cancer_pie, mancha_crece).
tiene_sintoma(cancer_pie, herida_no_sana).
tiene_sintoma(cancer_pie, bulto).
tiene_sintoma(cancer_pie, dolor).
tiene_sintoma(cancer_pie, inflamacion).
tiene_sintoma(cancer_pie, picazon).

tiene_sintoma(meningitis, fiebre).
tiene_sintoma(meningitis, dolor_cabeza_severo).
tiene_sintoma(meningitis, rigidez_cuello).
tiene_sintoma(meningitis, confusion).
tiene_sintoma(meningitis, vomitos).
tiene_sintoma(meningitis, sensibilidad_luz).

tratamiento(gripe, 'Reposo, hidratacion, paracetamol y aislamiento.').
tratamiento(alergia, 'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana, 'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').
tratamiento(gonorrea, 'Antibioticos prescritos por un medico y seguimiento clinico.').
tratamiento(diabetes_gestacional, 'Control de dieta, ejercicio, monitoreo de glucosa y posible insulina.').
tratamiento(cancer_pie, 'Cirugia y seguimiento dermatologico; posible radioterapia o quimioterapia.').
tratamiento(meningitis, 'URGENTE: hospitalizacion, antibioticos IV y cuidados intensivos.').

% Lista de pares de síntomas que el sistema considera "contradictorios"
contradictorio(fiebre, picazon_ojos).
contradictorio(nauseas, estornudos).
contradictorio(dolor_cabeza_severo, congestion).
contradictorio(rigidez_cuello, dolor_garganta).

% =========================================================
% UTILIDADES MEDICAS
% =========================================================
% Reglas para extraer síntomas, diagnosticar, calcular probabilidad, severidad, riesgo y reportes

extraer_sintomas([], []).
% Regresa lista vacía si ya no hay tokens

extraer_sintomas([Token|R], [Token|SR]) :-
    tiene_sintoma(_, Token),
    % Si el token es un síntoma válido (aparece en la base) se agrega
    extraer_sintomas(R, SR), !.

extraer_sintomas([_|R], SR) :-
    % Si no es síntoma válido, se ignora
    extraer_sintomas(R, SR).

diagnostico_basico(P, E) :-
    % Una enfermedad es posible si el paciente tiene algún síntoma de esa enfermedad
    tiene_sintoma(E, S),
    sintoma(P, S).

todos_confirmados(_, []).
% Caso base: ya se confirmaron todos

todos_confirmados(P, [S|R]) :-
    % Verifica que el paciente tenga cada síntoma de la lista
    sintoma(P, S),
    todos_confirmados(P, R).

diagnostico_completo(P, E) :-
    % Diagnóstico estricto: el paciente debe tener todos los síntomas de esa enfermedad
    findall(S, tiene_sintoma(E, S), L),
    todos_confirmados(P, L).

contar_sintomas_confirmados(P, E, C) :-
    % Cuenta cuántos síntomas de E tiene el paciente
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, 'Severa')   :- contar_sintomas_confirmados(P, E, C), C >= 3, !.
severidad(P, E, 'Moderada') :- contar_sintomas_confirmados(P, E, C), C =:= 2, !.
severidad(P, E, 'Leve')     :- contar_sintomas_confirmados(P, E, C), C =:= 1, !.
% Clasifica severidad según número de síntomas confirmados

diagnostico_exclusivo(P, E) :-
    % Un síntoma exclusivo es aquel que solo aparece en una enfermedad
    tiene_sintoma(E, S),
    sintoma(P, S),
    \+ (tiene_sintoma(Otra, S), Otra \= E).

probabilidad(P, E, Porcentaje) :-
    % Probabilidad = (síntomas confirmados / síntomas totales de la enfermedad) * 100
    findall(S, tiene_sintoma(E, S), Todos),
    length(Todos, Tot),
    Tot > 0,
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), Conf),
    length(Conf, Num),
    Porcentaje is (Num / Tot) * 100.

diagnostico_preventivo(P, E) :-
    % Está "en riesgo" si tiene al menos 1 síntoma, pero no todos
    findall(S, tiene_sintoma(E, S), Todos),
    length(Todos, Tot),
    Tot > 0,
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), Conf),
    length(Conf, Num),
    Num >= 1,
    Num < Tot.

enfermedades_similares(E1, E2) :-
    % Dos enfermedades son similares si comparten al menos 2 síntomas
    E1 \= E2,
    findall(S, (tiene_sintoma(E1,S), tiene_sintoma(E2,S)), Comunes),
    length(Comunes, N),
    N >= 2.

sintomas_contradictorios(P) :-
    % Detecta si el paciente tiene algún par de síntomas contradictorios
    sintoma(P, S1),
    sintoma(P, S2),
    S1 \= S2,
    (contradictorio(S1,S2) ; contradictorio(S2,S1)).

arbol_diagnostico(P, E) :-
    % Árbol de decisión: reglas tipo "si tiene X, entonces probablemente Y"
    ( sintoma(P, fiebre) ->
        ( sintoma(P, rigidez_cuello) -> E = meningitis
        ; sintoma(P, confusion)      -> E = meningitis
        ; sintoma(P, dolor_cabeza)   -> E = gripe
        ; E = gripe
        )
    ; sintoma(P, estornudos) ->
        ( sintoma(P, picazon_ojos)   -> E = alergia
        ; sintoma(P, dolor_garganta) -> E = resfriado
        ; E = resfriado
        )
    ; sintoma(P, dolor_cabeza_severo) ->
        ( sintoma(P, sensibilidad_luz) -> E = migrana
        ; sintoma(P, rigidez_cuello)   -> E = meningitis
        ; E = migrana
        )
    ; sintoma(P, ardor) -> E = gonorrea
    ; sintoma(P, secrecion) -> E = gonorrea
    ; sintoma(P, sed) -> E = diabetes_gestacional
    ; sintoma(P, lunar_cambia) -> E = cancer_pie
    ; sintoma(P, mancha_crece) -> E = cancer_pie
    ; fail
    ).

% Riesgo: reglas para enfermedades graves según cuántos síntomas coinciden
riesgo(P, meningitis, alto) :- contar_sintomas_confirmados(P, meningitis, C), C >= 3, !.
riesgo(P, meningitis, medio) :- contar_sintomas_confirmados(P, meningitis, C), C =:= 2, !.
riesgo(P, meningitis, bajo) :- contar_sintomas_confirmados(P, meningitis, C), C =:= 1, !.

riesgo(P, cancer_pie, alto) :- contar_sintomas_confirmados(P, cancer_pie, C), C >= 4, !.
riesgo(P, cancer_pie, medio) :- contar_sintomas_confirmados(P, cancer_pie, C), C >= 2, C < 4, !.
riesgo(P, cancer_pie, bajo) :- contar_sintomas_confirmados(P, cancer_pie, C), C =:= 1, !.

riesgo(P, gonorrea, alto) :- contar_sintomas_confirmados(P, gonorrea, C), C >= 4, !.
riesgo(P, gonorrea, medio) :- contar_sintomas_confirmados(P, gonorrea, C), C >= 2, C < 4, !.
riesgo(P, gonorrea, bajo) :- contar_sintomas_confirmados(P, gonorrea, C), C =:= 1, !.

tratamiento_combinado(P, Lista) :-
    % Junta tratamientos de todas las enfermedades candidatas del paciente (sin repetir)
    findall(Trat,
        (diagnostico_basico(P, E), tratamiento(E, Trat)),
        Temp),
    list_to_set(Temp, Lista).

recomendacion(P, E, Texto) :-
    % Genera recomendación dependiendo de la severidad del caso
    severidad(P, E, Nivel),
    recomendacion_texto(Nivel, E, Texto).

recomendacion_texto('Severa', meningitis,
    'URGENTE: acudir inmediatamente a emergencias.').
recomendacion_texto('Severa', _,
    'Se recomienda acudir al medico lo antes posible.').
recomendacion_texto('Moderada', _,
    'Consultar con un medico en las proximas 24-48 horas.').
recomendacion_texto('Leve', _,
    'Monitorear sintomas y acudir si empeoran o persisten.').

diagnosticar_y_tratar(P, Diagnostico, Trat) :-
    % Intenta diagnosticar (por árbol o básico) y devuelve el tratamiento
    ( arbol_diagnostico(P, Diagnostico)
    ; diagnostico_basico(P, Diagnostico)
    ),
    tratamiento(Diagnostico, Trat).

reporte(P) :-
    % Genera un reporte completo con todas las métricas del paciente
    findall(S, sintoma(P,S), Sint0),
    list_to_set(Sint0, Sintomas),
    findall(E, diagnostico_basico(P,E), E0),
    list_to_set(E0, Enfermedades),
    format('~n==============================~n', []),
    format('        REPORTE COMPLETO~n', []),
    format('==============================~n', []),
    format('Paciente: ~w~n', [P]),
    format('Sintomas: ~w~n', [Sintomas]),
    format('------------------------------~n', []),
    ( Enfermedades == [] ->
        format('Enfermedades posibles: (ninguna con evidencia suficiente)~n', []),
        format('Sugerencia: agrega mas sintomas o consulta profesional.~n', [])
    ;
        format('Enfermedades posibles: ~w~n~n', [Enfermedades]),
        forall(member(E, Enfermedades),
            (
                (probabilidad(P,E,Pr) -> true ; Pr = 0),
                (severidad(P,E,Sev) -> true ; Sev = 'desconocida'),
                (riesgo(P,E,Rg) -> true ; Rg = 'no evaluable'),
                (tratamiento(E,Tr) -> true ; Tr = 'no disponible'),
                (recomendacion(P,E,Rec) -> true ; Rec = 'no disponible'),
                format('• ~w~n', [E]),
                format('  - Probabilidad: ~2f%%~n', [Pr]),
                format('  - Severidad: ~w~n', [Sev]),
                format('  - Riesgo: ~w~n', [Rg]),
                format('  - Tratamiento: ~w~n', [Tr]),
                format('  - Recomendacion: ~w~n~n', [Rec])
            )
        ),
        ( sintomas_contradictorios(P) ->
            format('ALERTA: Se detectaron sintomas contradictorios.~n', [])
        ; true
        )
    ),
    format('==============================~n~n', []).

% =========================================================
% BASE FAMILIA
% =========================================================
% Hechos (datos) y reglas para responder preguntas familiares

hombre(filiberto).
hombre(eustacio).
hombre(tavo).
hombre(gabriel).
hombre(luis).
hombre(luised).
hombre(alan).
hombre(santiago).
hombre(tavito).
hombre(baltazar).
hombre(adrian).
hombre(baltazarjr).

mujer(eugenia).
mujer(mela).
mujer(sol).
mujer(luna).
mujer(catalina).
mujer(fatima).
mujer(martha).
mujer(andrea).
mujer(barbara).

padrede(filiberto, sol).
padrede(filiberto, luna).
padrede(filiberto, gabriel).
padrede(filiberto, tavo).
padrede(filiberto, luis).
padrede(eustacio, baltazar).
padrede(eustacio, catalina).
padrede(tavo, tavito).
padrede(luis, alan).
padrede(luis, luised).
padrede(luised, santiago).
padrede(luised, barbara).
padrede(baltazar, adrian).
padrede(baltazar, baltazarjr).

madrede(eugenia, sol).
madrede(eugenia, luna).
madrede(eugenia, gabriel).
madrede(eugenia, tavo).
madrede(eugenia, luis).
madrede(mela, baltazar).
madrede(mela, catalina).
madrede(fatima, tavito).
madrede(martha, alan).
madrede(martha, luised).
madrede(sol, adrian).
madrede(sol, baltazarjr).
madrede(luna, andrea).

esposo(filiberto, eugenia).
esposo(eustacio, mela).
esposo(tavo, fatima).
esposo(luis, martha).
esposo(baltazar, sol).

esposa(eugenia, filiberto).
esposa(mela, eustacio).
esposa(fatima, tavo).
esposa(martha, luis).
esposa(sol, baltazar).

abuelo(X, Y) :-
    % X es abuelo de Y si X es hombre y es padre/madre de alguien que es padre/madre de Y
    hombre(X),
    (padrede(X, Z); madrede(X, Z)),
    (padrede(Z, Y); madrede(Z, Y)).

abuela(X, Y) :-
    % X es abuela de Y si X es mujer y cumple la misma lógica de abuelo
    mujer(X),
    (padrede(X, Z); madrede(X, Z)),
    (padrede(Z, Y); madrede(Z, Y)).

hermano(X, Y) :-
    % X y Y son hermanos si comparten padre o madre
    hombre(X),
    ( (padrede(Z, X), padrede(Z, Y))
    ; (madrede(W, X), madrede(W, Y))
    ),
    X \= Y.

hermana(X, Y) :-
    % Versión hermana: comparte padre o madre pero X es mujer
    mujer(X),
    ( (padrede(Z, X), padrede(Z, Y))
    ; (madrede(W, X), madrede(W, Y))
    ),
    X \= Y.

tio(X, Y) :-
    % X es tío de Y si X es hermano de uno de los padres de Y
    hombre(X),
    (hermano(X, Z); hermana(Z, X)),
    (padrede(Z, Y); madrede(Z, Y)),
    X \= Z.

tia(X, Y) :-
    % X es tía de Y si X es hermana de uno de los padres de Y
    mujer(X),
    (hermano(Z, X); hermana(X, Z)),
    (padrede(Z, Y); madrede(Z, Y)),
    X \= Z.

primo(X, Y) :-
    % X es primo de Y si los padres de X y Y son hermanos
    hombre(X),
    (padrede(Z, X); madrede(Z, X)),
    (padrede(W, Y); madrede(W, Y)),
    (hermano(Z, W); hermana(Z, W); hermano(W, Z); hermana(W, Z)),
    X \= Y, Z \= W.

prima(X, Y) :-
    % Versión prima: misma lógica pero X es mujer
    mujer(X),
    (padrede(Z, X); madrede(Z, X)),
    (padrede(W, Y); madrede(W, Y)),
    (hermano(Z, W); hermana(Z, W); hermano(W, Z); hermana(W, Z)),
    X \= Y, Z \= W.

nieto(X, Y) :-
    % X es nieto de Y si Y es padre/madre de uno de los padres de X (X hombre)
    hombre(X),
    (padrede(Z, X); madrede(Z, X)),
    (padrede(Y, Z); madrede(Y, Z)).

nieta(X, Y) :-
    % Versión nieta: X mujer
    mujer(X),
    (padrede(Z, X); madrede(Z, X)),
    (padrede(Y, Z); madrede(Y, Z)).

sobrino(X, Y) :-
    % X es sobrino de Y si uno de los padres de X es hermano/hermana de Y (X hombre)
    hombre(X),
    (padrede(Z, X); madrede(Z, X)),
    (hermano(Z, Y); hermana(Z, Y)),
    Z \= Y.

sobrina(X, Y) :-
    % Versión sobrina: X mujer
    mujer(X),
    (padrede(Z, X); madrede(Z, X)),
    (hermano(Z, Y); hermana(Z, Y)),
    Z \= Y.

cunado(X, Y) :-
    % X es cuñado de Y si X está casado con alguien que es hermano/hermana de Y
    hombre(X),
    esposo(X, Z),
    (hermano(Z, Y); hermana(Z, Y)),
    Z \= Y.

cunada(X, Y) :-
    % Versión cuñada: X mujer
    mujer(X),
    esposa(X, Z),
    (hermano(Z, Y); hermana(Z, Y)),
    Z \= Y.

% =========================================================
% BASE CARROS
% =========================================================
% Hechos con información de autos (marca, precio, país, motor, consumo, etc.)

mejor_marca(toyota).
carro_mas_rapido(bugatti_chiron).

precio(toyota_corolla, 25).
precio(honda_civic, 28).
precio(ford_mustang, 35).
precio(bmw_x3, 45).
precio(mercedes_c_class, 50).
precio(ferrari_488, 280).
precio(lamborghini_huracan, 250).
precio(bugatti_chiron, 3500).

fabrica_en(toyota_corolla, [japon, tailandia, brasil]).
fabrica_en(honda_civic, [japon, estados_unidos, canada]).
fabrica_en(ford_mustang, [estados_unidos]).
fabrica_en(bmw_x3, [alemania, estados_unidos]).
fabrica_en(mercedes_c_class, [alemania, sudafrica]).
fabrica_en(ferrari_488, [italia]).
fabrica_en(lamborghini_huracan, [italia]).
fabrica_en(bugatti_chiron, [francia]).

potencia(toyota_corolla, 169).
potencia(honda_civic, 158).
potencia(ford_mustang, 450).
potencia(bmw_x3, 248).
potencia(mercedes_c_class, 255).
potencia(ferrari_488, 661).
potencia(lamborghini_huracan, 631).
potencia(bugatti_chiron, 1479).

motor(toyota_corolla, 'motor 4 cilindros 2.0L').
motor(honda_civic, 'motor 4 cilindros 1.5L turbo').
motor(ford_mustang, 'motor V8 5.0L').
motor(bmw_x3, 'motor 4 cilindros 2.0L turbo').
motor(mercedes_c_class, 'motor 4 cilindros 2.0L turbo').
motor(ferrari_488, 'motor V8 3.9L biturbo').
motor(lamborghini_huracan, 'motor V10 5.2L').
motor(bugatti_chiron, 'motor W16 8.0L quad-turbo').

consumo(toyota_corolla, 15).
consumo(honda_civic, 16).
consumo(ford_mustang, 8).
consumo(bmw_x3, 12).
consumo(mercedes_c_class, 13).
consumo(ferrari_488, 6).
consumo(lamborghini_huracan, 7).
consumo(bugatti_chiron, 4).

seguridad(toyota_corolla, 5).
seguridad(honda_civic, 5).
seguridad(ford_mustang, 4).
seguridad(bmw_x3, 5).
seguridad(mercedes_c_class, 5).
seguridad(ferrari_488, 4).
seguridad(lamborghini_huracan, 4).
seguridad(bugatti_chiron, 4).

% =========================================================
% REPLACE0 (RESPUESTAS)
% =========================================================
% Aquí se construyen las respuestas finales del chatbot según la intención detectada

replace0([I], Input, _, [flagSetPaciente], R) :-
    % Guarda el nombre del paciente actual
    nth0(I, Input, Nombre),
    nb_setval(paciente_actual, Nombre),
    R = [hola, Nombre, ',', quedas, registrado, como, paciente, '.'].

replace0(_, Input, _, [flagDiagnostico], R) :-
    % Guarda síntomas del paciente y sugiere enfermedades posibles
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre, '(', ejemplo, ':', mi, nombre, es, juan, ')']
    ;
        extraer_sintomas(Input, Nuevos),
        ( Nuevos == [] ->
            R = [no, detecte, sintomas, validos, en, tu, mensaje]
        ;
            forall(member(S, Nuevos), (sintoma(Pac,S) -> true ; assertz(sintoma(Pac,S)))),
            findall(E, diagnostico_basico(Pac,E), E0),
            list_to_set(E0, Enfermedades),
            ( Enfermedades \= [] ->
                R = [segun, tus, sintomas, podrias, tener | Enfermedades]
            ;   R = [aun, no, puedo, inferir, una, enfermedad, con, esos, sintomas]
            )
        )
    ).

replace0([], _, _, [flagExclusivo], R) :-
    % Indica si el paciente tiene algún síntoma exclusivo de una enfermedad
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; diagnostico_exclusivo(Pac, E) ->
        R = [un, sintoma, exclusivo, sugiere, E]
    ;   R = [no, se, detecto, sintoma, exclusivo]
    ).

replace0([I], Input, _, [flagTratamiento], R) :-
    % Da el tratamiento si la enfermedad está registrada en la base
    nth0(I, Input, E),
    ( tratamiento(E, Tr) ->
        R = [el, tratamiento, para, E, es, Tr]
    ;   R = [no, tengo, informacion, del, tratamiento, para, E]
    ).

replace0([I], Input, _, [flagSintomas], R) :-
    % Lista los síntomas de una enfermedad registrada
    nth0(I, Input, E),
    findall(S, tiene_sintoma(E,S), Sint),
    ( Sint \= [] ->
        R = [los, sintomas, de, E, son | Sint]
    ;   R = [no, tengo, sintomas, para, E]
    ).

replace0([I], Input, _, [flagProbabilidad], R) :-
    % Calcula probabilidad de una enfermedad según síntomas del paciente
    nb_getval(paciente_actual, Pac),
    nth0(I, Input, E),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; probabilidad(Pac, E, P) ->
        R = [la, probabilidad, de, tener, E, es, P, porciento]
    ;   R = [no, puedo, calcular, la, probabilidad, de, E]
    ).

replace0([I], Input, _, [flagPreventivo], R) :-
    % Responde si el paciente está en riesgo de desarrollar una enfermedad
    nb_getval(paciente_actual, Pac),
    nth0(I, Input, E),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; diagnostico_preventivo(Pac, E) ->
        R = [si, estas, en, riesgo, de, desarrollar, E]
    ;   R = [no, estas, en, riesgo, inmediato, de, E]
    ).

replace0(_, Input, _, [flagSimilares], R) :-
    % Lista enfermedades similares a la enfermedad indicada
    last(Input, E),
    findall(X, enfermedades_similares(E, X), Sim),
    ( Sim \= [] ->
        R = [enfermedades, similares, a, E, son | Sim]
    ;   R = [no, conozco, enfermedades, similares, a, E]
    ).

replace0([], _, _, [flagContradictorios], R) :-
    % Detecta si hay síntomas contradictorios en el paciente
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; sintomas_contradictorios(Pac) ->
        R = [si, hay, sintomas, contradictorios]
    ;   R = [no, hay, sintomas, contradictorios]
    ).

replace0([], _, _, [flagArbol], R) :-
    % Da un diagnóstico usando el árbol de decisión
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; arbol_diagnostico(Pac, E) ->
        R = [segun, el, arbol, podrias, tener, E]
    ;   R = [no, se, pudo, determinar, un, diagnostico, por, arbol]
    ).

replace0([], _, _, [flagSeveridad], R) :-
    % Responde qué tan grave es la condición según la severidad calculada
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ;
        findall(E, diagnostico_basico(Pac,E), E0),
        list_to_set(E0, Es),
        ( Es = [E|_] ->
            severidad(Pac, E, Sev),
            R = [tu, condicion, es, Sev, para, E]
        ;   R = [primero, necesito, saber, tus, sintomas]
        )
    ).

replace0([], _, _, [flagRiesgo], R) :-
    % Responde el nivel de riesgo (si aplica para esa enfermedad)
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ;
        findall(E, diagnostico_basico(Pac,E), E0),
        list_to_set(E0, Es),
        ( Es = [E|_] ->
            ( riesgo(Pac, E, Nivel) ->
                R = [tu, nivel, de, riesgo, es, Nivel, para, E]
            ;   R = [no, puedo, evaluar, el, riesgo, para, E]
            )
        ;   R = [primero, necesito, saber, tus, sintomas]
        )
    ).

replace0([], _, _, [flagTratamientoCombinado], R) :-
    % Devuelve una lista de tratamientos posibles para las enfermedades candidatas
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; tratamiento_combinado(Pac, Lista), Lista \= [] ->
        R = [tratamientos, sugeridos | Lista]
    ;   R = [no, hay, tratamientos, disponibles]
    ).

replace0([], _, _, [flagRecomendacion], R) :-
    % Devuelve una recomendación según severidad
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ;
        findall(E, diagnostico_basico(Pac,E), E0),
        list_to_set(E0, Es),
        ( Es = [E|_] ->
            recomendacion(Pac, E, Texto),
            R = [Texto]
        ;   R = [no, puedo, generar, recomendacion, sin, diagnostico]
        )
    ).

replace0([], _, _, [flagDiagTratar], R) :-
    % Hace diagnóstico + tratamiento en una sola respuesta
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        R = [primero, dime, tu, nombre]
    ; diagnosticar_y_tratar(Pac, D, T) ->
        R = [diagnostico, D, tratamiento, T]
    ;   R = [no, pude, diagnosticar, y, tratar]
    ).

replace0([], _, _, [flagReporteBonito], [reporte, generado]) :-
    % Imprime el reporte completo en consola
    nb_getval(paciente_actual, Pac),
    ( Pac == desconocido ->
        writeln('primero dime tu nombre: "mi nombre es juan".'), !
    ; reporte(Pac)
    ).

replace0([I1, I2], Input, _, [flagFamilia, hijo], R) :-
    % Verifica si A es hijo de B (por padre o madre)
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (padrede(B, A); madrede(B, A)) ->
        R = [si, A, es, hijo, de, B]
    ;   R = [no, A, no, es, hijo, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, padre], R) :-
    % Verifica si A es padre de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( padrede(A, B) ->
        R = [si, A, es, padre, de, B]
    ;   R = [no, A, no, es, padre, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, madre], R) :-
    % Verifica si A es madre de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( madrede(A, B) ->
        R = [si, A, es, madre, de, B]
    ;   R = [no, A, no, es, madre, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, hermano], R) :-
    % Verifica si A y B son hermanos/as
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (hermano(A, B); hermana(A, B)) ->
        R = [si, A, y, B, son, hermanos]
    ;   R = [no, A, y, B, no, son, hermanos]
    ).

replace0([I1, I2], Input, _, [flagFamilia, abuelo], R) :-
    % Verifica si A es abuelo/abuela de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (abuelo(A, B); abuela(A, B)) ->
        R = [si, A, es, abuelo, de, B]
    ;   R = [no, A, no, es, abuelo, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, tio], R) :-
    % Verifica si A es tío/tía de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (tio(A, B); tia(A, B)) ->
        R = [si, A, es, tio, de, B]
    ;   R = [no, A, no, es, tio, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, primo], R) :-
    % Verifica si A y B son primos/as
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (primo(A, B); prima(A, B)) ->
        R = [si, A, y, B, son, primos]
    ;   R = [no, A, y, B, no, son, primos]
    ).

replace0([I1, I2], Input, _, [flagFamilia, nieto], R) :-
    % Verifica si A es nieto/nieta de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (nieto(A, B); nieta(A, B)) ->
        R = [si, A, es, nieto, de, B]
    ;   R = [no, A, no, es, nieto, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, sobrino], R) :-
    % Verifica si A es sobrino/sobrina de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (sobrino(A, B); sobrina(A, B)) ->
        R = [si, A, es, sobrino, de, B]
    ;   R = [no, A, no, es, sobrino, de, B]
    ).

replace0([I1, I2], Input, _, [flagFamilia, esposo], R) :-
    % Verifica si A y B están casados
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (esposo(A, B); esposa(A, B)) ->
        R = [si, A, y, B, estan, casados]
    ;   R = [no, A, y, B, no, estan, casados]
    ).

replace0([I1, I2], Input, _, [flagFamilia, cunado], R) :-
    % Verifica si A es cuñado/cuñada de B
    nth0(I1, Input, A), nth0(I2, Input, B),
    ( (cunado(A, B); cunada(A, B)) ->
        R = [si, A, es, cunado, de, B]
    ;   R = [no, A, no, es, cunado, de, B]
    ).

replace0([I], Input, _, [flagFamilia, quien_padre], R) :-
    % Responde quién es el padre de la persona
    nth0(I, Input, Persona),
    ( padrede(Padre, Persona) ->
        R = [el, padre, de, Persona, es, Padre]
    ;   R = [no, se, quien, es, el, padre, de, Persona]
    ).

replace0([I], Input, _, [flagFamilia, quien_madre], R) :-
    % Responde quién es la madre de la persona
    nth0(I, Input, Persona),
    ( madrede(Madre, Persona) ->
        R = [la, madre, de, Persona, es, Madre]
    ;   R = [no, se, quien, es, la, madre, de, Persona]
    ).

replace0([I], Input, _, [flagFamilia, quien_hijos], R) :-
    % Lista los hijos de una persona
    nth0(I, Input, Persona),
    findall(H, (padrede(Persona, H); madrede(Persona, H)), Hijos),
    ( Hijos \= [] ->
        append([los, hijos, de, Persona, son], Hijos, R)
    ;   R = [Persona, no, tiene, hijos]
    ).

replace0([I], Input, _, [flagFamilia, quien_hermanos], R) :-
    % Lista los hermanos de una persona
    nth0(I, Input, Persona),
    findall(H, (hermano(H, Persona); hermana(H, Persona)), Hermanos),
    ( Hermanos \= [] ->
        append([los, hermanos, de, Persona, son], Hermanos, R)
    ;   R = [Persona, no, tiene, hermanos]
    ).

replace0([I], Input, _, [flagFamilia, quien_abuelos], R) :-
    % Lista los abuelos de una persona
    nth0(I, Input, Persona),
    findall(A, (abuelo(A, Persona); abuela(A, Persona)), Abuelos),
    ( Abuelos \= [] ->
        append([los, abuelos, de, Persona, son], Abuelos, R)
    ;   R = [no, conozco, los, abuelos, de, Persona]
    ).

replace0([I], Input, _, [flagFamilia, cuantos_hijos], R) :-
    % Cuenta cuántos hijos tiene una persona
    nth0(I, Input, Persona),
    findall(H, (padrede(Persona, H); madrede(Persona, H)), Hijos),
    length(Hijos, N),
    R = [Persona, tiene, N, hijos].

replace0([I], Input, _, [flagFamilia, cuantos_hermanos], R) :-
    % Cuenta cuántos hermanos tiene una persona
    nth0(I, Input, Persona),
    findall(H, (hermano(H, Persona); hermana(H, Persona)), Hermanos),
    length(Hermanos, N),
    R = [Persona, tiene, N, hermanos].

replace0([], _, _, [flagCarros, mejor_marca], R) :-
    % Responde cuál es la mejor marca (según la base)
    mejor_marca(M),
    R = [la, mejor, marca, de, carros, es, M].

replace0([], _, _, [flagCarros, mas_rapido], R) :-
    % Responde cuál es el carro más rápido (según la base)
    carro_mas_rapido(C),
    R = [el, carro, mas, rapido, del, mundo, es, C].

replace0([I], Input, _, [flagCarros, precio], R) :-
    % Responde el precio aproximado del carro
    nth0(I, Input, Carro),
    ( precio(Carro, P) ->
        R = [el, Carro, cuesta, aproximadamente, P, mil, dolares]
    ;   R = [no, tengo, informacion, del, precio, del, Carro]
    ).

replace0([I], Input, _, [flagCarros, fabrica], R) :-
    % Responde en qué países se fabrica un carro
    nth0(I, Input, Carro),
    ( fabrica_en(Carro, Lugares) ->
        append([el, Carro, se, fabrica, en], Lugares, R)
    ;   R = [no, se, donde, se, fabrica, el, Carro]
    ).

replace0([I], Input, _, [flagCarros, potencia], R) :-
    % Responde cuántos caballos de fuerza tiene el carro
    nth0(I, Input, Carro),
    ( potencia(Carro, HP) ->
        R = [el, Carro, tiene, HP, caballos, de, fuerza]
    ;   R = [no, tengo, informacion, de, la, potencia, del, Carro]
    ).

replace0([I], Input, _, [flagCarros, motor], R) :-
    % Responde qué motor tiene el carro
    nth0(I, Input, Carro),
    ( motor(Carro, Tipo) ->
        R = [el, Carro, tiene, Tipo]
    ;   R = [no, tengo, informacion, del, motor, del, Carro]
    ).

replace0([I], Input, _, [flagCarros, consumo], R) :-
    % Responde el consumo aproximado del carro
    nth0(I, Input, Carro),
    ( consumo(Carro, KmL) ->
        R = [el, Carro, consume, aproximadamente, KmL, km, por, litro]
    ;   R = [no, tengo, informacion, del, consumo, del, Carro]
    ).

replace0([I], Input, _, [flagCarros, seguridad], R) :-
    % Responde la calificación de seguridad del carro
    nth0(I, Input, Carro),
    ( seguridad(Carro, Est) ->
        R = [el, Carro, tiene, una, calificacion, de, seguridad, de, Est, estrellas]
    ;   R = [no, tengo, informacion, de, seguridad, del, Carro]
    ).

replace0([], _, _, Resp, Resp) :- !.
% Caso base: si no se requiere procesamiento, devuelve Resp tal cual
% Esto ayuda a que el sistema siempre tenga una salida y no falle

```
