# Pruebas de escritorio del PDF
## Codigo
```
%%declaraciones de enfermedades
enfermedad(gripe).
enfermedad(rubeola).
enfermedad(malaria).
enfermedad(hepatitis).
enfermedad(tuberculosis).
enfermedad(anemia).

%enfermode(manuel,gripe).
%tienesintoma(alicia,cansancio).
%declaraciones de síntomas, según enfermedad
sintomade(tos, gripe).
sintomade(cansancio, gripe).
sintomade(fiebre, gripe).
sintomade(dolorcabeza, gripe).
sintomade(nauseas, hepatitis).
sintomade(diarrea, hepatitis).
sintomade(ictericia, hepatitis).
sintomade(cansancio, anemia).
sintomade(apatia, anemia).
sintomade(nauseas, anemia).
sintomade(tos, tuberculosis).
sintomade(cansancio, tuberculosis).
sintomade(fiebre, tuberculosis).
sintomade(escalofrios, tuberculosis).
sintomade(escalofrios, malaria).
sintomade(fiebre, malaria).
sintomade(diarrea, malaria).
sintomade(ictericia, malaria).
sintomade(fiebre, rubeola).
sintomade(jaqueca, rubeola).
sintomade(secrecion, rubeola).

% Reglas para determinar que probabilidad una persona puede tener una
% enfermedad X dado n síntomas
buscar([], E, 0).
buscar(X, E, 1):- sintomade(X, E).
buscar([X|Xs], E, P) :- enfermedad(E), buscar(X, E, S1), buscar(Xs, E,S2), P is S1 + S2.

%%función que devuelve la cantidad de síntomas totales de la enfermedad seleccionada
cantSint(E, C) :- findall(X, sintomade(X, E), L), length(L, R), C is R.

%% Esta función es parecida a la de buscar
diagnostico([X|Xs], E, K):- buscar([X|Xs], E, P), cantSint(E, T), K is P*100/T.

% declaraciones de los hechos para determinar medicina de una enfermedad
medicinade(contrex, gripe).
medicinade(jarabe, gripe).
medicinade(pastillas, tubercolosis).
medicinade(vacuna, malaria).
medicinade(vacuna, rubeola).
medicinade(vitaminas, anemia).
medicinade(pastillas, hepatitis).

%declaración de reglas
% receta medica según síntoma
recetade(M, S):-sintomade(S, Z), medicinade(M, Z).

especialistade(otorrino, gripe).
especialistade(nutricionista, anemia).
especialistade(endocrinologia, hepatitis).
especialistade(medicinageneral, rubeola).
especialistade(nutricionista, tubercolosis).
especialistade(medicinageneral, malaria).

%---------------------------------------------------------------------------------------------------------------------
% Reglas
%---------------------------------------------------------------------------------------------------------------------

% Esta regla te dice qué especialista atiende un síntoma (a través de la enfermedad)
% Es útil, pero no la usaremos en 'mereceta' para evitar el error lógico.
atiendeespecialista(E, S):- 
    sintomade(S, Z),
    especialistade(E, Z).

%---------------------------------------------------------------------------------------------------------------------
% REGLA 'mereceta' 
%---------------------------------------------------------------------------------------------------------------------
% Lógica: Un especialista (Es) receta una medicina (M) para una enfermedad (E) SI:
% 1. La medicina (M) es para la enfermedad (E).
% 2. El especialista (Es) trata esa enfermedad (E).
mereceta(Es, M, E):-
    medicinade(M, E), 
    especialistade(Es, E).
```
## Pruebas realizadas en Terminal
<img width="545" height="80" alt="Captura de pantalla 2025-10-24 111940" src="https://github.com/user-attachments/assets/53160821-8fbc-4ae3-9a0b-ca3fa7afd271" />

Prueba: Se le preguntó a Prolog por la medicina para la malaria usando una variable X.

Proceso: Prolog buscó en sus hechos (Call).

Resultado: Prolog encontró el hecho medicinade(vacuna, malaria) (Exit) y, por lo tanto, asignó el valor vacuna a la variable X.

Conclusión: La prueba fue exitosa y la respuesta es X = vacuna.

<img width="512" height="123" alt="Captura de pantalla 2025-10-24 112016" src="https://github.com/user-attachments/assets/2f025690-749e-4550-b7f8-4c491dc6575a" />

Prueba: Se le preguntó a Prolog por las medicinas para la gripe usando una variable X.

Proceso: Prolog buscó (Call) y encontró una primera respuesta: contrex (Exit).

Interacción: El usuario pidió una respuesta alternativa (tecleando ;).

Proceso (Continuación): Prolog reanudó la búsqueda (Redo).

Resultado: Prolog encontró una segunda respuesta: jarabe (Exit).

Conclusión: La prueba fue exitosa y demostró que hay múltiples medicinas para la gripe en la base de conocimiento: contrex y jarabe

<img width="554" height="74" alt="Captura de pantalla 2025-10-24 112046" src="https://github.com/user-attachments/assets/8dcd4a28-42bf-40d8-87c6-0b923e72c575" />

Prueba: Se le preguntó a Prolog por el especialista (X) que atiende la gripe.

Proceso: Prolog inició la búsqueda (Call) en su base de conocimiento.

Resultado: Prolog encontró el hecho especialistade(otorrino, gripe) (Exit) y, por eso, le dio el valor otorrino a la variable X.

Conclusión: La prueba fue exitosa y la respuesta es X = otorrino.

<img width="606" height="266" alt="Captura de pantalla 2025-10-24 112119" src="https://github.com/user-attachments/assets/f6cc6369-a488-4d25-a742-64eefa47bcb8" />

Prueba: Se preguntó por el especialista (E) y la medicina (M) para la gripe usando la regla mereceta.

Proceso (Regla): Prolog ejecutó una regla que consistía en dos pasos: 1) encontrar una medicinade y 2) encontrar un especialistade.

Resultado 1: Encontró contrex (medicina) y otorrino (especialista). Mostró la combinación E = otorrino, M = contrex.

Interacción: El usuario pidió más respuestas (con ;).

Proceso (Redo): Prolog regresó al paso 1, encontró la siguiente medicina (jarabe) y repitió el paso 2 (encontró otorrino de nuevo).

Resultado 2: Mostró la segunda combinación: E = otorrino, M = jarabe.

Conclusión: La prueba fue exitosa y encontró dos combinaciones posibles que cumplen la regla.

<img width="617" height="299" alt="Captura de pantalla 2025-10-24 112448" src="https://github.com/user-attachments/assets/4cc9752b-f518-4435-bf21-356f5957d889" />

Prueba: Se preguntó qué medicina (M) se receta para la tos.

Proceso: Prolog usó una regla que buscaba una enfermedad (E) para la tos y luego una medicina (M) para esa enfermedad (E).

Resultado 1 (Gripe): Encontró tos -> gripe y luego gripe -> contrex. Devolvió M = contrex.

Resultado 2 (Gripe): El usuario pidió más. Prolog buscó otra medicina para gripe y encontró gripe -> jarabe. Devolvió M = jarabe.

Resultado 3 (Tuberculosis): El usuario pidió más. Prolog buscó otra enfermedad para tos y encontró tos -> tuberculosis.

Fallo: Prolog intentó encontrar una medicina para tuberculosis, pero no encontró ninguna (Fail).

Conclusión: La búsqueda terminó. Las únicas respuestas fueron contrex y jarabe. El último intento de búsqueda (tuberculosis) falló y el sistema respondió false. (falso), indicando que no hay más soluciones.
