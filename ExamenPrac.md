# Examen Practico 24/11/2025
```
% Busca si esta el valor 
esta(X, [X|_]).
esta(X, [_|Cola]) :- esta(X, Cola).

% Saca un elemento de la lista
sacar(X, [X|Resto], Resto).
sacar(X, [Cabeza|Cola], [Cabeza|Resto]) :- sacar(X, Cola, Resto).

resolver(Tabla) :-
    Tabla = [
        persona(ana,    E1, H1, B1, Eq1, P1),
        persona(bruno,  E2, H2, B2, Eq2, P2),
        persona(carlos, E3, H3, B3, Eq3, P3),
        persona(diana,  E4, H4, B4, Eq4, P4),
        persona(elisa,  E5, H5, B5, Eq5, P5)
    ],

    Especialidades = [genetica, microbiologia, bioquimica, inmunologia, neurociencia],
    sacar(E1, Especialidades, R1), sacar(E2, R1, R2), sacar(E3, R2, R3), sacar(E4, R3, R4), sacar(E5, R4, _),
    E1 \= genetica, E1 \= neurociencia, % Regla 2
    E4 = microbiologia,                 % Regla 12

    Horarios = [6, 8, 10, 12, 14],
    sacar(H1, Horarios, RH1), sacar(H2, RH1, RH2), sacar(H3, RH2, RH3), sacar(H4, RH3, RH4), sacar(H5, RH4, _),
    esta(persona(_, genetica, 6, _, _, _), Tabla), % R1

    Equipos = [microscopio, centrifuga, pcr, espectrometro, incubadora],
    sacar(Eq1, Equipos, REq1), sacar(Eq2, REq1, REq2), sacar(Eq3, REq2, REq3), sacar(Eq4, REq3, REq4), sacar(Eq5, REq4, _),
    Eq3 = espectrometro, % R5
    esta(persona(_, inmunologia, _, _, pcr, _), Tabla), % R7
    esta(persona(_, _, 14, _, incubadora, _), Tabla),   % R9
    esta(persona(_, _, 8, _, microscopio, _), Tabla),   % R13
    esta(persona(_, _, HPCR, _, pcr, _), Tabla), esta(persona(_, microbiologia, HMicro, _, _, _), Tabla), HPCR > HMicro, % R16

    Bebidas = [cafe, te, jugo, mate, agua],
    sacar(B1, Bebidas, RB1), sacar(B2, RB1, RB2), sacar(B3, RB2, RB3), sacar(B4, RB3, RB4), sacar(B5, RB4, _),
    esta(persona(_, _, _, te, centrifuga, _), Tabla), % R3
    esta(persona(_, _, HCafe, cafe, _, _), Tabla), esta(persona(_, _, HJugo, jugo, _, _), Tabla), HJugo =:= HCafe + 2, % R6
    B5 \= te, B5 \= cafe, % R11
    esta(persona(_, _, _, agua, EqAgua, _), Tabla), EqAgua \= pcr, EqAgua \= espectrometro, % R18
    esta(persona(_, neurociencia, HNeuro, _, _, _), Tabla), HNeuro > HJugo, % R19

    Paises = [mexico, chile, espana, argentina, peru],
    sacar(P1, Paises, RP1), sacar(P2, RP1, RP2), sacar(P3, RP2, RP3), sacar(P4, RP3, RP4), sacar(P5, RP4, _),
    esta(persona(_, _, 10, _, _, peru), Tabla),          % R4
    esta(persona(_, bioquimica, _, _, _, chile), Tabla), % R8
    esta(persona(_, _, _, mate, _, argentina), Tabla),   % R10
    P2 \= mexico,                                        % R14
    esta(persona(_, neurociencia, _, _, _, espana), Tabla), % R15
    esta(persona(_, _, _, _, EqMex, mexico), Tabla), EqMex \= microscopio, EqMex \= incubadora, % R17
    esta(persona(_, _, _, BPeru, _, peru), Tabla), BPeru \= agua. % R20

% Pregunta 1: ¿Qué hora tiene P (Persona)?
hora(P, H) :- resolver(T), esta(persona(P, _, H, _, _, _), T).

% Pregunta 2: ¿Qué especialidad tiene P?
especialidad(P, E) :- resolver(T), esta(persona(P, E, _, _, _, _), T).

% Pregunta 3: ¿Qué bebe P?
bebida(P, B) :- resolver(T), esta(persona(P, _, _, B, _, _), T).

% Pregunta 4: ¿Qué equipo usa P?
equipo(P, Eq) :- resolver(T), esta(persona(P, _, _, _, Eq, _), T).

% Pregunta 5: ¿De qué país es P?
pais(P, Pais) :- resolver(T), esta(persona(P, _, _, _, _, Pais), T).
```
```
% Resolver el acertijo
resolver(Tabla).
% Pregunta 1 (La hora de todos)
hora(Quien, Hora).
& Pregunta 2 (Especialidad)
especialidad(Quien, Esp).
% Pregunta 3 (Bebida)
bebida(Quien, Bebida).
% Pregunta 4 (Equipos)
equipo(Quien, Equipo).
%Pregunta 5 (Paises)
pais(Quien, Pais).
```
## Consultas en Prolog
<img width="1906" height="835" alt="Captura de pantalla 2025-11-24 095032" src="https://github.com/user-attachments/assets/050b7a32-146f-4bf5-bc3e-ceff3e9991f7" />
<img width="264" height="194" alt="Captura de pantalla 2025-11-24 095037" src="https://github.com/user-attachments/assets/84cf7b75-e49c-4243-9c72-00709367d198" />
