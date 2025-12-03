# Sistema Medico Experto en Prolog

```
% ==========================================================
% HECHOS: Enfermedades y Síntomas
% ==========================================================

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

% gonorrea
tiene_sintoma(gonorrea, ardor).
tiene_sintoma(gonorrea, secrecion).
tiene_sintoma(gonorrea, dolor_testiculos).
tiene_sintoma(gonorrea, dolor_pelvico).
tiene_sintoma(gonorrea, sangrado).
tiene_sintoma(gonorrea, picazon_rectal).
tiene_sintoma(gonorrea, dolor_garganta).
tiene_sintoma(gonorrea, dolor_ojos).

% Diabetes gestacional
tiene_sintoma(diabetes_gestacional, sed).
tiene_sintoma(diabetes_gestacional, orina_frecuente).
tiene_sintoma(diabetes_gestacional, fatiga).
tiene_sintoma(diabetes_gestacional, vision_borrosa).
tiene_sintoma(diabetes_gestacional, infecciones).
tiene_sintoma(diabetes_gestacional, nauseas).

% Cáncer de piel en el pie
tiene_sintoma(cancer_piel_pie, lunar_cambia).
tiene_sintoma(cancer_piel_pie, mancha_crece).
tiene_sintoma(cancer_piel_pie, herida_no_sana).
tiene_sintoma(cancer_piel_pie, bulto).
tiene_sintoma(cancer_piel_pie, dolor).
tiene_sintoma(cancer_piel_pie, inflamacion).
tiene_sintoma(cancer_piel_pie, picazon).

% ==========================================================
% HECHOS: Tratamientos
% ==========================================================

tratamiento(gripe, 'Reposo, hidratacion, paracetamol y aislamiento.').
tratamiento(alergia, 'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana, 'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').
tratamiento(gonorrea, 'Antibioticos prescritos por un medico, evitar relaciones sexuales hasta la curacion y seguimiento clinico.').
tratamiento(diabetes_gestacional, 'Control de la dieta, ejercicio moderado, monitoreo de glucosa y en algunos casos insulina.').
tratamiento(cancer_piel_pie, 'Cirugia para extirpar la lesion, tratamientos complementarios como radioterapia o quimioterapia y seguimiento dermatologico.').

% ==========================================================
% PREDICADO DINÁMICO
% ==========================================================

:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).

% Sistema de interacción
pregunta(Paciente, Sintoma) :-
    sintoma(Paciente, Sintoma), !.

pregunta(Paciente, Sintoma) :-
    write('¿El paciente '), write(Paciente),
    write(' tiene '), write(Sintoma), write('? (si/no): '),
    read(Resp),
    ( Resp = si ->
        assertz(sintoma(Paciente, Sintoma))
    ;
        fail
    ).

% ==========================================================
% DIAGNÓSTICO BÁSICO
% ==========================================================

diagnostico_basico(Paciente, Enfermedad) :-
    tiene_sintoma(Enfermedad, S),
    pregunta(Paciente, S).

% ==========================================================
% DIAGNÓSTICO COMPLETO
% ==========================================================

diagnostico_completo(Paciente, Enfermedad) :-
    findall(S, tiene_sintoma(Enfermedad, S), Lista),
    todos_confirmados(Paciente, Lista).

todos_confirmados(_, []).
todos_confirmados(Paciente, [S|R]) :-
    pregunta(Paciente, S),
    todos_confirmados(Paciente, R).

% ==========================================================
% DISTINCIÓN FUERTE Y TRATAMIENTOS
% ==========================================================

distincion_fuerte(P, gripe) :-
    diagnostico_basico(P, gripe),
    pregunta(P, fiebre),
    \+ pregunta(P, estornudos).

distincion_fuerte(P, resfriado) :-
    diagnostico_basico(P, resfriado),
    pregunta(P, estornudos),
    \+ pregunta(P, fiebre).

obtener_tratamiento(P, Trat) :-
    (distincion_fuerte(P, E) ; diagnostico_basico(P, E)),
    tratamiento(E, Trat).

% ==========================================================
% SEVERIDAD
% ==========================================================

contar_sintomas_confirmados(P, Enfermedad, C) :-
    findall(S, (tiene_sintoma(Enfermedad,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, 'Severa') :-
    contar_sintomas_confirmados(P, E, C), C >= 3, !.

severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.

severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.
```
