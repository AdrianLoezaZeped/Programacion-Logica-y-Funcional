# Eliza con emfermedades
```
%%==================================================================================================
%% ELIZA MEDICA - CHATBOT CON SISTEMA DE DIAGNOSTICO
%%==================================================================================================

eliza:-	writeln('Hola, mi nombre es Eliza tu chatbot medico,
	por favor ingresa tu consulta,
	usar solo minusculas sin . al final:'),
	readln(Input),
	eliza(Input),!.

%%--------------------------------------------------------------------------------------------------
%% DESPEDIDAS
%%--------------------------------------------------------------------------------------------------

eliza(Input):- member(Input, [
	[chao],
	[hasta, luego],
	[nos, vemos],
	[hasta, pronto],
	[cuidate],
	[bye],
	[hasta, manana],
	[nos, vemos, luego],
	[que, tengas, buen, dia],
	[que, tengas, buena, noche],
	[adios, eliza],
	[exit],
	[quit],
	[salir],
	[terminar],
	[ya, me, voy],
	[me, retiro],
	[hasta, la, proxima],
	[nos, vemos, pronto],
	[ya, es, todo]
]),
	writeln('Adios. Espero haberte ayudado. Cuidate!'), !.

%%--------------------------------------------------------------------------------------------------
%% PLANTILLAS PRINCIPALES
%%--------------------------------------------------------------------------------------------------

eliza(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	eliza(Input1), !.

%%--------------------------------------------------------------------------------------------------
%% SALUDOS
%%--------------------------------------------------------------------------------------------------

template([que, tal], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([como, estas], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([como, te, va], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([buenos, dias], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([buenas, tardes], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([buenas, noches], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([hey], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([que, onda], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([que, hay], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([saludos], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([bienvenido], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([hola, hola], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([que, hay, de, nuevo], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([gusto, en, saludarte], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([q, tal], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([holi], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([hola, eliza], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([buen, dia], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([hello], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).
template([hi], ['Hola', ',', 'en', 'que', 'puedo', 'ayudarte', '?'], []).

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_), '.'], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).
template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_), '.'], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).
template([hola, _], ['Hola', 'como', estas, tu, '?'], []).
template([buendia, _], ['Buendia', 'Como', estas, tu, '?'], []).

%%--------------------------------------------------------------------------------------------------
%% PLANTILLAS MEDICAS - DIAGNOSTICO
%%--------------------------------------------------------------------------------------------------

template([tengo, s(_)|_], [flagDiagnostico], [1]).
template([siento, s(_)|_], [flagDiagnostico], [1]).
template([me, duele, s(_)|_], [flagDiagnostico], [2]).
template([presento, s(_)|_], [flagDiagnostico], [1]).
template([diagnostico, s(_)|_], [flagDiagnostico], [1]).

%%--------------------------------------------------------------------------------------------------
%% PLANTILLAS MEDICAS - MEDICAMENTOS
%%--------------------------------------------------------------------------------------------------

template([que, medicina, para, s(_)], [flagMedicina], [3]).
template([medicamento, para, s(_)], [flagMedicina], [2]).
template([que, tomar, para, s(_)], [flagMedicina], [3]).
template([receta, para, s(_)], [flagMedicina], [2]).

%%--------------------------------------------------------------------------------------------------
%% PLANTILLAS MEDICAS - ESPECIALISTAS
%%--------------------------------------------------------------------------------------------------

template([que, especialista, para, s(_)], [flagEspecialista], [3]).
template([que, doctor, para, s(_)], [flagEspecialista], [3]).
template([especialista, para, s(_)], [flagEspecialista], [2]).

%%--------------------------------------------------------------------------------------------------
%% PLANTILLAS ORIGINALES DE ELIZA
%%--------------------------------------------------------------------------------------------------

template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu, '.'], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_),'.'], [porque, eres, tu, 0, '?'], [2]).

template([te, gustan, las, s(_)], [flagLike], [3]).
template([te, gustan, los, s(_)], [flagLike], [3]).
template([te, gustan, el, s(_)], [flagLike], [3]).
template([te, gusta, s(_)], [flagLike], [2]).
template([te, gustan, lo, s(_)], [flagLike], [3]).

template([tu, eres, s(_), _], [flagDo], [2]).
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [1]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).
template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).

template(_, ['Por favor, explica un poco mas o describe tus sintomas.'], []).

%%--------------------------------------------------------------------------------------------------
%% DECLARACION DE ENFERMEDADES
%%--------------------------------------------------------------------------------------------------

enfermedad(gonorrea).
enfermedad(diabetes_gestacional).
enfermedad(cancer_piel_pie).

%%--------------------------------------------------------------------------------------------------
%% SINTOMAS DE CADA ENFERMEDAD
%%--------------------------------------------------------------------------------------------------

%% Gonorrea
sintomade(ardor, gonorrea).
sintomade(secrecion, gonorrea).
sintomade(dolor_testiculos, gonorrea).
sintomade(dolor_pelvico, gonorrea).
sintomade(sangrado, gonorrea).
sintomade(picazon_rectal, gonorrea).
sintomade(dolor_garganta, gonorrea).
sintomade(dolor_ojos, gonorrea).

%% Diabetes gestacional
sintomade(sed, diabetes_gestacional).
sintomade(orina_frecuente, diabetes_gestacional).
sintomade(fatiga, diabetes_gestacional).
sintomade(vision_borrosa, diabetes_gestacional).
sintomade(infecciones, diabetes_gestacional).
sintomade(nauseas, diabetes_gestacional).

%% Cancer de piel en el pie
sintomade(lunar_cambia, cancer_piel_pie).
sintomade(mancha_crece, cancer_piel_pie).
sintomade(herida_no_sana, cancer_piel_pie).
sintomade(bulto, cancer_piel_pie).
sintomade(dolor, cancer_piel_pie).
sintomade(inflamacion, cancer_piel_pie).
sintomade(picazon, cancer_piel_pie).

%%--------------------------------------------------------------------------------------------------
%% CALCULO DE PROBABILIDAD
%%--------------------------------------------------------------------------------------------------

buscar([], _, 0).
buscar(X, E, 1):- sintomade(X, E).
buscar([X|Xs], E, P) :- enfermedad(E), buscar(X, E, S1), buscar(Xs, E, S2), P is S1 + S2.

cantSint(E, C) :- findall(X, sintomade(X, E), L), length(L, R), C is R.

diagnostico([X|Xs], E, K):- buscar([X|Xs], E, P), cantSint(E, T), K is P*100/T.

%%--------------------------------------------------------------------------------------------------
%% MEDICAMENTOS POR ENFERMEDAD
%%--------------------------------------------------------------------------------------------------

%% Gonorrea
medicinade(ceftriaxona, gonorrea).
medicinade(azitromicina, gonorrea).
medicinade(doxiciclina, gonorrea).
medicinade(cefixima, gonorrea).
medicinade(gentamicina, gonorrea).

%% Diabetes gestacional
medicinade(insulina, diabetes_gestacional).
medicinade(metformina, diabetes_gestacional).
medicinade(gliburida, diabetes_gestacional).

%% Cancer de piel en el pie
medicinade(pembrolizumab, cancer_piel_pie).
medicinade(nivolumab, cancer_piel_pie).
medicinade(ipilimumab, cancer_piel_pie).
medicinade(vemurafenib, cancer_piel_pie).
medicinade(dabrafenib, cancer_piel_pie).
medicinade(trametinib, cancer_piel_pie).
medicinade(dacarbazina, cancer_piel_pie).
medicinade(temozolomida, cancer_piel_pie).

%%--------------------------------------------------------------------------------------------------
%% RELACION SINTOMA A MEDICAMENTO
%%--------------------------------------------------------------------------------------------------

recetade(M, S):- sintomade(S, E), medicinade(M, E).

%%--------------------------------------------------------------------------------------------------
%% ESPECIALISTAS
%%--------------------------------------------------------------------------------------------------

especialistade(urologo, gonorrea).
especialistade(ginecologo, gonorrea).
especialistade(endocrinologo, diabetes_gestacional).
especialistade(oncologo, cancer_piel_pie).
especialistade(dermatologo, cancer_piel_pie).

%%--------------------------------------------------------------------------------------------------
%% QUIEN RECETA QUE
%%--------------------------------------------------------------------------------------------------

mereceta(Especialista, Medicina, Enfermedad):-
    medicinade(Medicina, Enfermedad),
    especialistade(Especialista, Enfermedad).

%%--------------------------------------------------------------------------------------------------
%% LO QUE LE GUSTA A ELIZA
%%--------------------------------------------------------------------------------------------------

elizaLikes(X, R):- likes(X), R = ['Si', me, gustan, los, X].
elizaLikes(X, R):- \+likes(X), R = ['No', no, me, gustan, los, X].

likes(manzanas).
likes(computadoras).
likes(carros).
likes(carnitas).
likes(pollos).
likes(frijoles).
likes(perros).
likes(gatos).
likes(clases).
likes(celulares).
likes(tacos).
likes(programar).
likes(aprender).
likes(ayudar).

%%--------------------------------------------------------------------------------------------------
%% LO QUE HACE ELIZA
%%--------------------------------------------------------------------------------------------------

elizaDoes(X, R):- does(X), R = ['Si', yo, X, y, me, encanta].
elizaDoes(X, R):- \+does(X), R = ['No', yo, no, X, es, muy, dificil, para, mi].

does(estudio).
does(cocino).
does(trabajo).
does(ayudo).

%%--------------------------------------------------------------------------------------------------
%% LO QUE ES ELIZA
%%--------------------------------------------------------------------------------------------------

elizaIs(X, R):- is0(X), R = ['Si', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', yo, no, soy, X].

is0(amable).
is0(inteligente).
is0(util).
is0(servicial).

%%--------------------------------------------------------------------------------------------------
%% FUNCIONES DE DIAGNOSTICO PARA ELIZA
%%--------------------------------------------------------------------------------------------------

elizaDiagnostico(Sintomas, R):-
    findall([E, P], (enfermedad(E), diagnostico(Sintomas, E, P), P > 0), Resultados),
    Resultados \= [],
    sort(2, @>=, Resultados, Ordenados),
    nth0(0, Ordenados, [MejorEnf, MejorProb]),
    format(atom(Msg), 'Basado en tus sintomas, podrias tener ~w con ~w% de probabilidad. Te recomiendo consultar un especialista.', [MejorEnf, MejorProb]),
    atom_chars(Msg, R).

elizaDiagnostico(_, R):-
    R = ['No', reconozco, esos, sintomas, '.', 'Describe', mejor, tu, malestar].

elizaMedicina(Sintoma, R):-
    findall(M, recetade(M, Sintoma), Medicinas),
    Medicinas \= [],
    format(atom(Msg), 'Para ~w se puede recetar: ~w. Consulta a un medico.', [Sintoma, Medicinas]),
    atom_chars(Msg, R).

elizaMedicina(_, R):-
    R = ['No', tengo, informacion, de, medicamentos, para, eso].

elizaEspecialista(Enfermedad, R):-
    findall(E, especialistade(E, Enfermedad), Especialistas),
    Especialistas \= [],
    format(atom(Msg), 'Para ~w debes consultar: ~w', [Enfermedad, Especialistas]),
    atom_chars(Msg, R).

elizaEspecialista(_, R):-
    R = ['No', se, que, especialista, recomendarte, para, eso].

%%--------------------------------------------------------------------------------------------------
%% FUNCIONES DE MATCHING Y REEMPLAZO
%%--------------------------------------------------------------------------------------------------

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S),
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

%% Flag para diagnostico
replace0([I|Index], Input, _, Resp, R):-
	nth0(0, Resp, X),
	X == flagDiagnostico,
	extraer_sintomas(Input, I, Sintomas),
	elizaDiagnostico(Sintomas, R).

%% Flag para medicina
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Sintoma),
	nth0(0, Resp, X),
	X == flagMedicina,
	elizaMedicina(Sintoma, R).

%% Flag para especialista
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Enfermedad),
	nth0(0, Resp, X),
	X == flagEspecialista,
	elizaEspecialista(Enfermedad, R).

%% Eliza likes
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	elizaLikes(Atom, R).

%% Eliza does
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	elizaDoes(Atom, R).

%% Eliza is
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagIs,
	elizaIs(Atom, R).

replace0([I|Index], Input, N, Resp, R):-
	length(Index, M), M =:= 0,
	nth0(I, Input, Atom),
	select(N, Resp, Atom, R1), append(R1, [], R),!.

replace0([I|Index], Input, N, Resp, R):-
	nth0(I, Input, Atom),
	length(Index, M), M > 0,
	select(N, Resp, Atom, R1),
	N1 is N + 1,
	replace0(Index, Input, N1, R1, R),!.

%%--------------------------------------------------------------------------------------------------
%% EXTRACCION DE SINTOMAS DE LA ENTRADA
%%--------------------------------------------------------------------------------------------------

extraer_sintomas(Input, Start, Sintomas):-
    length(Input, Len),
    End is Len - 1,
    extraer_rango(Input, Start, End, Sintomas).

extraer_rango(_, Start, End, []):- Start > End, !.
extraer_rango(Input, Start, End, [S|Rest]):-
    nth0(Start, Input, S),
    Start1 is Start + 1,
    extraer_rango(Input, Start1, End, Rest).

%%==================================================================================================
%% FIN DEL PROGRAMA
%%==================================================================================================
```
