# Chatbot
## Codigo 
```
eliza:-	writeln('Hola , mi nombre es  Eliza tu  chatbot,
	por favor ingresa tu consulta,
	usar solo minúsculas sin . al final:'),
	readln(Input),
	eliza(Input),!.

% ======================================
% DESPEDIDAS
% ======================================

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
	writeln('Adios. espero poder verte ayudado.'), !.

% ======================================
% PLANTILLAS PRINCIPALES
% ======================================

eliza(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	eliza(Input1), !.

%Saludos

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

template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu, '.'], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_),'.'], [porque, eres, tu, 0, '?'], [2]).

% pregunta algo que le gusta a eliza
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).

		 % pregunta algo que hace eliza
template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).
template([i, have, s(_), with, s(_), '.'], ['You', have, to, deal, with, your, 0, and, your, 1, in, a, mature, way, '.'], [2, 4]).
template([i, s(_),  _], [i, can, recommend, you, a, book, about, that, issue], []).
template([please, s(_), _], ['No', i, can, not, help, ',', i, am, just, a, machine], []). 
		 template([tell, me, a, s(_), _], ['No', i, can, not, ',', i, am, bad, at, that], []).

				  
template(_, ['Please', explain, a, little, more, '.'], []). 
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Yeah', i, like, X].
elizaLikes(X, R):- \+likes(X), R = ['Nope', i, do, not, like, X].
likes(apples).
likes(ponies).
likes(zombies).
likes(manzanas).
likes(computadoras).
likes(carros).



% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Yes', i, X, and, i, love, it].
elizaDoes(X, R):- \+does(X), R = ['No', i, do, not, X ,'.', it, is, too, hard, for, me].
does(study).
does(cook).
does(work).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Yes', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', i, am, not, X].
is0(dumb).
is0(weird).
is0(nice).
is0(fine).
is0(happy).
is0(redundant).

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S), % si I es un s(X) devuelve falso
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% Eliza likes:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	elizaLikes(Atom, R).

% Eliza does:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	elizaDoes(Atom, R).

% Eliza is:
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
```
## Explicacion de las 20 entradas y salidas 
### Despedida
Todas las despedidas están en una lista grande.

member(Input, [...]) revisa si lo que escribiste está en esa lista.

Si coincide → imprime el mensaje de despedida y termina el programa.

Está antes de todas las otras reglas, así que siempre funciona primero.

### Saludos
template([palabras_que_reconoce], [respuesta], [posiciones_a_reemplazar])

[palabras_que_reconoce] → frase que Eliza entiende

[respuesta] → frase que Eliza dice

[posiciones_a_reemplazar] → lugares donde se insertan palabras del usuario (si es dinámico)

En tu caso de [que, tal]:

Eliza reconoce “que tal”

Responde “Hola, en que puedo ayudarte?”

No hay reemplazos dinámicos → la lista de posiciones es [].

# Modificacion de 30 likes agregados al codigo
```
eliza:-	writeln('Hola , mi nombre es  Eliza tu  chatbot,
	por favor ingresa tu consulta,
	usar solo minúsculas sin . al final:'),
	readln(Input),
	eliza(Input),!.

% ======================================
% DESPEDIDAS
% ======================================

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
	writeln('Adios. espero poder verte ayudado.'), !.

% ======================================
% PLANTILLAS PRINCIPALES
% ======================================

eliza(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	eliza(Input1), !.

%Saludos

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

template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu, '.'], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_),'.'], [porque, eres, tu, 0, '?'], [2]).

% pregunta algo que le gusta a eliza
template([te, gustan, las, s(_)], [flagLike], [3]).
template([te, gustan, los, s(_)], [flagLike], [3]).
template([te, gustan, el, s(_)], [flagLike], [3]).
template([te, gusta, s(_)], [flagLike], [3]).
template([te, gustan, lo, s(_)], [flagLike], [3]).

		 % pregunta algo que hace eliza
template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).
template([i, have, s(_), with, s(_), '.'], ['You', have, to, deal, with, your, 0, and, your, 1, in, a, mature, way, '.'], [2, 4]).
template([i, s(_),  _], [i, can, recommend, you, a, book, about, that, issue], []).
template([please, s(_), _], ['No', i, can, not, help, ',', i, am, just, a, machine], []). 
		 template([tell, me, a, s(_), _], ['No', i, can, not, ',', i, am, bad, at, that], []).

				  
template(_, ['Please', explain, a, little, more, '.'], []). 
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Yeah', i, like, X].
elizaLikes(X, R):- \+likes(X), R = ['Nope', i, do, not, like, X].
likes(apples).
likes(ponies).
likes(zombies).
likes(manzanas).
likes(computadoras).
likes(carros).
% Agregacion de mas Likes
likes(carnitas).
likes(pollos).
likes(frijoles).
likes(peras).
likes(perros).
likes(gatos).
likes(ratones).
likes(leones).
likes(clases).
likes(hombres).
likes(mujeres).
likes(chamarras).
likes(aviones).
likes(barcos).
likes(tareas).
likes(celulares).
likes(laptops).
likes(luces).
likes(tortas).
likes(burritos).
likes(yogurth).
likes(chilaquiles).
likes(tacos).
likes(java).
likes(clisp).
likes(caminar).
likes(emsamblador).
likes(minecraft).
likes(jugar).
likes(aprender).
likes(plantas).
likes(nadar).
likes(estudiar).
likes(platicar).


% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Yes', i, X, and, i, love, it].
elizaDoes(X, R):- \+does(X), R = ['No', i, do, not, X ,'.', it, is, too, hard, for, me].
does(study).
does(cook).
does(work).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Yes', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', i, am, not, X].
is0(dumb).
is0(weird).
is0(nice).
is0(fine).
is0(happy).
is0(redundant).

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S), % si I es un s(X) devuelve falso
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% Eliza likes:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	elizaLikes(Atom, R).

% Eliza does:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	elizaDoes(Atom, R).

% Eliza is:
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
```
## Explicacion de lo realizado
```
% Agregacion de mas Likes
likes(carnitas).
likes(pollos).
likes(frijoles).
likes(peras).
likes(perros).
likes(gatos).
likes(ratones).
likes(leones).
likes(clases).
likes(hombres).
likes(mujeres).
likes(chamarras).
likes(aviones).
likes(barcos).
likes(tareas).
likes(celulares).
likes(laptops).
likes(luces).
likes(tortas).
likes(burritos).
likes(yogurth).
likes(chilaquiles).
likes(tacos).
likes(java).
likes(clisp).
likes(caminar).
likes(emsamblador).
likes(minecraft).
likes(jugar).
likes(aprender).
likes(plantas).
likes(nadar).
likes(estudiar).
likes(platicar).
```
Se agregaron 30 echos mas de cosas que le gustan a Eliza
```
template([te, gustan, el, s(_)], [flagLike], [3]).
template([te, gusta, s(_)], [flagLike], [3]).
template([te, gustan, lo, s(_)], [flagLike], [3]).
```
Se agregaron 3 templates mas para dar congruencia a lo agregado en los likes
<img width="478" height="169" alt="image" src="https://github.com/user-attachments/assets/68cdcca5-2b11-4c4a-94cb-b800bdd8b504" />

