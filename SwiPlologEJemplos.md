# Fibonacci
```
% Si preguntas fib(0, F), Prolog unificará F con 0 y terminará.
fib(0, 0).
% Si preguntas fib(1, F), Prolog unificará F con 1 y terminará.
fib(1, 1).

fib(N, F) :-
    % Condición 1: N debe ser mayor que 1.
    % Esto asegura que la regla no se aplique para los casos base (0 y 1).
    N > 1,

    % Paso 1: Calcular el número anterior a N.
    % La variable N1 guardará el resultado de N - 1.
    N1 is N - 1,

    % Paso 2: Calcular el número dos posiciones antes de N.
    % La variable N2 guardará el resultado de N - 2.
    N2 is N - 2,

    % Paso 3: LLAMADA RECURSIVA para encontrar el Fibonacci de N-1.
    % Prolog buscará una regla que coincida con fib(N1, F1).
    % El resultado se guardará en la variable F1.
    fib(N1, F1),

    % Paso 4: LLAMADA RECURSIVA para encontrar el Fibonacci de N-2.
    % Prolog buscará una regla que coincida con fib(N2, F2).
    % El resultado se guardará en la variable F2.
    fib(N2, F2),

    % Paso 5: Sumar los dos resultados obtenidos de las llamadas recursivas.
    % El resultado final (F) es la suma de F1 y F2.
    F is F1 + F2.
```
# Division con Restas
```
% -----------------
% --- CASO BASE ---
% En este punto, ya no podemos restar más.
% - El Cociente (cuántas veces más podemos restar) es 0.
% - El Residuo es el propio Dividendo que sobró.
division(Dividendo, Divisor, 0, Dividendo) :-
    Dividendo < Divisor.

% ---------------------
% --- REGLA RECURSIVA --
    % Condición: El Dividendo debe ser mayor o igual que el Divisor.
    Dividendo >= Divisor,

    % Paso 1: Realizamos una resta.
    % Calculamos el nuevo dividendo después de quitarle el divisor una vez.
    NuevoDividendo is Dividendo - Divisor,

    % Paso 2: LLAMADA RECURSIVA.
    % Volvemos a llamar a la función con el "problema más pequeño".
    % El Residuo final será el que calcule esta cadena de llamadas.
    division(NuevoDividendo, Divisor, CocienteParcial, Residuo),

    % Paso 3: Calculamos el cociente final.
    % El cociente es 1 (por la resta que acabamos de hacer) más
    % el cociente que se obtuvo de las demás restas (CocienteParcial).
    Cociente is CocienteParcial + 1.
```
# Potencia con sumas
```
% ---------------------------------------------
% --- PREDICADO 1: Multiplicar usando Sumas ---
% ---------------------------------------------

% Caso base: Multiplicar cualquier número por 0 es 0.
multiplicar(_, 0, 0).

% Regla recursiva: A * B = A + (A * (B-1))
multiplicar(A, B, Resultado) :-
    B > 0,
    B1 is B - 1,
    multiplicar(A, B1, ResultadoParcial), % Llamada recursiva
    Resultado is A + ResultadoParcial.

% ------------------------------------------
% --- PREDICADO 2: Potencia usando Sumas ---
% ------------------------------------------

% Caso base: Cualquier base elevada a 0 es 1.
potencia(_, 0, 1).

% Regla recursiva: Base^Exp = Base * (Base^(Exp-1))
potencia(Base, Exp, Resultado) :-
    Exp > 0,
    Exp1 is Exp - 1,
    potencia(Base, Exp1, ResultadoParcial), % Llamada recursiva para la potencia
    multiplicar(Base, ResultadoParcial, Resultado). % Usa nuestro predicado de multiplicar
```
