# Pruebas de escritorio
---

##  Primer enfoque (recursivo)

### Código

```prolog
rotar(X, X, 0).
rotar([X|Y], L, N) :-
    N1 is N - 1,
    append(Y, [X], Y1),
    rotar(Y1, L, N1).
```

### Prueba de escritorio

Llamada inicial:

```
rotar([1,2,3,4], L, 2)
```

| Llamada | X | Y       | N | Operación                                | Resultado temporal   |
| ------- | - | ------- | - | ---------------------------------------- | -------------------- |
| 1       | 1 | [2,3,4] | 2 | N1 = 1, append([2,3,4],[1],Y1=[2,3,4,1]) | rotar([2,3,4,1],L,1) |
| 2       | 2 | [3,4,1] | 1 | N1 = 0, append([3,4,1],[2],Y1=[3,4,1,2]) | rotar([3,4,1,2],L,0) |
| 3       | — | —       | 0 | Caso base                                | L = [3,4,1,2]        |

 **Resultado final:**

```
L = [3,4,1,2]
```

---

##  Segundo enfoque (usando append y size)

### Código

```prolog
rotar(L, R, N) :-
    append(X, Y, L),
    size(X, N),
    append(Y, X, R).
```

### Prueba de escritorio

Llamada inicial:

```
rotar([1,2,3,4], R, 2)
```

| Paso | X     | Y     | size(X,N) | append(Y,X,R)         | Resultado   |
| ---- | ----- | ----- | --------- | --------------------- | ----------- |
| 1    | [1,2] | [3,4] | N=2       | append([3,4],[1,2],R) | R=[3,4,1,2] |

 **Resultado final:**

```
R = [3,4,1,2]
```

---

