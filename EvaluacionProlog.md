# Biblioteca de Libros
### Formalizar los siguientes enunciados y generar los posibles hechos y reglas
-----

### **1. La Biblioteca Nacional tiene el libro "Don Quijote de la Mancha".**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Bibliotecas y libros. | Universo de los objetos. |
    | **Constantes** | `biblioteca_nacional`, `don_quijote` | Objetos específicos y con nombre. |
    | **Relación** | `tiene` | Conecta los dos objetos. |
    | **Cuantificadores**| Ninguno. | Es una afirmación específica. |

  * **Prolog:**

    ```prolog
    % Hecho: La Biblioteca Nacional posee el libro Don Quijote.
    tiene(biblioteca_nacional, don_quijote_de_la_mancha).

    %Regla
    libro_disponible_en(L, B) :- tiene(B, L).
    ```

-----

### **2. Todos los libros en la Biblioteca Nacional están catalogados.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y sus propiedades. | Universo de los objetos. |
    | **Constante** | `biblioteca_nacional` | Objeto específico. |
    | **Propiedad** | `están catalogados` | Característica de los libros. |
    | **Cuantificador** | `Todos los libros` (Universal) | Generalización que se aplica a un conjunto. |

  * **Prolog (Regla):**

    ```prolog
    % Regla: Un libro (X) está catalogado si se encuentra en la Biblioteca Nacional.
    esta_catalogado(X) :- tiene(biblioteca_nacional, X).
    ```

-----

### **3. Existen libros que están en más de una biblioteca.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y bibliotecas. | Universo de los objetos. |
    | **Relación** | `están en` | Conexión entre libros y bibliotecas. |
    | **Cuantificador**| `Existen libros` (Existencial) | Afirma la existencia de al menos un caso. |

  * **Prolog :**
    En Prolog, la existencia se demuestra con ejemplos concretos.

    ```prolog
    % Hechos: Demuestran que "el_principito" está en dos lugares.
    tiene(biblioteca_central, el_principito).
    tiene(biblioteca_municipal, el_principito).

    %Regla
    esta_en_multiples_bibliotecas(L) :- tiene(B1, L), tiene(B2, L), B1 \= B2.
    ```

-----

### **4. Si un libro es raro, entonces no se puede prestar.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y sus estados. | Universo de los objetos. |
    | **Propiedades** | `es raro`, `no se puede prestar` | Características de un libro. |
    | **Condicional**| `Si..., entonces...` | Implicación lógica. |
    | **Cuantificador**| Implícito (para todo libro). | La regla se aplica a cualquier libro. |

  * **Prolog :**

    ```prolog
    % Regla: No se puede prestar un libro (X) si ese libro es raro.
    no_prestable(X) :- es_raro(X).

    % Hecho de ejemplo para que la regla funcione:
    es_raro(manuscrito_voynich).
    ```

-----

### **5. La Biblioteca Central tiene más de 10,000 libros.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Bibliotecas y sus propiedades. | Universo de los objetos. |
    | **Constante** | `biblioteca_central` | Objeto específico. |
    | **Propiedad** | `tiene más de 10,000 libros` | Característica numérica. |
    | **Cuantificador**| Ninguno. | Afirmación sobre un objeto específico. |

  * **Prolog :**

    ```prolog
    % Hecho: Se establece la cantidad de libros de la Biblioteca Central.
    cantidad_libros(biblioteca_central, 15200).

    % Regla: Una biblioteca (B) tiene muchos libros si su cantidad (N) es mayor a 10000.
    tiene_muchos_libros(B) :- cantidad_libros(B, N), N > 10000.
    ```

-----

### **6. Todos los autores tienen al menos un libro en una biblioteca.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Autores, libros, bibliotecas. | Universo de los objetos. |
    | **Relaciones** | `autor_de`, `tiene` | Conexiones entre entidades. |
    | **Cuantificador**| `Todos los autores` (Universal) | La regla se aplica a cada autor. |

  * **Prolog :**
    Esta es una restricción o condición del sistema. Se puede formular como una regla que verifica si un autor cumple la condición.

    ```prolog
    % Regla: Un autor (A) cumple la condición si existe un libro (L) del que es autor y una biblioteca (B) que tiene ese libro.
    autor_con_libro_publicado(A) :- autor(A), escrito_por(L, A), tiene(B, L).

    % Hechos de ejemplo:
    autor(cervantes).
    escrito_por(don_quijote, cervantes).
    tiene(biblioteca_nacional, don_quijote).
    ```

-----

### **7. Existe un autor que tiene más de 5 libros publicados.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Autores y libros. | Universo de los objetos. |
    | **Relación** | `escrito_por` | Relación entre libro y autor. |
    | **Cuantificador**| `Existe un autor` (Existencial) | Se demuestra con un ejemplo. |

  * **Prolog :**

    ```prolog
    % Hechos: Demuestran que 'asimov' tiene más de 5 libros.
    escrito_por(fundacion, asimov).
    escrito_por(yo_robot, asimov).
    escrito_por(el_fin_de_la_eternidad, asimov).
    escrito_por(los_propios_dioses, asimov).
    escrito_por(preludio_a_la_fundacion, asimov).
    escrito_por(hacia_la_fundacion, asimov).

    %Regla
    es_prolifico(A) :- findall(L, escrito_por(L, A), Libros), length(Libros, N), N > 5.
    ```

-----

### **8. No todos los libros de la biblioteca están en buen estado.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y sus estados. | Universo de los objetos. |
    | **Propiedad** | `están en buen estado` | Característica de un libro. |
    | **Cuantificador**| `No todos` (Negación universal) | Equivale a "Existe al menos uno que no...". |

  * **Prolog :**

    ```prolog
    % Hechos: Uno está en buen estado, el otro no.
    estado(don_quijote, buen_estado).
    estado(mapa_antiguo, mal_estado).

    %Regla
    no_esta_en_buen_estado(L) :- estado(L, E), E \= buen_estado.
    ```

-----

### **9. Si un libro está en buen estado, puede ser prestado.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y estados. | Universo de los objetos. |
    | **Propiedades** | `en buen estado`, `puede ser prestado` | Características. |
    | **Condicional**| `Si..., ...` | Implicación lógica. |
    | **Cuantificador**| Implícito (para todo libro). | La regla se aplica a cualquier libro. |

  * **Prolog :**

    ```prolog
    % Regla: Un libro (X) es prestable si su estado es 'buen_estado'.
    prestable(X) :- estado(X, buen_estado).
    ```

-----

### **10. Todos los usuarios registrados pueden tomar prestado un libro.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Usuarios y acciones. | Universo de los objetos. |
    | **Propiedades** | `registrado`, `puede tomar prestado` | Características y permisos. |
    | **Cuantificador**| `Todos los usuarios` (Universal) | Se aplica a cada usuario. |

  * **Prolog :**

    ```prolog
    % Regla: Un usuario (U) puede pedir prestado si está registrado.
    % (Esta regla podría ser más compleja, por ejemplo, si no tiene multas).
    puede_pedir_prestado(U) :- es_registrado(U).

    % Hecho de ejemplo:
    es_registrado(juan_perez).
    ```

-----

### **11. Existen libros que solo se pueden consultar en la biblioteca.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y sus permisos. | Universo de los objetos. |
    | **Propiedad** | `solo se pueden consultar` | Característica de un libro. |
    | **Cuantificador**| `Existen libros` (Existencial) | Se demuestra con un ejemplo. |

  * **Prolog :**

    ```prolog
    % Hecho: El 'atlas_historico' es un libro de solo consulta.
    modo_acceso(atlas_historico, solo_consulta).
    %Regla
    es_de_solo_consulta(L) :-
    modo_acceso(L, solo_consulta).
    ```

-----

### **12. Todo libro prestado debe ser devuelto en 15 días.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Préstamos y plazos. | Universo de los objetos. |
    | **Propiedad** | `plazo de devolución de 15 días` | Característica del préstamo. |
    | **Cuantificador**| `Todo libro prestado` (Universal) | Se aplica a cada libro en préstamo. |

  * **Prolog :**

    ```prolog
    % Regla: El plazo de devolución para un libro (L) es de 15 días si está prestado.
    plazo_devolucion(L, 15) :- prestado(L, _Usuario). % No nos importa quién lo tiene.

    % Hecho de ejemplo:
    prestado(cien_anios_de_soledad, ana_gomez).
    ```

-----

### **13. Hay un libro que nadie ha pedido en préstamo.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y préstamos. | Universo de los objetos. |
    | **Propiedad** | `nadie ha pedido en préstamo` | Característica (negativa) de un libro. |
    | **Cuantificador**| `Hay un libro` (Existencial) | Se demuestra con un ejemplo. |

  * **Prolog :**

    ```prolog
    % Hecho: 'algebra_avanzada' es un libro.
    libro(algebra_avanzada).

    % Regla: Un libro (L) nunca ha sido prestado si es un libro y no existe
    % un registro de que haya sido prestado a algún usuario.
    nunca_prestado(L) :- libro(L), \+ prestado(L, _).
    ```

-----

### **14. Si un usuario tiene una multa, no puede pedir un libro prestado.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Usuarios, multas, préstamos. | Universo de los objetos. |
    | **Relación** | `tiene multa` | Estado del usuario. |
    | **Condicional**| `Si..., no...` | Implicación lógica. |
    | **Cuantificador**| Implícito (para todo usuario). | Se aplica a cualquier usuario. |

  * **Prolog :**

    ```prolog
    % Regla: Un usuario (U) tiene bloqueados los préstamos si tiene una multa.
    prestamo_bloqueado(U) :- tiene_multa(U).

    % Hecho de ejemplo:
    tiene_multa(pedro_lopez).
    ```

-----

### **15. Todos los libros escritos por un mismo autor están en la misma sección.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros, autores, secciones. | Universo de los objetos. |
    | **Relaciones** | `escrito_por`, `ubicado_en` | Conexiones entre entidades. |
    | **Cuantificador**| `Todos los libros` (Universal) | Se aplica a cada libro. |

  * **Prolog :**
    Esta es una regla de consistencia. Se puede formular para verificar si dos libros de un mismo autor están en la misma sección.

    ```prolog
    % Regla: Verifica la consistencia. Si L1 y L2 son del mismo autor (A),
    % deben estar en la misma sección (S).
    misma_seccion_autor(L1, L2) :-
        escrito_por(L1, A),
        escrito_por(L2, A),
        ubicado_en(L1, S),
        ubicado_en(L2, S),
        L1 \= L2. % L1 y L2 no son el mismo libro.
    ```

-----

### **16. Existe un libro que tiene más de un ejemplar en la biblioteca.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y ejemplares. | Universo de los objetos. |
    | **Propiedad** | `tiene más de un ejemplar` | Característica de un libro. |
    | **Cuantificador**| `Existe un libro` (Existencial) | Se demuestra con un ejemplo. |

  * **Prolog:**

    ```prolog
    % Hechos: Se definen dos ejemplares distintos para el mismo título de libro.
    ejemplar(don_quijote, ej1_quijote).
    ejemplar(don_quijote, ej2_quijote).
    ejemplar(don_quijote, ej3_quijote).
    %Regla
    tiene_multiples_ejemplares(L) :-
    ejemplar(L, E1),
    ejemplar(L, E2),
    E1 \= E2.
    ```

-----

### **17. Todo usuario con más de tres préstamos debe devolver uno para pedir otro.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Usuarios y préstamos. | Universo de los objetos. |
    | **Propiedad** | `límite de préstamos alcanzado` | Estado de un usuario. |
    | **Cuantificador**| `Todo usuario` (Universal) | Se aplica a cada usuario. |

  * **Prolog :**

    ```prolog
    % Regla: Un usuario (U) debe devolver un libro si el número de libros
    % que tiene prestados (N) es mayor o igual a 3.
    debe_devolver_para_prestar(U) :-
        findall(L, prestado(L, U), Libros), % Encuentra todos los libros prestados a U
        length(Libros, N),                 % Cuenta cuántos son
        N >= 3.
    ```

-----

### **18. Hay una sección de la biblioteca donde todos los libros son de ciencias.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Secciones, libros, géneros. | Universo de los objetos. |
    | **Relación** | `son de ciencias` | Propiedad de los libros en una sección. |
    | **Cuantificador**| `Hay una sección` (Existencial) | Se demuestra con un ejemplo. |

  * **Prolog :**

    ```prolog
    % Hechos de ejemplo:
    ubicado_en(cosmos, seccion_divulgacion).
    genero(cosmos, ciencias).
    ubicado_en(una_breve_historia_del_tiempo, seccion_divulgacion).
    genero(una_breve_historia_del_tiempo, ciencias).

    % Regla para verificar: Una sección (S) es 'solo_ciencias' si no existe un libro (L)
    % en esa sección cuyo género no sea 'ciencias'.
    seccion_solo_ciencias(S) :-
        seccion(S),
        \+ (ubicado_en(L, S), \+ genero(L, ciencias)).
    ```
-----

### **19. No todos los libros en la biblioteca tienen más de 100 páginas.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Libros y propiedades. | Universo de los objetos. |
    | **Propiedad** | `tienen más de 100 páginas` | Característica de los libros. |
    | **Cuantificador**| `No todos` (Negación universal) | Se demuestra con un contraejemplo. |

  * **Prolog :**

    ```prolog
    % Hechos: Demuestran que existe al menos un libro con 100 o menos páginas.
    num_paginas(don_quijote, 863).
    num_paginas(el_principito, 96).
    % Regla
    es_corto(L) :-
    num_paginas(L, N),
    N =< 100.
    ```

-----

### **20. Existe un usuario que ha tomado prestados todos los libros de la sección infantil.**

  * **Formalizacion:**
    | Concepto de LPO | Elemento de la Frase | Tipo de Uso |
    | :--- | :--- | :--- |
    | **Dominio** | Usuarios, préstamos, secciones. | Universo de los objetos. |
    | **Relación** | `ha tomado prestados todos` | Relación compleja entre usuario y sección. |
    | **Cuantificador**| `Existe un usuario` (Existencial) | Afirma la existencia de un caso. |

  * **Prolog :**

    ```prolog
    % Hechos de ejemplo:
    usuario(lucia).
    seccion(matilda, infantil).
    seccion(donde_viven_los_monstruos, infantil).
    seccion(don_quijote, clasicos). % Libro de otra sección
    prestado(matilda, lucia).
    prestado(donde_viven_los_monstruos, lucia).

    % Regla: Un usuario (U) ha prestado todo de una sección (S) si para todo
    % libro (L) en la sección (S), ese libro ha sido prestado al usuario (U).
    presto_toda_la_seccion(U, S) :-
        usuario(U), seccion_definida(S),
        \+ (seccion(L, S), \+ prestado(L, U)).
    ```
