# ELIZA en CLISP

El sistema se basa en **plantillas (templates), reglas y bases de conocimiento** para interpretar las frases del usuario y generar respuestas.

El chatbot puede interactuar en distintos dominios:

* Conversación general
* Registro y seguimiento de pacientes
* Análisis médico básico
* Consultas sobre una base de familia
* Consultas sobre una base de carros

## Funcionalidades principales

El chatbot es capaz de:

* Mantener una conversación básica (saludos, despedidas, charla general)
* Registrar un paciente por nombre
* Guardar síntomas del paciente
* Inferir posibles enfermedades
* Calcular probabilidad de una enfermedad
* Determinar severidad y nivel de riesgo
* Detectar síntomas contradictorios
* Generar un reporte médico completo
* Responder preguntas sobre relaciones familiares
* Responder preguntas sobre automóviles (precio, potencia, consumo, etc.)

---

## Flujo general del sistema

El funcionamiento general de ELIZA sigue el siguiente flujo:

1. El sistema muestra un mensaje de bienvenida.
2. El usuario escribe una frase.
3. La entrada se normaliza (minúsculas y sin signos).
4. La frase se compara contra una lista de plantillas.
5. Si hay coincidencia, se ejecuta la acción asociada.
6. El chatbot genera una respuesta.
7. El sistema espera una nueva entrada.
8. El ciclo se repite hasta que el usuario escriba `adios` o `bye`.

---

## Normalización de la entrada

Para evitar errores al comparar texto, la entrada del usuario se **normaliza** antes de procesarse:

* Todas las palabras se convierten a minúsculas.
* Se eliminan signos de puntuación como:

  ```
  . , ; : ( ) ? ! " '
  ```
* La frase se divide en palabras (tokens).

### Ejemplo

**Entrada del usuario:**

```
Hola, MI Nombre es Juan.
```

**Entrada normalizada:**

```
("hola" "mi" "nombre" "es" "juan")
```

Esto permite que el sistema compare frases de manera consistente, sin importar cómo escriba el usuario.

---

## Motor de patrones (Templates)

El corazón del chatbot es un **motor de coincidencia de patrones** basado en plantillas.

Cada plantilla define:

1. **Patrón**: estructura esperada de la frase.
2. **Acción**: qué debe hacer el sistema (bandera o texto).
3. **Índices**: posiciones de palabras importantes dentro de la frase.

### Ejemplo conceptual

```
(mi nombre es s)
```

Detecta frases como:

```
mi nombre es juan
mi nombre es ana
```

El comodín `s` permite aceptar cualquier palabra en esa posición.

---

## Comodines en las plantillas

Las plantillas pueden usar comodines para capturar palabras variables.

### Ejemplo

```
(tengo s)
```

Acepta frases como:

* tengo ardor
* tengo fiebre
* tengo dolor

Esto hace que el chatbot sea más flexible y acepte muchas variantes de una misma intención.

---

## Generación de respuestas

Cuando una plantilla coincide con la entrada:

1. Se identifica la **bandera** asociada (por ejemplo `flagSintoma`).
2. Se extraen las palabras importantes usando los índices.
3. Se llama a la función correspondiente (médico, familia o carros).
4. Se construye la respuesta final combinando texto y datos.

Toda esta lógica se concentra en el sistema de **handlers**, que deciden qué hacer según el tipo de consulta.

---

## Manejo de memoria

El sistema recuerda información durante la conversación usando variables globales.

### Paciente actual

Se guarda el paciente que está interactuando:

```
*paciente-actual*
```

Esto permite que todos los síntomas y diagnósticos se asocien correctamente a la misma persona.

### Síntomas del paciente

Los síntomas se almacenan en una tabla hash:

```
*paciente-sintomas*
```

Cada paciente tiene su propia lista de síntomas, que se va acumulando conforme avanza la conversación.

---

## Base médica

La base médica está formada por hechos simples, como:

* Síntomas asociados a enfermedades
* Medicinas disponibles
* Causas
* Tratamientos
* Lugares donde es común una enfermedad

A partir de esta información, el sistema puede:

* Sugerir enfermedades posibles
* Calcular probabilidad
* Evaluar severidad
* Determinar nivel de riesgo
* Generar recomendaciones médicas

---

## Diagnóstico básico

Una enfermedad se considera **posible** si el paciente presenta **al menos uno de sus síntomas**.

Esto permite dar diagnósticos tempranos aunque no estén todos los síntomas confirmados.

---

## Probabilidad, severidad y riesgo

### Probabilidad

Se calcula como:

```
(síntomas confirmados / síntomas totales de la enfermedad) * 100
```

### Severidad

* 1 síntoma → Leve
* 2 síntomas → Moderada
* 3 o más síntomas → Severa

### Riesgo

El nivel de riesgo se calcula con reglas simples basadas en la cantidad de síntomas y el tipo de enfermedad.

---

## Árbol de decisión

El sistema incluye un **árbol de decisión médico sencillo** que prioriza ciertos síntomas clave, por ejemplo:

* Ardor o secreción → Gonorrea
* Sed u orina frecuente → Diabetes gestacional
* Cambios en lunares o heridas → Cáncer de piel

Este árbol permite obtener diagnósticos rápidos cuando los síntomas son claros.

---

## Reporte médico completo

El usuario puede solicitar un reporte con frases como:

```
dame un reporte completo
```

El reporte incluye:

* Nombre del paciente
* Síntomas confirmados
* Enfermedades posibles
* Probabilidad
* Severidad
* Nivel de riesgo
* Tratamiento sugerido
* Recomendación médica
* Alertas por síntomas contradictorios (si existen)

---

## Base de familia

El sistema incluye una base de conocimiento familiar basada en hechos simples como:

* Padre
* Madre
* Esposo / esposa

Con estas relaciones se pueden inferir:

* Hijos
* Hermanos
* Abuelos

### Ejemplos de preguntas

* quien es el padre de sol
* adrian es hermano de baltazarjr
* quienes son los hermanos de tavo

---

## Base de carros

El chatbot también puede responder preguntas sobre automóviles.

La base de carros contiene información como:

* Precio
* País de fabricación
* Potencia
* Tipo de motor
* Consumo
* Nivel de seguridad

A diferencia del módulo médico, **no se hacen inferencias**, solo consultas directas.

### Ejemplos de preguntas

* cuanto cuesta el ferrari 488
* donde se fabrica el ford mustang
* que motor tiene el bugatti chiron
* cuantos caballos tiene el honda civic

---

# Codigo
```
;;;; ============================================================
;;;; ELIZA en CLISP (Familia / Carros / Médico)
;;;; Documentación simple y “humana” para GitHub (README o comentario)
;;;; ============================================================

;;;; ============================================================
;;;; 1) UTILIDADES: ESPACIOS, PUNTUACIÓN Y TOKENIZACIÓN
;;;; ============================================================

(defun eliza--space-p (c)
  ;; Regresa T si el carácter es un “espacio” (space, tab, enter, etc.)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline) (char= c #\Return)))

(defparameter *eliza-punct-chars* ".,;:()?!\"'")
;; Lista de caracteres que se consideran puntuación para limpiarlos del texto.

(defun clean-and-tokenize (line)
  ;; Convierte una línea de texto a una lista de tokens (palabras).
  ;; Pasos:
  ;; 1) Pasa todo a minúsculas.
  ;; 2) Reemplaza puntuación por espacios.
  ;; 3) Separa por espacios y devuelve lista de strings.
  (when line
    (let* ((lower (string-downcase line))
           (clean-str
             (with-output-to-string (out)
               (loop for c across lower do
                     (if (find c *eliza-punct-chars* :test #'char-equal)
                         (write-char #\Space out)
                         (write-char c out))))))
      (let ((tokens '())
            (cur ""))
        (loop for c across clean-str do
              (if (eliza--space-p c)
                  (when (> (length cur) 0)
                    (push cur tokens)
                    (setf cur ""))
                  (setf cur (concatenate 'string cur (string c)))))
        (when (> (length cur) 0) (push cur tokens))
        (nreverse tokens)))))

(defun string-to-symbol (str)
  ;; Convierte un string a símbolo.
  ;; - Pasa a MAYÚSCULAS (estándar de símbolos en Lisp)
  ;; - Reemplaza espacios por guiones (por si el token trae espacios)
  (intern (string-upcase (substitute #\- #\Space str))))

(defun get-token-at (input pos)
  ;; Regresa el token (string) en la posición pos de la lista input.
  ;; Si pos está fuera de rango, regresa "".
  (if (and input (>= pos 0) (< pos (length input)))
      (nth pos input)
      ""))

(defun symbol->pretty (sym)
  ;; Convierte un símbolo a string “bonito” (en minúsculas) para imprimir.
  (string-downcase (symbol-name sym)))

;;;; ============================================================
;;;; 2) ESTADO: PACIENTE ACTUAL Y SUS SÍNTOMAS
;;;; ============================================================

(defparameter *paciente-actual* nil)
;; Guarda el símbolo del paciente actual (por ejemplo: JUAN).
;; Si no hay paciente, es NIL.

(defparameter *paciente-sintomas* (make-hash-table :test #'eq))
;; HashTable: clave = paciente (símbolo), valor = lista de síntomas (símbolos).

(defun set-paciente (nombre-sym)
  ;; Define quién es el paciente actual.
  ;; Si no existe en la tabla, lo inicializa con lista vacía de síntomas.
  (setf *paciente-actual* nombre-sym)
  (unless (gethash nombre-sym *paciente-sintomas*)
    (setf (gethash nombre-sym *paciente-sintomas*) '()))
  nombre-sym)

(defun paciente-sintomas (paciente)
  ;; Regresa una copia de la lista de síntomas del paciente.
  (copy-list (or (gethash paciente *paciente-sintomas*) '())))

(defun add-sintomas (paciente sintomas)
  ;; Agrega síntomas a un paciente, evitando repetidos.
  (let* ((prev (paciente-sintomas paciente))
         (new (remove-duplicates (append sintomas prev) :test #'eq)))
    (setf (gethash paciente *paciente-sintomas*) new)
    new))

(defun reset-sintomas-paciente (paciente)
  ;; Borra (reinicia) la lista de síntomas de un paciente.
  (setf (gethash paciente *paciente-sintomas*) '())
  t)

(defun require-paciente ()
  ;; “Guard” para verificar que ya se registró paciente.
  ;; Si no hay paciente, regresa un mensaje (lista de strings).
  ;; Si sí hay paciente, regresa T.
  (unless *paciente-actual*
    (return-from require-paciente
      (list "Primero dime tu nombre con:" "mi nombre es juan")))
  t)

;;;; ============================================================
;;;; 3) BASE DE DATOS FAMILIAR (HECHOS + CONSULTAS BÁSICAS)
;;;; ============================================================

(defparameter *familia*
  ;; Base familiar como lista de relaciones:
  ;; (padre <nombre> (<hijos...>))
  ;; (madre <nombre> (<hijos...>))
  ;; (esposo <nombre> <pareja>)
  '((padre filiberto (sol luna gabriel tavo luis))
    (padre eustacio (baltazar catalina))
    (padre tavo (tavito))
    (padre luis (alan luised))
    (padre luised (santiago barbara))
    (padre baltazar (adrian baltazarjr))
    (madre eugenia (sol luna gabriel tavo luis))
    (madre mela (baltazar catalina))
    (madre fatima (tavito))
    (madre martha (alan luised))
    (madre sol (adrian baltazarjr))
    (madre luna (andrea))
    (esposo filiberto eugenia)
    (esposo eustacio mela)
    (esposo tavo fatima)
    (esposo luis martha)
    (esposo baltazar sol)))

(defun padrede (padre hijo)
  ;; Busca si PADRE es padre de HIJO dentro de *familia*.
  (find-if (lambda (rel)
             (and (eq (first rel) 'padre)
                  (eq (second rel) padre)
                  (member hijo (third rel))))
           *familia*))

(defun madrede (madre hijo)
  ;; Busca si MADRE es madre de HIJO dentro de *familia*.
  (find-if (lambda (rel)
             (and (eq (first rel) 'madre)
                  (eq (second rel) madre)
                  (member hijo (third rel))))
           *familia*))

(defun hermanos (persona)
  ;; Regresa lista de hermanos(as) por compartir el mismo padre (según la base).
  ;; Nota: aquí se usa SOLO la relación de padre, no madre.
  (let ((padre-rel (find-if (lambda (r)
                              (and (eq (first r) 'padre)
                                   (member persona (third r))))
                            *familia*)))
    (when padre-rel
      (remove persona (third padre-rel)))))

(defun abuelo (abuelo nieto)
  ;; Verifica si “abuelo” es abuelo de “nieto”:
  ;; 1) busca si abuelo es PADRE de alguien (sus hijos)
  ;; 2) si alguno de esos hijos es padre/madre de “nieto”, entonces sí.
  (find-if (lambda (padre-rel)
             (and (eq (first padre-rel) 'padre)
                  (eq (second padre-rel) abuelo)
                  (find-if (lambda (hijo)
                             (or (padrede hijo nieto)
                                 (madrede hijo nieto)))
                           (third padre-rel))))
           *familia*))

;;;; ============================================================
;;;; 4) BASE DE DATOS MÉDICA (ENFERMEDADES, SÍNTOMAS, MEDICINAS)
;;;; ============================================================

(defparameter *enfermedades* '(gonorrea diabetes-gestacional cancer_pie))
;; Lista de enfermedades que el bot conoce.

(defparameter *sintomas*
  ;; Lista tipo (sintoma enfermedad)
  '((ardor gonorrea)
    (secrecion gonorrea)
    (dolor-testiculos gonorrea)
    (dolor-pelvico gonorrea)
    (sangrado gonorrea)
    (picazon-rectal gonorrea)
    (dolor-garganta gonorrea)
    (dolor-ojos gonorrea)

    (sed diabetes-gestacional)
    (orina-frecuente diabetes-gestacional)
    (fatiga diabetes-gestacional)
    (vision-borrosa diabetes-gestacional)
    (infecciones diabetes-gestacional)
    (nauseas diabetes-gestacional)

    (lunar-cambia cancer_pie)
    (mancha-crece cancer_pie)
    (herida-no-sana cancer_pie)
    (bulto cancer_pie)
    (dolor cancer_pie)
    (inflamacion cancer_pie)
    (picazon cancer_pie)))

(defparameter *medicinas*
  ;; Lista tipo (medicina enfermedad)
  '((ceftriaxona gonorrea)
    (azitromicina gonorrea)
    (doxiciclina gonorrea)
    (cefixima gonorrea)
    (gentamicina gonorrea)
    (insulina diabetes-gestacional)
    (metformina diabetes-gestacional)
    (gliburida diabetes-gestacional)
    (pembrolizumab cancer_pie)
    (nivolumab cancer_pie)
    (ipilimumab cancer_pie)
    (vemurafenib cancer_pie)
    (dabrafenib cancer_pie)
    (trametinib cancer_pie)
    (dacarbazina cancer_pie)
    (temozolomida cancer_pie)))

(defparameter *causas*
  ;; Lista tipo (causa enfermedad)
  '((contacto-sexual gonorrea)
    (bacteria gonorrea)
    (embarazo diabetes-gestacional)
    (sobrepeso diabetes-gestacional)
    (genetica diabetes-gestacional)
    (rayos-uv cancer_pie)
    (exposicion-solar cancer_pie)))

(defparameter *tratamientos*
  ;; Lista tipo (enfermedad "texto tratamiento")
  '((gonorrea "Antibioticos como ceftriaxona y azitromicina. Tratar tambien a parejas sexuales.")
    (diabetes-gestacional "Control de glucosa, dieta saludable y ejercicio. Insulina si es necesario.")
    (cancer_pie "Cirugia de extirpacion. Radioterapia o quimioterapia en casos avanzados.")))

(defparameter *lugares-enfermedad*
  ;; Lista tipo (enfermedad "texto lugar/incidencia")
  '((gonorrea "Mundial. Alta incidencia en Africa y Pacifico Occidental.")
    (diabetes-gestacional "Mundial. Mayor prevalencia en poblaciones con sobrepeso.")
    (cancer_pie "Alta incidencia en Australia; tambien elevado en zonas cercanas al ecuador.")))

(defparameter *contradictorios*
  ;; Pares de síntomas que se marcan como “contradictorios”
  '((sed sangrado)
    (vision-borrosa dolor-ojos)))

(defun contradictorio-p (s1 s2)
  ;; True si el par (s1 s2) o (s2 s1) está en la lista de contradictorios.
  (or (member (list s1 s2) *contradictorios* :test #'equal)
      (member (list s2 s1) *contradictorios* :test #'equal)))

(defun sintoma-de (sintoma)
  ;; Dado un síntoma, regresa lista de enfermedades donde aparece ese síntoma.
  (remove-duplicates
   (mapcar #'second (remove-if-not (lambda (s) (eq (first s) sintoma)) *sintomas*))
   :test #'eq))

(defun medicinas-para (enfermedad)
  ;; Lista de medicinas asociadas a esa enfermedad.
  (mapcar #'first (remove-if-not (lambda (m) (eq (second m) enfermedad)) *medicinas*)))

(defun sintomas-de (enfermedad)
  ;; Lista de síntomas de una enfermedad.
  (mapcar #'first (remove-if-not (lambda (s) (eq (second s) enfermedad)) *sintomas*)))

(defun causas-de (enfermedad)
  ;; Lista de causas de una enfermedad.
  (mapcar #'first (remove-if-not (lambda (c) (eq (second c) enfermedad)) *causas*)))

(defun tratamiento-de (enfermedad)
  ;; Regresa texto de tratamiento de una enfermedad.
  (second (assoc enfermedad *tratamientos*)))

(defun lugar-de (enfermedad)
  ;; Regresa texto de lugar/incidencia de una enfermedad.
  (second (assoc enfermedad *lugares-enfermedad*)))

(defun diagnostico-exclusivo (paciente)
  ;; Regresa enfermedades sugeridas por síntomas que SOLO aparecen en una enfermedad.
  (let ((ps (paciente-sintomas paciente))
        (res '()))
    (dolist (s ps)
      (let ((enfs (sintoma-de s)))
        (when (= (length enfs) 1)
          (push (first enfs) res))))
    (remove-duplicates res :test #'eq)))

(defun probabilidad (paciente enfermedad)
  ;; Probabilidad = (# síntomas confirmados del paciente / # síntomas totales de la enfermedad) * 100
  (let* ((todos (sintomas-de enfermedad))
         (tot (length todos)))
    (if (<= tot 0)
        0.0
        (let* ((ps (paciente-sintomas paciente))
               (conf (length (intersection ps todos :test #'eq))))
          (* 100.0 (/ conf tot))))))

(defun diagnostico-preventivo-p (paciente enfermedad)
  ;; Preventivo: tiene al menos 1 síntoma pero no TODOS.
  (let* ((todos (sintomas-de enfermedad))
         (tot (length todos))
         (ps (paciente-sintomas paciente))
         (conf (length (intersection ps todos :test #'eq))))
    (and (> tot 0) (>= conf 1) (< conf tot))))

(defun enfermedades-similares (e1 e2)
  ;; Dos enfermedades son similares si comparten >= 2 síntomas.
  (when (and (member e1 *enfermedades* :test #'eq)
             (member e2 *enfermedades* :test #'eq)
             (not (eq e1 e2)))
    (let* ((s1 (sintomas-de e1))
           (s2 (sintomas-de e2))
           (comunes (intersection s1 s2 :test #'eq)))
      (>= (length comunes) 2))))

(defun lista-enfermedades-similares (e)
  ;; Regresa lista de enfermedades similares a e.
  (remove-if-not (lambda (x) (enfermedades-similares e x)) *enfermedades*))

(defun sintomas-contradictorios-p (paciente)
  ;; True si el paciente tiene al menos un par de síntomas contradictorios.
  (let ((ps (paciente-sintomas paciente)))
    (loop for a in ps do
          (loop for b in ps do
                (when (and (not (eq a b)) (contradictorio-p a b))
                  (return-from sintomas-contradictorios-p t))))
    nil))

(defun arbol-diagnostico (paciente)
  ;; Diagnóstico rápido por reglas:
  ;; - Si hay ardor o secreción: gonorrea
  ;; - Si hay sed u orina frecuente: diabetes-gestacional
  ;; - Si hay cambios de lunar/mancha/herida: cancer_pie
  (let ((ps (paciente-sintomas paciente)))
    (cond
      ((or (member 'ardor ps) (member 'secrecion ps)) 'gonorrea)
      ((or (member 'sed ps) (member 'orina-frecuente ps)) 'diabetes-gestacional)
      ((or (member 'lunar-cambia ps) (member 'mancha-crece ps) (member 'herida-no-sana ps)) 'cancer_pie)
      (t nil))))

(defun contar-sintomas-confirmados (paciente enfermedad)
  ;; Cuenta cuántos síntomas de “enfermedad” están presentes en el paciente.
  (let ((ps (paciente-sintomas paciente))
        (todos (sintomas-de enfermedad)))
    (length (intersection ps todos :test #'eq))))

(defun severidad (paciente enfermedad)
  ;; Severidad basada en cuántos síntomas coinciden:
  ;; 3+ = severa, 2 = moderada, 1 = leve.
  (let ((c (contar-sintomas-confirmados paciente enfermedad)))
    (cond
      ((>= c 3) 'severa)
      ((= c 2) 'moderada)
      ((= c 1) 'leve)
      (t 'desconocida))))

(defun riesgo (paciente enfermedad)
  ;; Riesgo por enfermedad usando umbrales (ejemplo simple):
  ;; cancer_pie: 4+ alto, 2-3 medio, 1 bajo.
  ;; gonorrea/diabetes-gestacional: igual patrón aquí.
  (let ((c (contar-sintomas-confirmados paciente enfermedad)))
    (cond
      ((eq enfermedad 'cancer_pie)
       (cond ((>= c 4) 'alto) ((>= c 2) 'medio) ((= c 1) 'bajo) (t 'bajo)))
      ((eq enfermedad 'gonorrea)
       (cond ((>= c 4) 'alto) ((>= c 2) 'medio) ((= c 1) 'bajo) (t 'bajo)))
      ((eq enfermedad 'diabetes-gestacional)
       (cond ((>= c 4) 'alto) ((>= c 2) 'medio) ((= c 1) 'bajo) (t 'bajo)))
      (t 'bajo))))

(defun diagnostico-basico-enfermedades (paciente)
  ;; Diagnóstico básico: junta todas las enfermedades asociadas a los síntomas del paciente.
  (let ((ps (paciente-sintomas paciente))
        (res '()))
    (dolist (s ps)
      (dolist (e (sintoma-de s))
        (push e res)))
    (remove-duplicates res :test #'eq)))

(defun tratamiento-combinado (paciente)
  ;; Regresa una lista de tratamientos para todas las enfermedades candidatas.
  (let ((enfs (diagnostico-basico-enfermedades paciente))
        (res '()))
    (dolist (e enfs)
      (let ((tto (tratamiento-de e)))
        (when tto (push tto res))))
    (remove-duplicates (nreverse res) :test #'string=)))

(defun recomendacion (paciente enfermedad)
  ;; Mensaje final de recomendación según severidad y enfermedad.
  (let ((sev (severidad paciente enfermedad)))
    (cond
      ((and (eq sev 'severa) (eq enfermedad 'cancer_pie))
       "URGENTE: acude con un especialista (dermatologia/oncologia) lo antes posible.")
      ((eq sev 'severa)
       "Se recomienda acudir al medico lo antes posible para una evaluacion completa.")
      ((eq sev 'moderada)
       "Se recomienda consultar con un medico en las proximas 24-48 horas.")
      ((eq sev 'leve)
       "Monitorea sintomas; si empeoran o persisten, acude al medico.")
      (t "No puedo determinar recomendacion sin sintomas suficientes."))))

(defun diagnosticar-y-tratar (paciente)
  ;; Devuelve 2 valores:
  ;; - diagnóstico (por árbol o por diagnóstico básico)
  ;; - tratamiento asociado
  (let* ((d (or (arbol-diagnostico paciente)
                (first (diagnostico-basico-enfermedades paciente))))
         (tto (and d (tratamiento-de d))))
    (values d tto)))

(defun reporte (paciente)
  ;; Imprime un reporte completo:
  ;; - paciente
  ;; - síntomas confirmados
  ;; - enfermedades posibles + probabilidad/severidad/riesgo/tratamiento/recomendación
  (let* ((sints (paciente-sintomas paciente))
         (enfs (diagnostico-basico-enfermedades paciente)))
    (format t "~%========================================~%")
    (format t "           REPORTE COMPLETO~%")
    (format t "========================================~%")
    (format t "Paciente: ~a~%" (symbol->pretty paciente))
    (format t "Sintomas confirmados: ~{~a~^, ~}~%"
            (mapcar #'symbol->pretty sints))
    (format t "----------------------------------------~%")
    (if (null enfs)
        (format t "Enfermedades posibles: (ninguna con evidencia suficiente)~%")
        (progn
          (format t "Enfermedades posibles: ~{~a~^, ~}~%~%"
                  (mapcar #'symbol->pretty enfs))
          (dolist (e enfs)
            (let* ((p (probabilidad paciente e))
                   (sev (severidad paciente e))
                   (rg (riesgo paciente e))
                   (tto (tratamiento-de e))
                   (rec (recomendacion paciente e)))
              (format t "• ~a~%" (symbol->pretty e))
              (format t "  - Probabilidad: ~,2f%%~%" p)
              (format t "  - Severidad: ~a~%" (symbol->pretty sev))
              (format t "  - Riesgo: ~a~%" (symbol->pretty rg))
              (format t "  - Tratamiento: ~a~%" (or tto "No disponible"))
              (format t "  - Recomendacion: ~a~%~%" rec)))))
    (when (sintomas-contradictorios-p paciente)
      (format t "ALERTA: Se detectaron sintomas contradictorios.~%"))
    (format t "========================================~%~%")))

;;;; ============================================================
;;;; 5) BASE DE DATOS DE CARROS
;;;; ============================================================

(defparameter *carros*
  ;; Base de carros como lista de hechos:
  ;; (precio <carro> <valor>)
  ;; (fabrica <carro> (<paises>))
  ;; (potencia <carro> <hp>)
  ;; (motor <carro> "texto")
  ;; (consumo <carro> <km/l>)
  ;; (seguridad <carro> <estrellas>)
  '((precio toyota-corolla 25)
    (precio honda-civic 28)
    (precio ford-mustang 35)
    (precio ferrari-488 280)
    (precio bugatti-chiron 3500)
    (fabrica toyota-corolla (japon tailandia brasil))
    (fabrica honda-civic (japon estados-unidos canada))
    (fabrica ford-mustang (estados-unidos))
    (fabrica ferrari-488 (italia))
    (fabrica bugatti-chiron (francia))
    (potencia toyota-corolla 169)
    (potencia honda-civic 158)
    (potencia ford-mustang 450)
    (potencia ferrari-488 661)
    (potencia bugatti-chiron 1479)
    (motor toyota-corolla "motor 4 cilindros 2.0L")
    (motor honda-civic "motor 4 cilindros 1.5L turbo")
    (motor ford-mustang "motor V8 5.0L")
    (motor ferrari-488 "motor V8 3.9L biturbo")
    (motor bugatti-chiron "motor W16 8.0L quad-turbo")
    (consumo toyota-corolla 15)
    (consumo honda-civic 16)
    (consumo ford-mustang 8)
    (consumo ferrari-488 6)
    (consumo bugatti-chiron 4)
    (seguridad toyota-corolla 5)
    (seguridad honda-civic 5)
    (seguridad ford-mustang 4)
    (seguridad ferrari-488 4)
    (seguridad bugatti-chiron 4)))

(defun get-carro-info (tipo carro)
  ;; Busca un “hecho” en *carros* por tipo y carro.
  ;; Ej: (get-carro-info 'precio 'toyota-corolla) => 25
  (let ((info (find-if (lambda (entry)
                         (and (eq (first entry) tipo)
                              (eq (second entry) carro)))
                       *carros*)))
    (when info (third info))))

;;;; ============================================================
;;;; 6) AYUDA / MENÚ
;;;; ============================================================

(defun eliza-help-lines ()
  ;; Lista de líneas para mostrar ejemplos de uso.
  (list
   "Puedo conversar, familia, carros y medico."
   "Ejemplos:"
   "  - mi nombre es juan"
   "  - tengo sed y fatiga"
   "  - que probabilidad tengo de tener gonorrea"
   "  - dame un reporte completo"
   "  - filiberto es padre de sol"
   "  - cuanto cuesta un bugatti chiron"
   "Comandos utiles:"
   "  - ayuda / menu / opciones"
   "  - reset sintomas"))

;;;; ============================================================
;;;; 7) TEMPLATES: PATRONES DE ENTRADA -> ACCIÓN/RESPUESTA
;;;; ============================================================

(defparameter *templates*
  ;; Cada template tiene 3 partes:
  ;; 1) “stimulus” o patrón (lista de símbolos, o t para fallback)
  ;; 2) respuesta (lista de strings o banderas tipo flagX)
  ;; 3) índices (posiciones) para extraer tokens de la entrada
  ;;
  ;; Ejemplo:
  ;; (mi nombre es (s)) -> flagSetPaciente con índice 3
  ;; para tomar el token 3 como nombre.
  (list
   ;; ... (muchos templates)
   ;; Al final hay un fallback:
   ;; (list t (list "Entiendo. Cuentame mas sobre eso.") nil)
   ))

;;;; ============================================================
;;;; 8) MATCH ENGINE: COMPARAR LA ENTRADA CON LOS TEMPLATES
;;;; ============================================================

(defun element-match-p (templ-el token)
  ;; Compara un elemento del template contra un token de entrada.
  ;; - Si templ-el es (s) se considera “comodín” (acepta cualquier token).
  ;; - Si templ-el es símbolo normal, debe igualar el token (case-insensitive).
  (cond
    ((null templ-el) t)
    ((and (consp templ-el)
          (symbolp (first templ-el))
          (string= (symbol-name (first templ-el)) "S"))
     (not (null token)))
    ((symbolp templ-el)
     (and token (string-equal (symbol-name templ-el) token)))
    (t nil)))

(defun match-template (stim input)
  ;; Revisa si “stim” (patrón) coincide con “input” (lista de tokens string).
  ;; Si stim es T, coincide con todo (fallback).
  (cond
    ((eq stim t) t)
    ((null stim) (null input))
    (t
     (labels ((rec (slist ilist)
                (cond
                  ((and (null slist) (null ilist)) t)
                  ((or (null slist) (null ilist)) nil)
                  (t
                   (let ((se (first slist))
                         (it (first ilist)))
                     (if (element-match-p se it)
                         (rec (rest slist) (rest ilist))
                         nil))))))
       (rec stim input)))))

(defun find-matching-template (input)
  ;; Busca el primer template que haga match con la entrada del usuario.
  (find-if (lambda (tpl) (match-template (first tpl) input))
           *templates*))

;;;; ============================================================
;;;; 9) HANDLERS: EJECUTAR ACCIONES SEGÚN LA BANDERA (FLAG)
;;;; ============================================================

(defun handle-medico-flag (flag indices input)
  ;; Maneja todas las banderas relacionadas a “médico”:
  ;; - registrar paciente
  ;; - guardar síntomas
  ;; - consultas de síntomas/medicinas/tratamientos/causas/lugar
  ;; - actividades 2–12: exclusivo, probabilidad, preventivo, similares,
  ;;   contradictorios, árbol, riesgo, tratamiento combinado, recomendación,
  ;;   diagnosticar y tratar, reporte completo, reset síntomas, ayuda.
  ;;
  ;; indices indica posiciones (en la entrada) que se deben leer.
  ;; token1 y token2 se convierten a símbolos para trabajar con la base de datos.
  (let* ((token1 (when (and indices (>= (length indices) 1))
                   (string-to-symbol (get-token-at input (first indices)))))
         (token2 (when (and indices (>= (length indices) 2))
                   (string-to-symbol (get-token-at input (second indices))))))
    (case flag
      ;; ... (case grande con todas las acciones)
      (t (list "No entiendo la pregunta.")))))

(defun handle-familia-flag (flag indices input)
  ;; Maneja banderas de familia:
  ;; - hijo/padre/madre/hermano
  ;; - quién es el padre/madre
  ;; - hermanos de X
  ;; - abuelo
  (let* ((persona1 (when (>= (length indices) 1)
                     (string-to-symbol (get-token-at input (first indices)))))
         (persona2 (when (>= (length indices) 2)
                     (string-to-symbol (get-token-at input (second indices))))))
    (case flag
      ;; ... (respuestas usando *familia* y funciones padrede/madrede/hermanos/abuelo)
      (t (list "No entiendo la relacion familiar.")))))

(defun handle-carro-flag (flag indices input)
  ;; Maneja banderas de carros.
  ;; Construye el símbolo del carro uniendo marca-modelo: "ford" + "-" + "mustang" => FORD-MUSTANG
  (let* ((marca (get-token-at input (first indices)))
         (modelo (get-token-at input (second indices)))
         (carro (string-to-symbol (concatenate 'string marca "-" modelo))))
    (case flag
      ;; ... (respuestas usando get-carro-info)
      (t (list "No entiendo la pregunta sobre el carro.")))))

(defun build-response-from-resp (resp indices input)
  ;; Decide a qué “handler” mandar la respuesta:
  ;; - Si resp es una lista que comienza con una bandera médica -> handle-medico-flag
  ;; - Si es bandera de familia -> handle-familia-flag
  ;; - Si es bandera de carros -> handle-carro-flag
  ;; - Si no es bandera: construye la salida mezclando texto + tokens del usuario
  (cond
    ;; ... (detección de banderas)
    (t
     (mapcan (lambda (e)
               (cond
                 ;; Si e es entero, se usa como índice para insertar token del input
                 ((integerp e)
                  (let ((idx (and indices (nth e indices))))
                    (list (get-token-at input (or idx -1)))))
                 ;; Símbolo/Texto/otros se convierten a string para imprimir
                 ((symbolp e) (list (symbol-name e)))
                 ((stringp e) (list e))
                 (t (list (princ-to-string e)))))
             resp))))

(defun respond-to (input)
  ;; Entrada: lista de tokens (strings).
  ;; 1) Busca template que coincida
  ;; 2) Genera salida según resp + indices
  ;; 3) Imprime la salida
  (let ((tpl (find-matching-template input)))
    (when tpl
      (let* ((resp (second tpl))
             (indices (third tpl))
             (out (build-response-from-resp resp indices input)))
        (format t "~{~a~^ ~}~%" out)))))

;;;; ============================================================
;;;; 10) LOOP PRINCIPAL: INTERFAZ EN CONSOLA
;;;; ============================================================

(defun eliza-loop ()
  ;; Imprime encabezado y entra a un loop infinito leyendo input del usuario.
  ;; - "adios" o "bye": termina
  ;; - Si no: tokeniza, busca template, responde.
  (format t "╔══════════════════════════════════════════════════════════╗~%")
  (format t "║   ELIZA - CLISP (Familia/Carros/Medico)                  ║~%")
  (format t "║   Escribe 'adios' o 'bye' para salir                     ║~%")
  (format t "╚══════════════════════════════════════════════════════════╝~%~%")
  (loop
    (format t "~%Tu> ")
    (force-output)
    (let ((line (read-line *query-io* nil nil)))
      (when (null line) (return))
      (let ((tokens (clean-and-tokenize line)))
        (cond
          ;; Condición de salida
          ((or (and tokens (string= (first tokens) "adios"))
               (and tokens (string= (first tokens) "bye")))
           (format t "~%Eliza> Adios! Fue un placer. Cuidate!~%")
           (return))
          ;; Respuesta normal
          (t
           (format t "Eliza> ")
           (respond-to tokens)))))))
```
