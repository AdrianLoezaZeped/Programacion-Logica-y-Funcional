# Reglas del CamelCase (lowerCamelCase):

1. **Primera palabra en minúscula**
2. **Cada palabra siguiente comienza con mayúscula**
3. **Sin espacios entre palabras**
4. **Sin guiones ni underscores**
5. **Todas las palabras se unen en una sola**

**Ejemplos correctos:**
- `nombreCompleto`
- `edadUsuario` 
- `calcularTotalFactura`
- `urlDeImagen`
- `miVariable`

**Ejemplos incorrectos:**
- `NombreCompleto` (empieza con mayúscula → PascalCase)
- `nombre_completo` (usa underscores)
- `nombre-completo` (usa guiones)
- `nombre completo` (tiene espacio)
- `nombrecompleto` (todas minúsculas)

**Uso principal:** Variables, atributos, funciones/métodos en programación.

# Reglas del Snake Case (snake_case):

1. **Todas las letras en minúsculas** (o mayúsculas para constantes)
2. **Las palabras se separan con guiones bajos (_)**
3. **No se utilizan espacios**
4. **No se utilizan guiones medios (-)**
5. **No se utilizan mayúsculas dentro de las palabras**

**Ejemplos correctos:**
- `nombre_completo`
- `edad_usuario`
- `calcular_total_factura`
- `url_de_imagen`
- `mi_variable`

**Ejemplos para constantes (SCREAMING_SNAKE_CASE):**
- `MAXIMO_INTENTOS`
- `PI`
- `VELOCIDAD_LUZ`
- `CODIGO_ERROR`

**Ejemplos incorrectos:**
- `nombreCompleto` (usa CamelCase)
- `nombre-completo` (usa guiones medios/kebab-case)
- `nombre completo` (tiene espacios)
- `Nombre_Completo` (usa mayúsculas incorrectamente)
- `_nombre_completo` (guión bajo al inicio, aunque esto a veces se usa para indicar privado)

**Uso principal:** Variables, funciones, nombres de archivos en lenguajes como Python, Ruby, PHP. Muy común en bases de datos para nombres de columnas y tablas.
