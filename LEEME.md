Una copia de este fichero en inglés está disponible en el fichero
`README.md`.

Dependencias
============

Todas las dependencias son gestionadas por `stack`. Para instalar
`stack` en sistemas operativos Unix puede ejecutarse
`make install-deps`. En Windows puede descargarse un binario de
<https://docs.haskellstack.org/en/stable/README/>.

La primera vez que se intenten generar los binarios del proyecto se
instalarán todas las dependencias por `stack`, incluyendo el compilador
de Haskell GHC. Esto puede llevar un tiempo.

Compilación
===========

**Nota importante:** El proceso de compilación sólo ha sido testeado en
distribuciones Linux. Para ejecutar el programa en Windows, el fichero
`stack.yaml` DEBE ser editado para que funcione el preprocesador. En
particular, `quipper/convert_template.sh` debe ser reemplazado por
`quipper/convert_template.bat`.

------------------------------------------------------------------------

`make build` compilará los binarios asociados y los copiará a la carpeta
`bin`.

La primera vez que se intenten generar los binarios del proyecto se
instalarán todas las dependencias por `stack`, incluyendo el compilador
de Haskell GHC. Esto puede llevar un tiempo.

Los binarios creados en mi ordenador (que utiliza Linux Mint 18.1) se
adjuntan en la carpeta `bin`.

Ejecución
=========

El proceso anterior genera dos binarios. Ambos binarios incluyen
instrucciones de ayuda ejecutándolos con la opción `--help`.

El primero, `quantum`, implementa una interfaz por linea de comandos
para la ejecución de varios algoritmos cuánticos. Cada algoritmo tiene
un subcomando asociado.

Algunos algoritmos toman un oráculo como entrada, definido por su tabla
de verdad. Esta puede ser pasada como un argumento, o, si el subcomando
es llamado sin este argumento, se leerá el oráculo de la entrada
estándar.

La sintaxis para los oráculos es una versión simplificada de la de los
ficheros CSV. Cada linea debe tener un valor de entrada, representado
por una cadena de caracteres de `0` y `1`, y, separado por una coma, un
valor de salida, representado por un único bit.

Por ejemplo, la operación lógica NOT puede ser expresada como

    0,1
    1,0

Algunos ejemplos de oráculos se incluyen en la carpeta `oracles`.

El segundo binario, `diagrams`, genera los diagramas usados en el documento.
La opción `--gen-all` genera una selección de los diagramas utilizados,
mientras que si se indica el nombre de un circuito en forma de una tabla de verdad
con la opción `--circuit`, se genera un diagrama para 
la versión reversible de ese circuito.

Documentación
=============

La documentación, generada con Haddock, está disponible en la carpeta
`docs`. Puede ser regenerada usando el comando `make docs`.

Nótese que el proceso de generación de la documentación requiere del
preprocesado manual de los ficheros de código y por tanto no puede
hacerse directamente con `stack`.

Si quiere generarse la documentación en Windows DEBE cambiarse en el `Makefile`
el script de preprocesado utilizado como en el caso de la compilación.

Pruebas
=======

El código incluye un conjunto de pruebas, tanto unitarias como basadas
en propiedades. Puede ser ejecutado utilizando el comando `make tests`.

Nótese que debido a la naturaleza de los algoritmos implementados, es
posible (aunque improbable) que algunos de estos tests fallen (esto está
indicado en la salida del programa).

Organización del código
=======================

El código está organizado como sigue

1.  la carpeta `oracles` incluye ejemplos de oráculos escritos en la
    sintaxis que aceptan varios algoritmos.
2.  la carpeta `quipper` incluye scripts de preprocesado tomados del
    proyecto Quipper para posibilitar la compilación.
3.  la carpeta `src` contiene el código. Tiene varias subcarpetas

    -   la subcarpeta `apps` incluye una carpeta por binario con su
        programa principal,
    -   la subcarpeta `lib` incluye ficheros comunes a ambos binaros,
        definienodo los algoritmos (en la carpeta `Algorithms`) y varios
        ficheros auxiliares.
    -   la subcarpeta `test` incluye los ficheros de pruebas

4.  la carpeta `docs` incluye la documentación generada.
5.  los ficheros `package.yaml` y `stack.yaml` incluyen las dependencias
    e instrucciones de compilación para `stack`.
