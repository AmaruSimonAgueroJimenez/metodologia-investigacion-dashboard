# Dashboard de Metodología de la Investigación · Shiny R

Aplicación **Shiny para R** con las 20 bases existentes de casos del curso DSPU0011: casos 01–19, incluyendo las variantes 06.1 y 06.2, con **7.729 registros** y sus diccionarios.

**[Abrir el dashboard](https://AmaruSimonAgueroJimenez.github.io/metodologia-investigacion-dashboard/)**

GitHub Pages ejecuta la aplicación R en el navegador mediante [Shinylive](https://posit-dev.github.io/r-shinylive/). La primera carga descarga el entorno R y sus paquetes y puede tardar más que las visitas siguientes.

## Explorar y descargar

- Seleccionar un caso y filtrar sus registros.
- Generar tablas de frecuencia, descriptivos y tablas cruzadas.
- Comparar grupos y medidas mediante distintos tipos de gráficos.
- Elegir entre cinco paletas, ocho colores principales y un color HEX personalizado. Las descargas conservan esos colores.
- Descargar las figuras en **SVG y PDF**, las tablas y la base filtrada en **CSV**.
- Consultar el diccionario de variables y las limitaciones del caso.

Cada descarga usa la misma población filtrada y las mismas variables de la vista. SVG es un formato vectorial editable; PDF permite incorporar la figura directamente en un trabajo.

## Datos y límites

Las bases son completamente ficticias o simuladas para docencia, según confirmación del responsable del curso el 25 de octubre de 2026. Se utilizaron los datos existentes de la versión para estudiantes; no se generaron observaciones nuevas. Se excluyeron identificadores de registro. No se publican nombres, correos, calificaciones, asistencias, entregas ni soluciones docentes.

Los códigos categóricos se tradujeron con los diccionarios originales. Se retiraron notas al pie, diccionarios laterales y filas vacías. Se conservan las medidas y categorías de las fuentes, sin imputación. Cada caso registra archivo, hoja y hash SHA-256 de procedencia.

Los escenarios son independientes y no representan estimaciones poblacionales ni estadísticas oficiales. La DE es muestral y los cuartiles usan el tipo 7 de R. El caso 14 tiene 349 registros de datos. El caso 16 no contiene el desenlace parto prematuro. La coherencia entre IMC y clasificación de obesidad del caso 13 requiere revisión docente.

## Ejecutar localmente en R o RStudio

```r
install.packages(c("shiny", "ggplot2", "DT", "jsonlite", "svglite"))
shiny::runApp("app")
```

Para exportar y servir el sitio estático:

```r
install.packages("shinylive")
shinylive::export("app", "site")
httpuv::runStaticServer("site")
```

## Actualizar GitHub Pages

El flujo `.github/workflows/pages.yml` verifica los datos, exporta con Shinylive y publica cada cambio de `main`. En Settings → Pages, la fuente debe ser **GitHub Actions**.

`app/app.R` contiene la aplicación y `app/data/cases.json` los datos simulados revisados. Los archivos de soporte de extracción están en `tools/`; las fuentes originales no se modifican.
