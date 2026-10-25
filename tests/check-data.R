library(jsonlite)
x <- fromJSON("app/data/cases.json", simplifyVector = FALSE)
stopifnot(length(x$cases) == 20L)
stopifnot(sum(vapply(x$cases, function(z) length(z$rows), integer(1))) == 7729L)
stopifnot(length(unique(vapply(x$cases, `[[`, character(1), "id"))) == 20L)
for (item in x$cases) {
  keys <- vapply(item$columns, `[[`, character(1), "key")
  stopifnot(length(item$rows) == item$n, !anyDuplicated(keys))
  stopifnot(!any(tolower(keys) %in% c("id", "rut", "email", "nombre", "correo")))
  for (row in item$rows) stopifnot(setequal(names(row), keys))
  for (column in item$columns) {
    if (column$type == "numeric") {
      values <- lapply(item$rows, `[[`, column$key)
      stopifnot(all(vapply(values, function(z) is.null(z) || (is.numeric(z) && is.finite(z)), logical(1))))
    }
  }
}
invisible(parse("app/app.R"))
cat("20 casos y 7.729 registros verificados; sintaxis R correcta.\n")
