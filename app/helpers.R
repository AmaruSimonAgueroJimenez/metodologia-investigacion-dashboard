# Pure transformations shared by screen tables, figures and downloads.
missing_value <- function(x) is.null(x) || !length(x) || is.na(x[[1]]) ||
  (is.character(x[[1]]) && !nzchar(trimws(x[[1]])))

value_label <- function(value, column) {
  if (missing_value(value)) return(NA_character_)
  key <- trimws(as.character(value[[1]]))
  mapped <- column$valueLabels[[key]]
  if (is.null(mapped)) key else as.character(mapped)
}

case_data_frame <- function(case) {
  result <- lapply(case$columns, function(column) {
    values <- lapply(case$rows, function(row) row[[column$key]])
    if (identical(column$type, "numeric")) {
      vapply(values, function(x) {
        if (missing_value(x)) return(NA_real_)
        n <- suppressWarnings(as.numeric(x[[1]]))
        if (length(n) == 1L && is.finite(n)) n else NA_real_
      }, numeric(1))
    } else vapply(values, value_label, character(1), column = column)
  })
  names(result) <- vapply(case$columns, `[[`, character(1), "key")
  as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
}

column_choices <- function(columns) {
  setNames(vapply(columns, `[[`, character(1), "key"),
           vapply(columns, `[[`, character(1), "label"))
}

column_for <- function(case, key, type = NULL, exclude = NULL) {
  columns <- Filter(function(c) (is.null(type) || identical(c$type, type)) &&
    !(c$key %in% exclude), case$columns)
  if (!length(columns)) return(NULL)
  found <- Filter(function(c) identical(c$key, key), columns)
  if (length(found)) found[[1]] else columns[[1]]
}

category_levels <- function(x, column) {
  observed <- unique(as.character(x[!is.na(x)]))
  if (isTRUE(column$ordered) && length(column$values)) {
    ordered <- unique(vapply(column$values, value_label, character(1), column = column))
    observed <- c(ordered[ordered %in% observed], setdiff(observed, ordered))
  }
  c(setdiff(observed, "Sin dato"), if (anyNA(x) || "Sin dato" %in% observed) "Sin dato")
}

describe_numeric <- function(x) {
  valid <- x[is.finite(x)]
  n <- length(valid)
  data.frame(`N válido` = n, `Sin dato` = length(x) - n,
    Media = if (n) mean(valid) else NA_real_,
    DE = if (n > 1L) sd(valid) else NA_real_,
    Mediana = if (n) median(valid) else NA_real_,
    Q1 = if (n) unname(quantile(valid, .25, type = 7)) else NA_real_,
    Q3 = if (n) unname(quantile(valid, .75, type = 7)) else NA_real_,
    Mínimo = if (n) min(valid) else NA_real_,
    Máximo = if (n) max(valid) else NA_real_, check.names = FALSE)
}

numeric_summary <- function(data, columns) {
  if (!length(columns)) return(data.frame())
  do.call(rbind, lapply(columns, function(column)
    cbind(Variable = column$label, describe_numeric(data[[column$key]]))))
}

frequency_table <- function(data, column) {
  x <- data[[column$key]]
  levels <- category_levels(x, column)
  x[is.na(x)] <- "Sin dato"
  counts <- table(factor(x, levels = levels))
  data.frame(Categoría = names(counts), n = as.integer(counts),
    `Porcentaje del total` = if (length(x)) 100 * as.integer(counts) / length(x) else numeric(0), check.names = FALSE)
}

group_summary <- function(data, measure, group) {
  x <- data[[group$key]]
  levels <- category_levels(x, group)
  x[is.na(x)] <- "Sin dato"
  if (!length(levels)) return(data.frame())
  do.call(rbind, lapply(levels, function(level)
    cbind(Grupo = level, describe_numeric(data[[measure$key]][x == level]))))
}

complete_pairs <- function(data, x, y) {
  data <- data[is.finite(data[[x$key]]) & is.finite(data[[y$key]]), , drop = FALSE]
  data.frame(x = data[[x$key]], y = data[[y$key]], data, check.names = FALSE)
}

pearson_summary <- function(data, x, y) {
  pairs <- complete_pairs(data, x, y)
  r <- if (nrow(pairs) >= 3L && sd(pairs$x) > 0 && sd(pairs$y) > 0)
    cor(pairs$x, pairs$y, method = "pearson") else NA_real_
  data.frame(`Pares completos` = nrow(pairs),
    `Filas excluidas` = nrow(data) - nrow(pairs), `r de Pearson` = r, check.names = FALSE)
}

cross_table <- function(data, x, y) {
  complete <- !is.na(data[[x$key]]) & !is.na(data[[y$key]])
  valid <- data[complete, , drop = FALSE]
  a <- factor(valid[[x$key]], levels = category_levels(valid[[x$key]], x))
  b <- factor(valid[[y$key]], levels = category_levels(valid[[y$key]], y))
  counts <- table(a, b)
  out <- as.data.frame(counts, stringsAsFactors = FALSE)
  names(out) <- c("Fila", "Columna", "n")
  out$`Porcentaje de la fila` <- if (nrow(out))
    100 * out$n / as.numeric(rowSums(counts)[as.character(out$Fila)]) else numeric(0)
  out$`Porcentaje del total válido` <- if (nrow(valid)) 100 * out$n / nrow(valid) else numeric(nrow(out))
  attr(out, "n_complete") <- nrow(valid)
  attr(out, "n_missing") <- nrow(data) - nrow(valid)
  out
}

dictionary_table <- function(case) {
  do.call(rbind, lapply(case$columns, function(column) {
    codes <- column$valueLabels
    coding <- if (length(codes)) paste(paste(names(codes), unlist(codes), sep = " = "), collapse = "; ")
      else if (identical(column$type, "categorical")) paste(unlist(column$values), collapse = "; ") else "Valor numérico"
    definition <- column$description
    if (is.null(definition) || !nzchar(definition)) definition <- case$dictionary[[column$key]]
    if (is.null(definition) || !nzchar(definition)) definition <- "Sin definición adicional en el archivo docente."
    data.frame(Variable = column$label,
      Tipo = if (identical(column$type, "numeric")) "Numérica" else if (isTRUE(column$ordered)) "Categórica ordinal" else "Categórica",
      Definición = definition, `Categorías y códigos de origen` = coding, check.names = FALSE)
  }))
}

safe_csv <- function(data, file) {
  for (key in names(data)) if (is.character(data[[key]]) || is.factor(data[[key]])) {
    x <- as.character(data[[key]])
    dangerous <- !is.na(x) & (grepl("^[[:space:]]*[=+@-]", x) | grepl("^[\t\r]", x))
    x[dangerous] <- paste0("'", x[dangerous])
    data[[key]] <- x
  }
  write.csv(data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

plot_palette <- c("#087f7a", "#d98039", "#4da5a0", "#e4a46f", "#82bab6", "#bd662c", "#325f5b", "#edbd94")
color_palettes <- list(
  teal = plot_palette,
  `blue-red` = c("#2563eb", "#dc4848", "#60a5fa", "#f38c7a", "#93c5fd", "#af233c", "#1e3a8a", "#f4b7ad"),
  viridis = c("#440154", "#46327e", "#365c8d", "#277f8e", "#1fa187", "#4ac16d", "#a0da39", "#fde725"),
  pastel = c("#75b6b2", "#e8ae8f", "#b9a6d6", "#a7c8e8", "#bdcf9e", "#e2b9d1", "#b5bcc6", "#e4d3a2"),
  gray = c("#4b5563", "#6b7280", "#9ca3af", "#d1d5db", "#374151", "#7b8390", "#b5bbc3", "#e5e7eb")
)
lighten_color <- function(color, amount = .8) {
  rgb <- as.numeric(grDevices::col2rgb(color)) / 255
  mixed <- rgb * (1 - amount) + amount
  grDevices::rgb(mixed[1], mixed[2], mixed[3])
}
wrap_label <- function(x) vapply(x, function(value) paste(strwrap(value, width = 20), collapse = "\n"), character(1))
figure_number <- function(x, digits = 2) {
  vapply(x, function(value) if (is.na(value) || !is.finite(value)) "NA" else
    formatC(value, format = "f", digits = digits, decimal.mark = ",", big.mark = "."), character(1))
}
contrast_color <- function(color) {
  vapply(color, function(value) {
    rgb <- as.numeric(grDevices::col2rgb(value))
    if (sum(rgb * c(.299, .587, .114)) < 150) "white" else "#173c3d"
  }, character(1), USE.NAMES = FALSE)
}
inside_summary <- function(plot, text, side = "right") {
  plot + annotate("label", x = if (side == "left") -Inf else Inf, y = Inf,
    label = text, hjust = if (side == "left") -.02 else 1.02, vjust = 1.04,
    size = 3, color = "#19363b", fill = "white", alpha = .95, linewidth = .2)
}
plot_theme <- function() theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "#53656a", size = 10),
    plot.caption = element_text(color = "#53656a", hjust = 0),
    panel.grid.minor = element_blank(), legend.position = "bottom",
    legend.text = element_text(size = 9, lineheight = .95),
    legend.key.width = grid::unit(.8, "lines"), legend.key.height = grid::unit(.9, "lines"),
    legend.spacing.x = grid::unit(.3, "lines"), legend.margin = margin(4, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_blank(), plot.margin = margin(14, 18, 12, 12))
