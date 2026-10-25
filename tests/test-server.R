#!/usr/bin/env Rscript
# Reproducible tests of the actual Shiny server and its download handlers.
# Run from the repository root with: Rscript tests/test-server.R
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(jsonlite))

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]))
  setwd(dirname(dirname(script_path)))
}

snapshot <- fromJSON("app/data/cases.json", simplifyVector = FALSE)
source_cases <- setNames(snapshot$cases, vapply(snapshot$cases, `[[`, character(1), "id"))
stopifnot(length(source_cases) == 20L,
          sum(vapply(source_cases, function(item) length(item$rows), integer(1))) == 7729L)

# Build an independent reference directly from the reviewed JSON. This does not
# call any application filtering, summary, formatting, or export helper.
source_frame <- function(item) {
  columns <- lapply(item$columns, function(column) {
    if (column$type == "numeric") {
      vapply(item$rows, function(row) {
        value <- row[[column$key]]
        if (is.null(value)) NA_real_ else as.numeric(value)
      }, numeric(1))
    } else {
      vapply(item$rows, function(row) {
        value <- row[[column$key]]
        if (is.null(value)) NA_character_ else as.character(value)
      }, character(1))
    }
  })
  names(columns) <- vapply(item$columns, `[[`, character(1), "key")
  as.data.frame(columns, check.names = FALSE, stringsAsFactors = FALSE)
}

same_rows <- function(actual, expected) {
  stopifnot(is.data.frame(actual), is.data.frame(expected),
            identical(names(actual), names(expected)), nrow(actual) == nrow(expected))
  for (key in names(expected)) {
    if (is.numeric(expected[[key]])) {
      stopifnot(isTRUE(all.equal(as.numeric(actual[[key]]), expected[[key]],
                                tolerance = 1e-10, check.attributes = FALSE)))
    } else {
      stopifnot(identical(as.character(actual[[key]]), as.character(expected[[key]])))
    }
  }
  invisible(TRUE)
}

read_download_csv <- function(path) {
  stopifnot(file.exists(path), file.info(path)$size > 0)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM", na.strings = "")
}

app_env <- new.env(parent = globalenv())
app <- source("app/app.R", local = app_env, chdir = TRUE)$value
stopifnot(inherits(app, "shiny.appobj"))

# The remaining assertions exercise the application reactives and registered
# download handlers. The server author keeps app.R independent of this file.
testServer(app, {
  # Every reviewed case must survive JSON-to-R conversion without losing rows,
  # changing continuous measures, or turning source category codes into numbers.
  for (item in source_cases) {
    session$setInputs(caso = item$id, filter1 = "none", filter2 = "none",
                     value1 = "__all__", value2 = "__all__", tab = "distribution",
                     variable = item$columns[[1]]$key, distribution_chart = "bar", bins = 10)
    same_rows(base_data(), source_frame(item))
    same_rows(filtered_data(), source_frame(item))
    stopifnot(inherits(plot_model()$plot, "ggplot"),
              is.data.frame(table_model()$data),
              nrow(ggplot2::ggplot_build(plot_model()$plot)$data[[1]]) > 0L)
    for (column in item$columns) {
      if (column$type == "numeric") stopifnot(is.numeric(base_data()[[column$key]]))
      if (isTRUE(column$ordered)) {
        observed <- unique(as.character(base_data()[[column$key]]))
        expected_order <- as.character(unlist(column$values))
        stopifnot(identical(app_env$category_levels(base_data()[[column$key]], column),
                            c(expected_order[expected_order %in% observed],
                              setdiff(observed, expected_order))))
      }
    }
  }

  # Two independent filters, intersected rather than added. Summary denominators
  # and exports must use this same cohort.
  item <- source_cases[["caso-01"]]
  reference <- source_frame(item)
  expected <- reference[reference$sexo == "HOMBRE" & reference$enferedad_cronica == "SI", , drop = FALSE]
  session$setInputs(caso = item$id, filter1 = "sexo", value1 = "HOMBRE",
                   filter2 = "enferedad_cronica", value2 = "SI",
                   tab = "distribution", variable = "edad_anos", bins = 10)
  same_rows(filtered_data(), expected)
  stopifnot(nrow(expected) > 0L, nrow(expected) < nrow(reference))
  numeric_table <- table_model()$data
  age_row <- numeric_table[numeric_table$Variable == "Edad (años)", , drop = FALSE]
  ages <- expected$edad_anos
  stopifnot(nrow(age_row) == 1L, age_row$`N válido` == length(ages), age_row$`Sin dato` == 0)
  actual <- unname(as.numeric(age_row[1, c("Media", "DE", "Mediana", "Q1", "Q3", "Mínimo", "Máximo")]))
  wanted <- c(mean(ages), sd(ages), median(ages), unname(quantile(ages, c(.25, .75), type = 7)), min(ages), max(ages))
  stopifnot(isTRUE(all.equal(actual, wanted, tolerance = 1e-10)))
  histogram <- ggplot2::ggplot_build(plot_model()$plot)$data[[1]]
  stopifnot(sum(histogram$count) == nrow(expected))
  same_rows(read_download_csv(output$download_base), expected)
  same_rows(read_download_csv(output$download_table), numeric_table)

  # Frequencies retain the documented ordinal scale. Percentages use the total
  # filtered cohort and sum to 100, while the chart counts match the table.
  session$setInputs(variable = "interes_en_control_likert", distribution_chart = "bar")
  frequencies <- table_model()$data
  ordered_column <- Filter(function(column) column$key == "interes_en_control_likert", item$columns)[[1]]
  desired_order <- as.character(unlist(ordered_column$values))
  observed <- expected$interes_en_control_likert
  desired_order <- desired_order[desired_order %in% observed]
  reference_counts <- table(factor(observed, levels = desired_order))
  stopifnot(identical(frequencies$Categoría, names(reference_counts)),
            identical(frequencies$n, as.integer(reference_counts)),
            isTRUE(all.equal(sum(frequencies$`Porcentaje del total`), 100)),
            isTRUE(all.equal(frequencies$`Porcentaje del total`, 100 * as.numeric(reference_counts) / nrow(expected))))
  stopifnot(sum(ggplot2::ggplot_build(plot_model()$plot)$data[[1]]$y) == nrow(expected))
  session$setInputs(distribution_chart = "pie")
  stopifnot(inherits(plot_model()$plot$coordinates, "CoordPolar"))

  # Both figure download handlers execute against the very same current ggplot
  # reactive. Validate actual files, vector headers, title, and stable plot data.
  plot_before <- ggplot2::ggplot_build(plot_model()$plot)$data
  svg_path <- output$download_svg
  svg <- paste(readLines(svg_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  stopifnot(file.info(svg_path)$size > 1000, grepl("<svg", svg, fixed = TRUE),
            grepl("Datos simulados", svg, fixed = TRUE),
            !grepl("<image", svg, fixed = TRUE))
  pdf_path <- output$download_pdf
  con <- file(pdf_path, "rb")
  pdf_magic <- rawToChar(readBin(con, "raw", n = 5))
  close(con)
  stopifnot(identical(pdf_magic, "%PDF-"), file.info(pdf_path)$size > 1000,
            identical(plot_before, ggplot2::ggplot_build(plot_model()$plot)$data))

  # Group summaries must agree with independent base-R calculations for every
  # group. Both boxplot and mean chart are exercised on the same filtered data.
  session$setInputs(caso = "caso-04", filter1 = "none", filter2 = "none",
                   value1 = "__all__", value2 = "__all__", tab = "relations",
                   relation_type = "group", measure = "tiempo_de_actividades_extraprogramaticas_hrs_mensuales",
                   group = "sintomas_psicologicos_emocionales", group_chart = "box")
  reference <- source_frame(source_cases[["caso-04"]])
  grouped <- table_model()$data
  for (i in seq_len(nrow(grouped))) {
    values <- reference$tiempo_de_actividades_extraprogramaticas_hrs_mensuales[
      reference$sintomas_psicologicos_emocionales == grouped$Grupo[[i]]]
    stopifnot(grouped$`N válido`[[i]] == length(values),
              isTRUE(all.equal(grouped$Media[[i]], mean(values))),
              isTRUE(all.equal(grouped$DE[[i]], sd(values))))
  }
  stopifnot(inherits(plot_model()$plot$layers[[1]]$geom, "GeomBoxplot"))
  session$setInputs(group_chart = "mean")
  stopifnot(inherits(plot_model()$plot$layers[[1]]$geom, "GeomCol"))

  # Pearson and scatter preserve pair counts and row filters. Two categorical
  # summaries below independently validate each count and row percentage.
  session$setInputs(caso = "caso-09", filter1 = "priorizado", value1 = "Priorizado",
                   filter2 = "none", value2 = "__all__", tab = "relations",
                   relation_type = "scatter", x = "dias_espera", y = "distancia_km", color = "none")
  reference <- source_frame(source_cases[["caso-09"]])
  expected <- reference[reference$priorizado == "Priorizado", , drop = FALSE]
  same_rows(filtered_data(), expected)
  correlation <- table_model()$data
  stopifnot(correlation$`Pares completos` == nrow(expected),
            correlation$`Filas excluidas` == 0,
            isTRUE(all.equal(correlation$`r de Pearson`, cor(expected$dias_espera, expected$distancia_km))),
            nrow(ggplot2::ggplot_build(plot_model()$plot)$data[[1]]) == nrow(expected))
  same_rows(read_download_csv(output$download_base), expected)

  session$setInputs(relation_type = "cross", cross_x = "sexo_biologico", cross_y = "gravedad_sintoma", cross_chart = "stacked")
  crossed <- table_model()$data
  reference_cross <- table(expected$sexo_biologico, expected$gravedad_sintoma)
  stopifnot(sum(crossed$n) == nrow(expected), attr(crossed, "n_complete") == nrow(expected))
  for (i in seq_len(nrow(crossed))) {
    a <- as.character(crossed$Fila[[i]]); b <- as.character(crossed$Columna[[i]])
    stopifnot(crossed$n[[i]] == reference_cross[a, b],
              isTRUE(all.equal(crossed$`Porcentaje de la fila`[[i]], 100 * reference_cross[a, b] / sum(reference_cross[a, ]))))
  }
  same_rows(read_download_csv(output$download_table), crossed)
  session$setInputs(cross_chart = "heatmap")
  stopifnot(inherits(plot_model()$plot$layers[[1]]$geom, "GeomTile"))

  # The raw-data tab and dictionary do not mutate the server's cohort or measures.
  session$setInputs(tab = "data", data_table = "base")
  displayed_base <- table_model()$data
  stopifnot(nrow(displayed_base) == nrow(expected),
            identical(names(displayed_base), vapply(current_case()$columns, `[[`, character(1), "label")))
  same_rows(read_download_csv(output$download_table), displayed_base)
  session$setInputs(data_table = "dictionary")
  stopifnot(nrow(table_model()$data) == length(current_case()$columns))
  same_rows(read_download_csv(output$download_base), expected)

  # Empty intersections yield empty downloads rather than silently reverting to
  # the full base. The figure gives a meaningful validation result.
  session$setInputs(caso = "caso-01", filter1 = "sexo", value1 = "HOMBRE",
                   filter2 = "sexo", value2 = "MUJER", tab = "distribution", variable = "edad_anos")
  stopifnot(nrow(filtered_data()) == 0L, nrow(read_download_csv(output$download_base)) == 0L)
  plot_error <- tryCatch({ plot_model(); NULL }, error = identity)
  stopifnot(inherits(plot_error, "shiny.silent.error"))
})

# Edge cases not present in the complete simulated dataset. These protect the
# declared handling of absent values, sample SD, and undefined correlation.
summary <- app_env$describe_numeric(c(2, 4, NA_real_))
stopifnot(summary$`N válido` == 2, summary$`Sin dato` == 1,
          summary$Media == 3, isTRUE(all.equal(summary$DE, sqrt(2))),
          summary$Q1 == 2.5, summary$Q3 == 3.5,
          is.na(app_env$describe_numeric(5)$DE))
constant_pairs <- data.frame(x = c(1, 1, 1, NA), y = c(2, 3, 4, 5))
correlation <- app_env$pearson_summary(constant_pairs, list(key = "x"), list(key = "y"))
stopifnot(correlation$`Pares completos` == 3, correlation$`Filas excluidas` == 1,
          is.na(correlation$`r de Pearson`))
cat("PASS: 20 cases, 7729 rows, filters, independent summaries, ordinal scales, all chart families, CSV cohorts, SVG/PDF downloads, missing-value boundaries.\n")
