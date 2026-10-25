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

plot_text_labels <- function(plot) {
  unlist(lapply(ggplot2::ggplot_build(plot)$data, function(layer) {
    if (is.null(layer$label)) character(0) else as.character(layer$label)
  }), use.names = FALSE)
}

label_number <- function(text, prefix) {
  expression <- paste0(prefix, "[[:space:]]*=[[:space:]]*(-?[[:digit:]]+(?:[.,][[:digit:]]+)?)")
  matches <- regmatches(text, regexec(expression, text, ignore.case = TRUE, perl = TRUE))
  vapply(matches, function(match) if (length(match) < 2L) NA_real_
         else as.numeric(sub(",", ".", match[[2]], fixed = TRUE)), numeric(1))
}

label_percentage <- function(text) {
  matches <- regmatches(text, regexec("([[:digit:]]+(?:[.,][[:digit:]]+)?)[[:space:]]*%", text, perl = TRUE))
  vapply(matches, function(match) if (length(match) < 2L) NA_real_
         else as.numeric(sub(",", ".", match[[2]], fixed = TRUE)), numeric(1))
}

labels_inside_columns <- function(plot) {
  built <- ggplot2::ggplot_build(plot)$data
  columns <- built[[1]]
  text_layers <- Filter(function(layer) !is.null(layer$label), built[-1])
  stopifnot(length(text_layers) > 0L, all(c("x", "ymin", "ymax") %in% names(columns)))
  text <- text_layers[[1]]
  text <- text[!is.na(text$label) & nzchar(as.character(text$label)), , drop = FALSE]
  for (i in seq_len(nrow(text))) {
    # Check rendered geometry rather than reproducing position_stack/dodge.
    midpoints <- (columns$ymin + columns$ymax) / 2
    stopifnot(any(abs(as.numeric(columns$x) - as.numeric(text$x[[i]])) < 1e-7 &
                    abs(midpoints - text$y[[i]]) < 1e-7))
  }
  invisible(TRUE)
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
  palette_first_colors <- c(teal = "#087f7a", `blue-red` = "#2563eb",
                            viridis = "#440154", pastel = "#75b6b2", gray = "#4b5563")
  for (palette_id in names(palette_first_colors)) {
    session$setInputs(palette = palette_id, primary_color = "palette", primary_hex = "")
    stopifnot(identical(tolower(plot_model()$plot$layers[[1]]$aes_params$fill),
                        unname(palette_first_colors[[palette_id]])))
  }
  session$setInputs(primary_color = "#dc4848", primary_hex = "#00Aa66")
  stopifnot(identical(tolower(plot_model()$plot$layers[[1]]$aes_params$fill), "#00aa66"))
  session$setInputs(primary_hex = "invalid")
  stopifnot(identical(tolower(plot_model()$plot$layers[[1]]$aes_params$fill), "#dc4848"),
            grepl("no válido", output$hex_status, fixed = TRUE))
  session$setInputs(palette = "teal", primary_color = "palette", primary_hex = "")
  # The visual picker is a real server input, and its selected color must reach
  # the same plot used by the screen and vector downloads.
  session$setInputs(picked_color = "#a13cb9")
  stopifnot(identical(tolower(plot_model()$plot$layers[[1]]$aes_params$fill), "#a13cb9"))
  session$setInputs(picked_color = "#087f7a")

  numeric_geoms <- c(hist = "GeomBar", density = "GeomDensity", box = "GeomBoxplot",
                     violin = "GeomViolin", ecdf = "GeomStep")
  for (chart in names(numeric_geoms)) {
    session$setInputs(numeric_chart = chart)
    plot <- plot_model()$plot
    stopifnot(inherits(plot$layers[[1]]$geom, numeric_geoms[[chart]]))
    labels <- plot_text_labels(plot)
    summary_labels <- labels[grepl("media", labels, ignore.case = TRUE)]
    stopifnot(length(summary_labels) > 0L,
              any(label_number(summary_labels, "\\bn") == nrow(expected), na.rm = TRUE),
              any(abs(label_number(summary_labels, "media") - mean(ages)) < .011, na.rm = TRUE),
              any(abs(label_number(summary_labels, "mediana") - median(ages)) < .011, na.rm = TRUE),
              !any(grepl("%", labels, fixed = TRUE)))
  }
  session$setInputs(numeric_chart = "hist")
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
  for (chart in c("bar", "vertical", "pie", "donut", "lollipop")) {
    session$setInputs(distribution_chart = chart)
    plot <- plot_model()$plot
    labelled <- plot$data
    stopifnot(is.data.frame(labelled), "Etiqueta" %in% names(labelled),
              identical(as.numeric(label_number(labelled$Etiqueta, "\\bn")), as.numeric(labelled$n)),
              all(abs(label_percentage(labelled$Etiqueta) - 100 * labelled$n / nrow(expected)) <= .051),
              sum(labelled$n) == nrow(expected))
    if (chart %in% c("pie", "donut")) stopifnot(inherits(plot$coordinates, "CoordPolar"))
    if (chart == "lollipop") stopifnot(inherits(plot$layers[[1]]$geom, "GeomSegment"))
    else labels_inside_columns(plot)
    if (chart == "vertical") stopifnot(plot$theme$axis.text.x$angle == 45)
    drawn_labels <- plot_text_labels(plot)
    stopifnot(all(labelled$Etiqueta %in% drawn_labels))
  }

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
  group_geoms <- c(box = "GeomBoxplot", mean = "GeomCol", violin = "GeomViolin",
                   median = "GeomCol", points = "GeomPoint")
  for (chart in names(group_geoms)) {
    session$setInputs(group_chart = chart)
    plot <- plot_model()$plot
    stopifnot(inherits(plot$layers[[1]]$geom, group_geoms[[chart]]))
    labels <- plot_text_labels(plot)
    labelled_n <- label_number(labels, "\\bn")
    labelled_n <- labelled_n[!is.na(labelled_n)]
    stopifnot(identical(sort(labelled_n), sort(as.numeric(grouped$`N válido`))),
              !any(grepl("%", labels, fixed = TRUE)))
  }

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
  session$setInputs(trend = TRUE)
  scatter <- plot_model()$plot
  scatter_labels <- plot_text_labels(scatter)
  stopifnot(any(label_number(scatter_labels, "\\bn") == nrow(expected), na.rm = TRUE),
            any(abs(label_number(scatter_labels, "\\br") - cor(expected$dias_espera, expected$distancia_km)) < .0011, na.rm = TRUE),
            !any(grepl("%", scatter_labels, fixed = TRUE)),
            any(vapply(scatter$layers, function(layer) inherits(layer$geom, "GeomSmooth"), logical(1))))
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
  for (chart in c("stacked", "grouped", "percent", "heatmap")) {
    session$setInputs(cross_chart = chart)
    plot <- plot_model()$plot
    labelled <- plot$data
    visible <- labelled$n > 0 | chart == "heatmap"
    stopifnot("Etiqueta" %in% names(labelled),
              identical(as.numeric(label_number(labelled$Etiqueta[visible], "\\bn")), as.numeric(labelled$n[visible])),
              all(abs(label_percentage(labelled$Etiqueta[visible]) - labelled$`Porcentaje de la fila`[visible]) <= .051),
              all(labelled$Etiqueta[!visible] == ""),
              sum(labelled$n) == nrow(expected), plot$theme$axis.text.x$angle == 45,
              all(labelled$Etiqueta %in% plot_text_labels(plot)))
    if (chart == "percent") {
      columns <- ggplot2::ggplot_build(plot)$data[[1]]
      stopifnot(all(columns$ymax <= 100 + 1e-9))
    }
    if (chart != "heatmap") labels_inside_columns(plot)
  }

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

# Boundary fixtures live only in the R test environment; the reviewed JSON and
# original case files remain untouched. A small cohort must not manufacture a
# density or regression, and a missing value must not become a zero observation.
local({
  saved_case <- app_env$cases[["caso-01"]]
  on.exit({ app_env$cases[["caso-01"]] <- saved_case }, add = TRUE)
  for (cohort_n in c(1L, 3L)) {
    fixture <- saved_case
    fixture$rows <- rep(list(saved_case$rows[[1]]), cohort_n)
    fixture$n <- cohort_n
    app_env$cases[["caso-01"]] <- fixture
    testServer(app, {
      session$setInputs(caso = "caso-01", filter1 = "none", filter2 = "none",
                       value1 = "__all__", value2 = "__all__", tab = "distribution",
                       variable = "edad_anos", numeric_chart = "density")
      stopifnot(nrow(filtered_data()) == cohort_n)
      for (mode in c("density", "violin")) {
        session$setInputs(numeric_chart = mode)
        result <- tryCatch(plot_model(), error = identity)
        stopifnot(inherits(result, "shiny.silent.error"), nzchar(conditionMessage(result)))
        download <- tryCatch(output$download_svg, error = identity)
        stopifnot(inherits(download, "shiny.silent.error"))
      }
      session$setInputs(tab = "relations", relation_type = "scatter",
                       x = "edad_anos", y = "distancia_al_cesfam_km", color = "none", trend = TRUE)
      plot <- plot_model()$plot
      stopifnot(!any(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomSmooth"), logical(1))),
                any(grepl("r = NA", plot_text_labels(plot), fixed = TRUE)),
                nrow(ggplot2::ggplot_build(plot)$data[[1]]) == cohort_n)
      session$setInputs(relation_type = "group", measure = "edad_anos", group = "sexo", group_chart = "violin")
      plot <- plot_model()$plot
      built <- ggplot2::ggplot_build(plot)
      stopifnot(nrow(built$data[[1]]) == 0L,
                any(vapply(plot$layers[-1], function(layer) inherits(layer$geom, "GeomPoint"), logical(1))))
      svg <- paste(readLines(output$download_svg, encoding = "UTF-8", warn = FALSE), collapse = "\n")
      stopifnot(grepl("<svg", svg, fixed = TRUE), !grepl("NaN|Infinity", svg),
                any(label_number(plot_text_labels(plot), "\\bn") == cohort_n, na.rm = TRUE))
    })
  }

  fixture <- saved_case
  fixture$rows <- rep(list(saved_case$rows[[1]]), 4L)
  for (i in seq_len(4L)) {
    fixture$rows[[i]]$sexo <- c("HOMBRE", "HOMBRE", "MUJER", NA_character_)[[i]]
    fixture$rows[[i]]$enferedad_cronica <- c("SI", "NO", NA_character_, "SI")[[i]]
    fixture$rows[[i]]$edad_anos <- c(2, 4, 8, NA_real_)[[i]]
  }
  fixture$n <- 4L
  app_env$cases[["caso-01"]] <- fixture
  testServer(app, {
    session$setInputs(caso = "caso-01", filter1 = "none", filter2 = "none",
                     value1 = "__all__", value2 = "__all__", tab = "distribution",
                     variable = "sexo", distribution_chart = "vertical")
    plotted <- plot_model()$plot$data
    stopifnot(identical(as.integer(label_number(plotted$Etiqueta, "\\bn")), c(2L, 1L, 1L)),
              identical(label_percentage(plotted$Etiqueta), c(50, 25, 25)))
    session$setInputs(variable = "edad_anos", numeric_chart = "hist")
    texts <- plot_text_labels(plot_model()$plot)
    summary_text <- texts[grepl("media", texts, ignore.case = TRUE)]
    stopifnot(any(label_number(summary_text, "\\bn") == 3L, na.rm = TRUE),
              !any(label_number(summary_text, "\\bn") == 4L, na.rm = TRUE),
              sum(ggplot2::ggplot_build(plot_model()$plot)$data[[1]]$count) == 3L,
              !any(grepl("%", texts, fixed = TRUE)))
    session$setInputs(tab = "relations", relation_type = "cross",
                     cross_x = "sexo", cross_y = "enferedad_cronica", cross_chart = "percent")
    plotted <- plot_model()$plot$data
    stopifnot(sum(plotted$n) == 2L,
              all(label_number(plotted$Etiqueta, "\\bn") == 1L),
              all(label_percentage(plotted$Etiqueta) == 50),
              attr(table_model()$data, "n_missing") == 2L)
    csv <- read_download_csv(output$download_base)
    stopifnot(nrow(csv) == 4L, sum(is.na(csv$edad_anos)) == 1L,
              identical(csv$edad_anos[1:3], c(2L, 4L, 8L)))
  })
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

# Manual denominators distinguish all rows from complete categorical pairs.
# The missing category represents 1/4 of the cohort, and the crossing has only
# the first two complete pairs. These values are stated independently of app
# transformations so labels cannot inherit a silently changed denominator.
manual <- data.frame(group = c("A", "A", "B", NA_character_),
                     answer = c("Sí", "No", NA_character_, "Sí"),
                     measure = c(2, 4, 8, NA_real_))
group_column <- list(key = "group", type = "categorical", ordered = FALSE)
answer_column <- list(key = "answer", type = "categorical", ordered = FALSE)
manual_frequency <- app_env$frequency_table(manual, group_column)
stopifnot(identical(manual_frequency$Categoría, c("A", "B", "Sin dato")),
          identical(manual_frequency$n, c(2L, 1L, 1L)),
          identical(manual_frequency$`Porcentaje del total`, c(50, 25, 25)))
manual_cross <- app_env$cross_table(manual, group_column, answer_column)
stopifnot(attr(manual_cross, "n_complete") == 2L,
          attr(manual_cross, "n_missing") == 2L,
          identical(as.integer(manual_cross$n), c(1L, 1L)),
          identical(manual_cross$`Porcentaje de la fila`, c(50, 50)),
          identical(manual_cross$`Porcentaje del total válido`, c(50, 50)))
cat("PASS: 20 cases, 7729 rows, independent summaries, filters, five palettes/picker, 20 chart modes, internal n/percent/mean/median/r labels and segment positions, CSV cohorts, SVG/PDF downloads, singleton/constant/missing-value boundaries.\n")
