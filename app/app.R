library(shiny)
library(ggplot2)
library(DT)
library(jsonlite)
library(svglite)
source("helpers.R", local = TRUE)

cases <- fromJSON("data/cases.json", simplifyVector = FALSE)$cases
stopifnot(length(cases) > 0L)
case_ids <- vapply(cases, `[[`, character(1), "id")
names(cases) <- case_ids
case_choices <- setNames(case_ids, vapply(cases, function(c) paste(c$number, c$title, sep = " · "), character(1)))

# Official Shinylive workaround for Chromium downloads.
download_btn <- function(...) {
  button <- shiny::downloadButton(...)
  button$attribs$download <- NULL
  button
}

ui <- fluidPage(
  tags$head(tags$title("Laboratorio de metodología | Shiny R"),
    tags$style(HTML("body{background:#f4f7f7;color:#19363b;font-family:system-ui,sans-serif}.container-fluid{max-width:1560px;padding:20px 26px}.brand{display:flex;justify-content:space-between;align-items:center;margin:0 0 22px}.brand h1{font-size:23px;font-weight:700;margin:0}.brand p{font-size:12px;color:#607478;margin:6px 0 0}.simulated{font-size:12px;padding:7px 11px;background:#e1f0eb;color:#176960;border-radius:20px}.well{background:#fff;border:1px solid #dbe4e3;border-radius:12px;box-shadow:none;padding:18px}.well h3{font-size:15px;margin-top:6px}.form-control,.selectize-input{border-radius:7px}.btn{border-radius:7px;font-size:12px}.btn-primary{background:#087f7a;border-color:#087f7a}.btn-primary:hover{background:#086a66;border-color:#086a66}.panel-block{background:#fff;border:1px solid #dbe4e3;border-radius:12px;padding:18px 20px;margin-top:18px}.case-title{font-size:24px;font-weight:650;line-height:1.2;margin:0 0 9px}.muted,.help-block{color:#607478;font-size:12px;line-height:1.6}.cohort{font-size:14px;margin-bottom:17px}.alert-note{background:#fff2e5;border-left:3px solid #d98039;padding:12px 15px;border-radius:4px;font-size:12px;line-height:1.6;margin:14px 0}.toolbar-downloads{display:flex;flex-wrap:wrap;gap:8px;margin-top:12px}.nav-tabs{margin-top:18px}.nav-tabs>li>a{color:#42676a}.nav-tabs>li.active>a{color:#087f7a;font-weight:600}.tab-content{background:white;padding:18px 16px 6px;border:1px solid #dbe4e3;border-top:0;border-radius:0 0 10px 10px}.tab-content .row{margin-bottom:0}h3{font-size:17px;font-weight:600}details{margin-top:22px;font-size:12px;line-height:1.7;color:#607478}summary{cursor:pointer;color:#365e61;font-weight:600}table.dataTable tbody td{vertical-align:top}.dataTables_wrapper{overflow-x:auto}#figure{width:100%}.shiny-output-error-validation{color:#607478;padding:20px;font-size:14px}@media(max-width:767px){.container-fluid{padding:15px}.brand{align-items:flex-start;gap:12px;flex-direction:column}.case-title{font-size:21px}.panel-block{padding:14px 12px}.simulated{font-size:11px}}"))),
  div(class = "brand", div(h1("Laboratorio de metodología"), p("DSPU0011 · 20 casos de investigación · Shiny R")),
      span(class = "simulated", "Datos simulados para docencia")),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("caso", "Caso de estudio", choices = case_choices, selected = case_ids[[1]]),
      hr(), h3("Cohorte de análisis"),
      p(class = "help-block", "Combina hasta dos filtros. Todas las figuras, tablas y descargas usan esta misma cohorte."),
      selectInput("filter1", "Filtrar por", choices = c("Sin filtro" = "none")), uiOutput("filter1_value"),
      selectInput("filter2", "Segundo filtro", choices = c("Sin filtro" = "none")), uiOutput("filter2_value"),
      actionButton("reset_filters", "Restablecer filtros"), hr(),
      h3("Colores de la figura"),
      selectInput("palette", "Paleta", choices = c("Verde y naranja" = "teal", "Azul y rojo" = "blue-red", "Viridis" = "viridis", "Pastel" = "pastel", "Escala de grises" = "gray")),
      selectInput("primary_color", "Color principal", choices = c("De la paleta" = "palette", "Verde petróleo" = "#087f7a", "Azul" = "#2563eb", "Naranja" = "#d98039", "Rojo" = "#dc4848", "Violeta" = "#7c3aed", "Verde" = "#15803d", "Gris" = "#6b7280", "Negro" = "#222222")),
      textInput("primary_hex", "Color HEX personalizado (opcional)", value = "", placeholder = "#087f7a"),
      p(class = "help-block", textOutput("hex_status")), uiOutput("palette_preview"),
      p(class = "help-block", "El color principal sustituye el primer color de la paleta. Se aplica a la figura y a sus descargas."), hr(),
      download_btn("download_base", "Base filtrada CSV", class = "btn-primary"),
      p(class = "help-block", "Sin identificadores. Un registro simulado por fila.")),
    mainPanel(width = 9,
      h2(class = "case-title", textOutput("case_title")),
      div(class = "cohort", textOutput("cohort_size")), uiOutput("case_note"),
      tabsetPanel(id = "tab",
        tabPanel("Distribuciones", value = "distribution",
          fluidRow(column(7, selectInput("variable", "Variable", choices = NULL)), column(5, uiOutput("distribution_controls")))),
        tabPanel("Relaciones", value = "relations",
          selectInput("relation_type", "Tipo de relación", choices = c("Medida por grupo" = "group", "Dos medidas" = "scatter", "Dos categorías" = "cross")),
          uiOutput("relation_controls")),
        tabPanel("Datos y diccionario", value = "data",
          selectInput("data_table", "Tabla", choices = c("Base filtrada" = "base", "Diccionario de variables" = "dictionary")),
          p(class = "muted", "Las categorías se muestran con sus etiquetas. El diccionario conserva los códigos del archivo docente."))),
      conditionalPanel("input.tab !== 'data'", div(class = "panel-block",
        plotOutput("figure", height = "430px"),
        div(class = "toolbar-downloads", download_btn("download_svg", "Figura SVG"), download_btn("download_pdf", "Figura PDF")),
        p(class = "muted", "La figura descargada corresponde a la selección actual. SVG y PDF conservan elementos vectoriales."))),
      div(class = "panel-block", h3(textOutput("table_title")), DTOutput("table"),
        div(class = "toolbar-downloads", uiOutput("table_download"))),
      p(class = "muted", style = "margin-top:14px", "DE: desviación estándar muestral. Q1/Q3: percentiles 25/75, tipo 7 de R. NA indica que no hay información suficiente. Una asociación no demuestra causalidad."),
      tags$details(tags$summary("Archivo docente y preparación"), uiOutput("source_details"))))
)

server <- function(input, output, session) {
  current_case <- reactive(cases[[if (!is.null(input$caso) && input$caso %in% case_ids) input$caso else case_ids[[1]]]])
  base_data <- reactive(case_data_frame(current_case()))
  numeric_columns <- reactive(Filter(function(c) identical(c$type, "numeric"), current_case()$columns))
  categorical_columns <- reactive(Filter(function(c) identical(c$type, "categorical"), current_case()$columns))
  palette_values <- reactive({
    palette_id <- if (!is.null(input$palette) && input$palette %in% names(color_palettes)) input$palette else "teal"
    colors <- color_palettes[[palette_id]]
    selected <- input$primary_color
    if (!is.null(selected) && grepl("^#[0-9a-fA-F]{6}$", selected)) colors[1] <- selected
    hex <- trimws(if (is.null(input$primary_hex)) "" else input$primary_hex)
    if (grepl("^#[0-9a-fA-F]{6}$", hex)) colors[1] <- hex
    colors
  })
  output$hex_status <- renderText({
    hex <- trimws(if (is.null(input$primary_hex)) "" else input$primary_hex)
    if (!nzchar(hex)) "Ejemplo: #087f7a. Déjalo vacío para usar el selector."
    else if (!grepl("^#[0-9a-fA-F]{6}$", hex)) "HEX no válido: usa # seguido de seis dígitos o letras A–F. Se mantiene el color del selector."
    else "Color personalizado aplicado."
  })
  output$palette_preview <- renderUI(div(style = "display:flex;gap:5px;margin-bottom:8px", lapply(palette_values(), function(color)
    span(title = color, style = paste0("display:inline-block;width:19px;height:19px;border-radius:4px;border:1px solid #ccd5d3;background:", color)))))
  observeEvent(input$caso, {
    c <- current_case()
    choices <- c("Sin filtro" = "none", column_choices(categorical_columns()))
    updateSelectInput(session, "filter1", choices = choices, selected = "none")
    updateSelectInput(session, "filter2", choices = choices, selected = "none")
    updateSelectInput(session, "variable", choices = column_choices(c$columns), selected = c$columns[[1]]$key)
    relation_choices <- c("Medida por grupo" = "group", if (length(numeric_columns()) >= 2L) c("Dos medidas" = "scatter"), "Dos categorías" = "cross")
    updateSelectInput(session, "relation_type", choices = relation_choices, selected = "group")
  }, ignoreNULL = FALSE)
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter1", selected = "none")
    updateSelectInput(session, "filter2", selected = "none")
  })
  filter_control <- function(field, id) {
    if (is.null(field) || !field %in% names(base_data())) return(NULL)
    values <- base_data()[[field]]
    column <- column_for(current_case(), field)
    levels <- category_levels(values, column)
    choices <- c("Todas las categorías" = "__all__", setNames(setdiff(levels, "Sin dato"), setdiff(levels, "Sin dato")),
      if (anyNA(values)) c("Sin dato" = "__missing__"))
    selectInput(id, "Categoría", choices = choices, selected = "__all__")
  }
  output$filter1_value <- renderUI(filter_control(input$filter1, "value1"))
  output$filter2_value <- renderUI(filter_control(input$filter2, "value2"))
  filtered_data <- reactive({
    data <- base_data()
    for (index in 1:2) {
      field <- input[[paste0("filter", index)]]
      value <- input[[paste0("value", index)]]
      if (!is.null(field) && field %in% names(data) && !is.null(value) && value != "__all__") {
        keep <- if (value == "__missing__") is.na(data[[field]]) else !is.na(data[[field]]) & data[[field]] == value
        data <- data[keep, , drop = FALSE]
      }
    }
    data
  })
  output$case_title <- renderText(paste(current_case()$number, current_case()$title, sep = " · "))
  output$cohort_size <- renderText(sprintf("%s de %s registros · %s variables", nrow(filtered_data()), nrow(base_data()), ncol(base_data())))
  output$case_note <- renderUI({
    c <- current_case()
    if (c$id %in% c("caso-13", "caso-16")) div(class = "alert-note", strong(if (c$id == "caso-16") "Alcance del caso. " else "Coherencia de la base. "), unlist(c$notes)[1])
  })
  output$distribution_controls <- renderUI({
    column <- column_for(current_case(), input$variable)
    if (column$type == "numeric") sliderInput("bins", "Intervalos del histograma", min = 5, max = 30, value = 10, step = 5)
    else selectInput("distribution_chart", "Gráfico", choices = c("Barras horizontales" = "bar", "Circular" = "pie"))
  })
  output$relation_controls <- renderUI({
    nums <- column_choices(numeric_columns()); cats <- column_choices(categorical_columns())
    mode <- input$relation_type
    if (is.null(mode) || mode == "group") fluidRow(
      column(4, selectInput("measure", "Medida", nums)), column(4, selectInput("group", "Agrupar por", cats)),
      column(4, selectInput("group_chart", "Gráfico", c("Caja y bigotes" = "box", "Medias por grupo" = "mean"))))
    else if (mode == "scatter") fluidRow(
      column(4, selectInput("x", "Eje X", nums)), column(4, selectInput("y", "Eje Y", nums[-1])),
      column(4, selectInput("color", "Color por", c("Sin agrupación" = "none", cats))))
    else fluidRow(column(4, selectInput("cross_x", "Filas", cats)), column(4, selectInput("cross_y", "Columnas", cats[-1])),
      column(4, selectInput("cross_chart", "Gráfico", c("Barras apiladas" = "stacked", "Mapa de calor" = "heatmap"))))
  })
  observeEvent(input$x, updateSelectInput(session, "y", choices = column_choices(Filter(function(c) c$key != input$x, numeric_columns()))))
  observeEvent(input$cross_x, updateSelectInput(session, "cross_y", choices = column_choices(Filter(function(c) c$key != input$cross_x, categorical_columns()))))

  table_model <- reactive({
    c <- current_case(); data <- filtered_data(); tab <- input$tab
    if (identical(tab, "data")) {
      if (identical(input$data_table, "dictionary")) return(list(title = "Diccionario de variables", kind = "diccionario", data = dictionary_table(c)))
      names(data) <- vapply(c$columns, `[[`, character(1), "label")
      return(list(title = "Base filtrada", kind = "base", data = data))
    }
    if (!identical(tab, "relations")) {
      column <- column_for(c, input$variable)
      if (column$type == "numeric") return(list(title = "Resumen de variables numéricas", kind = "resumen-numerico", data = numeric_summary(data, numeric_columns())))
      return(list(title = paste("Frecuencias:", column$label), kind = paste0("frecuencias-", column$key), data = frequency_table(data, column)))
    }
    if (identical(input$relation_type, "scatter")) {
      x <- column_for(c, input$x, "numeric"); y <- column_for(c, input$y, "numeric", x$key)
      shiny::validate(need(!is.null(y), "Este caso tiene una sola medida numérica."))
      return(list(title = "Correlación de Pearson: pares completos", kind = "pearson", data = pearson_summary(data, x, y)))
    }
    if (identical(input$relation_type, "cross")) {
      x <- column_for(c, input$cross_x, "categorical"); y <- column_for(c, input$cross_y, "categorical", x$key)
      out <- cross_table(data, x, y)
      return(list(title = sprintf("Tabla cruzada: %s pares completos; %s filas excluidas", attr(out, "n_complete"), attr(out, "n_missing")), kind = "tabla-cruzada", data = out))
    }
    measure <- column_for(c, input$measure, "numeric"); group <- column_for(c, input$group, "categorical")
    list(title = paste(measure$label, "según", group$label), kind = "resumen-por-grupo", data = group_summary(data, measure, group))
  })

  plot_model <- reactive({
    c <- current_case(); data <- filtered_data()
    plot_palette <- palette_values()
    shiny::validate(need(nrow(data) > 0, "No hay registros para esta combinación. Amplía la cohorte con los filtros."))
    title <- ""; kind <- "figura"; subtitle <- sprintf("Cohorte filtrada: %s registros", nrow(data))
    if (!identical(input$tab, "relations")) {
      column <- column_for(c, input$variable); title <- column$label
      if (column$type == "numeric") {
        kind <- paste0("histograma-", column$key)
        x <- data[[column$key]]; valid <- is.finite(x)
        shiny::validate(need(any(valid), "La variable no contiene valores numéricos válidos en esta cohorte."))
        subtitle <- sprintf("n válido = %s; sin dato = %s", sum(valid), sum(!valid))
        p <- ggplot(data.frame(valor = x[valid]), aes(valor)) + geom_histogram(bins = if (is.null(input$bins)) 10 else input$bins, fill = plot_palette[1], color = "white", closed = "left") + labs(x = column$label, y = "Frecuencia (n)")
      } else {
        out <- frequency_table(data, column)
        out$Categoría <- factor(out$Categoría, levels = out$Categoría)
        if (identical(input$distribution_chart, "pie")) {
          kind <- paste0("circular-", column$key)
          p <- ggplot(out, aes(x = "", y = n, fill = Categoría)) + geom_col(width = 1, color = "white") + coord_polar(theta = "y") +
            scale_fill_manual(values = rep(plot_palette, length.out = nrow(out))) + labs(x = NULL, y = NULL) + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
        } else {
          kind <- paste0("barras-", column$key)
          p <- ggplot(out, aes(Categoría, n)) + geom_col(fill = plot_palette[1], width = .7) + coord_flip() + scale_x_discrete(labels = wrap_label) + labs(x = NULL, y = "Frecuencia (n)")
        }
        subtitle <- "Porcentajes en la tabla sobre todos los registros, incluidos los ausentes"
      }
    } else if (identical(input$relation_type, "scatter")) {
      x <- column_for(c, input$x, "numeric"); y <- column_for(c, input$y, "numeric", x$key)
      shiny::validate(need(!is.null(y), "Este caso tiene una sola medida numérica."))
      pairs <- complete_pairs(data, x, y); shiny::validate(need(nrow(pairs) > 0, "No hay pares completos para estas medidas."))
      r <- pearson_summary(data, x, y)$`r de Pearson`
      title <- paste(x$label, "y", y$label)
      kind <- paste0("dispersion-", x$key, "-", y$key)
      subtitle <- sprintf("%s pares completos; r de Pearson = %s", nrow(pairs), if (is.na(r)) "NA (sin variación o n < 3)" else format(round(r, 3), decimal.mark = ","))
      p <- ggplot(pairs, aes(x, y))
      if (!is.null(input$color) && input$color %in% names(pairs)) {
        pairs$grupo <- pairs[[input$color]]; pairs$grupo[is.na(pairs$grupo)] <- "Sin dato"
        p <- ggplot(pairs, aes(x, y, color = grupo)) + geom_point(alpha = .7, size = 2.3) + scale_color_manual(values = rep(plot_palette, length.out = length(unique(pairs$grupo))))
      } else p <- p + geom_point(color = plot_palette[1], alpha = .65, size = 2.3)
      p <- p + labs(x = x$label, y = y$label)
    } else if (identical(input$relation_type, "cross")) {
      x <- column_for(c, input$cross_x, "categorical"); y <- column_for(c, input$cross_y, "categorical", x$key)
      out <- cross_table(data, x, y); shiny::validate(need(attr(out, "n_complete") > 0, "No hay pares categóricos completos."))
      title <- paste(x$label, "según", y$label)
      kind <- paste0("cruce-", x$key, "-", y$key, "-", if (identical(input$cross_chart, "heatmap")) "calor" else "barras")
      subtitle <- sprintf("%s pares completos; %s filas excluidas por dato ausente", attr(out, "n_complete"), attr(out, "n_missing"))
      if (identical(input$cross_chart, "heatmap")) {
        rgb <- as.numeric(grDevices::col2rgb(plot_palette[1]))
        dark_primary <- sum(rgb * c(.299, .587, .114)) < 150
        text_colors <- ifelse(dark_primary & out$n > .55 * max(out$n), "white", "#173c3d")
        p <- ggplot(out, aes(Fila, Columna, fill = n)) + geom_tile(color = "white") + geom_text(aes(label = n), color = text_colors) + scale_fill_gradient(low = lighten_color(plot_palette[1], .9), high = plot_palette[1]) + scale_x_discrete(labels = wrap_label) + scale_y_discrete(labels = wrap_label) + labs(x = x$label, y = y$label)
      }
      else p <- ggplot(out, aes(Fila, n, fill = Columna)) + geom_col(width = .7) + scale_fill_manual(values = rep(plot_palette, length.out = length(unique(out$Columna)))) + scale_x_discrete(labels = wrap_label) + labs(x = x$label, y = "Frecuencia (n)")
    } else {
      measure <- column_for(c, input$measure, "numeric"); group <- column_for(c, input$group, "categorical")
      title <- paste(measure$label, "según", group$label)
      kind <- paste0(if (identical(input$group_chart, "mean")) "medias-" else "cajas-", measure$key, "-por-", group$key)
      valid <- is.finite(data[[measure$key]]); shiny::validate(need(any(valid), "No hay valores numéricos válidos para comparar."))
      x <- data[[group$key]]; levels <- category_levels(x, group); x[is.na(x)] <- "Sin dato"
      out <- data.frame(grupo = factor(x[valid], levels = levels), valor = data[[measure$key]][valid])
      if (identical(input$group_chart, "mean")) {
        means <- group_summary(data, measure, group); means$Grupo <- factor(means$Grupo, levels = levels)
        p <- ggplot(means, aes(Grupo, Media)) + geom_col(fill = plot_palette[1], width = .7) + labs(y = paste("Media de", measure$label))
      } else p <- ggplot(out, aes(grupo, valor)) + geom_boxplot(fill = lighten_color(plot_palette[1], .7), color = plot_palette[1], outlier.color = plot_palette[2]) + labs(y = measure$label)
      p <- p + coord_flip() + scale_x_discrete(labels = wrap_label) + labs(x = NULL)
      subtitle <- sprintf("%s valores numéricos válidos; %s sin dato", sum(valid), sum(!valid))
    }
    p <- p + plot_theme() + labs(title = paste(strwrap(title, 36), collapse = "\n"), subtitle = paste(strwrap(subtitle, 48), collapse = "\n"), caption = "Datos simulados")
    if (!identical(input$tab, "relations") && identical(input$distribution_chart, "pie") &&
        column_for(c, input$variable)$type != "numeric") p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
    list(title = title, kind = kind, plot = p)
  })
  output$figure <- renderPlot({ print(plot_model()$plot) }, res = 110)
  output$table_title <- renderText(table_model()$title)
  output$table <- renderDT({
    data <- table_model()$data
    widget <- datatable(data, rownames = FALSE, filter = if (nrow(data) > 8) "top" else "none",
      options = list(pageLength = 8, lengthMenu = c(8, 25, 50, 100), scrollX = TRUE,
        columnDefs = list(list(defaultContent = "NA", targets = "_all")),
        language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ filas", info = "_START_ a _END_ de _TOTAL_ filas", infoEmpty = "Sin filas", emptyTable = "No hay registros en esta selección", zeroRecords = "Sin coincidencias", paginate = list(first = "Primera", previous = "Anterior", `next` = "Siguiente", last = "Última"))))
    decimals <- names(data)[vapply(data, is.numeric, logical(1)) & !names(data) %in% c("n", "N válido", "Sin dato", "Pares completos", "Filas excluidas")]
    if (length(decimals)) widget <- formatRound(widget, columns = decimals, digits = 2, dec.mark = ",", mark = ".")
    widget
  }, server = FALSE)
  output$table_download <- renderUI(download_btn("download_table", paste("CSV:", table_model()$title)))
  output$download_table <- downloadHandler(
    filename = function() paste0(current_case()$id, "-", table_model()$kind, ".csv"),
    content = function(file) safe_csv(table_model()$data, file), contentType = "text/csv;charset=utf-8")
  output$download_base <- downloadHandler(filename = function() paste0(current_case()$id, "-base-filtrada.csv"),
    content = function(file) safe_csv(filtered_data(), file), contentType = "text/csv;charset=utf-8")
  output$download_svg <- downloadHandler(filename = function() paste0(current_case()$id, "-", plot_model()$kind, ".svg"),
    content = function(file) { p <- plot_model()$plot; svglite(file, width = 10, height = 6.5); on.exit(dev.off(), add = TRUE); print(p) }, contentType = "image/svg+xml")
  output$download_pdf <- downloadHandler(filename = function() paste0(current_case()$id, "-", plot_model()$kind, ".pdf"),
    content = function(file) { p <- plot_model()$plot; pdf(file, width = 10, height = 6.5, useDingbats = FALSE); on.exit(dev.off(), add = TRUE); print(p) }, contentType = "application/pdf")
  output$source_details <- renderUI({
    c <- current_case()
    tagList(p(strong("Archivo: "), c$sourceFile), p(strong("Hoja: "), c$sheet),
      p("Datos completamente ficticios/simulados para docencia, según confirmación del responsable. Se excluyeron identificadores y materiales de evaluación. Se preservaron los ausentes y se decodificaron categorías con el diccionario disponible."),
      lapply(c$notes, function(note) p(note)),
      p("Los archivos CSV conservan números y celdas vacías para ausentes. La búsqueda de la tabla no cambia la cohorte ni el CSV; usa los filtros laterales para acotar todas las salidas."))
  })
}

shinyApp(ui, server)
