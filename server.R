#
# server.R
# Premium server logic for CheckerBoardR.shiny.
# Connects the new calculation engine and visualizers with dynamic reactivity.
#

library(shiny)
library(plotly)
source("SynergyCalculations.R")
source("Make3DPlotFunctions.R")

# Enable verbose un-sanitized error traces in logs for bulletproof debugging
options(shiny.sanitize.errors = FALSE)

SAMPLE_FILES <- c(
  "1" = "testData3.tab",
  "2" = "anticancer_synergy.tab",
  "3" = "antagonism.csv",
  "4" = "paclitaxel_carboplatin.json",
  "5" = "fluconazole_voriconazole.xml",
  "6" = "testData.xlsx"
)

sample_content_type <- function(path) {
  switch(tolower(tools::file_ext(path)),
         csv = "text/csv", tab = "text/tab-separated-values",
         tsv = "text/tab-separated-values", json = "application/json",
         xml = "application/xml",
         xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
         "application/octet-stream")
}

read_excel_condition_labels <- function(path) {
  tryCatch({
    sheets <- readxl::excel_sheets(path)
    sheet_name <- if ("Conditions" %in% sheets) "Conditions" else if (length(sheets) >= 2) sheets[[2]] else NULL
    if (is.null(sheet_name)) return(NULL)
    metadata <- as.data.frame(readxl::read_excel(path, sheet = sheet_name), stringsAsFactors = FALSE)
    if (!all(c("Field", "Value") %in% colnames(metadata))) return(NULL)
    values <- setNames(trimws(as.character(metadata$Value)), trimws(as.character(metadata$Field)))
    if (!all(c("Condition A", "Condition B") %in% names(values))) return(NULL)
    if (!nzchar(values[["Condition A"]]) || !nzchar(values[["Condition B"]])) return(NULL)
    list(a = values[["Condition A"]], b = values[["Condition B"]], derived = TRUE)
  }, error = function(e) NULL)
}

standardize_checkerboard_labels <- function(data) {
  data <- as.data.frame(data, check.names = FALSE)
  if (is.null(colnames(data)) || all(colnames(data) == paste0("V", seq_len(ncol(data))))) {
    colnames(data) <- paste0(seq(0, length.out = ncol(data)), "uM")
  }
  if (is.null(rownames(data)) || all(rownames(data) == as.character(seq_len(nrow(data))))) {
    rownames(data) <- paste0(seq(0, length.out = nrow(data)), "uM")
  }
  colnames(data) <- format_concentration_labels(colnames(data))
  rownames(data) <- format_concentration_labels(rownames(data))
  data
}

shinyServer(function(input, output, session) {

  for (sample_index in seq_along(SAMPLE_FILES)) {
    local({
      index <- sample_index
      source_path <- unname(SAMPLE_FILES[[index]])
      output[[paste0("downloadSample", index)]] <- downloadHandler(
        filename = function() basename(source_path),
        content = function(file) {
          if (!file.copy(source_path, file, overwrite = TRUE)) stop("Unable to prepare sample download.")
        },
        contentType = sample_content_type(source_path)
      )
    })
  }
  
  # Reactive handler to clear text input field
  observe({
    if (input$clearText_button == 0) return()
    isolate({ updateTextInput(session, "myData", label = "", value = "") })
  })

  uploadedMatrices <- reactive({
    in_files <- input$upload
    if (is.null(in_files) || nrow(in_files) == 0) return(NULL)
    separator <- switch(input$fileSepDF, '1' = ",", '2' = "\t", '3' = ";")
    matrices <- lapply(in_files$datapath, function(path) {
      parsed <- if (isTRUE(input$fileHeader)) {
        read.table(path, sep = separator, header = TRUE, row.names = 1,
                   fill = TRUE, check.names = FALSE)
      } else {
        read.table(path, sep = separator, header = FALSE, fill = TRUE,
                   check.names = FALSE)
      }
      standardize_checkerboard_labels(parsed)
    })
    reference_dim <- dim(matrices[[1]])
    reference_names <- dimnames(matrices[[1]])
    shiny::validate(shiny::need(
      all(vapply(matrices, function(x) identical(dim(x), reference_dim), logical(1))),
      "Replicate matrices must have identical dimensions."
    ))
    shiny::validate(shiny::need(
      all(vapply(matrices, function(x) identical(dimnames(x), reference_names), logical(1))),
      "Replicate matrices must have identical row and column concentration labels."
    ))
    matrices
  })
  
  # *** Read raw data matrix from different sources ***
  dataM <- reactive({
    if (input$dataInput == 1) {
      # Sample Data
      if (input$sampleData == 1) {
        data <- read.table("testData3.tab", sep = "\t", header = FALSE)			
      } else if (input$sampleData == 2) {
        data <- read.table("anticancer_synergy.tab", sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)
      } else if (input$sampleData == 3) {
        data <- read.table("antagonism.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
      } else if (input$sampleData == 4) {
        # Load JSON and update Shiny GUI input controls reactively!
        library(jsonlite)
        payload <- fromJSON("paclitaxel_carboplatin.json")
        conc_a <- as.numeric(payload$concentrations_a)
        conc_b <- as.numeric(payload$concentrations_b)
        mat <- as.matrix(payload$matrix)
        colnames(mat) <- paste0(conc_a, "uM")
        rownames(mat) <- paste0(conc_b, "uM")
        if (!is.null(payload$settings)) {
          if (!is.null(payload$settings$synergy_model)) updateSelectInput(session, "synergyModel", selected = payload$settings$synergy_model)
          if (!is.null(payload$settings$theme_preset)) updateSelectInput(session, "themePreset", selected = payload$settings$theme_preset)
          if (!is.null(payload$settings$plot_engine)) updateSelectInput(session, "plotEngine", selected = payload$settings$plot_engine)
          if (!is.null(payload$settings$orientation)) updateRadioButtons(session, "myOrientation", selected = payload$settings$orientation)
          if (!is.null(payload$data_type)) updateRadioButtons(session, "dataType", selected = payload$data_type)
        }
        data <- as.data.frame(mat)
      } else if (input$sampleData == 5) {
        # Load XML and update Shiny GUI input controls reactively!
        library(xml2)
        xml_doc <- read_xml("fluconazole_voriconazole.xml")
        drug_a_node <- xml_find_first(xml_doc, "//drug_a")
        conc_a <- as.numeric(strsplit(xml_text(xml_find_first(drug_a_node, "./concentrations")), ",")[[1]])
        drug_b_node <- xml_find_first(xml_doc, "//drug_b")
        conc_b <- as.numeric(strsplit(xml_text(xml_find_first(drug_b_node, "./concentrations")), ",")[[1]])
        data_type <- xml_text(xml_find_first(xml_doc, "//data_representation"))
        row_nodes <- xml_find_all(xml_doc, "//matrix/row")
        mat <- matrix(0, nrow = length(row_nodes), ncol = length(conc_a))
        for (idx in seq_along(row_nodes)) {
          mat[idx, ] <- as.numeric(strsplit(xml_text(row_nodes[idx]), ",")[[1]])
        }
        colnames(mat) <- paste0(conc_a, "uM")
        rownames(mat) <- paste0(conc_b, "uM")
        settings_node <- xml_find_first(xml_doc, "//settings")
        if (!is.na(settings_node)) {
          syn_model <- xml_text(xml_find_first(settings_node, "./synergy_model"))
          plot_eng <- xml_text(xml_find_first(settings_node, "./plot_engine"))
          theme_pre <- xml_text(xml_find_first(settings_node, "./theme_preset"))
          orient <- xml_text(xml_find_first(settings_node, "./orientation"))
          if (!is.na(syn_model)) updateSelectInput(session, "synergyModel", selected = syn_model)
          if (!is.na(theme_pre)) updateSelectInput(session, "themePreset", selected = theme_pre)
          if (!is.na(plot_eng)) updateSelectInput(session, "plotEngine", selected = plot_eng)
          if (!is.na(orient)) updateRadioButtons(session, "myOrientation", selected = orient)
          if (!is.na(data_type)) updateRadioButtons(session, "dataType", selected = data_type)
        }
        data <- as.data.frame(mat)
      } else {
        # Load Excel Spreadsheet (.xlsx)
        library(readxl)
        excel_data <- read_excel("testData.xlsx", sheet = 1, .name_repair = "minimal")
        # Convert first column to row names
        df <- as.data.frame(excel_data, check.names = FALSE)
        rownames(df) <- df[, 1]
        df <- df[, -1]
        data <- df
      }
    } else if (input$dataInput == 2) {
      matrices <- uploadedMatrices()
      if (is.null(matrices)) return(NULL)
      data <- as.data.frame(Reduce(`+`, lapply(matrices, as.matrix)) / length(matrices), check.names = FALSE)
    } else {
      # Pasted Text Data
      if (is.null(input$myData) || input$myData == "") return(NULL)
      
      raw_txt <- trimws(input$myData)
      
      if (startsWith(raw_txt, "{") || startsWith(raw_txt, "[")) {
        # 1. JSON Payload parsing
        library(jsonlite)
        payload <- fromJSON(raw_txt)
        conc_a <- as.numeric(payload$concentrations_a)
        conc_b <- as.numeric(payload$concentrations_b)
        mat <- as.matrix(payload$matrix)
        
        colnames(mat) <- paste0(conc_a, "uM")
        rownames(mat) <- paste0(conc_b, "uM")
        
        # Update inputs reactively if settings exist
        if (!is.null(payload$settings)) {
          if (!is.null(payload$settings$synergy_model)) updateSelectInput(session, "synergyModel", selected = payload$settings$synergy_model)
          if (!is.null(payload$settings$theme_preset)) updateSelectInput(session, "themePreset", selected = payload$settings$theme_preset)
          if (!is.null(payload$settings$plot_engine)) updateSelectInput(session, "plotEngine", selected = payload$settings$plot_engine)
          if (!is.null(payload$settings$orientation)) updateRadioButtons(session, "myOrientation", selected = payload$settings$orientation)
          if (!is.null(payload$data_type)) updateRadioButtons(session, "dataType", selected = payload$data_type)
        }
        
        data <- as.data.frame(mat)
      } else if (startsWith(raw_txt, "<")) {
        # 2. XML Payload parsing
        library(xml2)
        xml_doc <- read_xml(raw_txt)
        
        # Drug A
        drug_a_node <- xml_find_first(xml_doc, "//drug_a")
        conc_a_txt <- xml_text(xml_find_first(drug_a_node, "./concentrations"))
        conc_a <- as.numeric(strsplit(conc_a_txt, ",")[[1]])
        
        # Drug B
        drug_b_node <- xml_find_first(xml_doc, "//drug_b")
        conc_b_txt <- xml_text(xml_find_first(drug_b_node, "./concentrations"))
        conc_b <- as.numeric(strsplit(conc_b_txt, ",")[[1]])
        
        # Data type
        data_rep_node <- xml_find_first(xml_doc, "//data_representation")
        data_type <- xml_text(data_rep_node)
        
        # Matrix
        row_nodes <- xml_find_all(xml_doc, "//matrix/row")
        mat <- matrix(0, nrow = length(row_nodes), ncol = length(conc_a))
        for (idx in seq_along(row_nodes)) {
          row_vals <- as.numeric(strsplit(xml_text(row_nodes[idx]), ",")[[1]])
          mat[idx, ] <- row_vals
        }
        
        colnames(mat) <- paste0(conc_a, "uM")
        rownames(mat) <- paste0(conc_b, "uM")
        
        # Settings
        settings_node <- xml_find_first(xml_doc, "//settings")
        if (!is.na(settings_node)) {
          syn_model <- xml_text(xml_find_first(settings_node, "./synergy_model"))
          plot_eng <- xml_text(xml_find_first(settings_node, "./plot_engine"))
          theme_pre <- xml_text(xml_find_first(settings_node, "./theme_preset"))
          orient <- xml_text(xml_find_first(settings_node, "./orientation"))
          
          if (!is.na(syn_model)) updateSelectInput(session, "synergyModel", selected = syn_model)
          if (!is.na(theme_pre)) updateSelectInput(session, "themePreset", selected = theme_pre)
          if (!is.na(plot_eng)) updateSelectInput(session, "plotEngine", selected = plot_eng)
          if (!is.na(orient)) updateRadioButtons(session, "myOrientation", selected = orient)
          if (!is.na(data_type)) updateRadioButtons(session, "dataType", selected = data_type)
        }
        
        data <- as.data.frame(mat)
      } else {
        # 3. Delimited Matrix fallback
        tmp <- matrix(strsplit(raw_txt, "\n")[[1]])
        mySep <- switch(input$fileSepP, '1' = ",", '2' = "\t", '3' = ";")
        
        myColnames <- strsplit(tmp[1], mySep)[[1]]
        data <- matrix(0, length(tmp) - 1, length(myColnames))
        colnames(data) <- myColnames
        
        for (i in 2:length(tmp)) {
          myRow <- as.numeric(strsplit(paste(tmp[i], mySep, mySep, sep = ""), mySep)[[1]])
          data[i - 1, ] <- myRow[1:ncol(data)]
        }
        
        data <- data.frame(data, check.names = FALSE)
        if (is.na(as.numeric(data[1, 1])) || all(data[, 1] == seq_len(nrow(data)))) {
          rownames(data) <- data[, 1]
          data <- data[, -1]
        }
      }
    }
    
    standardize_checkerboard_labels(data)
  })

  analysisMatrices <- reactive({
    if (input$dataInput == 2) {
      matrices <- uploadedMatrices()
      if (is.null(matrices)) return(NULL)
      return(matrices)
    }
    current <- dataM()
    if (is.null(current)) return(NULL)
    list(current)
  })

  derivedConditionLabels <- reactive({
    labels <- list(a = "Drug A", b = "Drug B", derived = FALSE)
    if (input$dataInput == 1) {
      source_name <- unname(SAMPLE_FILES[[as.character(input$sampleData)]])
      labels <- condition_names_from_filename(source_name)
      if (!isTRUE(labels$derived) && identical(as.character(input$sampleData), "6")) {
        excel_labels <- read_excel_condition_labels(source_name)
        if (!is.null(excel_labels)) labels <- excel_labels
      }
    } else if (input$dataInput == 2 && !is.null(input$upload) && nrow(input$upload) > 0) {
      labels <- condition_names_from_filename(input$upload$name[[1]])
    } else if (input$dataInput == 3 && !is.null(input$myData) && nzchar(trimws(input$myData))) {
      raw_text <- trimws(input$myData)
      if (startsWith(raw_text, "{") || startsWith(raw_text, "[")) {
        payload <- tryCatch(jsonlite::fromJSON(raw_text), error = function(e) NULL)
        if (!is.null(payload) && length(payload$drug_a) == 1 && length(payload$drug_b) == 1) {
          labels <- list(a = as.character(payload$drug_a), b = as.character(payload$drug_b), derived = TRUE)
        }
      } else if (startsWith(raw_text, "<")) {
        xml_labels <- tryCatch({
          document <- xml2::read_xml(raw_text)
          list(a = xml2::xml_attr(xml2::xml_find_first(document, "//drug_a"), "name"),
               b = xml2::xml_attr(xml2::xml_find_first(document, "//drug_b"), "name"),
               derived = TRUE)
        }, error = function(e) NULL)
        if (!is.null(xml_labels) && nzchar(xml_labels$a) && nzchar(xml_labels$b)) labels <- xml_labels
      }
    }
    labels$axis_a <- paste(labels$a, "Concentration")
    labels$axis_b <- paste(labels$b, "Concentration")
    labels
  })

  activeConditionLabels <- reactive({
    labels <- derivedConditionLabels()
    if (isTRUE(input$labelsTitle)) {
      if (!is.null(input$myXlab) && nzchar(trimws(input$myXlab))) labels$axis_a <- trimws(input$myXlab)
      if (!is.null(input$myYlab) && nzchar(trimws(input$myYlab))) labels$axis_b <- trimws(input$myYlab)
    }
    labels
  })

  observeEvent(derivedConditionLabels(), {
    labels <- derivedConditionLabels()
    if (!isTRUE(input$labelsTitle)) {
      updateTextInput(session, "myXlab", value = labels$axis_a)
      updateTextInput(session, "myYlab", value = labels$axis_b)
    }
    updateCheckboxInput(session, "flipDataX", label = paste0("Flip data by X-axis (", labels$a, ")"))
    updateCheckboxInput(session, "flipDataY", label = paste0("Flip data by Y-axis (", labels$b, ")"))
  }, ignoreInit = FALSE)

  effectiveDataType <- reactive({
    declared <- NULL
    if (input$dataInput == 3 && !is.null(input$myData) && nzchar(trimws(input$myData))) {
      raw_text <- trimws(input$myData)
      if (startsWith(raw_text, "{") || startsWith(raw_text, "[")) {
        declared <- tryCatch(jsonlite::fromJSON(raw_text)$data_type, error = function(e) NULL)
      } else if (startsWith(raw_text, "<")) {
        declared <- tryCatch(xml2::xml_text(xml2::xml_find_first(xml2::read_xml(raw_text), "//data_representation")),
                             error = function(e) NULL)
      }
    } else if (input$dataInput == 1 && input$sampleData == 4) {
      declared <- tryCatch(jsonlite::fromJSON("paclitaxel_carboplatin.json")$data_type, error = function(e) NULL)
    } else if (input$dataInput == 1 && input$sampleData == 5) {
      declared <- tryCatch(xml2::xml_text(xml2::xml_find_first(xml2::read_xml("fluconazole_voriconazole.xml"), "//data_representation")),
                           error = function(e) NULL)
    }
    if (length(declared) == 1 && declared %in% c("viability", "inhibition")) declared else input$dataType
  })
  
  # *** Perform robust synergy calculations reactive pipeline ***
  synergyResults <- reactive({
    matrices <- analysisMatrices()
    if (is.null(matrices)) return(NULL)
    df <- matrices[[1]]
    
    shiny::validate(
      shiny::need(nrow(df) >= 2 && ncol(df) >= 2, "A checkerboard requires at least two rows and columns."),
      shiny::need(input$ctrlRow <= nrow(df) && input$ctrlCol <= ncol(df), "Control coordinates are outside the matrix.")
    )
    withProgress(message = "Evaluating synergy models...", value = 0.3, {
      setProgress(message = "Executing calculations...", value = 0.6)
      common_args <- list(
        data_type = effectiveDataType(),
        use_fit = isTRUE(input$useFit),
        control_row = if (effectiveDataType() == "viability") input$ctrlRow else 1,
        control_col = if (effectiveDataType() == "viability") input$ctrlCol else 1,
        baseline_method = if (is.null(input$baselineMethod)) "none" else input$baselineMethod
      )
      res <- if (length(matrices) > 1) {
        do.call(calculate_replicate_synergy, c(list(
          matrices = matrices,
          iterations = if (is.null(input$bootstrapIterations)) 200 else input$bootstrapIterations
        ), common_args))
      } else {
        do.call(calculate_synergy, c(list(xx = df), common_args))
      }
      condition_labels <- activeConditionLabels()
      res$condition_A <- condition_labels$a
      res$condition_B <- condition_labels$b
      res$axis_label_A <- condition_labels$axis_a
      res$axis_label_B <- condition_labels$axis_b
      setProgress(message = "Finalizing visualization grids...", value = 0.9)
      Sys.sleep(0.15) # Brief pause so the progress bar is visible and satisfying
      res
    })
  })

  observeEvent(input$synergyModel, {
    model <- if (is.null(input$synergyModel)) "Bliss" else input$synergyModel
    choices <- if (model == "Data") {
      c("Observed inhibition" = "observed", "Original inhibition" = "original")
    } else if (model == "ZIP") {
      c("Synergy score" = "score", "Reference effect" = "reference",
        "Fitted response" = "fitted", "Observed inhibition" = "observed")
    } else {
      c("Synergy score" = "score", "Reference effect" = "reference",
        "Observed inhibition" = "observed")
    }
    # A model change starts from its primary analytical view: score for a
    # reference model and observed inhibition for raw Data. Users can then
    # deliberately switch to reference, fitted, or observed matrices.
    updateSelectInput(session, "plotValue", choices = choices, selected = unname(choices)[1])
  }, ignoreInit = FALSE)

  observeEvent(synergyResults(), {
    res <- synergyResults()
    req(res)
    a_choices <- setNames(seq_along(colnames(res$adjusted_inhibition)), colnames(res$adjusted_inhibition))
    b_choices <- setNames(seq_along(rownames(res$adjusted_inhibition)), rownames(res$adjusted_inhibition))
    updateSelectInput(session, "barometerA", choices = a_choices,
                      selected = if (length(a_choices) > 1) 2 else 1,
                      label = paste(activeConditionLabels()$a, "concentration:"))
    updateSelectInput(session, "barometerB", choices = b_choices,
                      selected = if (length(b_choices) > 1) 2 else 1,
                      label = paste(activeConditionLabels()$b, "concentration:"))
  }, ignoreInit = FALSE)
  
  # *** Render matrix data preview table ***
  output$filetable <- renderTable({
    df <- dataM()
    if (is.null(df)) return(NULL)
    
    # Return limited preview for neat visualization
    if (nrow(df) > 100) {
      df[1:100, 1:min(ncol(df), 20)]
    } else {
      df[, 1:min(ncol(df), 20)]
    }
  }, rownames = TRUE)
  
  # *** Render interactive 3D Plotly Surface ***
  output$plotlyPlot <- renderPlotly({
    res <- synergyResults()
    if (is.null(res)) return(NULL)
    
    plotly_synergy_surface(
      res, 
      input$synergyModel, 
      input$themePreset,
      camera_theta = input$plotlyTheta,
      camera_phi = input$plotlyPhi,
      camera_zoom = input$plotlyZoom,
      flip_x = isTRUE(input$flipDataX),
      flip_y = isTRUE(input$flipDataY),
      flip_z = isTRUE(input$flipDataZ),
      value_type = input$plotValue
    )
  })
  
  # *** Render modern 2D and 1D ggplot plots ***
  output$ggplotPlot <- renderPlot({
    tryCatch({
      res <- synergyResults()
      if (is.null(res)) return(NULL)
      
      # Robust safety guards for input parameters
      if (is.null(input$plotEngine) || length(input$plotEngine) == 0) return(NULL)
      if (is.null(input$synergyModel) || length(input$synergyModel) == 0) return(NULL)
      
      if (input$plotEngine == "2d_ggplot") {
        if (is.null(input$myOrientation) || length(input$myOrientation) == 0) return(NULL)
        p <- ggplot_synergy_heatmap(
          res, 
          input$synergyModel, 
          input$myOrientation, 
          input$themePreset, 
          input$myTitle,
          flip_x = isTRUE(input$flipDataX),
          flip_y = isTRUE(input$flipDataY),
          flip_z = isTRUE(input$flipDataZ),
          value_type = input$plotValue,
          uncertainty_display = input$uncertaintyDisplay
        )
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        # Fallback Base R 3D view
        raw_plot(
          res,
          input$synergyModel,
          theme_preset = input$themePreset,
          theta = input$plotlyTheta,
          phi = input$plotlyPhi,
          flip_x = isTRUE(input$flipDataX),
          flip_y = isTRUE(input$flipDataY),
          flip_z = isTRUE(input$flipDataZ),
          value_type = input$plotValue
        )
      }
    }, error = function(e) {
      cat("RENDERPLOT_ERROR:", e$message, "\n")
      # Print traceback trace to stderr
      sink(stderr())
      print(traceback())
      sink()
      stop(e)
    })
  }, height = 550, width = 750)
  
  # *** Generate scientific summary statistics table ***
  output$checkerboardStatsTable <- renderTable({
    res <- synergyResults()
    if (is.null(res)) return(NULL)
    
    selected <- select_analysis_matrix(res, input$synergyModel, input$plotValue)
    scores <- selected$matrix
    par_name <- selected$title
    
    max_score <- max(scores, na.rm = TRUE)
    min_score <- min(scores, na.rm = TRUE)
    mean_score <- mean(scores, na.rm = TRUE)
    
    max_idx <- which(scores == max_score, arr.ind = TRUE)[1, ]
    min_idx <- which(scores == min_score, arr.ind = TRUE)[1, ]
    
    par_A <- res$single_fit_A
    par_B <- res$single_fit_B
    condition_a <- if (!is.null(res$condition_A)) res$condition_A else "Drug A"
    condition_b <- if (!is.null(res$condition_B)) res$condition_B else "Drug B"
    
    data.frame(
      Scientific_Metric = c(
        paste("Max Synergy /", par_name),
        sprintf("Max Synergy Position (%s, %s)", condition_a, condition_b),
        paste("Max Antagonism / Min", par_name),
        sprintf("Max Antagonism Position (%s, %s)", condition_a, condition_b),
        "Mean Score across Screening Grid",
        sprintf("%s IC50 (Fitted 4PL Hill)", condition_a),
        sprintf("%s Hill Slope (Fitted 4PL Hill)", condition_a),
        sprintf("%s IC50 (Fitted 4PL Hill)", condition_b),
        sprintf("%s Hill Slope (Fitted 4PL Hill)", condition_b),
        "Baseline correction method",
        "Estimated fitted baseline",
        "Baseline correction applied",
        "Independent replicate matrices",
        "Bootstrap iterations"
      ),
      Computed_Value = c(
        sprintf("%.4f", max_score),
        sprintf("%s, %s", colnames(scores)[max_idx[2]], rownames(scores)[max_idx[1]]),
        sprintf("%.4f", min_score),
        sprintf("%s, %s", colnames(scores)[min_idx[2]], rownames(scores)[min_idx[1]]),
        sprintf("%.4f", mean_score),
        if (!is.null(par_A)) sprintf("%.4f", par_A[3]) else "N/A (Linear Fallback)",
        if (!is.null(par_A)) sprintf("%.4f", par_A[4]) else "N/A (Linear Fallback)",
        if (!is.null(par_B)) sprintf("%.4f", par_B[3]) else "N/A (Linear Fallback)",
        if (!is.null(par_B)) sprintf("%.4f", par_B[4]) else "N/A (Linear Fallback)",
        res$baseline_method,
        sprintf("%.6f", res$baseline_value),
        if (isTRUE(res$baseline_applied)) "Yes" else "No",
        as.character(res$replicate_count),
        as.character(res$bootstrap_iterations)
      ),
      stringsAsFactors = FALSE
    )
  })

  output$barometerPlot <- renderPlot({
    res <- synergyResults()
    req(res, input$barometerA, input$barometerB)
    print(ggplot_synergy_barometer(res, input$barometerB, input$barometerA, input$themePreset))
  }, height = 330)

  output$barometerTable <- renderTable({
    res <- synergyResults()
    req(res, input$barometerA, input$barometerB)
    table <- synergy_barometer_data(res, input$barometerB, input$barometerA)
    table$Model <- as.character(table$Model)
    table$Reference <- sprintf("%.6f", table$Reference)
    table$Observed <- sprintf("%.6f", table$Observed)
    table$Delta <- sprintf("%.6f", table$Delta)
    table
  })

  output$downloadMatrixCSV <- downloadHandler(
    filename = function() paste0("CheckerboardR_score_reference_matrices_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(build_matrix_export(synergyResults()), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
  
  # *** Handle publication EPS Download ***
  output$downloadPlotEPS <- downloadHandler(
    filename = function() { paste0('Checkerboard_Synergy_', input$synergyModel, '.eps') },
    content = function(file) {
      res <- synergyResults()
      if (is.null(res)) return(NULL)
      
      w <- if (isTRUE(input$plotSize)) input$myWidth / 72 else 750 / 72
      h <- if (isTRUE(input$plotSize)) input$myHeight / 72 else 550 / 72
      
      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = w, height = h)
      if (input$plotEngine == "2d_ggplot") {
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle,
                                    flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                                    flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue,
                                    uncertainty_display = input$uncertaintyDisplay)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue)
      }
      dev.off()
    },
    contentType = 'application/postscript'
  )
  
  # *** Handle publication PDF Download ***
  output$downloadPlotPDF <- downloadHandler(
    filename = function() { paste0('Checkerboard_Synergy_', input$synergyModel, '.pdf') },
    content = function(file) {
      res <- synergyResults()
      if (is.null(res)) return(NULL)
      
      w <- if (isTRUE(input$plotSize)) input$myWidth / 72 else 750 / 72
      h <- if (isTRUE(input$plotSize)) input$myHeight / 72 else 550 / 72
      
      pdf(file, width = w, height = h)
      if (input$plotEngine == "2d_ggplot") {
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle,
                                    flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                                    flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue,
                                    uncertainty_display = input$uncertaintyDisplay)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue)
      }
      dev.off()
    },
    contentType = 'application/pdf'
  )
  
  # *** Handle publication SVG Download ***
  output$downloadPlotSVG <- downloadHandler(
    filename = function() { paste0('Checkerboard_Synergy_', input$synergyModel, '.svg') },
    content = function(file) {
      res <- synergyResults()
      if (is.null(res)) return(NULL)
      
      w <- if (isTRUE(input$plotSize)) input$myWidth / 72 else 750 / 72
      h <- if (isTRUE(input$plotSize)) input$myHeight / 72 else 550 / 72
      
      svg(file, width = w, height = h)
      if (input$plotEngine == "2d_ggplot") {
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle,
                                    flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                                    flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue,
                                    uncertainty_display = input$uncertaintyDisplay)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ), value_type = input$plotValue)
      }
      dev.off()
    },
    contentType = 'image/svg'
  )
  
})
