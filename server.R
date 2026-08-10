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

shinyServer(function(input, output, session) {
  
  # Reactive handler to clear text input field
  observe({
    if (input$clearText_button == 0) return()
    isolate({ updateTextInput(session, "myData", label = "", value = "") })
  })
  
  # *** Read raw data matrix from different sources ***
  dataM <- reactive({
    if (input$dataInput == 1) {
      # Sample Data
      if (input$sampleData == 1) {
        data <- read.table("testData3.tab", sep = "\t", header = FALSE)			
      } else if (input$sampleData == 2) {
        data <- read.table("testData.tab", sep = "\t", header = TRUE, row.names = 1)		
      } else if (input$sampleData == 3) {
        data <- read.table("antagonism.csv", sep = ",", header = TRUE, row.names = 1)
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
        excel_data <- read_excel("testData.xlsx", sheet = 1)
        # Convert first column to row names
        df <- as.data.frame(excel_data)
        rownames(df) <- df[, 1]
        df <- df[, -1]
        data <- df
      }
    } else if (input$dataInput == 2) {
      # File Upload
      inFile <- input$upload
      if (is.null(inFile)) return(NULL)
      
      mySep <- switch(input$fileSepDF, '1' = ",", '2' = "\t", '3' = ";")
      
      if (input$fileHeader) { 
        data <- read.table(inFile$datapath, sep = mySep, header = TRUE, row.names = 1, fill = TRUE)
      } else {
        data <- read.table(inFile$datapath, sep = mySep, header = FALSE, fill = TRUE)
      }
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
        
        data <- data.frame(data)
        if (is.na(as.numeric(data[1, 1])) || all(data[, 1] == seq_len(nrow(data)))) {
          rownames(data) <- data[, 1]
          data <- data[, -1]
        }
      }
    }
    
    # Standardize column and row name attributes
    if (is.null(colnames(data)) || all(colnames(data) == paste0("V", 1:ncol(data)))) {
      colnames(data) <- paste0(seq(0, length.out = ncol(data)), "uM")
    }
    if (is.null(rownames(data)) || all(rownames(data) == as.character(1:nrow(data)))) {
      rownames(data) <- paste0(seq(0, length.out = nrow(data)), "uM")
    }
    
    return(data)
  })
  
  # *** Perform robust synergy calculations reactive pipeline ***
  synergyResults <- reactive({
    df <- dataM()
    if (is.null(df)) return(NULL)
    
    shiny::validate(
      shiny::need(nrow(df) >= 2 && ncol(df) >= 2, "A checkerboard requires at least two rows and columns."),
      shiny::need(input$ctrlRow <= nrow(df) && input$ctrlCol <= ncol(df), "Control coordinates are outside the matrix.")
    )
    withProgress(message = "Evaluating synergy models...", value = 0.3, {
      setProgress(message = "Executing calculations...", value = 0.6)
      res <- calculate_synergy(
        xx = df, 
        data_type = input$dataType, 
        use_fit = input$useFit, 
        control_row = if (input$dataType == "viability") input$ctrlRow else 1,
        control_col = if (input$dataType == "viability") input$ctrlCol else 1
      )
      setProgress(message = "Finalizing visualization grids...", value = 0.9)
      Sys.sleep(0.15) # Brief pause so the progress bar is visible and satisfying
      res
    })
  })
  
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
      flip_z = isTRUE(input$flipDataZ)
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
          flip_z = isTRUE(input$flipDataZ)
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
          flip_z = isTRUE(input$flipDataZ)
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
    
    model <- input$synergyModel
    if (model == "Data") {
      scores <- res$raw_inhibition
      par_name <- "Inhibition"
    } else {
      scores <- res[[model]]$scores
      par_name <- paste(model, "Score")
    }
    
    max_score <- max(scores, na.rm = TRUE)
    min_score <- min(scores, na.rm = TRUE)
    mean_score <- mean(scores, na.rm = TRUE)
    
    max_idx <- which(scores == max_score, arr.ind = TRUE)[1, ]
    min_idx <- which(scores == min_score, arr.ind = TRUE)[1, ]
    
    par_A <- res$single_fit_A
    par_B <- res$single_fit_B
    
    data.frame(
      Scientific_Metric = c(
        paste("Max Synergy /", par_name),
        "Max Synergy Position (Drug A, Drug B)",
        paste("Max Antagonism / Min", par_name),
        "Max Antagonism Position (Drug A, Drug B)",
        "Mean Score across Screening Grid",
        "Drug A IC50 (Fitted 4PL Hill)",
        "Drug A Hill Slope (Fitted 4PL Hill)",
        "Drug B IC50 (Fitted 4PL Hill)",
        "Drug B Hill Slope (Fitted 4PL Hill)"
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
        if (!is.null(par_B)) sprintf("%.4f", par_B[4]) else "N/A (Linear Fallback)"
      ),
      stringsAsFactors = FALSE
    )
  })
  
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
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ))
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
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ))
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
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        raw_plot(res, input$synergyModel, theme_preset = input$themePreset,
                 theta = input$plotlyTheta, phi = input$plotlyPhi,
                 flip_x = isTRUE(input$flipDataX), flip_y = isTRUE(input$flipDataY),
                 flip_z = isTRUE(input$flipDataZ))
      }
      dev.off()
    },
    contentType = 'image/svg'
  )
  
})
