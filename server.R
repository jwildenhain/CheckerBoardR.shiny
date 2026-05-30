#
# server.R
# Premium server logic for CheckerBoardR.shiny.
# Connects the new calculation engine and visualizers with dynamic reactivity.
#

library(shiny)
library(plotly)
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
        data <- read.table("sinus.csv", sep = ",", header = FALSE)
      } else if (input$sampleData == 4) {
        data <- read.table("simple.csv", sep = ",", header = TRUE, row.names = 1)
      } else {
        data <- read.table("cos.csv", sep = ",", header = TRUE, row.names = 1)
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
      
      tmp <- matrix(strsplit(input$myData, "\n")[[1]])
      mySep <- switch(input$fileSepP, '1' = ",", '2' = "\t", '3' = ";")
      
      # Determine row and column elements
      myColnames <- strsplit(tmp[1], mySep)[[1]]
      data <- matrix(0, length(tmp) - 1, length(myColnames))
      colnames(data) <- myColnames
      
      for (i in 2:length(tmp)) {
        myRow <- as.numeric(strsplit(paste(tmp[i], mySep, mySep, sep = ""), mySep)[[1]])
        data[i - 1, ] <- myRow[1:ncol(data)]
      }
      
      # Handle row labels if non-numeric or explicit
      data <- data.frame(data)
      if (is.na(as.numeric(data[1, 1])) || all(data[, 1] == seq_len(nrow(data)))) {
        rownames(data) <- data[, 1]
        data <- data[, -1]
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
    
    # Run the synergy calculator engine
    calculate_synergy(
      xx = df, 
      data_type = input$dataType, 
      use_fit = input$useFit, 
      control_row = if (input$dataType == "viability") input$ctrlRow else 1,
      control_col = if (input$dataType == "viability") input$ctrlCol else 1
    )
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
    
    plotly_synergy_surface(res, input$synergyModel, input$themePreset)
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
        p <- ggplot_synergy_heatmap(res, input$synergyModel, input$myOrientation, input$themePreset, input$myTitle)
        print(p)
      } else if (input$plotEngine == "1d_curves") {
        p <- ggplot_single_agent_fits(res, input$themePreset)
        print(p)
      } else {
        # Fallback Base R 3D view
        raw_plot(res$raw_inhibition)
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
        raw_plot(res$raw_inhibition)
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
        raw_plot(res$raw_inhibition)
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
        raw_plot(res$raw_inhibition)
      }
      dev.off()
    },
    contentType = 'image/svg'
  )
  
})
