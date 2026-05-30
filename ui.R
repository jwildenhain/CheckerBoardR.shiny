#
# ui.R
# Beautiful, premium glassmorphic user interface for CheckerBoardR.shiny.
# Highly interactive controls for robust synergy calculation models.
#

library(shiny)
library(plotly)

shinyUI(fluidPage(
  # Include modern typography and high-end visual stylesheets
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&family=Inter:wght@300;400;500;600;700&display=swap"),
    tags$style(HTML("
      /* Design System Variables */
      :root {
        --bg-gradient: linear-gradient(135deg, #0b0f19 0%, #111827 50%, #030712 100%);
        --primary-gradient: linear-gradient(135deg, #0ea5e9 0%, #0284c7 100%);
        --accent-gradient: linear-gradient(135deg, #ec4899 0%, #db2777 100%);
        --glass-bg: rgba(17, 24, 39, 0.7);
        --glass-border: rgba(255, 255, 255, 0.08);
        --text-main: #f3f4f6;
        --text-muted: #9ca3af;
      }
      
      body {
        background: var(--bg-gradient);
        font-family: 'Outfit', 'Inter', sans-serif;
        color: var(--text-main);
        min-height: 100vh;
        padding-top: 20px;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Outfit', sans-serif;
        font-weight: 600;
        color: #ffffff;
      }
      
      .title-container {
        text-align: center;
        margin-bottom: 40px;
        padding: 20px;
        background: var(--glass-bg);
        border: 1px solid var(--glass-border);
        border-radius: 20px;
        box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.3);
        backdrop-filter: blur(12px);
      }
      
      .title-header {
        font-size: 2.5rem;
        background: var(--primary-gradient);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin: 0 0 10px 0;
      }
      
      .title-sub {
        font-size: 1.1rem;
        color: var(--text-muted);
      }
      
      /* Glassmorphic Sidebars & Cards */
      .well {
        background: var(--glass-bg) !important;
        border: 1px solid var(--glass-border) !important;
        border-radius: 20px !important;
        box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.4) !important;
        backdrop-filter: blur(12px) !important;
        color: var(--text-main) !important;
        padding: 24px !important;
        margin-bottom: 24px;
      }
      
      .tab-content {
        padding: 24px;
        background: var(--glass-bg);
        border: 1px solid var(--glass-border);
        border-top: none;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 10px 30px 0 rgba(0, 0, 0, 0.3);
        backdrop-filter: blur(12px);
      }
      
      /* Navigation Tabs Modernization */
      .nav-tabs {
        border-bottom: 1px solid var(--glass-border);
      }
      
      .nav-tabs > li > a {
        color: var(--text-muted) !important;
        font-weight: 500;
        border: 1px solid transparent !important;
        border-radius: 12px 12px 0 0 !important;
        padding: 12px 20px;
        transition: all 0.3s ease;
      }
      
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
        background: var(--primary-gradient) !important;
        color: #ffffff !important;
        border: 1px solid var(--glass-border) !important;
        border-bottom-color: transparent !important;
        box-shadow: 0 -4px 12px rgba(14, 165, 233, 0.2);
      }
      
      .nav-tabs > li > a:hover {
        background: rgba(255, 255, 255, 0.05) !important;
        color: #ffffff !important;
      }
      
      /* Beautiful Form Control Overrides */
      .form-control, input[type='text'], input[type='number'], select, textarea {
        background: rgba(15, 23, 42, 0.6) !important;
        border: 1px solid var(--glass-border) !important;
        border-radius: 10px !important;
        color: #ffffff !important;
        padding: 8px 12px !important;
        box-shadow: none !important;
        transition: all 0.3s ease !important;
      }
      
      .form-control:focus, input[type='text']:focus, select:focus {
        border-color: #0ea5e9 !important;
        box-shadow: 0 0 0 3px rgba(14, 165, 233, 0.25) !important;
      }
      
      /* Stylish Action Buttons */
      .btn-primary, .btn-default, .action-button {
        background: var(--primary-gradient) !important;
        border: none !important;
        border-radius: 10px !important;
        color: #ffffff !important;
        font-weight: 600;
        padding: 10px 20px !important;
        transition: all 0.3s ease !important;
        box-shadow: 0 4px 14px rgba(14, 165, 233, 0.3) !important;
      }
      
      .btn-primary:hover, .action-button:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(14, 165, 233, 0.5) !important;
      }
      
      /* Download Buttons */
      .shiny-download-link {
        background: var(--accent-gradient) !important;
        border: none !important;
        border-radius: 10px !important;
        color: #ffffff !important;
        font-weight: 600;
        padding: 10px 16px !important;
        display: inline-block;
        margin-right: 8px;
        margin-bottom: 12px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 14px rgba(236, 72, 153, 0.3);
      }
      
      .shiny-download-link:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(236, 72, 153, 0.5);
        color: #ffffff !important;
        text-decoration: none;
      }
      
      /* Custom radio buttons and checkbox spacing */
      .checkbox, .radio {
        margin-top: 12px;
        margin-bottom: 12px;
      }
      
      label {
        font-weight: 500;
        color: #e5e7eb;
      }
      
      /* Dynamic layout details */
      .shiny-plot-output {
        border-radius: 16px;
        overflow: hidden;
      }
    "))
  ),
  
  # Page Header Banner
  div(class = "title-container",
      h1(class = "title-header", "CheckerboardR Synergy Calculator"),
      p(class = "title-sub", "Next-generation web portal to calculate drug combination interaction landscapes (HSA, Bliss, Loewe, and ZIP) with publication-quality visualizations")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Controls for Data Upload Tab
      conditionalPanel(
        condition = "input.tabs1 == 'Data upload'",
        h4("Data Input Configuration"),
        radioButtons("dataInput", "Source:", list("Sample Data" = 1, "Upload File" = 2, "Paste Data" = 3)),
        
        conditionalPanel(
          condition = "input.dataInput == '1'",
          radioButtons("sampleData", "Sample Datasets:", 
                       list("Anti-fungal Screening (testData3.tab)" = 1,
                            "Anticancer Agent Grid (testData.tab)" = 2, 
                            "Fancy Sinus Wave" = 3, 
                            "Fancy Cosine Surface" = 5, 
                            "Simple Linear Plane" = 4))
        ),
        
        conditionalPanel(
          condition = "input.dataInput == '2'",
          fileInput("upload", "Upload delimited file (.csv, .tab, .txt):", multiple = FALSE),
          checkboxInput("fileHeader", "Header contains concentration levels", TRUE),
          radioButtons("fileSepDF", "Delimiter:", list("Comma (,)" = 1, "Tab (\\t)" = 2, "Semicolon (;)" = 3))
        ),
        
        conditionalPanel(
          condition = "input.dataInput == '3'",
          h5("Paste matrix below (columns = Drug A, rows = Drug B):"),
          tags$textarea(id = "myData", rows = 8, cols = 30, ""),
          br(),
          actionButton('clearText_button', 'Clear Field'),
          radioButtons("fileSepP", "Separator:", list("Comma (,)" = 1, "Tab (\\t)" = 2, "Semicolon (;)" = 3))
        ),
        
        hr(),
        h4("Data Preprocessing"),
        radioButtons("dataType", "Input Data Representation:",
                     list("Cell Viability / OD (requires control normalization)" = "viability",
                          "Normalized Inhibition / Cell Death percentage" = "inhibition")),
        
        conditionalPanel(
          condition = "input.dataType == 'viability'",
          h5("Control Well Coordinates:"),
          numericInput("ctrlRow", "Control Row (1-indexed):", value = 1, min = 1),
          numericInput("ctrlCol", "Control Column (1-indexed):", value = 1, min = 1)
        )
      ),
      
      # Controls for Data Visualization Tab
      conditionalPanel(
        condition = "input.tabs1 == 'Data visualization'",
        h4("Model & Algorithmic Choices"),
        selectInput("synergyModel", "Synergy Model Choice:",
                    choices = list("Bliss Independence" = "Bliss",
                                   "Highest Single Agent (HSA)" = "HSA",
                                   "Loewe Additivity" = "Loewe",
                                   "Zero Interaction Potency (ZIP)" = "ZIP",
                                   "Raw Input Data" = "Data")),
        
        conditionalPanel(
          condition = "input.synergyModel != 'Data'",
          radioButtons("myOrientation", "Biological Interpretation:",
                       list("Synergism (Positive Synergy = Red)" = "synergism",
                            "Antagonism (Antagonism = Blue)" = "antagonism"))
        ),
        
        checkboxInput("useFit", "Apply 4PL Hill Single-Agent Curve Fitting", TRUE),
        
        hr(),
        h4("Visualization Engine"),
        selectInput("plotEngine", "Plotting Engine:",
                    choices = list("2D Heatmap (ggplot2 Contoured)" = "2d_ggplot",
                                   "3D Surface (Interactive Plotly)" = "3d_plotly",
                                   "1D Single-Agent Fit Curves" = "1d_curves",
                                   "3D Static Fallback (Base R)" = "3d_base")),
        
        selectInput("themePreset", "Publication Style Theme:",
                    choices = list("Nature (Classic Grey)" = "Nature",
                                   "Science (High-Contrast White)" = "Science",
                                   "The Economist (Sleek Light Blue)" = "The Economist",
                                   "Financial Times (Warm Salmon)" = "Financial Times")),
        
        hr(),
        checkboxInput("labelsTitle", "Customize Labels & Title", FALSE),
        conditionalPanel(
          condition = "input.labelsTitle",
          textInput("myXlab", "X-axis Label (Drug A):", value = "Drug A"),
          textInput("myYlab", "Y-axis Label (Drug B):", value = "Drug B"),
          textInput("myTitle", "Custom Plot Title:", value = "")
        ),
        
        checkboxInput("plotSize", "Adjust Export Dimensions", FALSE),
        conditionalPanel(
          condition = "input.plotSize",
          numericInput("myHeight", "Height (pixels):", value = 550, min = 200),
          numericInput("myWidth", "Width (pixels):", value = 750, min = 200)
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 h3("Welcome to CheckerboardR Synergy Calculator"),
                 p("CheckerboardR is a professional, high-performance web tool designed for pharmacologists and cancer researchers to analyze combined drug screens. By uploading raw cell viability or optical density (OD) readings from multi-well plates, the tool calculates complete interaction surfaces to determine if a combination is synergistic, additive, or antagonistic."),
                 h4("Key Features"),
                 tags$ul(
                   tags$li("Flexible support for all four reference models: Highest Single Agent (HSA), Bliss Independence, Loewe Additivity, and Zero Interaction Potency (ZIP)."),
                   tags$li("Zero-dependency robust single-agent 4-parameter logistic (4PL) Hill curve fitting with linear monotonic interpolation fallbacks."),
                   tags$li("Stunning interactive 3D WebGL visualizations via Plotly and high-resolution 2D contoured heatmaps using ggplot2."),
                   tags$li("Export vectors directly in EPS, PDF, and SVG formats matching styling guidelines from premier scientific journals.")
                 ),
                 br(),
                 h5("Academic & Formula References"),
                 p(HTML("Borisy A, Keith C, et al. <i>Multicomponent therapeutics for networked systems.</i> Nat Rev Drug Discov. 2005;4(1):71-78.<br>"),
                   HTML("Yadav B, et al. <i>Zero Interaction Potency (ZIP) model for drug combinations.</i> Comput Struct Biotechnol J. 2015;13:504-513."))
        ),
        
        tabPanel("Data upload", 
                 h4("Uploaded Matrix Preview"),
                 p("A preview of the first 100 rows/columns of your uploaded matrix is displayed below. Ensure column names and row names represent concentration levels:"),
                 div(style = "overflow-x: auto; background-color: rgba(15, 23, 42, 0.4); border-radius: 12px; padding: 12px; border: 1px solid var(--glass-border);",
                     tableOutput("filetable")
                 )
        ),
        
        tabPanel("Data visualization",
                 div(style = "margin-bottom: 20px;",
                     downloadButton("downloadPlotPDF", "Download PDF Vector"),
                     downloadButton("downloadPlotSVG", "Download SVG Vector"),
                     downloadButton("downloadPlotEPS", "Download EPS Vector")
                 ),
                 
                 # Dynamic render UI depending on selection
                 conditionalPanel(
                   condition = "input.plotEngine == '3d_plotly'",
                   plotlyOutput("plotlyPlot", height = "550px")
                 ),
                 conditionalPanel(
                   condition = "input.plotEngine != '3d_plotly'",
                   plotOutput("ggplotPlot", height = "100%", width = "100%")
                 ),
                 
                 br(),
                 h4("Computed Synergy Summary Statistics"),
                 div(style = "overflow-x: auto; background-color: rgba(15, 23, 42, 0.4); border-radius: 12px; padding: 12px; border: 1px solid var(--glass-border);",
                     tableOutput("checkerboardStatsTable")
                 )
        ),
        
        id = "tabs1"
      )
    )
  )
))
