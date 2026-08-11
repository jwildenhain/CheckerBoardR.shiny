#
# ui.R
# Beautiful, premium glassmorphic user interface for CheckerBoardR.shiny.
# Highly interactive controls for robust synergy calculation models.
#

library(shiny)
library(plotly)

sample_download_choice <- function(description, output_id, filename) {
  tagList(
    description, " (",
    tags$span(
      class = "sample-file-link-wrap",
      onclick = "event.stopPropagation();",
      downloadLink(output_id, filename, class = "sample-file-link")
    ),
    ")"
  )
}

shinyUI(fluidPage(
  style = "padding: 30px; max-width: 1400px; margin: 0 auto;",
  
  # Include modern typography and high-end visual stylesheets
  tags$head(
    tags$style(HTML("
      /* Premium Light Blue Bootstrap Styling */
      body, h1, h2, h3, h4, h5, h6, .shiny-text-output, label {
        font-family: 'Outfit', 'Inter', sans-serif !important;
      }
      
      body {
        background: linear-gradient(135deg, #f0f7ff 0%, #f8fafc 100%);
        background-attachment: fixed;
        min-height: 100vh;
      }
      
      .header-bar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        background: rgba(255, 255, 255, 0.7);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border: 1px solid rgba(255, 255, 255, 0.4);
        padding: 18px 28px;
        border-radius: 20px;
        margin-top: 10px;
        margin-bottom: 28px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.04);
      }
      
      .header-bar h1 {
        font-size: 26px;
        font-weight: 700;
        margin: 0;
        background: linear-gradient(135deg, #0f172a 0%, #0369a1 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        display: flex;
        align-items: baseline;
        gap: 12px;
      }
      
      .header-bar .subtitle {
        font-size: 15px;
        font-weight: 400;
        color: #64748b;
        -webkit-text-fill-color: #64748b;
      }
      
      .version-badge {
        background: linear-gradient(135deg, #0ea5e9 0%, #0284c7 100%);
        color: white;
        font-size: 12px;
        font-weight: 600;
        padding: 4px 12px;
        border-radius: 99px;
        box-shadow: 0 4px 10px rgba(14, 165, 233, 0.2);
      }
      
      label.radio { display: inline-block; }
      .radio input[type=\"radio\"] { float: none; }
      
      /* Glassmorphism Sidebar */
      .well {
        background: rgba(255, 255, 255, 0.7) !important;
        backdrop-filter: blur(16px) !important;
        -webkit-backdrop-filter: blur(16px) !important;
        border: 1px solid rgba(255, 255, 255, 0.5) !important;
        border-radius: 20px !important;
        box-shadow: 0 15px 35px rgba(15, 23, 42, 0.04) !important;
        padding: 24px !important;
        margin-bottom: 24px;
      }
      
      .tab-content {
        padding: 28px !important;
        background: #ffffff !important;
        border: 1px solid rgba(226, 232, 240, 0.8) !important;
        border-top: none !important;
        border-radius: 0 0 24px 24px !important;
        box-shadow: 0 20px 40px rgba(15, 23, 42, 0.04) !important;
      }
      
      /* Navigation Tabs Modernization */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0 !important;
        margin-bottom: 24px !important;
        display: flex;
        gap: 6px;
      }
      
      .nav-tabs > li {
        margin-bottom: -2px !important;
      }
      
      .nav-tabs > li > a {
        border: none !important;
        border-radius: 12px 12px 0 0 !important;
        padding: 10px 18px !important;
        color: #64748b !important;
        font-weight: 600 !important;
        background: transparent !important;
        transition: all 0.25s ease !important;
        font-size: 14.5px !important;
      }
      
      .nav-tabs > li > a:hover {
        color: #0f172a !important;
        background: rgba(14, 165, 233, 0.05) !important;
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        color: #ffffff !important;
        background: linear-gradient(135deg, #0ea5e9 0%, #0284c7 100%) !important;
        box-shadow: 0 8px 20px rgba(14, 165, 233, 0.2) !important;
      }
      
      /* Form Controls */
      .form-control, input[type=\"text\"], input[type=\"number\"], select, textarea {
        background: rgba(255, 255, 255, 0.9) !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 10px !important;
        padding: 8px 12px !important;
        font-size: 14px !important;
        transition: all 0.2s ease-in-out !important;
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.02) !important;
        color: #1e293b !important;
      }
      
      .form-control:focus, input[type=\"text\"]:focus, input[type=\"number\"]:focus, select:focus, textarea:focus {
        border-color: #0ea5e9 !important;
        box-shadow: 0 0 0 4px rgba(14, 165, 233, 0.15), inset 0 2px 4px rgba(0, 0, 0, 0.02) !important;
        outline: none !important;
      }
      
      label {
        font-weight: 600 !important;
        color: #334155 !important;
        font-size: 13.5px !important;
        margin-bottom: 6px !important;
      }
      
      /* Premium Selectize overrides to match style */
      .selectize-control .selectize-input {
        background: rgba(255, 255, 255, 0.9) !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 10px !important;
        padding: 6px 12px !important;
        font-size: 14px !important;
        transition: all 0.2s ease-in-out !important;
        box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.02) !important;
        color: #1e293b !important;
        height: 34px !important;
        min-height: 34px !important;
        line-height: 20px !important;
        box-sizing: border-box !important;
      }
      
      .selectize-control .selectize-input.focus {
        border-color: #0ea5e9 !important;
        box-shadow: 0 0 0 4px rgba(14, 165, 233, 0.15), inset 0 2px 4px rgba(0, 0, 0, 0.02) !important;
        outline: none !important;
        border-radius: 10px !important;
      }
      
      .selectize-dropdown {
        border-radius: 12px !important;
        border: 1px solid rgba(0, 0, 0, 0.05) !important;
        box-shadow: 0 10px 25px rgba(15, 23, 42, 0.08) !important;
        overflow: hidden !important;
        background: #ffffff !important;
        padding: 6px 0 !important;
      }
      
      .selectize-dropdown .selected {
        background: linear-gradient(135deg, #0ea5e9 0%, #0284c7 100%) !important;
        color: white !important;
        font-weight: 600 !important;
      }
      
      .selectize-dropdown .active {
        background: rgba(14, 165, 233, 0.08) !important;
        color: #0284c7 !important;
        font-weight: 500 !important;
      }
      
      /* Stylish Buttons */
      .btn-primary, .btn-default, .action-button {
        background: linear-gradient(135deg, #0ea5e9 0%, #0284c7 100%) !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 12px !important;
        padding: 10px 18px !important;
        font-weight: 600 !important;
        font-size: 13.5px !important;
        box-shadow: 0 4px 14px rgba(14, 165, 233, 0.3) !important;
        transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1) !important;
        margin-bottom: 5px;
      }
      
      .btn-primary:hover, .btn-default:hover, .action-button:hover {
        background: linear-gradient(135deg, #0284c7 0%, #0369a1 100%) !important;
        color: #ffffff !important;
        transform: translateY(-1.5px) !important;
        box-shadow: 0 6px 20px rgba(14, 165, 233, 0.4) !important;
      }
      
      .btn-primary:active, .btn-default:active, .action-button:active {
        transform: translateY(0.5px) !important;
      }
      
      /* Accent download buttons */
      .shiny-download-link {
        background: linear-gradient(135deg, #ec4899 0%, #db2777 100%) !important;
        border: none !important;
        border-radius: 12px !important;
        color: #ffffff !important;
        font-weight: 600 !important;
        padding: 10px 16px !important;
        display: inline-block;
        margin-right: 8px;
        margin-bottom: 12px;
        transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1) !important;
        box-shadow: 0 4px 14px rgba(236, 72, 153, 0.3) !important;
      }
      
      .shiny-download-link:hover {
        background: linear-gradient(135deg, #db2777 0%, #be185d 100%) !important;
        transform: translateY(-1.5px) !important;
        box-shadow: 0 6px 20px rgba(236, 72, 153, 0.5) !important;
        color: #ffffff !important;
        text-decoration: none !important;
      }

      /* Inline sample filenames remain links without inheriting export-button styling. */
      .sample-file-link-wrap { display: inline; }
      .sample-file-link.shiny-download-link,
      .sample-file-link.shiny-download-link:hover {
        display: inline !important;
        background: transparent !important;
        color: #0284c7 !important;
        border: none !important;
        border-radius: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        box-shadow: none !important;
        transform: none !important;
        font-weight: 700 !important;
        text-decoration: underline !important;
      }
      
      /* Card Layout for Tables & Previews */
      .table-card {
        background: #ffffff !important;
        border-radius: 20px !important;
        padding: 24px !important;
        box-shadow: 0 10px 30px rgba(15, 23, 42, 0.03) !important;
        border: 1px solid rgba(226, 232, 240, 0.8) !important;
        margin-top: 15px;
      }
      
      .table-card table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        margin-top: 12px;
      }
      
      .table-card th {
        background: #f8fafc;
        color: #475569;
        font-weight: 700;
        text-transform: uppercase;
        font-size: 11px;
        letter-spacing: 0.05em;
        padding: 12px 16px;
        border-bottom: 2px solid #e2e8f0;
      }
      
      .table-card td {
        padding: 12px 16px;
        border-bottom: 1px solid #f1f5f9;
        color: #334155;
        font-size: 13.5px;
      }
      
      .table-card tr:last-child td {
        border-bottom: none;
      }
      
      h4 {
        font-size: 18px !important;
        font-weight: 700 !important;
        color: #0f172a !important;
        margin-top: 0 !important;
        margin-bottom: 16px !important;
      }
      
      h5 {
        font-size: 15px !important;
        font-weight: 600 !important;
        color: #1e293b !important;
        margin-top: 18px !important;
        margin-bottom: 10px !important;
      }
      
      p {
        color: #475569 !important;
        line-height: 1.6 !important;
        font-size: 14px !important;
      }
      
      /* Custom radio buttons and checkbox spacing */
      .checkbox, .radio {
        margin-top: 12px;
        margin-bottom: 12px;
      }
      
      /* Dynamic layout details */
      .shiny-plot-output {
        border-radius: 16px;
        overflow: hidden;
      }
      
      /* Prevent custom input[type='text'] styles from bloating the selectize inline search box */
      .selectize-control .selectize-input > input[type='text'] {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
        margin: 0 !important;
        height: auto !important;
        min-height: 0 !important;
        line-height: inherit !important;
        box-sizing: border-box !important;
      }
      
      /* Beautiful custom sliders matching premium light blue theme */
      .irs-bar {
        background: linear-gradient(90deg, #0ea5e9 0%, #0284c7 100%) !important;
        border-top: 1px solid #0284c7 !important;
        border-bottom: 1px solid #0284c7 !important;
        height: 8px !important;
      }
      .irs-single {
        background: #0284c7 !important;
        font-weight: 600 !important;
        border-radius: 6px !important;
      }
      .irs-slider {
        background: #f8fafc !important;
        border: 2px solid #0284c7 !important;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1) !important;
        width: 18px !important;
        height: 18px !important;
        border-radius: 99px !important;
      }
      .irs-line {
        height: 8px !important;
        border-radius: 4px !important;
      }
    "))
  ),
  
  # Page Header Banner
  div(
    class = "header-bar",
    h1("CheckerboardR", span("a web-tool for synergy calculations", class = "subtitle")),
    span("v2.1.2", class = "version-badge")
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
          radioButtons(
            "sampleData", "Sample Datasets:",
            choiceNames = list(
              sample_download_choice("Anti-fungal Screening", "downloadSample1", "testData3.tab"),
              sample_download_choice("Synthetic Anticancer Synergy Grid", "downloadSample2", "anticancer_synergy.tab"),
              sample_download_choice("Antagonistic Combination", "downloadSample3", "antagonism.csv"),
              sample_download_choice("Chemotherapy Grid", "downloadSample4", "paclitaxel_carboplatin.json"),
              sample_download_choice("Antifungal Grid", "downloadSample5", "fluconazole_voriconazole.xml"),
              sample_download_choice("Excel Spreadsheet Grid", "downloadSample6", "testData.xlsx")
            ),
            choiceValues = as.character(seq_len(6)),
            selected = "1"
          )
        ),
        
        conditionalPanel(
          condition = "input.dataInput == '2'",
          fileInput("upload", "Upload one matrix or matched replicate matrices:", multiple = TRUE,
                    accept = c(".csv", ".tab", ".tsv", ".txt")),
          helpText("Multiple files are analysed as independent replicates and must use identical concentration labels."),
          checkboxInput("fileHeader", "Header contains concentration levels", TRUE),
          radioButtons("fileSepDF", "Delimiter:", list("Comma (,)" = 1, "Tab (\\t)" = 2, "Semicolon (;)" = 3)),
          numericInput("bootstrapIterations", "Replicate bootstrap iterations:", value = 200, min = 20, max = 5000, step = 20)
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

        selectInput("baselineMethod", "Fitted baseline correction:",
                    choices = list("None (preserve current behaviour)" = "none",
                                   "Correct negative inhibition values only" = "negative",
                                   "Correct the full inhibition matrix" = "all")),
        
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
                                   "Conservative Bliss/Loewe/HSA Consensus" = "Consensus",
                                   "Raw Input Data" = "Data")),

        selectInput("plotValue", "Matrix to visualize:",
                    choices = list("Synergy score" = "score", "Reference effect" = "reference",
                                   "Observed inhibition" = "observed")),

        selectInput("uncertaintyDisplay", "Replicate uncertainty labels:",
                    choices = list("None" = "none", "Mean ± SEM" = "sem", "Bootstrap 95% CI" = "ci")),
        
        conditionalPanel(
          condition = "input.synergyModel != 'Data'",
          radioButtons("myOrientation", "Biological Interpretation:",
                       list("Synergism (Positive Synergy = Red)" = "synergism",
                            "Antagonism (Antagonism = Blue)" = "antagonism"))
        ),
        
        checkboxInput("useFit", "Apply 4PL Hill Single-Agent Curve Fitting", TRUE),
        
        checkboxInput("flipDataX", "Flip data by X-axis (Drug A)", FALSE),
        checkboxInput("flipDataY", "Flip data by Y-axis (Drug B)", FALSE),
        checkboxInput("flipDataZ", "Flip data by Z-axis (Invert)", FALSE),
        
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
        
        conditionalPanel(
          condition = "input.plotEngine == '3d_plotly' || input.plotEngine == '3d_base'",
          hr(),
          h4("3D Camera View Settings"),
          sliderInput("plotlyTheta", "3D Rotation (Azimuth):", min = 0, max = 360, value = 45),
          sliderInput("plotlyPhi", "3D Elevation (Altitude):", min = -90, max = 90, value = 30),
          sliderInput("plotlyZoom", "3D Zoom (Distance):", min = 0.5, max = 3.0, value = 1.8, step = 0.1)
        ),
        
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
                 h4("Data Preprocessing & Formats"),
                 p(HTML("The synergy engine requires a 2D grid matrix where the columns represent concentrations of <b>Drug A</b>, rows represent concentrations of <b>Drug B</b>, and the control coordinates (dose = 0) are located at index <code>[1,1]</code>. The tool normalizes inputs by dividing each cell by the control value to establish relative viability values in the range <code>[0, 1.2]</code>, and derives cell inhibition values via <code>1 - Viability</code>.<br><br>",
                        "To cater to automated workflows, CheckerboardR supports three data formats:<br>",
                        "• <b>Delimited Tables (Tab/CSV):</b> Standard text files with headers representing concentration levels.<br>",
                        "• <b>Structured JSON Schema:</b> Auto-decodable formats representing drug metadata, viability matrices, and visual presets.<br>",
                        "• <b>Structured XML Schema:</b> Strongly-typed tag structures mapping drug components, dose gradients, and settings.")),
                 br(),
                 h5("Academic & Formula References"),
                 p(HTML("Borisy A, Keith C, et al. <i>Multicomponent therapeutics for networked systems.</i> Nat Rev Drug Discov. 2005;4(1):71-78.<br>"),
                   HTML("Yadav B, et al. <i>Zero Interaction Potency (ZIP) model for drug combinations.</i> Comput Struct Biotechnol J. 2015;13:504-513."))
        ),
        
        tabPanel("Data upload", 
                 h4("Uploaded Matrix Preview"),
                 p("A preview of the first 100 rows/columns of your uploaded matrix is displayed below. Ensure column names and row names represent concentration levels:"),
                 div(style = "overflow-x: auto; background-color: #ffffff; border-radius: 12px; padding: 12px; border: 1px solid rgba(226, 232, 240, 0.8); box-shadow: 0 4px 10px rgba(15, 23, 42, 0.02);",
                     tableOutput("filetable")
                 )
        ),
        
        tabPanel("Data visualization",
                 div(style = "margin-bottom: 20px;",
                     downloadButton("downloadPlotPDF", "Download PDF Vector"),
                     downloadButton("downloadPlotSVG", "Download SVG Vector"),
                     downloadButton("downloadPlotEPS", "Download EPS Vector"),
                     downloadButton("downloadMatrixCSV", "Export Score & Reference Matrices")
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
                 div(style = "overflow-x: auto; background-color: #ffffff; border-radius: 12px; padding: 12px; border: 1px solid rgba(226, 232, 240, 0.8); box-shadow: 0 4px 10px rgba(15, 23, 42, 0.02);",
                     tableOutput("checkerboardStatsTable")
                 ),
                 br(),
                 h4("Dose-pair Synergy Barometer"),
                 fluidRow(
                   column(6, selectInput("barometerA", "Drug A concentration:", choices = NULL)),
                   column(6, selectInput("barometerB", "Drug B concentration:", choices = NULL))
                 ),
                 plotOutput("barometerPlot", height = "330px"),
                 div(style = "overflow-x: auto; background-color: #ffffff; border-radius: 12px; padding: 12px; border: 1px solid rgba(226, 232, 240, 0.8);",
                     tableOutput("barometerTable"))
        ),
        
        tabPanel("News",
                 h4("News & Release Notes"),
                 h5("August 11, 2026"),
                 p(HTML("<b>v2.1.2 Data Provenance & Analysis Extension:</b><br>",
                        "• Added score, reference-effect, observed-response, and ZIP fitted-response matrix views.<br>",
                        "• Added matched-replicate SD, SEM, and bootstrap 95% confidence intervals, including labelled 2D heatmaps and full-precision CSV export.<br>",
                        "• Added conservative HSA/Bliss/Loewe Consensus analysis, dose-pair synergy barometers, and explicit fitted-baseline correction modes.<br>",
                        "• Replaced the duplicated anticancer example and normalized numeric-leading concentration labels across supported sample formats.<br>",
                        "• Made bundled sample filenames directly downloadable and derived Drug A/Drug B condition labels from underscore-separated filenames.<br>",
                        "• Added an editable Conditions metadata sheet to the Excel example and expanded numerical and Playwright regression coverage.")),
                 br(),
                 h5("May 31, 2026"),
                 p(HTML("<b>v1.1 Advanced Control & Performance Upgrade:</b><br>",
                        "• Integrated reactive 3D Camera View sliders directly into the sidebar to control azimuth, elevation, and zoom settings for reproducible plotly surfaces.<br>",
                        "• Restored the legacy blue-to-yellow color ramp for static 3D plots (`3d_base`) to maintain consistency with published papers.<br>",
                        "• Added FAQ section with comprehensive data formatting guide for flawless checkerboard matrix imports.<br>",
                        "• Implemented a real-time computation progress bar during reactive synergy evaluations.<br>",
                        "• Created a standardized Docker environment for seamless distribution and local execution.")),
                 br(),
                 h5("May 30, 2026"),
                 p(HTML("<b>v1.0 Complete Engine & Modern Visuals Redesign:</b><br>",
                        "• Full modernization of the R Shiny dashboard with a premium, sleek light theme layout.<br>",
                        "• Expanded calculation pipelines to support 4 reference synergy models: HSA, Bliss, Loewe, and ZIP.<br>",
                        "• Added 4PL Hill curve-fitting for accurate single-agent concentration-response profiling.")),
                 br(),
                 h5("April 23, 2015"),
                 p(HTML("<b>v0.1 Legacy Launch:</b><br>",
                        "• Original release with basic cell viability normalization, raw data previews, and early Bliss and HSA calculation routines."))
        ),
        
        tabPanel("FAQ",
                 h4("Frequently Asked Questions"),
                 h5("Q: What file formats are supported for loading data?"),
                 p(HTML("A: The web tool accepts standard tab-separated values (<code>.tab</code>, <code>.tsv</code>), comma-separated values (<code>.csv</code>), as well as structured <b>JSON</b> and <b>XML</b> payloads pasted directly in the input area.")),
                 br(),
                 h5("Q: How do the JSON and XML schemas look, and how are they parsed?"),
                 p(HTML("A: When pasted, the R backend automatically detects the start tags (<code>{</code> or <code>&lt;</code>), decodes the payload, and dynamically updates the visual selections (like synergy model, plotting engine, and orientation presets) to generate the graph automatically. Here are the schemas:")),
                 
                 h6("Structured JSON Schema Example (Paclitaxel + Carboplatin):"),
                 pre("{\n  \"drug_a\": \"Paclitaxel\",\n  \"drug_b\": \"Carboplatin\",\n  \"concentrations_a\": [0.0, 0.25, 0.5, 1.0, 2.0, 4.0],\n  \"concentrations_b\": [0.0, 5.0, 10.0, 20.0, 40.0, 80.0],\n  \"data_type\": \"viability\",\n  \"matrix\": [\n    [1.00, 0.95, 0.90, 0.82, 0.70, 0.55],\n    [0.92, 0.88, 0.82, 0.73, 0.60, 0.42],\n    [0.85, 0.80, 0.72, 0.61, 0.48, 0.32],\n    [0.72, 0.65, 0.58, 0.45, 0.35, 0.20],\n    [0.55, 0.48, 0.40, 0.30, 0.22, 0.12],\n    [0.38, 0.30, 0.22, 0.15, 0.10, 0.05]\n  ],\n  \"settings\": {\n    \"synergy_model\": \"Bliss\",\n    \"plot_engine\": \"2d_ggplot\",\n    \"theme_preset\": \"Nature\",\n    \"orientation\": \"synergism\"\n  }\n}"),
                 
                 h6("Structured XML Schema Example (Fluconazole + Voriconazole):"),
                 pre("&lt;drug_combination&gt;\n  &lt;drug_a name=\"Fluconazole\"&gt;\n    &lt;concentrations&gt;0,0.125,0.25,0.5,1.0,2.0&lt;/concentrations&gt;\n  &lt;/drug_a&gt;\n  &lt;drug_b name=\"Voriconazole\"&gt;\n    &lt;concentrations&gt;0,0.015,0.03,0.06,0.12,0.24&lt;/concentrations&gt;\n  &lt;/drug_b&gt;\n  &lt;data_representation&gt;viability&lt;/data_representation&gt;\n  &lt;matrix&gt;\n    &lt;row&gt;1.00,0.94,0.88,0.80,0.72,0.65&lt;/row&gt;\n    &lt;row&gt;0.95,0.89,0.82,0.75,0.68,0.60&lt;/row&gt;\n    &lt;row&gt;0.88,0.82,0.75,0.68,0.60,0.52&lt;/row&gt;\n    &lt;row&gt;0.78,0.72,0.65,0.58,0.50,0.42&lt;/row&gt;\n    &lt;row&gt;0.65,0.58,0.50,0.42,0.35,0.28&lt;/row&gt;\n    &lt;row&gt;0.50,0.42,0.35,0.28,0.20,0.12&lt;/row&gt;\n  &lt;/matrix&gt;\n  &lt;settings&gt;\n    &lt;synergy_model&gt;Loewe&lt;/synergy_model&gt;\n    &lt;plot_engine&gt;2d_ggplot&lt;/plot_engine&gt;\n    &lt;theme_preset&gt;Science&lt;/theme_preset&gt;\n    &lt;orientation&gt;synergism&lt;/orientation&gt;\n  &lt;/settings&gt;\n&lt;/drug_combination&gt;"),
                 br(),
                 h5("Q: What is the Model Context Protocol (MCP) server, and how do I configure it?"),
                 p(HTML("A: CheckerBoardR.shiny includes an stdio-based Python MCP server (<code>checkerboardr_mcp_server.py</code>). ",
                        "This lets AI assistants (like Claude, Cursor, or Antigravity) programmatically call the synergy calculation engine ",
                        "and save rendered synergy graphs directly to your drive using R. Add the following to your MCP client config file:")),
                 pre("{\n  \"mcpServers\": {\n    \"checkerboardr-mcp\": {\n      \"command\": \"python3\",\n      \"args\": [\n        \"/home/jw/Source/CheckerBoardR.shiny/checkerboardr_mcp_server.py\"\n      ]\n    }\n  }\n}"),
                 br(),
                 h5("Q: How should the standard delimited matrix rows and columns look?"),
                 p(HTML("A: When using raw files, the file must include a header row containing the concentration levels for Drug A (e.g. <code>0uM, 0.25uM, 0.5uM</code>) ",
                        "and the first column must represent the concentration levels for Drug B. The remaining cells must contain numeric raw cell viability values (such as cell density or OD) or inhibition fractions.")),
                 br(),
                 h5("Q: Example format of a standard delimited grid:"),
                 pre("conc\t0uM\t.25uM\t.5uM\t1uM\t2uM\n0uM\t0.98\t0.95\t0.90\t0.85\t0.80\n.5uM\t0.92\t0.88\t0.82\t0.75\t0.70\n1uM\t0.85\t0.80\t0.72\t0.60\t0.55\n2uM\t0.75\t0.70\t0.60\t0.50\t0.42"),
                 br(),
                 h5("Q: How do I build and run the Docker container locally?"),
                 p(HTML("A: Run these standard shell commands inside the project root directory:<br>",
                        "<pre><code>docker build -t checkerboardr .</code></pre>",
                        "Then run the container:<br>",
                        "<pre><code>docker run -d -p 3838:3838 --name checkerboardr-app checkerboardr</code></pre>",
                        "Now, open <code>http://localhost:3838</code> in your browser to run the full application!"))
        ),
        
        id = "tabs1"
      )
    )
  )
))
