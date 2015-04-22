
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("CheckerboardR: a web-tool for generation of checkerboards",
              tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
              tags$style(type="text/css", "select { max-width: 200px; }"),
              tags$style(type="text/css", "textarea { max-width: 185px; }"),
              tags$style(type="text/css", ".jslider { max-width: 200px; }"),
              tags$style(type='text/css', ".well { max-width: 330px; }"),
              tags$style(type='text/css', ".span4 { max-width: 330px; }"))                       
  ),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    conditionalPanel(condition="input.tabs1=='About'",
                     h4("Introduction")
    ),
    conditionalPanel(condition="input.tabs1=='Data upload'",
                     h4("Enter data"),
                     radioButtons("dataInput", "", list("Load sample data"=1,"Upload file"=2,"Paste data"=3)),
                     conditionalPanel(condition="input.dataInput=='1'",
                                      h5("Load sample data:"),
                                      radioButtons("sampleData", "Load sample data", list("Example 1"=1,"Example 2"=2))
                     ),
                     conditionalPanel(condition="input.dataInput=='2'",
                                      h5("Upload delimited text file: "),
                                      fileInput("upload", "", multiple = FALSE),
                                      checkboxInput("fileHeader", "File with row and column concentrations", FALSE),
                                      radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3)),#, "Space"=4))
                                      HTML('<p>Data in <a href="http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon. 
				For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
                     ),
                     conditionalPanel(condition="input.dataInput=='3'",
                                      h5("Paste data below:"),
                                      #				HTML('<input type="button" value="Clear" onclick="javascript:eraseText();">'),
                                      #			    HTML('<textarea id='output' rows=20 cols=90></textarea>'),
                                      #			    HTML('<textarea id="output" rows="3" cols="40">Default value</textarea>'),
                                      tags$textarea(id="myData", rows=10, cols=5, ""),
                                      actionButton('clearText_button','Clear data'),
                                      radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3))
                     )
    ),
    conditionalPanel(condition="input.tabs1=='Data visualization'",
                     
    checkboxInput("inverseData", "Inverse data matrix", FALSE),
    checkboxInput("flipDataX", "Flip data by x-axis", FALSE),
    checkboxInput("flipDataY", "Flip data by y-axis", FALSE),
    radioButtons("plotType", "", list("3D"=0, "2D"=1)),
    conditionalPanel(condition="input.plotType=='0'",
                     h5("Rotate view:"),
                     sliderInput("obs", 
                                 "Horizontal:", 
                                 min = 1, 
                                 max = 360, 
                                 value = 110),
                     sliderInput("obsv", 
                                 "Vertical:", 
                                 min = 1, 
                                 max = 360, 
                                 value = 30)                     
    ),
    conditionalPanel(condition="input.plotType=='1'",
                     h5("Algorithm choices:"),
                     radioButtons("otherPlotType", "", list("Bliss"=0, "HSA"=1, "Data"=2)),
                     h5("Schema for Synergy or Antagonism:"),
                     radioButtons("myOrientation", "", list("Synergism"=0, "Antagonism"=1))
                     
    ),
    checkboxInput("labelsTitle", "Modify labels and title", FALSE),
    conditionalPanel(condition="input.labelsTitle",
                     textInput("myXlab", "X-axis label:", value=c("Drug A")),
                     textInput("myYlab", "Y-axis label:", value=c("Drug B")),
                     textInput("myZlab", "Z-axis label:", value=c("Absorbance")),
                     textInput("myTitle", "Title:", value=c(""))
 
    ),
    checkboxInput("plotSize", "Adjust plot size", FALSE),
    conditionalPanel(condition="input.plotSize",
                     numericInput("myHeight", "Plot height:", value=550),
                     numericInput("myWidth", "Plot width:", value=750)
    ),
    checkboxInput("fontSizes", "Change font sizes", FALSE),
    conditionalPanel(condition="input.fontSizes",
                     numericInput("cexTitle", "Title font size:", value=10),
                     numericInput("cexAxislabel", "Axis label size:", value=10),
                     numericInput("cexAxis", "Axis font size:", value=10)
    )
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      # Welcome tab
      tabPanel("About",
               HTML('<p>This application allows users to generate customized Checkerboards with Plots can be exported as eps, pdf and svg files.</p>'),
               h5("Software references"),
               HTML('<p>R Development Core Team. <i><a href="http://www.r-project.org/">R</a>:  A Language and Environment for Statistical Computing.</i> R Foundation for Statistical Computing, Vienna (2013) <br>
				RStudio and Inc. <i><a href="http://www.rstudio.com/shiny/">shiny</a>: Web Application Framework for R.</i> R package version 0.5.0 (2013) <br>'),
               h5("Further references"),
               h6("This application was created by jw. This application uses the ", 
                  a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
               ),
      # Data upload tab
      tabPanel("Data upload", tableOutput("filetable"),
               h6("This application was created by jw. This application uses the ", 
                  a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
      ),
      # Raw tab
      #tabPanel("Data visualization", downloadButton("downloadPlotEPS", "Download eps-file"),
      #  plotOutput("distPlot")
      #),
      tabPanel("Data visualization", 
               downloadButton("downloadPlotEPS", "Download eps-file"),
               downloadButton("downloadPlotPDF", "Download pdf-file"),
               downloadButton("downloadPlotSVG", "Download svg-file"),
               plotOutput("rawPlot", height='100%', width='100%'),
               h4("Data statistics"), 
               tableOutput("checkerboardStatsTable")
      ),
      id="tabs1"
    ) # tabset panel
  )
))
