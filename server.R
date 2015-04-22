
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source("Make3DPlotFunctions.R")

shinyServer(function(input, output) {
   
  observe({
    if (input$clearText_button == 0) return()
    isolate({ updateTextInput(session, "myData", label = ",", value = "") })
  })
  # *** Read in data matrix ***
  dataM <- reactive({
    if(input$dataInput==1){
      if(input$sampleData==1){
        data<-read.table("testData3.tab", sep="\t", header=FALSE)			
      } else {
        data<-read.table("testData.tab", sep="\t", header=TRUE,row.names=1)		
      }
    } else if(input$dataInput==2){
      inFile <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload))  {return(NULL)}
      
      # Get the separator
      mySep<-switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="") #list("Comma"=1,"Tab"=2,"Semicolon"=3)
      if(file.info(inFile$datapath)$size<=10485800){
        if (input$fileHeader) { 
           data<-read.table(inFile$datapath, sep=mySep, header=TRUE,row.names=1,fill=TRUE)
        } else {
           data<-read.table(inFile$datapath, sep=mySep, header=FALSE, fill=TRUE)
        }
      } else print("File is bigger than 10MB and will not be uploaded.")
    } else { # To be looked into again - for special case when last column has empty entries in some rows
      if(is.null(input$myData)) {return(NULL)} 
      tmp<-matrix(strsplit(input$myData, "\n")[[1]])
      mySep<-switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
      myColnames<-strsplit(tmp[1], mySep)[[1]]
      data<-matrix(0, length(tmp)-1, length(myColnames))
      colnames(data)<-myColnames
      rownames(data)<-myColnames[1:nrow(data)]
      for(i in 2:length(tmp)){
        myRow<-as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
        data[i-1,]<-myRow[-length(myRow)]
      }
      data<-data.frame(data)		
    }
    return(data)
  })
  # *** The plot dimensions ***
  heightSize <- reactive ({ input$myHeight })
  widthSize <- reactive ({ input$myWidth })
  
  ## *** Data in table ***
  output$filetable <- renderTable({
    print(nrow(dataM()))
    if(nrow(dataM())<500){
      return(dataM())
    } else {return(dataM()[1:100,])}
  })

  
  
  # *** Generate the box plot ***
  generateCheckerboardPlot<-function(xx, ...){
    
    showplot <- TRUE
    if( length(list(...)) ){
      Lst <- list(...)
      if( !is.null(Lst$plot) ){
        showplot <- Lst$plot
      }
    }
    
    if(input$inverseData==TRUE){
      xx <- create_inverse(xx)
    }
    
    if(input$flipDataX==TRUE){
      xx <- create_x_flip(xx)
    }
    if(input$flipDataY==TRUE){
      xx <- create_y_flip(xx)
    }
    
    par(mar=c(2.1, 2.1, 4.1, 2.1))
    # *** Generate xyz raw data plot ***    
    if(input$plotType=='0'){
       if (showplot == TRUE) {
          raw_plot(xx,input$myXlab,input$myYlab,input$myZlab,input$myTitle,input$obs,input$obsv,input$cexTitle,input$cexAxislabel,input$cexAxis)
       } else {
          my.stat <- data.frame(stats="NA")
       }
    } else { 
    # *** Generate synergy stats ***
      max_tmp  <- (1 - xx/max(xx,na.rm=TRUE))
      max_tmp_x <- which.max(max_tmp[1,])
      max_tmp_y <- which.max(max_tmp[,1])
      max_real <- max(max_tmp)
      max_real_exp <- max_tmp[max_tmp_y,max_tmp_x]
       
      if(input$otherPlotType==0) { # Bliss
        bliss <- bliss_calculus(xx)
        reverse <- 1
        if (max(bliss) < abs(min(bliss))) {
          reverse <- -1
          max_bliss <- find_xy(abs(bliss))   
        } else {
          max_bliss <- find_xy(bliss)
        }
        
        if (showplot == TRUE) {
          if(input$myOrientation==1) { # address Antagonism
            bliss.show <- bliss
            bliss.show[bliss.show > 0] <-0
            myImagePlotReverse(abs(bliss.show),reverse=-1,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle="Bliss Antagonism") # =-1 
          } else {
            bliss.show <- bliss
            bliss.show[bliss.show < 0] <-0
            myImagePlotReverse(bliss.show,reverse,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle="Bliss Synergy")
          }
        }
        my.bliss <- max_tmp[max_bliss$y,max_bliss$x] - ( max_tmp[1,max_bliss$x] + max_tmp[max_bliss$y,1] - (max_tmp[1,max_bliss$x] * max_tmp[max_bliss$y,1]) )
        my.stat <- data.frame(BLISS=my.bliss,Idx_A=max_bliss$x,Idx_B=max_bliss$y,value_A=max_tmp[1,max_bliss$x],value_B=max_tmp[max_bliss$y,1],value_AB=max_tmp[max_bliss$y,max_bliss$x])
      }
      if(input$otherPlotType==1) { # HSA
        reverse <- 1
        hsa <- hsa_calculus(xx)
        if (max(hsa) < abs(min(hsa))) {
          reverse <- -1
          max_hsa <- find_xy(abs(hsa))
        } else {
          max_hsa <- find_xy(hsa)
        }
        # myImagePlot(hsa)
        if (showplot == TRUE) {
          
          if(input$myOrientation==1) { # address Antagonism
            hsa.show <- hsa
            hsa.show[hsa.show > 0] <-0
            myImagePlotReverse(abs(hsa.show),reverse=-1,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle="HSA Antagonism")
          } else { # synergism
            hsa.show <- hsa
            hsa.show[hsa.show < 0] <-0
            myImagePlotReverse(hsa.show,reverse,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle="HSA Synergy")
          }
        }
        my.hsa <- max_tmp[max_hsa$y,max_hsa$x] - ( max_tmp[1,max_hsa$x] + max_tmp[max_hsa$y,1] - (max_tmp[1,max_hsa$x] * max_tmp[max_hsa$y,1]) )
        my.stat <- data.frame(HSA=my.hsa,Idx_A=max_hsa$x,Idx_B=max_hsa$y,value_A=max_tmp[1,max_hsa$x],value_B=max_tmp[max_hsa$y,1],value_AB=max_tmp[max_hsa$y,max_hsa$x])
      }
      if(input$otherPlotType==2) { # raw
        max_inh <- find_xy(max_tmp)
        my.stat <- data.frame(Max_Inhibition_AB=max_real,Idx_A=max_inh$x,Idx_B=max_inh$y,Max_Inhibition_A=max_tmp[1,max_tmp_x],Max_Inhibition_B=max_tmp[max_tmp_y,1],Max_Inhibition_Expected_AB=max_real_exp,Idx_Exp_A=max_tmp_x,Idx_Exp_B=max_tmp_y)
        if (showplot ==TRUE) {
          if(input$myOrientation==1) {
            myImagePlotReverse(xx,reverse=-1,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle=input$myZlab)
          } else {
            myImagePlotReverse(xx,reverse=1,xLab=input$myXlab,yLab=input$myYlab, title=input$myTitle,cTitle=input$myZlab)          
          }
        }
      }
           
    }
    if (showplot == FALSE) {
      df.basics <- data.frame(ConditionA=input$myXlab,ConditionB=input$myYlab,Observation=input$myZlab)
      df.settings <- data.frame(InverseDataMatrix=input$inverseData,FlipDataMatrixOnX=input$flipDataX,FlipDataMatrixOnY=input$flipDataY) # input$myOrientation,input$otherPlotType
      tmp <- cbind(df.basics,df.settings,my.stat)
      rownames(tmp) <- c("Values")
      return(t(tmp))
    }
    
  }
  
  output$rawPlot <- renderPlot({
    
    # generate and plot checkerboard data
    generateCheckerboardPlot(dataM())
    
  }, height = heightSize, width = widthSize)
  
  # *** Output checkerboard data in table below plot ***
  output$checkerboardStatsTable <- renderTable({
    M <- generateCheckerboardPlot(dataM(),plot=FALSE)
    M
  })
  
  
  
  ## *** Download EPS file ***
  output$downloadPlotEPS <- downloadHandler(
    filename <- function() { paste('Checkerboard.eps') },
    content <- function(file) {
      postscript(file, horizontal = FALSE, onefile = FALSE, paper = "special", width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    },
    contentType = 'application/postscript'
  )
  ## *** Download PDF file ***
  output$downloadPlotPDF <- downloadHandler(
    filename <- function() { paste('Checkerboard.pdf') },
    content <- function(file) {
      pdf(file, width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    },
    contentType = 'application/pdf' # MIME type of the image
  )
  ## *** Download SVG file ***
  output$downloadPlotSVG <- downloadHandler(
    filename <- function() { paste('Checkerboard.svg') },
    content <- function(file) {
      svg(file, width = input$myWidth/72, height = input$myHeight/72)
      ## ---------------
      generateCheckerboardPlot(dataM())
      ## ---------------
      dev.off()
    },
    contentType = 'image/svg'
  )
  
  
})
