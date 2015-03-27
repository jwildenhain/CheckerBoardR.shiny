
# Submit checkerboard OD readings and obtain list with two entries: 
#   1) re-oriented checkerboards (lowest concentration in top left corner
#   2) matrix with BLISS values
# These can them be used for visualisation (as one heatmap or separate heatmaps) and output
# Format:
# 'mini' mini checkerboards, 6 checkerboards, control wells are bottom right
# 'normal' single checkerboard with control well top left
# 'TC' tissue culture - columns 1, 2 and 11 are controls, 4 checkerboards in the middle
determine.BLISS<-function(xx, matrixFormat="normal"){

	if(matrixFormat=="normal"){
		myOD <- as.matrix(xx)
		myBliss <- bliss_calculus(xx)
	} else if (matrixFormat=="mini"){
		# go through all 6 checkerboards, flip them (create_inverse) and get BLISS for each separately
		myOD<-matrix(0,8,12, dimnames=list(c(LETTERS[1:8]), c(1:12)))
		myBliss<-matrix(0,8,12, dimnames=list(c(LETTERS[1:8]), c(1:12)))
		pos<-matrix(0,4,6)
		pos[1,]<-c(1,1,1,5,5,5)
		pos[2,]<-c(4,4,4,8,8,8)
		pos[3,]<-c(1,5,9,1,5,9)
		pos[4,]<-c(4,8,12,4,8,12)
		
		for(i in 1:6){
			myOD[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]] <- as.matrix(create_inverse(xx[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]]))
			myBliss[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]] <- bliss_calculus(myOD[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]])
		}
	} else if (matrixFormat=="TC"){
		myOD<-matrix(0,8,12, dimnames=list(c(LETTERS[1:8]), c(1:12)))
		myBliss<-matrix(0,8,12, dimnames=list(c(LETTERS[1:8]), c(1:12)))
		pos<-matrix(0,4,4)
		pos[1,]<-c(1,1,5,5)
		pos[2,]<-c(4,4,8,8)
		pos[3,]<-c(3,7,3,7)
		pos[4,]<-c(6,10,6,10)
		for(i in 1:4){
			myOD[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]] <- as.matrix(create_inverse(xx[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]]))
			myBliss[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]] <- bliss_calculus(myOD[pos[1,i]:pos[2,i], pos[3,i]:pos[4,i]])
		}
	}
	
	return(list(round(myOD, 2), round(myBliss, 2)))
}

##########################################################################
# Bliss calculus according to                                            #
# Multicomponent therapeutics for networked systems Keith C. Borisy A.   #
#                                                                        #
##########################################################################
bliss_calculus <- function(xx) {
  
   z <- as.matrix(xx)
#   z <- z/(max(z)) # was max(z) before, now z[1,1]
   z <- z/z[1,1] # was max(z) before, now z[1,1]
   z <- 1 - z
   bm <- as.matrix(xx)

   a <- z[1,]
   b <- z[,1]

   for (i in 1:length(a)) {
      for (j in 1:length(b)) {
         bm[j,i] <-  a[i] + b[j] - ( a[i]*b[j] )
      }
   }

   return(z-bm)
}
