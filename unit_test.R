source("Make3DPlotFunctions.R")

df <- read.csv("testData3.tab", sep="\t")
raw_plot(df)

df <- read.csv("sinus.csv")
raw_plot(df)

df <- read.csv("sinus.csv")
raw_plot(df * -1)

df <- round(matrix(seq(0, 1, len=16), 4, 4),2)
rownames(df) <- c("A","B","C","D")
colnames(df) <- c("A","B","C","D")
write.csv(df, "simple.csv")
raw_plot(df)

x<-seq(-5.12, 5.12, length=50)
y<-seq(-5.12, 5.12, length=50)
f<-function(x,y) { 20+(x^2-10*cos(.5*3.14*x))+
    (y^2-10*cos(.5*3.14*y)) }
df <-outer(x,y,f)
write.csv(df, "cos.csv")
raw_plot(df)

z <- df
nrz<-nrow(z)
ncz<-ncol(z)
jet.colors <-  colorRampPalette(c("midnightblue","blue",
                                  "cyan","green", "yellow","orange","red", "darkred"))
nbcol<-64
color<-jet.colors(nbcol)
zfacet<-z[-1,-1]+z[-1,-ncz]+z[-nrz,-1]+z[-nrz,-ncz]
facetcol<-cut(zfacet,nbcol)
persp(x,y,z,col=color[facetcol],phi=20,theta=-60,
      ticktype="detailed",d=5,r=1,shade=0.1,expand=0.6) 

