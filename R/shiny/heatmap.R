# Programma per l'interpolazione della mappa di temperatura ottenuta 
# con il sensore AMG8833
#rm(list = ls())

library(oce)
library(fields)
require(graphics)

# inserire il path della cartella in cui è contenuto il file
# "matrixIR_T.txt"
setwd("/home/USER/Documents/acquarioClimatico/wiringAndProgs/gridEye_gfx")

jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
{
  thisdata <- paste("_",format(Sys.time(), format="%Y-%m-%d_%H:%M:%S"), sep = "")
  Sys.sleep(20)
  vettore <- read.table(file =
                        "/home/USER/Documents/acquarioClimatico/wiringAndProgs/gridEye_gfx/matrixIR_T.txt", header = FALSE, sep = " ", dec = ".")
  vettore <- vettore[1:64]
  #-----------------------------------------------------------------
  matrice <- matrix(data = as.numeric(vettore), nrow = 8, ncol = 8)
  image(matrice, col = (jet.colors(256)))
  #----
  inc.matrice<- matrice[rep(1:nrow(matrice), each=25), rep(1:ncol(matrice), each=25)]
  image.plot(matrixSmooth(inc.matrice, passes = 1000), col = (jet.colors(256)))
  image.plot(matrixSmooth(inc.matrice, passes = 1000), col = (jet.colors(256)), zlim=c(20,30))
  #----------------------
  png(filename=paste("gridEye",thisdata,".png", sep = ""), width=1600, height=1600, res=300)
  image.plot(matrixSmooth(inc.matrice, passes = 1000), col = (jet.colors(256)))
  dev.off()
  #
  png(filename=paste("gridEye_lim",thisdata,".png", sep = ""), width=1600, height=1600, res=300)
  image.plot(matrixSmooth(inc.matrice, passes = 1000), col = (jet.colors(256)), zlim=c(20,30))
  dev.off()
}
