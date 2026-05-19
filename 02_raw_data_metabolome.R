BiocManager::install(version = "3.22")
BiocManager::install(c("xcms", "multtest"), ask = FALSE, update = TRUE)
library(xcms)
library(multtest)
library(BiocParallel)
library(xcms)
library(multtest)
library(BiocParallel)

setwd("E:/Metaboloma time course colonizacion bacteria/Roots/ESI +/10d")
xset <- xcmsSet(method='centWave', ppm=10, BPPARAM = SerialParam() , snthresh=10, prefilter=c(4,200), scanrange = c(40,1200), peakwidth = c(5,30), integrate=1,fitgauss=F,verbose.columns=T)
xset
xset <- group(xset)
xset2 <- retcor(xset, family="s", plottype="m")
xset2 <- group(xset, bw=10)
xset3<- fillPeaks(xset2)
reporttab <- diffreport(xset3, 'C', 'Bm', 'report ESI+ 10d', 20, metlin = 0.15)

