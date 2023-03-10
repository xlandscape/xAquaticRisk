### R code from vignette source 'datasets.Rnw'

###################################################
### code chunk number 1: datasets.Rnw:5-6
###################################################
options(SweaveHooks=list(fig=function() par(mar=c(1,1,1,1))))


###################################################
### code chunk number 2: datasets.Rnw:27-34
###################################################
library(spatstat)
sdate <- read.dcf(file = system.file("DESCRIPTION", package = "spatstat"),
         fields = "Date")
sversion <- read.dcf(file = system.file("DESCRIPTION", package = "spatstat"),
         fields = "Version")
spatstat.options(transparent=FALSE)
options(useFancyQuotes=FALSE)


###################################################
### code chunk number 3: datasets.Rnw:219-237
###################################################
opa <- par()
## How to set all margins to zero and eliminate all outer spaces
zeromargins <- function() {
  par(
      mar=rep(0,4),
      omd=c(0,1,0,1),
      xaxs="i",
      yaxs="i"
  )
  invisible(NULL)
}
## Set 'mar'
setmargins <- function(...) {
  x <- c(...)
  x <- rep(x, 4)[1:4]
  par(mar=x)
  invisible(NULL)
}


###################################################
### code chunk number 4: datasets.Rnw:246-247 (eval = FALSE)
###################################################
## plot(amacrine)


###################################################
### code chunk number 5: datasets.Rnw:249-251
###################################################
getOption("SweaveHooks")[["fig"]]()
setmargins(0,1,2,0)
plot(amacrine)


###################################################
### code chunk number 6: datasets.Rnw:260-261 (eval = FALSE)
###################################################
## plot(anemones, markscale=1)


###################################################
### code chunk number 7: datasets.Rnw:263-265
###################################################
getOption("SweaveHooks")[["fig"]]()
setmargins(0,0,2,0)
plot(anemones, markscale=1)


###################################################
### code chunk number 8: datasets.Rnw:278-279 (eval = FALSE)
###################################################
## ants.extra$plotit()


###################################################
### code chunk number 9: datasets.Rnw:281-283
###################################################
getOption("SweaveHooks")[["fig"]]()
setmargins(0,0,1,0)
ants.extra$plotit()


###################################################
### code chunk number 10: datasets.Rnw:291-292
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(austates)


###################################################
### code chunk number 11: datasets.Rnw:302-304 (eval = FALSE)
###################################################
## plot(bdspots, equal.scales=TRUE, pch="+", 
##      panel.args=function(i)list(cex=c(0.15, 0.2, 0.7)[i]))


###################################################
### code chunk number 12: datasets.Rnw:306-310
###################################################
getOption("SweaveHooks")[["fig"]]()
zeromargins()
plot(bdspots, equal.scales=TRUE, pch="+", main="",
     mar.panel=0, hsep=1,
     panel.args=function(i)list(cex=c(0.15, 0.2, 0.7)[i]))


###################################################
### code chunk number 13: datasets.Rnw:320-322 (eval = FALSE)
###################################################
## plot(bei.extra$elev, main="Beilschmiedia")
## plot(bei, add=TRUE, pch=16, cex=0.3)


###################################################
### code chunk number 14: datasets.Rnw:324-327
###################################################
getOption("SweaveHooks")[["fig"]]()
setmargins(0,0,2,0)
plot(bei.extra$elev, main="Beilschmiedia")
plot(bei, add=TRUE, pch=16, cex=0.3)


###################################################
### code chunk number 15: datasets.Rnw:333-339 (eval = FALSE)
###################################################
## M <- persp(bei.extra$elev, 
##            theta=-45, phi=18, expand=7,
##            border=NA, apron=TRUE, shade=0.3, 
##            box=FALSE, visible=TRUE,
##            main="")
## perspPoints(bei, Z=bei.extra$elev, M=M, pch=16, cex=0.3)


###################################################
### code chunk number 16: datasets.Rnw:348-349
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(betacells)


###################################################
### code chunk number 17: datasets.Rnw:354-355
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(bramblecanes, cols=1:3)


###################################################
### code chunk number 18: datasets.Rnw:360-361 (eval = FALSE)
###################################################
## plot(split(bramblecanes))


###################################################
### code chunk number 19: datasets.Rnw:371-372
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(bronzefilter,markscale=2)


###################################################
### code chunk number 20: datasets.Rnw:380-381
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(btb, which.marks="spoligotype", cols=2:5, chars=1:4)


###################################################
### code chunk number 21: datasets.Rnw:390-391
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(cells)


###################################################
### code chunk number 22: datasets.Rnw:399-400
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(cetaceans.extra$patterns, main="Cetaceans data", cols=1:5, hsep=1)


###################################################
### code chunk number 23: datasets.Rnw:409-412
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(chicago, main="Chicago Crimes", col="grey",
     cols=c("red", "blue", "black", "blue", "red", "blue", "blue"),
     chars=c(16,2,22,17,24,15,6), leg.side="left", show.window=FALSE)


###################################################
### code chunk number 24: datasets.Rnw:422-423
###################################################
getOption("SweaveHooks")[["fig"]]()
chorley.extra$plotit()


###################################################
### code chunk number 25: datasets.Rnw:439-441
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(clmfires, which.marks="cause", cols=2:5, cex=0.25,
     main="Castilla-La Mancha forest fires")


###################################################
### code chunk number 26: datasets.Rnw:451-452
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(clmfires.extra$clmcov100$elevation, main="Elevation")


###################################################
### code chunk number 27: datasets.Rnw:463-464
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(concrete,chars="+",cols="blue",col="yellow")


###################################################
### code chunk number 28: datasets.Rnw:475-477
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(copper$Points, main="Copper")
plot(copper$Lines, add=TRUE)


###################################################
### code chunk number 29: datasets.Rnw:484-486
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(demohyper, quote({ plot(Image, main=""); plot(Points, add=TRUE) }),
      parargs=list(mar=rep(1,4)))


###################################################
### code chunk number 30: datasets.Rnw:493-494
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(demopat)


###################################################
### code chunk number 31: datasets.Rnw:508-509
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(dendrite, leg.side="bottom", main="", cex=0.75, cols=2:4)


###################################################
### code chunk number 32: datasets.Rnw:517-518
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(finpines, main="Finnish pines")


###################################################
### code chunk number 33: datasets.Rnw:531-535
###################################################
getOption("SweaveHooks")[["fig"]]()
wildM1 <- with(flu, virustype == "wt" & stain == "M2-M1")
plot(flu[wildM1, 1, drop=TRUE],
     main=c("flu data", "wild type virus, M2-M1 stain"),
     chars=c(16,3), cex=0.4, cols=2:3)


###################################################
### code chunk number 34: datasets.Rnw:543-544
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(gordon, main="People in Gordon Square", pch=16)


###################################################
### code chunk number 35: datasets.Rnw:559-560
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(gorillas, which.marks=1, chars=c(1,3), cols=2:3, main="Gorilla nest sites")


###################################################
### code chunk number 36: datasets.Rnw:564-565 (eval = FALSE)
###################################################
## system.file("rawdata/gorillas/vegetation.asc", package="spatstat")


###################################################
### code chunk number 37: datasets.Rnw:574-575
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(hamster, cols=c(2,4))


###################################################
### code chunk number 38: datasets.Rnw:585-586
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(heather$coarse)


###################################################
### code chunk number 39: datasets.Rnw:590-591 (eval = FALSE)
###################################################
## plot(heather)


###################################################
### code chunk number 40: datasets.Rnw:601-602
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(humberside)


###################################################
### code chunk number 41: datasets.Rnw:614-615
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(hyytiala, cols=2:5)


###################################################
### code chunk number 42: datasets.Rnw:624-625
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(japanesepines)


###################################################
### code chunk number 43: datasets.Rnw:634-635
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(lansing)


###################################################
### code chunk number 44: datasets.Rnw:641-642 (eval = FALSE)
###################################################
## plot(split(lansing))


###################################################
### code chunk number 45: datasets.Rnw:649-650
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(longleaf)


###################################################
### code chunk number 46: datasets.Rnw:659-661
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(mucosa, chars=c(1,3), cols=c("red", "green"))
plot(mucosa.subwin, add=TRUE, lty=3)


###################################################
### code chunk number 47: datasets.Rnw:677-680
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(murchison$greenstone, main="Murchison data", col="lightgreen")
plot(murchison$gold, add=TRUE, pch=3, col="blue")
plot(murchison$faults, add=TRUE, col="red")


###################################################
### code chunk number 48: datasets.Rnw:686-687
###################################################
reedy <- owin(c(580, 650) * 1000, c(6986, 7026) * 1000)


###################################################
### code chunk number 49: datasets.Rnw:692-695
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(murchison$greenstone[reedy], main="Murchison data", col="lightgreen")
plot(murchison$gold[reedy], add=TRUE, pch=3, col="blue")
plot(murchison$faults[reedy], add=TRUE, col="red")


###################################################
### code chunk number 50: datasets.Rnw:703-704
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(nbfires, use.marks=FALSE, pch=".")


###################################################
### code chunk number 51: datasets.Rnw:710-711 (eval = FALSE)
###################################################
## plot(split(nbfires), use.marks=FALSE, chars=".")


###################################################
### code chunk number 52: datasets.Rnw:714-719
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mar=c(0,0,2,0))
plot(split(nbfires)$"2000", which.marks="fire.type",
     main=c("New Brunswick fires 2000", "by fire type"),
     cols=c("blue", "green", "red", "cyan"),
     leg.side="left")


###################################################
### code chunk number 53: datasets.Rnw:727-729
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(nztrees)
plot(trim.rectangle(as.owin(nztrees), c(0,5), 0), add=TRUE, lty=3)


###################################################
### code chunk number 54: datasets.Rnw:742-743
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(osteo[1:4,], main.panel="", pch=21, bg='white')


###################################################
### code chunk number 55: datasets.Rnw:749-750 (eval = FALSE)
###################################################
## system.file("rawdata/osteo/osteo36.txt", package="spatstat")


###################################################
### code chunk number 56: datasets.Rnw:759-760
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(paracou, cols=2:3, chars=c(16,3))


###################################################
### code chunk number 57: datasets.Rnw:768-769
###################################################
getOption("SweaveHooks")[["fig"]]()
ponderosa.extra$plotit()


###################################################
### code chunk number 58: datasets.Rnw:781-782
###################################################
pyr <- pyramidal[c(FALSE,TRUE), ]


###################################################
### code chunk number 59: datasets.Rnw:785-787
###################################################
getOption("SweaveHooks")[["fig"]]()
pyr$grp <- abbreviate(pyr$group, minlength=7)
plot(pyr, quote(plot(Neurons, pch=16, main=grp)), main="Pyramidal Neurons")


###################################################
### code chunk number 60: datasets.Rnw:807-809
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(redwood)
plot(redwood3, add=TRUE, pch=20)


###################################################
### code chunk number 61: datasets.Rnw:812-813
###################################################
getOption("SweaveHooks")[["fig"]]()
redwoodfull.extra$plotit()


###################################################
### code chunk number 62: datasets.Rnw:827-829
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(as.solist(residualspaper[c("Fig1", "Fig4a", "Fig4b", "Fig4c")]), 
     main="")


###################################################
### code chunk number 63: datasets.Rnw:837-838
###################################################
getOption("SweaveHooks")[["fig"]]()
shapley.extra$plotit(main="Shapley")


###################################################
### code chunk number 64: datasets.Rnw:845-846
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(simdat)


###################################################
### code chunk number 65: datasets.Rnw:854-855
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(spiders, pch=16, show.window=FALSE)


###################################################
### code chunk number 66: datasets.Rnw:862-865
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(sporophores, chars=c(16,1,2), cex=0.6)
points(0,0,pch=16, cex=2)
text(15,8,"Tree", cex=0.75)


###################################################
### code chunk number 67: datasets.Rnw:874-875
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(spruces, maxsize=min(nndist(spruces)))


###################################################
### code chunk number 68: datasets.Rnw:884-885
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(swedishpines)


###################################################
### code chunk number 69: datasets.Rnw:894-895
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(urkiola, cex=0.5, cols=2:3)


###################################################
### code chunk number 70: datasets.Rnw:902-904
###################################################
getOption("SweaveHooks")[["fig"]]()
par(mar=c(0,0,2,0))
plot(waka, markscale=0.04, main=c("Waka national park", "tree diameters"))


###################################################
### code chunk number 71: datasets.Rnw:911-915
###################################################
getOption("SweaveHooks")[["fig"]]()
v <- rotate(vesicles, pi/2)
ve <- lapply(vesicles.extra, rotate, pi/2)
plot(v, main="Vesicles")
plot(ve$activezone, add=TRUE, lwd=3)


###################################################
### code chunk number 72: datasets.Rnw:940-941 (eval = FALSE)
###################################################
## system.file("rawdata/vesicles/mitochondria.txt", package="spatstat")


###################################################
### code chunk number 73: datasets.Rnw:949-950
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(waterstriders)


