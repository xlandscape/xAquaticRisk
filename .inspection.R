.libPaths(
  c(
    "model/variant/AnalysisObserver/R-4.1.1/library",
    "model/variant/LP50/R-4.1.1/library",
    "model/variant/XSprayDrift/module/R-4.1.2/library"
  )
)
.GlobalEnv$`.` <- function() {}
library(data.table)
library(ggplot2)
library(pbapply)
library(drc)
library(directlabels)
library(x3df)
library(sf)
library(hdf5r)
library(terra)
library(xdrift)
