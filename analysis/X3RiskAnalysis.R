library(data.table)
library(directlabels)
library(ggplot2)
library(h5)
library(matrixStats)
library(openxlsx)
library(optparse)
library(pbapply)
library(raster)
library(rgdal)

pboptions(type = "timer")

initializeParameters <- function(inputs, args = NULL) {
  arguments <- c()
  if(is.null(args)) {
    arguments <- commandArgs(TRUE)
  }
  else {
    positionalMatch <- match(names(args), inputs$positional)
    arguments <- c(
      sapply(names(args)[is.na(positionalMatch)], function(x) paste0("--", x, "=", args[[x]]), USE.NAMES = FALSE),
      sapply(inputs$positional, function(x) args[[x]], USE.NAMES = FALSE)
    )
  }
  parsedArgs <- parse_args(
    OptionParser(
      usage = paste("%prog [options]", paste(inputs$positional, collapse = " ")), option_list = inputs$optional),
    arguments, positional_arguments = length(inputs$positional))
  optionalArguments <- lapply(names(parsedArgs$options), function(x) parsedArgs$options[[x]])
  names(optionalArguments) <- names(parsedArgs$options)
  positionalArguments <- as.list(parsedArgs$args)
  names(positionalArguments) <- inputs$positional
  .libPaths(optionalArguments$rlibpath)
  c(optionalArguments, positionalArguments)
}

defineScales <- function(...) {
  .x <- list(...)
  s <- as.data.table(expand.grid(.x, stringsAsFactors = FALSE))
  r <- copy(s)
  for (col in names(r)) {
    r[, (col) := as.character(get(col))]
    if (attr(.x[[col]], "data.class") == class(character()))
      r[, (col) := paste0('"', get(col), '"')]
    r[!is.na(get(col)), (col) := paste0(
      col,
      ifelse(any(startsWith(get(col), c("=", ">", "<", "%"))), " ", " == "),
      get(col))]
  }
  r <- r[, do.call(paste, c(.SD, sep = " & "))]
  s[, .def := gsub(" & NA", "", r, fixed = TRUE)]
  for (col in names(s))
    s[, (col) := as.character(get(col))]
  for (i in seq_along(.x)) {
    col <- names(.x)[i]
    s[, (col) := names(.x[[i]])[match(get(col), .x[[i]])]]
  }
  s[, .def := gsub("NA", "TRUE", .def, TRUE)]
  class(s) <- c("x3scales", class(s))
  s
}

allLevels <- function(x, col, add.na = NULL) {
  v <- unique(x[, eval(substitute(col), x)])
  if (!is.null(add.na)) {
    v2 <- c(NA, v)
    names(v2) <- c(add.na, v)
    v <- v2
  } else {
    names(v) <- v
  }
  attr(v, "data.class") <- class(x[, eval(substitute(col), x)])
  class(v) <- c("x3scale", class(v))
  v
}

intervals <- function(lower, upper, add.na = NULL) {
  d <- data.table(lower, upper)
  r <- d[, paste0("%between% c(", lower + 1e-4, ", ", upper, ")")]
  if (!is.null(add.na)) {
    r2 <- c(NA, r)
    names(r2) <- c(add.na, d[, paste0(lower, "-", upper)])
    r <- r2
  } else {
    names(r) <- d[, paste0(lower, "-", upper)]
  }
  attr(r, "data.class") <- class(numeric())
  class(r) <- c("x3scale", class(r))
  r
}

temporalAggregation <- function(x3df, dataset, t.quantiles = seq(0, 1, .01)) {
  cat("Preparing analysis...\n")
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  dataset <- hdf5[dataset]
  chunks <- data.table(
    expand.grid(seq(1, dataset@dim[2], dataset@chunksize[2]), seq(1, dataset@dim[3], dataset@chunksize[3])))
  chunks[,
    c("Var3", "Var4") := list(
      ifelse(Var1 + dataset@chunksize[2] > dataset@dim[2], dataset@dim[2] - Var1 + 1, dataset@chunksize[2]),
      ifelse(Var2 + dataset@chunksize[3] > dataset@dim[3], dataset@dim[3] - Var2 + 1, dataset@chunksize[3])
    )]
  chunks <- chunks[Var3 > 0 & Var4 > 0 & !(Var3 == 1 & Var4 == 1)]
  cat("Temporal aggregation\n")
  data.qt <- rbindlist(pblapply(1:nrow(chunks), function(i) {
    dataSpace <- selectDataSpace(
      dataset, c(1, chunks[i, Var1], chunks[i, Var2]), c(dataset@dim[1], chunks[i, Var3], chunks[i, Var4]))
    q <- as.data.table(
      apply(apply(readDataSet(dataset, dataSpace), c(2, 3), function(x) quantile(x, t.quantiles)), 1, function(x) x))
    q[,
      cell := 0:(dataSpace@count[2] - 1) + chunks[i, Var1] + dataset@dim[2] * (rep(
         0:(dataSpace@count[3] - 1), each = dataSpace@count[2]) + chunks[i, Var2] - 1)
    ]
  }))
  h5close(hdf5)
  setkey(data.qt, cell)
  data.qt
}

riskAnalysisMaps <- function(
  data, output, spatialInfo, valueName = "Value", t.quantiles = c("50%", "75%", "90%", "95%", "100%")) {
  cat(paste0("Risk analysis maps max", valueName, "|t (x, MCrun)\n"))
  r <- raster(
    xmn = spatialInfo$extent[1],
    xmx = spatialInfo$extent[2],
    ymn = spatialInfo$extent[3],
    ymx = spatialInfo$extent[4],
    crs = spatialInfo$crs,
    resolution = spatialInfo$resolution
  )
  pbsapply(t.quantiles, function(qt) {
    val <- data[[qt]]
    length(val) <- ncell(r)
    r[] <- val
    writeRaster(flip(r, "y"), file.path(output, paste0(valueName, " QT", qt, ".tif")))
    TRUE
  })
}

getSpatialInfo <- function(x3df, extent, crs, resolution = 1) {
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  ext <- hdf5[extent][]
  crs <- showP4(hdf5[crs][])
  res <- resolution
  h5close(hdf5)
  list(extent = ext, crs = crs, resolution = res)
}

prepareSpatialAnalysisScales <- function(x3df, data, scales = list(), t.quantiles = seq(0, 1, .01)) {
  cat("Preparing spatial analysis scales...\n")
  data.qt <- data[, paste0(t.quantiles * 100, "%"), with = FALSE]
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  sapply(1:length(scales), function(i) data.qt[, names(scales)[i] := flip(raster(hdf5[scales[[i]]][]), "y")[]])
  h5close(hdf5)
  data.qt
}

getValues <- function(x3df, dataset, type="int") {
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  result <- as.integer(strsplit(hdf5[dataset][], ", ", TRUE)[[1]])
  h5close(hdf5)
  result
}

riskAnalysisPercentiles <- function(data, analysisScales, valueName = "Value", s.quantiles = seq(0, 1, .01)) {
  cat(paste0("Risk analysis Xth%ile (max", valueName, "|t)|x (MCrun)\n"))
  data.qt <- melt(data, setdiff(names(analysisScales), ".def"), variable.name = "t.percentile")
  rbindlist(pblapply(1:nrow(analysisScales), function(i) {
    res <- data.qt[
      eval(parse(text = analysisScales[i, .def])),
      list(s.percentile = paste0(s.quantiles * 100, "%"), value = quantile(value, s.quantiles)),
      t.percentile
    ]
    res[, names(analysisScales) := as.list(analysisScales[i])][, .def := NULL]
  }))
}

riskAnalysisTable <- function(
  data,
  valueName = "Value",
  s.quantiles = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1),
  t.quantiles = c(.5, .75, .9, .95, 1)
) {
  cat(paste0("Risk analysis table < Xth%ile (max", valueName, "|t)|x >...\n"))
  sq <- paste0(s.quantiles * 100, "%")
  tq <- paste0(t.quantiles * 100, "%")
  overview <- dcast(
    data[s.percentile %in% sq & t.percentile %in% tq],
    lulc + distance + t.percentile ~ s.percentile,
    value.var = "value"
  )
  setcolorder(overview, c("lulc", "distance", "t.percentile", sq))
  overview
}

writeToXlsx <- function(data, file, author = "X3 Risk Analysis") {
  wb <- createWorkbook(author)
  for(i in 1:length(data)) {
    if(is.character(data[[i]])) {
      addWorksheet(wb, names(data)[[i]])
      row <- 1
      for(paragraph in data[[i]]) {
        for(line in strwrap(paragraph, 80)) {
          writeData(wb, names(data)[[i]], line, 1, row)
          row <- row + 1
        }
      }
    } else if(is.data.frame(data[[i]])) {
      addWorksheet(wb, names(data)[[i]])
      writeDataTable(wb, names(data)[[i]], data[[i]])
    } else {
      sapply(1:length(data[[i]]), function(j) {
          addWorksheet(wb, names(data[[i]])[j])
          writeDataTable(wb, names(data[[i]])[j], data[[i]][[j]])
          TRUE
      })
    }
  }


  saveWorkbook(wb, file)
}

contourplots <- function(
  data, analysisScales, output, prefix = "QX(QT)--", suffix = "", threshold = 0, type = "contour") {
  cat("Contourplots\n")
  suppressMessages(pbsapply(1:nrow(analysisScales), function(i) {
    dat <- data[lulc == analysisScales[i, lulc] & distance == analysisScales[i, distance] & value >= threshold]
    p <- ggplot(dat, aes(s.percentile, t.percentile))
    if(type == "raster")
      p <- p + geom_raster(aes(fill = log10(value)))
    else if(type == "contour")
      p <- p + geom_contour(aes(z = log10(value), colour = ..level..))
    p <- p +
      xlab("Spatial percentile") +
      ylab("Temporal percentile") +
      ggtitle(
        paste(
          "Spatial and temporal percentiles of",
          analysisScales[i, lulc],
          "(log10) --",
          analysisScales[i, distance],
          "m"
        )
      ) +
      scale_x_continuous(breaks = 1:10 * 10) +
      scale_y_continuous(breaks = 1:10 * 10) +
      scale_color_gradient(low = "darkgreen", high = "red")
    if(type == "contour")
      direct.label(p, "bottom.pieces")
    ggsave(
      file.path(output, paste0(prefix, analysisScales[i, distance], "--", analysisScales[i, lulc], suffix, ".png")))
    TRUE
  }))
}

plotPerTimestep <- function(x3df, dataset, output, spatialInfo, max.value) {
  cat("Plot per timestep\n")
  dir.create(output)
  r <- raster(
    xmn = spatialInfo$extent[1],
    xmx = spatialInfo$extent[2],
    ymn = spatialInfo$extent[3],
    ymx = spatialInfo$extent[4],
    crs = spatialInfo$crs,
    resolution = spatialInfo$resolution
  )
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  dataset <- hdf5[dataset]
  pbsapply(1:dataset@dim[1], function(t) {
    r[] <- as.vector(sqrt(dataset[t,,]))
    r[r <= 1e-5] <- NA
    png(file = file.path(output, sprintf("%06d.png", t)), width = 1080, height = 1080)
    plot(
      flip(r, "y"),
      zlim = c(0, max.value),
      col = rev(rainbow(255, start = 0, end = 1 / 3)),
      main = paste0("t = ", t),
      colNA = "lightgrey"
    )
    dev.off()
    TRUE
  })
  h5close(hdf5)
}

animate <- function(rasters, output, ffmpeg) {
  cat("Animate...\n")
  system2(ffmpeg, c("-i", shQuote(rasters, "cmd"), "-vcodec", "msmpeg4v2", shQuote(output, "cmd")))
}

coocurrenceSprayDriftRunOff <- function(x3df, spraydrift, runoff) {
  cat("Cooccurence of spray-drift and run-off events\n")
  hdf5 <- h5file(paste0(x3df, "/arr.dat"), "r")
  dataset <- hdf5[spraydrift]
  events <- rbindlist(pblapply(1:dataset@dim[1], function(t) {
    data.table(t, spraydrift = any(hdf5[spraydrift][t,,] > 0), runoff = any(hdf5[runoff][t,,] > 0))
  }))
  daysRunOffAfterSprayDrift <- outer(events[runoff == TRUE, t], events[spraydrift == TRUE, t], "-")
  daysRunOffAfterSprayDrift[daysRunOffAfterSprayDrift < 0] <- Inf
  quantiles <- quantile(colMins(daysRunOffAfterSprayDrift), seq(0, 1, .05))
  result <- data.table(q = names(quantiles), days = quantiles)
  h5close(hdf5)
  result
}

loadRawPercentiles <- function(input) {
  cat("Loading raw percentiles\n")
  dataFiles <- list.files(input, "^.*\\.rds$", full.names = TRUE, recursive = TRUE)
  rbindlist(pblapply(dataFiles, readRDS))
}

analyzePercentiles <- function(perc) {
  cat("Analyzing...\n")
  perc[,
    list(
      mean = mean(value),
      stddev = sd(value),
      upper_conf = mean(value) + qnorm(.975) * sd(value) / sqrt(.N),
      lower_conf = mean(value) - qnorm(.975) * sd(value) / sqrt(.N)
    ),
    list(distance, t.percentile, s.percentile)
  ]
}

filterPercentiles <- function(
  t.perc = c(.5, .75, .9, 1), s.perc = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)) {
  cat("Filtering percentiles\n")
  poi.t <- paste0(t.perc * 100, "%")
  poi.s <- paste0(s.perc * 100, "%")
  data <- pblapply(poi.t, function(poit) {
    x.mean <- dcast(
      percentiles[t.percentile == poit & s.percentile %in% poi.s], distance ~ s.percentile, sum, value.var = "mean")
    x.lower <- dcast(
      percentiles[t.percentile == poit & s.percentile %in% poi.s],
      distance ~ s.percentile,
      sum,
      value.var = "lower_conf"
    )
    x.upper <- dcast(
      percentiles[t.percentile == poit & s.percentile %in% poi.s],
      distance ~ s.percentile,
      sum,
      value.var = "upper_conf"
    )
    x.mean[, characteristic := "mean"]
    x.lower[, characteristic := "lowerConf95"]
    x.upper[, characteristic := "upperConf95"]
    x <- rbindlist(list(x.mean, x.lower, x.upper))
    setkey(x, distance)
    setcolorder(x, c("distance", poi.s, "characteristic"))
    x
  })
  names(data) <- paste0("qx(qt", poi.t, ")")
  data
}

boxplots <- function(output, t.perc = c(.5, .75, .9, 1), s.perc = c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1)) {
  cat("Boxplots\n")
  poi.t <- paste0(t.perc * 100, "%")
  poi.s <- paste0(s.perc * 100, "%")
  pbsapply(poi.t, function(poit) {
    for (dist in unique(perc[, distance])) {
      data <- perc[distance == dist & t.percentile == poit & s.percentile %in% poi.s]
      png(
        file.path(output, paste0("QX(QT", sub("%", "", poit, fixed = TRUE), ")_", dist, ".png")),
        width = 1024,
        height = 1024
      )
      boxplot(
        data[, value] ~ data[, as.numeric(sub("%", "", s.percentile, fixed = TRUE))],
        at = unique(data[, as.numeric(sub("%", "", s.percentile, fixed = TRUE))]),
        xlab = "Spatial percentile",
        ylab = params$options$dsname,
        main = paste0("QX(QT", poit, ") | ", dist)
      )
      dev.off()
    }
    TRUE
  })
}
