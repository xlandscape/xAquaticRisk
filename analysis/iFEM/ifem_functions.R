# Functions for analysing Landscape model run data
checkAndLoadPackages <- function(required.packages = c("devtools","tidyverse","scales","drc", "gridExtra","patchwork","hdf5r","parallel","snow",
                                                       "sp","Rcpp","rgeos","rgdal","ggspatial","ggridges","gstat","openxlsx",
                                                       "gstat","maptools","terra","raster","usdm","plotKML","igraph","colorspace","mgcv")){
  lapply(required.packages,function(x){require(x, character.only = T)})
  
}

# Wrapper functions to prepare notebook work environments
prepareEnvironmentR <- function(first.year, last.year,
                                first.application.day,
                                last.application.day,
                                first.application.month,
                                last.application.month,
                                hydrography.file.path){
  params <- NULL
  params$first_year <- as.integer(first.year)
  params$last_year <- as.integer(last.year)
  params$first_app_month <- as.integer(first.application.month)
  params$first_app_day <- as.integer(first.application.day)
  params$last_app_month <- as.integer(last.application.month)
  params$last_app_day <- as.integer(last.application.day)
  params$hydrography <- as.character(hydrography.file.path)
  
  options(scipen = 999)
  suppressWarnings(suppressMessages(checkAndLoadPackages()))
  options(repr.plot.width=30, repr.plot.height=15)
  return(params)
}

prepareLP50Data <- function(params,run.name){
  reach.info <- suppressWarnings(suppressMessages(getReachInfoFromScenario(scenario.path = params$hydrography.file.path)))
  
  mc.folder <- list.dirs(paste0("../run/",run.name,"/mcs"),recursive = F)
  lp50 <- suppressWarnings(suppressMessages(readLP50DataFromStore(data.store.fpath = paste0(mc.folder,"/store/arr.dat"),
                                                                  first.year = params$first_year, last.year = params$last_year, 
                                                                  reach.info = reach.info)))
  # Calculate median values
  medLP50 <- suppressWarnings(suppressMessages(medianLP50Values(lp50 = lp50,reach.info = reach.info)))
  
  return(list(reach.info = reach.info,lp50 = lp50,medLP50 = medLP50))
}

preparePECData <- function(params, run.name){
  reach.info <- suppressWarnings(suppressMessages(getReachInfoFromScenario(scenario.path = params$hydrography.file.path)))
  
  mc.folder <- list.dirs(paste0("./run/",run.name,"/mcs"),recursive = F)
  PEC <- suppressWarnings(suppressMessages(readPECDataFromStore(data.store.fpath = paste0(mc.folder,"/store/arr.dat"), 
                                                                first.year = params$first_year, last.year = params$last_year)))
  # Calculate median values
  medPEC <- suppressWarnings(suppressMessages(medianPECValues(max.pec= PEC,reach.info = reach.info)))
  
  return(list(reach.info = reach.info,PEC = PEC,medPEC = medPEC))
}
######################## GIS related functions #################################
readDRNFromScenario <- function(scenario.path){
  drn <- readOGR(scenario.path)
  return(drn)
}

getReachInfoFromScenario <- function(scenario.path){
  drn <- readOGR(scenario.path)@data
  drn$RchID <- gsub(pattern = "r",replacement = "R",drn$key_r)
  drn[,c("key","uparea","uplength","landuse","shape","key_r","isDiffusiv")] <- NULL
  return(drn)
}

createCatchmentPlotSTLP50 <- function(scenario.path, plot.output.location,plot.name,stlp50.data.frame){
  drn <- readDRNFromScenario(scenario.path = paste0(scenario.path,"ReachList_shp.shp"))
  drn@data$RchID <- paste0("R",drn@data$key)
  ctm <- readDRNFromScenario(scenario.path = paste0(scenario.path,"LULC.shp"))
  
  # Attach colour info to reaches in drn
  drn@data <- left_join(drn@data, stlp50.data.frame, by = "RchID")
  ## Fortify data sets
  drn_f <- fortify(drn)
  drn_df <- data.frame(id = as.character(as.numeric(lapply(drn@lines, function(x) x@ID) %>% do.call(rbind,.))),drn@data)
  drn_f <- left_join(drn_f, drn_df,by = "id")
  ctm_f <- fortify(ctm)
  ctm_df <- data.frame(id = as.character(as.numeric(lapply(ctm@polygons, function(x) x@ID) %>% do.call(rbind,.))),ctm@data)
  ctm_f <- left_join(ctm_f, ctm_df,by = "id")
  
  p <- ggplot() +
    geom_polygon(data = ctm_f, aes(x = long, y = lat, group = group),colour = "NA",fill = "forestgreen",alpha = 0.4) +
    geom_path(data = drn_f, aes(x = long, y = lat, group = group,colour = as.factor(STLP50)), size = 1) +
    scale_colour_manual("LP50", values = c("1" = "red","2" = "orange","3" = "yellow","4" = "forestgreen","5" = "lightskyblue1"),
                        label = c("LP50 < 1", "1 < LP50 < 10", "10 < LP50 < 100","LP50 > 100","Effect free year","No effect"),
                        drop = F,na.value = "lightsteelblue3") +
    annotation_north_arrow(which_north = "grid", location = "br",height = unit(1.5,"cm"),width = unit(0.75,"cm")) +
    annotation_scale(width_hint = 0.25,plot_unit = "m",unit_category = "metric", style = "bar", location = "br", 
                     pad_x = unit(1.25,units = "cm"), text_cex = 1.5) +
    # labs(x = "X (m)", y = "Y (m)") +
    coord_fixed() + theme_light() + theme(text = element_text(size = 20),
                                          legend.text = element_text(size = 20),
                                          axis.text = element_blank())
  ggsave(plot = p, filename = paste0(plot.output.location,plot.name,".png"),width = 25,height = 25,units = "cm",dpi = 600)
}

createCatchmentPlotSTPEC <- function(scenario.path, plot.output.location,plot.name,stPEC.data.frame, min.concentration,max.concentration,
                                     log.steps.concentration.range){
  drn <- readDRNFromScenario(scenario.path = paste0(scenario.path,"ReachList_shp.shp"))
  drn@data$RchID <- paste0("R",drn@data$key)
  ctm <- readDRNFromScenario(scenario.path = paste0(scenario.path,"LULC.shp"))
  
  # Attach colour info to reaches in drn
  drn@data <- left_join(drn@data, stPEC.data.frame, by = "RchID")
  ## Fortify data sets
  drn_f <- fortify(drn)
  drn_df <- data.frame(id = as.character(as.numeric(lapply(drn@lines, function(x) x@ID) %>% do.call(rbind,.))),drn@data)
  drn_f <- left_join(drn_f, drn_df,by = "id")
  ctm_f <- fortify(ctm)
  ctm_df <- data.frame(id = as.character(as.numeric(lapply(ctm@polygons, function(x) x@ID) %>% do.call(rbind,.))),ctm@data)
  ctm_f <- left_join(ctm_f, ctm_df,by = "id")
  
  # now bin data in user defined bins, first get bin values then cut PEC values
  bn1 <- c(-Inf,seq(log10(min.concentration), log10(max.concentration), 
                    by = log.steps.concentration.range))
  
  p <- ggplot() +
    geom_polygon(data = ctm_f, aes(x = long, y = lat, group = group),colour = "NA",fill = "forestgreen",alpha = 0.4) +
    geom_path(data = drn_f, aes(x = long, y = lat, group = group,colour = STPEC), size = 1) +
    scale_colour_brewer(paste0("Concentration (","\u00B5","g/L)"), na.value = "lightsteelblue3",
                        palette = "Spectral", direction = -1,labels = c(paste0("<",as.character(signif(10^(bn1[-1]),3))), "No Exposure")) +
    annotation_north_arrow(which_north = "grid", location = "br",height = unit(1.5,"cm"),width = unit(0.75,"cm")) +
    annotation_scale(width_hint = 0.25,plot_unit = "m",unit_category = "metric", style = "bar", location = "br", 
                     pad_x = unit(1.25,units = "cm"), text_cex = 1.5) +
    # labs(x = "X (m)", y = "") +
    coord_fixed() + theme_light() + theme(text = element_text(size = 20),
                                          legend.text = element_text(size = 20),
                                          axis.text = element_blank(),
                                          axis.title = element_blank())
  ggsave(plot = p, filename = paste0(plot.output.location,plot.name,".png"),width = 25,height = 25,units = "cm",dpi = 600)
}

######################### PEC functions ########################################
readPECDataFromStore <- function(data.store.fpath,first.year,last.year){
  # initiate data store
  df <- hdf5r::H5File$new(filename = data.store.fpath,
                          mode = "r+")
  # create hourly time series
  start.time <- df[["Hydrology/TimeSeriesStart"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  end.time <- df[["Hydrology/TimeSeriesEnd"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  time.period <- data.frame(Tstamp = seq(start.time,(end.time+(3600)*24),by=3600))
  time.period$Year <- format(time.period$Tstamp,"%Y") %>% as.numeric()
  time.period$Month <- format(time.period$Tstamp,"%m") %>% as.numeric()
  
  # Get reach IDs
  rch <- df[["Hydrology/Reaches"]]$read() %>% paste0("R",.)
  yrs <- unique(time.period$Year) %>% .[.>=first.year&.<=last.year]
  
  maxPECs <- function(yrs){
    # initiate data store for each core
    df <- hdf5r::H5File$new(filename = data.store.fpath,
                            mode = "r+")
    # t1 <- df[["CascadeToxswa/ConLiqWatTgtAvgHrAvg"]][,(time.period$Year==yrs & time.period$Month %in% c(4,5) & time.period$Year>=first.year & time.period$Year <= last.year)]
    t1 <- df[["CascadeToxswa/ConLiqWatTgtAvgHrAvg"]][,(time.period$Year==yrs & time.period$Year>=first.year & time.period$Year <= last.year)]
    if(is.null(nrow(t1))){
      t2 <- data.frame(Year = yrs, RchID = rch, Max_PEC_Avg =  max(t1))
    }else{
      t2 <- data.frame(Year = yrs, RchID = rch, Max_PEC_Avg = apply(t1,1,function(x) max(x)))
    }
    
    # convert g/m3 to ug/L
    t2$Max_PEC_Avg <- (t2$Max_PEC_Avg * 1000)
    t2
  }
  cl <- makeCluster(getOption("cl.cores",detectCores()))
  clusterExport(cl, c("data.store.fpath","rch","yrs","maxPECs","time.period","first.year","last.year"),
                envir=environment())
  df.1 <- parLapply(cl = cl, x = yrs, fun = maxPECs)
  df.1 <- data.table::rbindlist(df.1)
  
  return(df.1)
}

medianPECValues <- function(max.pec,reach.info){
  # Used to create a plot of PEC values, sorted by strahler order
  # Get median PEC across years for each reach
  medPEC <- aggregate(.~RchID,data = max.pec[,c("RchID","Max_PEC_Avg")],median)
  colnames(medPEC) <- c("Reach","Max_PEC")
  medPEC <- left_join(medPEC,reach.info, by = c("Reach" = "RchID"))
  medPEC <- medPEC[order(as.numeric(medPEC$strahler),medPEC$Max_PEC),]
  rownames(medPEC) <- NULL
  medPEC$Type <- "Average"
  medPEC$rowID <- 1:nrow(medPEC)
  medPEC$Max_PEC <- ifelse(medPEC$Max_PEC==0,1*10^-12,medPEC$Max_PEC)
  return(medPEC)
}

createPECbyStrahlerPlot <- function(max.pec,
                                    reach.info,
                                    medPEC,
                                    breaks_y = c(-5,-4,-3.-2.-1,0),
                                    labels_y = c("0.000001","0.00001","0.0001","0.001","0.01","0.1"),
                                    point_colour = "red",
                                    linewidth = 0.75,
                                    output.folder = NULL,
                                    model.name = NULL,
                                    model.version = NULL
                                    ){
  vpos <- aggregate(.~strahler,data = reach.info[,c("strahler","width")],FUN = length)
  vpos <- vpos[order(vpos$strahler),]
  row.names(vpos) <- NULL
  vpos$x <- cumsum(vpos$width)
  
  max.pec <- left_join(max.pec,unique(medPEC[,c("Reach", "rowID")]),by = c("RchID"="Reach"))
  max.pec <- max.pec[,c("rowID","RchID","Year","strahler","Max_PEC_Avg")]
  colnames(max.pec) <- c("rowID","Reach","Year","strahler","Max_PEC")
  max.pec <- max.pec[order(as.numeric(max.pec$strahler),max.pec$Max_PEC),]
  max.pec$Max_PEC <- ifelse(max.pec$Max_PEC==0,NA,max.pec$Max_PEC)
  
  PEC.plots <- ggplot() +
    # Basic line plot
    geom_point(data = max.pec,
               aes(x = rowID,y = log10(Max_PEC),
                   group = as.factor(strahler)),
               colour = "red", #pointcolour,
               alpha = 0.5,lwd = 0.75) +#
    geom_line(data = medPEC,
              aes(x = rowID,y = log10(Max_PEC),
                  group = as.factor(strahler)),colour = "black",lwd = 0.75) +  #lwd = linewidth
    guides(colour = "none") +
    scale_y_continuous(paste0("Concentration (","\u00B5","g/L)"),
                       breaks = c(-6,-5,-4,-3,-2,-1), # breaks_y
                       labels = c("0.000001","0.00001","0.0001","0.001","0.01","0.1")) + #labels_y
    # Add vertical lines to delineate Strahler order
    geom_vline(data = vpos[,1:3],
               aes(xintercept = x),
               colour = "black",
               linetype = "dashed") +
    scale_x_continuous("Number of reaches (0 - 100% per Strahler order)",
                       breaks = c(vpos$x[1]/2,vpos$x[1],vpos$x[2]-(vpos$width[2]/2),vpos$x[2],vpos$x[3]-(vpos$width[3]/2),vpos$x[3],vpos$x[4]-(vpos$width[4]/2),vpos$x[4]),
                       labels = c("","100","","100","","100","","100"),
                       limits = c(0,max(vpos$x)),
                       expand = c(0,20)) +
    coord_cartesian(ylim = c(-6.5, 0)) +
    ggtitle(paste0("Model: " ,model.name, " ", model.version)) +
    theme_bw() +
    theme(text = element_text(size = 12),axis.text = element_text(size = 12),plot.title = element_text(size = 6,hjust = 1)) +
    geom_text(data = data.frame(x = vpos$x,y = rep(-1,nrow(vpos)),
                                labels = paste0(c("Strahler order 1",as.character(vpos$strahler[!vpos$strahler==1])))),
              aes(x = x, y = y, label = labels), size = 6, hjust = "inward")
  if(!is.null(output.folder)){
    ggsave(plot = PEC.plots,paste0(output.folder,"./PEC_",".png"),
           width = 20, height = 15,units = "cm",dpi = 200)}
  return(PEC.plots)
}

createSpatialTemporalPECPlots <- function(max.pec,assessment.period,reach.info,output.folder,temporal.conditioning.year,min.concentration, max.concentration,log.steps.concentration.range, model.name = NULL,model.version = NULL){
  
  PEC <- max.pec
  PEC <- PEC[PEC$Year %in% assessment.period,]
  pec.mat <- PEC[,c("Max_PEC_Avg","Year","RchID")] %>% pivot_wider(.,names_from = "RchID",values_from = "Max_PEC_Avg") %>% as.data.frame()
  PEC <- left_join(PEC,reach.info,by = "RchID")
  
  ls.output <- list()
  sorted.dfs <- list()
  for (j in temporal.conditioning.year){
    pec.rank <- pec.mat
    rownames(pec.rank) <- pec.rank$Year
    pec.rank <- pec.rank[,-1] %>% as.matrix()
    # give unexposed reaches NA
    no.exposure <- apply(pec.rank,2,function(x) sum(x)==0)
    pec.rank[,no.exposure]<-NA
    
    # Rank pec values
    pec.rank <- apply(pec.rank,2,FUN = function(x) x[order(x,decreasing = T,na.last = T)]) %>% 
      .[,order(.[j,],decreasing = T,na.last = T)]
    
    # Now create plot
    spatial.perc <- data.frame(RchID = colnames(pec.rank))
    spatial.perc <- left_join(spatial.perc,unique(PEC[,c("RchID","lenght")]))
    spatial.perc$lenght <- 1
    spatial.perc$cumsum <- cumsum(spatial.perc$lenght)
    spatial.perc$x <- (100/sum(spatial.perc$lenght))*(spatial.perc$cumsum - (spatial.perc$lenght/2))
    spatial.perc <- spatial.perc[,c(1,4)]
    
    temp.perc <- data.frame(Yr = 1:nrow(pec.rank))
    temp.perc$y <- (100/nrow(pec.rank))*(temp.perc$Yr - (1/2))
    temp.perc$Yr <- as.character(temp.perc$Yr)
    
    plot.df <- as.data.frame(pec.rank) 
    plot.df$Yr <- rownames(plot.df)
    plot.df <- plot.df %>% pivot_longer(.,cols = starts_with("R"),names_to = "RchID",values_to = "STPEC") %>% as.data.frame()
    plot.df <- left_join(plot.df,temp.perc)
    plot.df <- left_join(plot.df,spatial.perc)
    plot.df$y <- round(plot.df$y)
    # plot.df$STPEC[is.na(plot.df$STPEC)] <- 0
    
    # now bin data in user defined bins, first get bin values then cut PEC values
    bn1 <- c(-Inf,seq(log10(min.concentration), log10(max.concentration), 
                      by = log.steps.concentration.range))
    
    plot.df$STPEC <- cut(log10(plot.df$STPEC),breaks = bn1)
    # plot.df$STPEC <- ifelse(plot.df$STPEC == 1,NA,plot.df$STPEC)
    
    sorted.dfs[[paste0("sorted_pecs")]] <- plot.df[plot.df$y==min(plot.df$y),]
    
    p<-ggplot(plot.df,aes(as.factor(x),as.factor(y))) + geom_tile(aes(fill = as.factor(STPEC))) +
      
      scale_x_discrete (paste0("Spatial percentile (n = ",nrow(spatial.perc),")"),breaks = as.factor(plot.df$x[round(seq(1,nrow(plot.df),length.out = 20))]),
                        labels = plot.df$x[round(seq(1,nrow(plot.df),length.out = 20))] %>% round() %>% as.character()) +
      scale_y_discrete(paste0("Temporal percentile (n = ",nrow(temp.perc),")")) +
      scale_fill_brewer(paste0("Concentration (","\u00B5","g/L)"), na.value = "lightsteelblue3",
                        palette = "Spectral", direction = -1,labels = c(paste0("<",as.character(signif(10^(bn1[-1]),3))), "No Exposure")) +
      # breaks = trans_breaks("log10", function(x) 10^x,n = 8),
      # labels = trans_format("log10", label_math(10^.x)),
      # breaks = c(-6,-5,-4,-3,-2,-1),
      # labels = c("0.000001","0.00001","0.0001","0.001","0.01","0.1")) +
       ggtitle(paste0("Model: " ,model.name, " ", model.version)) + theme(text = element_text(size = 12),plot.title = element_text(size = 5))
    ggsave(plot = p,paste0(output.folder,"/PEC_plot_conditioningPercentile_",temp.perc$y[j],".png"),
           width = 20, height = 15,units = "cm",dpi = 200)
    
    ls.output[[paste0("PEC_plot")]] <- p
  }
  return(list(plots = ls.output, dfs = sorted.dfs))
}


####################### Lp50 functions ###########################
readLP50DataFromStore <- function(data.store.fpath,first.year,last.year,reach.info){
  # initiate data store
  df <- H5File$new(filename = data.store.fpath,
                   mode = "r+")
  # Get reach IDs
  rch <- reach.info$RchID
  
  # list with species and model versions
  ls.spec <- list.groups(df)[grepl("IndEffect_LP50_Cascade",list.groups(df))]
  model.versions <- lapply(ls.spec,function(x){
    # get species name and model type from hdf5
    t1 <- df[[paste0(x,"/ProcessingPath")]]$read() %>% strsplit(.,"\\\\") %>% unlist()
    t2 <- strsplit(t1[grepl("ind_lp50_",t1)],"_") %>% unlist()  # todo
    t3 <- data.frame(Group = x,Model_type = t2[grepl("it|sd",t2)],Species = gsub(" ","_",t2[grepl("Asellus|Cloeon|Gammarus",t2)]))
    t3
  }) %>% do.call(rbind,.)
  
  # create year time series
  time.period <- data.frame(Year = (df[[paste0(ls.spec[1],"/SimulationStart")]]$read() %>% strsplit(.,"-") %>% unlist() %>% .[1] %>% as.numeric()):last.year)
  
  df.1 <- lapply(ls.spec, function(x){
    # using mclapply, need to initiate data store for each core
    df <- H5File$new(filename = data.store.fpath,
                     mode = "r+")
    t1 <- data.frame(df[[paste0(x,"/LP50")]][,(time.period$Year>= first.year & time.period$Year <= last.year)])
    colnames(t1) <- time.period$Year[(time.period$Year>= first.year & time.period$Year <= last.year)]
    # Add reach info
    t1$RchID <- rch
    # Add group info for joining and pivot long for plotting later on
    t1$Group <- x
    t1 <- pivot_longer(t1,cols = -c(RchID,Group),names_to = "Year",values_to = "LP50") %>% as.data.frame()
    # Join species and model version info
    t1 <- left_join(t1,model.versions)
    t1
  }) %>% do.call(rbind,.)
  min.rep.val <- df[[paste0(ls.spec[1],"/MinimumReportValue")]]$read()
  max.rep.val <- df[[paste0(ls.spec[1],"/MaximumReportValue")]]$read()
  err.rep.val <- df[[paste0(ls.spec[1],"/ErrorReportValue")]]$read()
  
  # set min and err values to NA to exclude for further analysis.
  df.1$LP50 <- ifelse(df.1$LP50 %in% c(max.rep.val,err.rep.val),Inf,df.1$LP50)
  return(df.1)
}

medianLP50Values <- function(lp50,reach.info){
  # Used to create a plot of LP50 values, sorted by strahler order
  medLP50 <- aggregate(.~RchID*Model_type*Species,data = lp50[,c("RchID","LP50","Model_type","Species")],FUN = function(x) median(x,na.rm = T))
  colnames(medLP50) <- c("Reach","Model_type","Species","LP50")
  medLP50 <- left_join(medLP50,reach.info, by = c("Reach" = "RchID"))
  medLP50$LP50 <- ifelse(medLP50$LP50 == Inf,NA,medLP50$LP50)
  medLP50 <- medLP50[order(-medLP50$LP50, na.last = F),]
  medLP50 <- medLP50[order(medLP50$Model_type,medLP50$Species,as.numeric(medLP50$strahler),na.last = F),]
  rownames(medLP50) <- NULL
  medLP50$rowID <- rep(1:nrow(reach.info), length(unique(lp50[,c("Model_type","Species")])))
  return(medLP50)
}

createLP50byStrahlerPlot <- function(lp50,
                                     reach.info,
                                     medLP50, 
                                     breaks_y = c(-2,-1,0,1,2,3,4,5),
                                     labels_y = c("0.01","0.1","1","10","100","1000","10000","100000"),
                                     point_colour = "red",
                                     linewidth = 0.75,
                                     LP50_category_colours = c("red","orange","yellow"),
                                     output.folder = NULL){
  vpos <- aggregate(.~strahler,data = reach.info[,c("strahler","width")],FUN = length)
  vpos <- vpos[order(vpos$strahler),]
  row.names(vpos) <- NULL
  vpos$x <- cumsum(vpos$width)
  breaks <- c(vpos$x[1]/2,vpos$x[1],
              lapply(2:nrow(vpos), function(x) c(vpos$x[x] - (vpos$width[x]/2),vpos$x[x])) %>% do.call(c,.))
  labs <- rep(c("","100"), length(breaks)/2)
  
  lp50 <- left_join(lp50,medLP50[,c("Reach","Model_type","Species","rowID","strahler")],by = c("RchID"="Reach","Model_type","Species"))
  lp50 <- lp50[order(lp50$Year),]
  row.names(lp50) <- NULL
  lp50$LP50 <- ifelse(lp50$LP50==Inf,NA,lp50$LP50)
  
  model.versions <- unique(lp50[,c("Model_type","Species")])
  LP50.plot <- list()
  for (j in 1:nrow(model.versions)){
    LP50.plot[[paste0(model.versions$Model_type[j],"_",model.versions$Species[j])]] <-ggplot() +
      # Basic line plot
      geom_point(data = lp50[lp50$Species==model.versions$Species[j]&lp50$Model_type==model.versions$Model_type[j],],
                 aes(x = rowID,y = log10(LP50),
                     group = interaction(as.factor(strahler),as.factor(Year))),
                 colour = point_colour,alpha = 0.05, lwd = 0.75) +
      geom_line(data = medLP50[medLP50$Species==model.versions$Species[j]&medLP50$Model_type==model.versions$Model_type[j],],
                aes(x = rowID,y = log10(LP50),
                    group = as.factor(strahler)),lwd = linewidth) +
      guides(colour = "none") +
      scale_y_reverse("LP50",
                      breaks = breaks_y,
                      labels = labels_y) +
      # Add horizontal lines for LP50 factors 1, 10, and 100
      geom_hline(data = data.frame(intercept = 0),
                 aes(yintercept = intercept),
                 colour = LP50_category_colours[1],
                 linetype = "dashed",
                 lwd = linewidth) +
      geom_hline(data = data.frame(intercept = 1),
                 aes(yintercept = intercept),
                 colour = LP50_category_colours[2],
                 linetype = "dashed",
                 lwd = linewidth) +
      geom_hline(data = data.frame(intercept = 2),
                 aes(yintercept = intercept),
                 colour = LP50_category_colours[3],
                 linetype = "dashed",
                 lwd = linewidth) +
      # Add vertical lines to delineate Strahler order
      geom_vline(data = vpos[,1:3],
                 aes(xintercept = x),
                 colour = "black",
                 linetype = "dashed") +
      scale_x_continuous("Number of reaches (0 - 100% per Strahler order)",
                         breaks = breaks,
                         labels = labs,
                         limits = c(0,max(vpos$x)),
                         expand = c(0,50)) +
      coord_cartesian(ylim = c(5,-2)) +
      ggtitle(paste0("Species: ",model.versions$Species[j],"\nModel type: ",model.versions$Model_type[j])) +
      theme_bw() +
      geom_text(data = data.frame(x = vpos$x,y = rep(-2,nrow(vpos)),
                                  labels = paste0(c("Strahler order 1",as.character(vpos$strahler[!vpos$strahler==1])))),
                aes(x = x, y = y, label = labels), size = 6, hjust = "inward") +
      theme(text = element_text(size = 20),plot.title = element_text(size = 12))
    if(!is.null(output.folder)){
      ggsave(plot = LP50.plot[[j]],paste0(output.folder,"./LP50_",model.versions$Model_type[j],"_",model.versions$Species[j],".png"),
             width = 20, height = 15,units = "cm",dpi = 200)}
  }
  
  return(LP50.plot)
}

createSpatialTemporalLP50Plots <- function(lp50,reach.info,output.folder,temporal.conditioning.year,model.name = NULL, model.version = NULL){
  
  lp50.mat <- lp50[,c("LP50","Year","RchID","Species","Model_type")] %>% pivot_wider(.,names_from = "RchID",values_from = "LP50") %>% as.data.frame()
  lp50 <- left_join(lp50,reach.info,by = "RchID")
  
  combinations <- unique(lp50.mat[,c("Species","Model_type")])
  ls.output <- list()
  sorted.dfs <- list()
  for (j in temporal.conditioning.year){
    for (i in 1:nrow(combinations)){
      lp50.rank <- lp50.mat[lp50.mat$Species==combinations$Species[i]&lp50.mat$Model_type==combinations$Model_type[i],-c(2,3)]
      rownames(lp50.rank) <- lp50.rank$Year
      lp50.rank <- lp50.rank[,-1] %>% as.matrix()
      # give unexposed reaches NA
      no.exposure <- apply(lp50.rank,2,function(x) sum(is.infinite(x))==length(x))
      lp50.rank[,no.exposure]<-NA
      
      if (nrow(lp50.rank)==1){
        file.create(paste(output.folder,"/single_year_only" , ".message" , sep=""))
      }else{
        # Rank LP50 values
        lp50.rank <- apply(lp50.rank,2,FUN = function(x) x[order(x,decreasing = F,na.last = T)]) %>% 
          .[,order(.[j,],decreasing = F,na.last = T)]
        
        # Now create plot input
        spatial.perc <- data.frame(RchID = colnames(lp50.rank))
        spatial.perc <- left_join(spatial.perc,unique(lp50[,c("RchID","lenght")]))
        spatial.perc$lenght <- 1
        spatial.perc$cumsum <- cumsum(spatial.perc$lenght)
        spatial.perc$x <- (100/sum(spatial.perc$lenght))*(spatial.perc$cumsum - (spatial.perc$lenght/2))
        spatial.perc <- spatial.perc[,c(1,4)]
        
        temp.perc <- data.frame(Yr = 1:nrow(lp50.rank))
        temp.perc$y <- (100/nrow(lp50.rank))*(temp.perc$Yr - (1/2))
        temp.perc$Yr <- as.character(temp.perc$Yr)
        
        plot.df <- as.data.frame(lp50.rank) 
        plot.df$Yr <- rownames(plot.df)
        plot.df <- plot.df %>% pivot_longer(.,cols = starts_with("R"),names_to = "RchID",values_to = "STLP50") %>% as.data.frame()
        plot.df <- left_join(plot.df,temp.perc)
        plot.df <- left_join(plot.df,spatial.perc)
        plot.df$STLP50 <- factor(with(plot.df, ifelse(STLP50 < 1,1,
                                                      ifelse(STLP50 > 1 & STLP50 < 10,2,
                                                             ifelse(STLP50 > 10 & STLP50 < 100,3,
                                                                    ifelse(STLP50 > 100 & !(STLP50 ==Inf), 4,
                                                                           ifelse(STLP50 == Inf,5,STLP50)))))),levels = c("1","2","3","4","5"))
        
        sorted.dfs[[paste0("CondP_",j,"_",combinations$Species[i],"_",combinations$Model_type[i])]] <- plot.df[plot.df$y==min(plot.df$y),]
        
        p<-ggplot(plot.df,aes(as.factor(x),as.factor(y))) + geom_tile(aes(fill = STLP50)) +
          scale_x_discrete(paste0("Spatial percentile (n = ",nrow(spatial.perc),")"),breaks = as.factor(plot.df$x[round(seq(1,nrow(plot.df),length.out = 20))]),
                           labels = plot.df$x[round(seq(1,nrow(plot.df),length.out = 20))] %>% round() %>% as.character()) +
          scale_y_discrete(paste0("Temporal percentile (n = ",nrow(temp.perc),")")) +
          scale_fill_manual("LP50", values = c("1" = "red","2" = "orange","3" = "yellow","4" = "forestgreen","5" = "lightskyblue1"),
                            label = c("LP50 < 1", "1 < LP50 < 10", "10 < LP50 < 100","LP50 > 100","Effect free year","No effect"),
                            drop = F,na.value = "lightsteelblue3") +
           ggtitle(paste0("Model: " ,model.name, " ", model.version)) + theme(text = element_text(size = 12),plot.title = element_text(size = 5))
        ggsave(plot = p,paste0(output.folder,"ALL_",combinations$Species[i],"_",combinations$Model_type[i],"_conditioningPercentile",temp.perc$y[j],".png"),
               width = 20, height = 15,units = "cm",dpi = 200)
        
        ls.output[[paste0("CondP_",j,"_",combinations$Species[i],"_",combinations$Model_type[i])]] <- p
      }
    }
  }
  return(list(plots = ls.output, dfs = sorted.dfs))
}

######################### Residence time functions #############################
# Mean daily residence time (hours) during application window
readResidenceTimeFromStore <- function(data.store.fpath,reach.info, first.year,last.year,
                                       first.app.month,last.app.month,first.app.day,last.app.day){
  # initiate data store
  df <- H5File$new(filename = data.store.fpath,
                   mode = "r+")
  # Get reach IDs
  rch <- df[["Hydrology/Reaches"]]$read() %>% paste0("R",.)
  # create hourly time series
  start.time <- df[["Hydrology/TimeSeriesStart"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  end.time <- df[["Hydrology/TimeSeriesEnd"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  time.period <- data.frame(Tstamp = seq(start.time,(end.time+(3600)*24),by=3600))
  time.period$Year <- format(time.period$Tstamp,"%Y") %>% as.numeric()
  time.period$Month <- format(time.period$Tstamp,"%m") %>% as.numeric()
  time.period$Day <- format(time.period$Tstamp,"%d") %>% as.numeric()
  time.period$Date <- format(time.period$Tstamp,"%d-%m-%Y") %>% as.character()
  
  s <- reach.info$bankslope[order(reach.info$RchID)]
  L <- reach.info$lenght[order(reach.info$RchID)]
  w <- reach.info$btmwidth[order(reach.info$RchID)]
  
  # Select only years after 1998 and month May for further residence time analysis in paper
  t.Q <- df[["Hydrology/Flow"]][,(time.period$Day>=first.app.day & time.period$Day <= last.app.day &
                                    time.period$Month>=first.app.month & time.period$Month <= last.app.month &
                                    time.period$Year>=first.year & time.period$Year <= last.year)]
  t.D <- df[["Hydrology/Depth"]][,(time.period$Day>=first.app.day & time.period$Day <= last.app.day &
                                     time.period$Month>=first.app.month & time.period$Month <= last.app.month &
                                     time.period$Year>=first.year & time.period$Year <= last.year)]
  # Convert cubic metre / day to cubic metre per hour
  t.Q <- (t.Q / 24)
  
  # Calculate hourly residence time FOR T-SHAPE!!!!!!
  residence.time <- data.frame(((t.D * (w + t.D * s)) * L) / t.Q)
  colnames(residence.time) <- time.period$Date[(time.period$Day>=first.app.day & time.period$Day <= last.app.day &
                                                  time.period$Month>=first.app.month & time.period$Month <= last.app.month &
                                                  time.period$Year>=first.year & time.period$Year <= last.year)]
  residence.time$RchID <- rch
  residence.time <- pivot_longer(residence.time,cols = -c("RchID"),names_to = "Date",values_to = "Residence_time") %>% data.frame()
  # Add depth information
  t.D <- as.data.frame(t.D)
  colnames(t.D) <- time.period$Date[(time.period$Day>=first.app.day & time.period$Day <= last.app.day &
                                       time.period$Month>=first.app.month & time.period$Month <= last.app.month &
                                       time.period$Year>=first.year & time.period$Year <= last.year)]
  t.D$RchID <- rch
  residence.time$Depth <- pivot_longer(data = t.D,cols = -c("RchID"),names_to = "Date",values_to = "Depth") %>% as.data.frame() %>% .$Depth
  
  # Aggregate over day to get daily average of residence time (hours)
  res.time <- aggregate(.~ Date*RchID, data = residence.time[,c("RchID","Date","Residence_time","Depth")], FUN = mean)
  res.time <- left_join(res.time,unique(time.period[,c("Date","Year")]))
  
  return(res.time)
}

# Max loading drift during application window
readLoadingDriftFromStore <- function(data.store.fpath,reach.info, first.year,last.year,
                                      first.app.month,last.app.month,first.app.day,last.app.day){
  # initiate data store
  df <- H5File$new(filename = data.store.fpath,
                   mode = "r+")
  # Get reach IDs
  rch <- df[["Hydrology/Reaches"]]$read() %>% paste0("R",.)
  # create hourly time series
  start.time <- df[["Hydrology/TimeSeriesStart"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  end.time <- df[["Hydrology/TimeSeriesEnd"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  time.period <- data.frame(Tstamp = seq(start.time,(end.time+(3600)*24) - 1,by=3600))
  time.period$Year <- format(time.period$Tstamp,"%Y") %>% as.numeric()
  time.period$Month <- format(time.period$Tstamp,"%m") %>% as.numeric()
  time.period$Day <- format(time.period$Tstamp,"%d") %>% as.numeric()
  time.period$Date <- format(time.period$Tstamp,"%d-%m-%Y") %>% as.character()
  load.period <- unique(time.period[,c("Year","Month","Day","Date")]) # OK to remove hardcoded subsetting with fix in L313 above.
  
  # Get loading drift information
  t.L <- df[["DepositionToReach/Deposition"]][,(load.period$Day>=first.app.day & load.period$Day <= last.app.day &
                                                  load.period$Month>=first.app.month &load.period$Month <= last.app.month &
                                                  load.period$Year>=first.year & load.period$Year <= last.year)]
  t.L <- as.data.frame(t.L)
  colnames(t.L) <- load.period$Date[(load.period$Day>=first.app.day & load.period$Day <= last.app.day &
                                       load.period$Month>=first.app.month &load.period$Month <= last.app.month &
                                       load.period$Year>=first.year & load.period$Year <= last.year)]
  t.L$RchID <- rch
  t.L <- pivot_longer(t.L,cols = -RchID,names_to = "Date",values_to = "Loading")
  t.L <- left_join(t.L,load.period[,c("Year","Date")])
  loading.drift <- aggregate(.~ RchID*Year, data = t.L[,c("RchID","Year","Loading")], FUN = max)
  # Convert units from g/ha to mg/m2
  loading.drift$Loading <- loading.drift$Loading / 10
  
  return(loading.drift)
}

createResidenceTimeDepthByStrahlerPlot <- function(residence.times,reach.info,application.window, first.year,last.year, model.name = NULL, model.version = NULL){
  reach.info <- left_join(reach.info,aggregate(.~RchID,residence.times[,c("RchID","Residence_time","Depth")],FUN = median))
  
  R.residence.time.by.order <- ggplot(data = reach.info, aes(x = as.factor(strahler), y = Residence_time * 60, group = as.factor(strahler))) +
    geom_violin(fill = "lightblue",scale = "count") +
    stat_summary(fun=median, geom="point", size=1, color="red") +
    scale_y_continuous(breaks = c(0.5,1,2,5,10,15,30,60,120),
                       labels = as.character(c(0.5,1,2,5,10,15,30,60,120))) +
    xlab("Strahler order") + ylab("Median residence time (minutes)") +
    theme_bw() + coord_trans(y = "log10") + theme(title = element_text(size = 12), plot.title = element_text(hjust = 0.5), 
                                                  legend.title = element_text(size = 12),
                                                  axis.text = element_text(size = 12),
                                                  axis.title = element_text(size = 12)) +
    ggtitle(paste0("Period: ", application.window, "\nbetween ",first.year, " and ",last.year))
  
  
  R.depth.by.order <- ggplot(data = reach.info, aes(x = as.factor(strahler), y = log2(Depth))) +
    geom_violin(fill = "lightblue",scale = "count") +
    stat_summary(fun=median, geom="point", size=1, color="red") +
    scale_y_continuous(breaks = log2(c(0,0.1,0.2,0.3,0.4,0.5)),
                       labels = as.character(signif(c(0,0.1,0.2,0.3,0.4,0.5),2))) +
    xlab("Strahler order") + ylab("Median depth (m)") +
    theme_bw() + theme(title = element_text(size = 5), 
                       legend.title = element_text(size = 12),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 12)) +
     ggtitle(paste0("Model: " ,model.name, " ", model.version))
  
  return(R.depth.by.order + R.residence.time.by.order)
}

# Plot for residence times, loading, PECs, and LP50
createRtimeLoadingPECsLP50Plot <- function(focal.year,first.app.day,last.app.day,first.app.month,last.app.month,
                                           residence.times,loading.drift,max.pec,lp50,scenario.path, model.name = NULL, model.version = NULL){
  #######################################
  ##### Spatial residence time plot #####
  #######################################
  riv.rummen <- readDRNFromScenario(scenario.path = scenario.path)
  riv.rummen@data$RchID <- paste0("R",as.character(riv.rummen@data$key))
  
  # Add residence time data for date of application
  riv.rummen@data <- left_join(riv.rummen@data, aggregate(.~RchID,data = residence.times[residence.times$Year==focal.year,c("RchID","Residence_time")],FUN = median))
  
  ## Plot of river network
  Rivs_f <- fortify(riv.rummen)
  riv_df <- data.frame(id = as.character(as.numeric(lapply(riv.rummen@lines, function(x) x@ID) %>% do.call(rbind,.))),riv.rummen@data)
  Rivs_f <- left_join(Rivs_f, riv_df,by = "id")
  
  seqfun <- function(input,n.steps = 4){seq(as.numeric(input[1]),as.numeric(input[2]),length.out = n.steps)}
  grad.colours <- seqfun(quantile(log10(riv.rummen@data$Residence_time),c(0.025,0.975)),n.steps = 5)
  
  R.catchment.plot <-ggplot() + # define variables
    geom_path(data = Rivs_f,
              aes(x = long, y = lat, group = group,colour = log10(Residence_time)),size = 1.25) + # plot rivers
    # scale_size_manual(values = c(1,1.5,2,2.5)) +
    scale_colour_gradientn(paste0("Mean residence time (hours)\nPeriod: ",first.app.day,"-",first.app.month,"/",
                                  last.app.day,"-",last.app.month," in ",focal.year),breaks = grad.colours,
                           # values = log10(as.numeric(summary(riv.rummen@data$residence_time))[c(-4,-6)]),
                           colours = c("green","yellow","orange","red"),
                           labels = as.character(round(10^grad.colours,digits = 2)),
                           limits = c(grad.colours[1],grad.colours[5])) +
    coord_equal() + guides(size = "none") +
    labs(x = "X (m)", y = "Y (m)")+
     ggtitle(paste0("Model: " ,model.name, " ", model.version)) +
    theme_linedraw() + theme_light() + theme(title = element_text(size = 5),
                                             legend.title = element_text(size = 10),
                                             axis.text = element_text(size = 10),
                                             axis.title = element_text(size = 10))
  
  #######################################
  ###### Spatial loading drif plot ######
  #######################################
  riv.rummen <- readDRNFromScenario(scenario.path = scenario.path)
  riv.rummen@data$RchID <- paste0("R",as.character(riv.rummen@data$key))
  
  # Add residence time data for date of application
  riv.rummen@data <- left_join(riv.rummen@data,loading.drift[loading.drift$Year==focal.year,])
  
  ## Plot of river network
  Rivs_f <- fortify(riv.rummen)
  riv_df <- data.frame(id = as.character(as.numeric(lapply(riv.rummen@lines, function(x) x@ID) %>% do.call(rbind,.))),riv.rummen@data)
  Rivs_f <- left_join(Rivs_f, riv_df,by = "id")
  
  grad.colours <- seqfun(quantile(riv.rummen@data$Loading[!is.na(riv.rummen@data$Loading)&riv.rummen@data$Loading>0],c(0.05,0.95)),n.steps = 5)
  
  L.catchment.plot <-ggplot() + # define variables
    geom_path(data = Rivs_f,
              aes(x = long, y = lat, group = group,colour = Loading),size = 1.25) + # plot rivers
    # scale_size_manual(values = c(1,1.5,2,2.5)) +
    scale_colour_gradientn(paste0("Maximum loading (mg/m^2)\nPeriod: ",first.app.day,"-",first.app.month,"/",
                                  last.app.day,"-",last.app.month," in ",focal.year),breaks = grad.colours,
                           # values = log10(as.numeric(summary(riv.rummen@data$residence_time))[c(-4,-6)]),
                           colours = c("green3","yellow","red"),
                           labels = as.character(signif(grad.colours,digits = 2)),
                           limits = c(grad.colours[1]*0.95,grad.colours[5]*1.1),
                           na.value = "grey50") +
    coord_equal() + guides(size = "none") +
    labs(x = "X (m)", y = "Y (m)")+
     ggtitle(paste0("Model: " ,model.name, " ", model.version)) +
    theme_linedraw() + theme_light()+ theme(title = element_text(size = 5),
                                            legend.title = element_text(size = 10),
                                            axis.text = element_text(size = 10),
                                            axis.title = element_text(size = 10))
  
  #######################################
  ###### Spatial maximum PEC plot #######
  #######################################
  riv.rummen <- readDRNFromScenario(scenario.path = scenario.path)
  riv.rummen@data$RchID <- paste0("R",as.character(riv.rummen@data$key))
  
  # Add residence time data for date of application
  riv.rummen@data <- left_join(riv.rummen@data,max.pec[max.pec$Year == focal.year, c("RchID","Max_PEC_Avg")])
  
  ## Plot of river network
  Rivs_f <- fortify(riv.rummen)
  riv_df <- data.frame(id = as.character(as.numeric(lapply(riv.rummen@lines, function(x) x@ID) %>% do.call(rbind,.))),riv.rummen@data)
  Rivs_f <- left_join(Rivs_f, riv_df,by = "id")
  
  grad.colours <- seqfun(range(log10(riv.rummen@data$Max_PEC_Avg[riv.rummen@data$Max_PEC_Avg > 10^-6]),na.rm = T),n.steps = 5)
  
  P.catchment.plot <-ggplot() + # define variables
    geom_path(data = Rivs_f,
              aes(x = long, y = lat, group = group,colour = log10(Max_PEC_Avg)),size = 1.25) + # plot rivers
    # scale_size_manual(values = c(1,1.5,2,2.5)) +
    scale_colour_gradientn(paste0("Concentration (","\u00B5","g/L)\nPeriod: ",first.app.day,"-",first.app.month,"/",
                                  last.app.day,"-",last.app.month," in ",focal.year),breaks = grad.colours,
                           colours = c("green3","yellow","red"),
                           labels = as.character(signif(10^grad.colours,digits = 3)),
                           limits = c(grad.colours[1]*0.9,grad.colours[5]*2),
                           na.value = "grey50") +
    coord_equal() + guides(size = "none") +
    labs(x = "X (m)", y = "Y (m)")+
     ggtitle(paste0("Model: " ,model.name, " ", model.version)) +
    theme_linedraw() + theme_light()+ theme(title = element_text(size = 5),
                                            legend.title = element_text(size = 10),
                                            axis.text = element_text(size = 10),
                                            axis.title = element_text(size = 10))
  
  #######################################
  ######### Spatial LP50 plot ###########
  #######################################
  riv.rummen <- readDRNFromScenario(scenario.path = scenario.path)
  riv.rummen@data$RchID <- paste0("R",as.character(riv.rummen@data$key))
  
  # Add residence time data for date of application
  riv.rummen@data <- left_join(riv.rummen@data,
                               lp50[as.numeric(lp50$Year) == focal.year & lp50$Model_type == "it" & lp50$Species=="Cloeon_dipterum",c("RchID","LP50")])
  
  ## Plot of river network
  Rivs_f <- fortify(riv.rummen)
  riv_df <- data.frame(id = as.character(as.numeric(lapply(riv.rummen@lines, function(x) x@ID) %>% do.call(rbind,.))),riv.rummen@data)
  Rivs_f <- left_join(Rivs_f, riv_df,by = "id")
  Rivs_f$LP50 <- factor(with(Rivs_f, ifelse(LP50 < 1,1,
                                                  ifelse(LP50 > 1 & LP50 < 10,2,
                                                         ifelse(LP50 > 10 & LP50 < 100,3,
                                                                ifelse(LP50 > 100 & !(LP50 ==Inf), 4,
                                                                       ifelse(LP50 == Inf,5,LP50)))))),levels = c("1","2","3","4","5"))
  
  LP50.catchment.plot <-ggplot() + # define variables
    geom_path(data = Rivs_f,
              aes(x = long, y = lat, group = group,colour = LP50),size = 1.25) + # plot rivers
    scale_colour_manual("LP50", values = c("1" = "red","2" = "orange","3" = "yellow","4" = "forestgreen", "5" = "lightskyblue1"),
                      label = c("LP50 < 1", "1 < LP50 < 10", "10 < LP50 < 100","LP50 > 100","No effect"),
                      drop = F,na.value = "lightsteelblue3") +
    coord_equal() + guides(size = "none") +
    labs(x = "X (m)", y = "Y (m)")+
     ggtitle(paste0("Model: " ,model.name, " ", model.version)) +
    theme_linedraw() + theme_light()+ theme(title = element_text(size = 5),
                                            legend.title = element_text(size = 10),
                                            axis.text = element_text(size = 10),
                                            axis.title = element_text(size = 10))
  return((R.catchment.plot + L.catchment.plot) / (P.catchment.plot + LP50.catchment.plot))
}

printLoadingVsTransferTable <- function(reach.info, max.pec, loading.drift){
  
  max.pec <- left_join(max.pec,reach.info[,c("RchID","strahler")])
  reach.infos <- left_join(reach.info, aggregate(.~RchID, loading.drift[,c("RchID","Loading")],FUN = max))
  
  zz <- cbind(aggregate(.~strahler, data = reach.infos[,c("strahler","Loading")],FUN = length),
              aggregate(.~strahler, data = reach.infos[,c("strahler","Loading")],FUN = function(x) sum(x>0)))
  
  zzz <- aggregate(.~strahler*RchID, data = max.pec[,c("strahler","Max_PEC_Avg","RchID")],FUN = function(x) min(x))
  zzz <- cbind(aggregate(.~strahler, data = zzz[,c("strahler","Max_PEC_Avg")],FUN = length),
               aggregate(.~strahler, data = zzz[,c("strahler","Max_PEC_Avg")],FUN = function(x) sum(x>0)))
  zzz <- cbind(zzz,zz)
  zzz <- zzz[,c(1,2,4,8)]
  names(zzz) <- c("strahler","N","Conc_above_0","Loading_drift_input")
  
  zzz$Transfer_input_only <- zzz$Conc_above_0 - zzz$Loading_drift_input
  zzz$perc_trans_loading <- round((zzz$Conc_above_0/zzz$N)*100,1)
  
  zzz$perc_trans_only <- round((zzz$Transfer_input_only/zzz$N)*100,1)
  zzz$perc_no_substance <- 100 - zzz$perc_trans_loading
  
  zzz <- zzz[,c(1,2,6,7,8)]
  
  print(patchwork::wrap_elements(gridExtra::tableGrob(zzz,
                                                      cols = c("Strahler order", "Number of reaches", 
                                                               "Percentage receiving\nloading drift and transfer",
                                                               "Percentage receiving\ntransfer only", 
                                                               "Percentage receiving\nno substance"))))
  
}
