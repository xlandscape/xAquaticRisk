# Functions for analysing Landscape model run data
checkAndLoadPackages <- function(packages = installed.packages(),
                                 required.packages = c("devtools","tidyverse","scales", "gridExtra","patchwork","hdf5r","pbapply",
                                                       "sp","raster","rgeos","rgdal")){
  required.packages <- required.packages
  not.installed <- required.packages[!required.packages %in% packages[,"Package"]]
  if(length(not.installed)==0){
    print("Required packages already installed, onwards and upwards!")
    print("Ignore any further warnings, these are known and of no consequence!")
    lapply(required.packages,require, character.only = T)}else{
      print("You need some additional packages, installing now.")
      print("Choose Yes to update packages if already loaded")
      print("Ignore any further warnings, these are known and of no consequence!")
      for (j in not.installed){
        if(j == "devtools"){install.packages(j)}else{require("devtools",character.only = T);install.packages(j)}
      }
      lapply(required.packages,require, character.only = T)
    }
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

######################### PEC functions ########################################
readPECDataFromStore <- function(data.store.fpath,first.year,last.year){
  # initiate data store
  df <- H5File$new(filename = data.store.fpath,
                   mode = "r+")
  # create hourly time series
  start.time <- df[["Hydrology/TimeSeriesStart"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  end.time <- df[["Hydrology/TimeSeriesEnd"]]$read() %>% strptime(.,format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
  time.period <- data.frame(Tstamp = seq(start.time,(end.time+(3600)*24),by=3600))
  time.period$Year <- format(time.period$Tstamp,"%Y") %>% as.numeric()
  time.period$Month <- format(time.period$Tstamp,"%m") %>% as.numeric()

  # Get reach IDs
  rch <- df[["Hydrology/Reaches"]]$read() %>% paste0("R",.)

  df.1 <- pblapply(unique(time.period$Year) %>% .[.>=first.year&.<=last.year], function(yr){
    # initiate data store for each core
    df <- H5File$new(filename = data.store.fpath,
                     mode = "r+")
    t1 <- df[["CascadeToxswa/ConLiqWatTgtAvgHrAvg"]][,(time.period$Year==yr & time.period$Month %in% c(4,5) & time.period$Year>=first.year & time.period$Year <= last.year)]
    t2 <- data.frame(Year = yr, RchID = rch, Max_PEC_Avg = apply(t1,1,function(x) max(x)))
    # convert g/m3 to ug/L
    t2$Max_PEC_Avg <- (t2$Max_PEC_Avg * 1000)
    t2
  }) %>% do.call(rbind,.)
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

createPECbyStrahlerPlot <- function(max.pec,reach.info,medPEC){
  vpos <- aggregate(.~strahler,data = reach.info[,c("strahler","width")],FUN = length)
  vpos <- vpos[order(vpos$strahler),]
  row.names(vpos) <- NULL
  vpos$x <- cumsum(vpos$width)

  max.pec <- max.pec[,c("RchID","Year","strahler","Max_PEC_Avg")]
  colnames(max.pec) <- c("Reach","Year","Strahler","Max_PEC")
  max.pec <- max.pec[order(as.numeric(max.pec$Strahler),max.pec$Max_PEC),]
  max.pec <- left_join(max.pec,unique(medPEC[,c("Reach", "rowID")]))
  max.pec$Max_PEC <- ifelse(max.pec$Max_PEC==0,NA,max.pec$Max_PEC)

  PEC.plots <- ggplot() +
    # Basic line plot
    geom_point(data = max.pec,
               aes(x = rowID,y = log10(Max_PEC),
                   group = as.factor(Strahler)),
               colour = "red",
               lwd = 0.75,alpha = 0.05) +
    geom_line(data = medPEC,
              aes(x = rowID,y = log10(Max_PEC),
                  group = as.factor(strahler)),colour = "black",lwd = 0.75) +
    guides(colour = F) +
    scale_y_continuous(paste0("Concentration (","\u00B5","g/L)"),
                       # breaks = trans_breaks("log10", function(x) 10^x,n = 8),
                       # labels = trans_format("log10", label_math(10^.x)),
                       breaks = c(-6,-5,-4,-3,-2,-1),
                       labels = c("0.000001","0.00001","0.0001","0.001","0.01","0.1")) +
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
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape") +
    theme_bw() +
    theme(text = element_text(size = 12),axis.text = element_text(size = 12),plot.title = element_text(size = 6,hjust = 1)) +
    geom_text(data = data.frame(x = vpos$x,y = c(-2,-2,-2,-2),
                                labels = paste0(c("Strahler order 1",as.character(vpos$strahler[!vpos$strahler==1])))),
              aes(x = x, y = y, label = labels), size = 6, hjust = "inward") +
  return(PEC.plots)
}

####################### Lp50 functions ###########################
readLP50DataFromStore <- function(data.store.fpath,first.year,last.year,reach.info){
  # initiate data store
  df <- H5File$new(filename = data.store.fpath,
                   mode = "r+")
  # Get reach IDs
  rch <- reach.info$RchID

  # list with species and model versions
  ls.spec <- list.groups(df)[grepl("IndEffect_LP50",list.groups(df))]
  model.versions <- lapply(ls.spec,function(x){
    # get species name and model type from hdf5
    t1 <- df[[paste0(x,"/ProcessingPath")]]$read() %>% strsplit(.,"\\\\") %>% unlist()
    t2 <- strsplit(t1[grepl("ind_lp50_",t1)],"_") %>% unlist()  # todo
    t3 <- data.frame(Group = x,Model_type = t2[grepl("it|sd",t2)],Species = gsub(" ","_",t2[grepl("Asellus|Cloeon|Gammarus",t2)]))
    t3
  }) %>% do.call(rbind,.)

  # create year time series
  time.period <- data.frame(Year = (df[[paste0(ls.spec[1],"/SimulationStart")]]$read() %>% strsplit(.,"-") %>% unlist() %>% .[1] %>% as.numeric()):last.year)

  df.1 <- pblapply(ls.spec, function(x){
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

createLP50byStrahlerPlot <- function(LP50,reach.info,medLP50){
  vpos <- aggregate(.~strahler,data = reach.info[,c("strahler","width")],FUN = length)
  vpos <- vpos[order(vpos$strahler),]
  row.names(vpos) <- NULL
  vpos$x <- cumsum(vpos$width)

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
                 colour = "red",alpha = 0.05, lwd = 0.75) +
      geom_line(data = medLP50[medLP50$Species==model.versions$Species[j]&medLP50$Model_type==model.versions$Model_type[j],],
                aes(x = rowID,y = log10(LP50),
                    group = as.factor(strahler)),lwd = 0.75) +
      guides(colour = F) +
      scale_y_reverse("LP50",
                      breaks = c(-2,-1,0,1,2,3,4,5),
                      labels = c("0.01","0.1","1","10","100","1000","10000","100000")) +
      # Add horizontal lines for LP50 factors 1, 10, and 100
      geom_hline(data = data.frame(intercept = 0),
                 aes(yintercept = intercept),
                 colour = "red",
                 linetype = "dashed",
                 lwd = 0.75) +
      geom_hline(data = data.frame(intercept = 1),
                 aes(yintercept = intercept),
                 colour = "orange",
                 linetype = "dashed",
                 lwd = 0.75) +
      geom_hline(data = data.frame(intercept = 2),
                 aes(yintercept = intercept),
                 colour = "yellow",
                 linetype = "dashed",
                 lwd = 0.75) +
      # Add vertical lines to delineate Strahler order
      geom_vline(data = vpos[,1:3],
                 aes(xintercept = x),
                 colour = "black",
                 linetype = "dashed") +
      scale_x_continuous("Number of reaches (0 - 100% per Strahler order)",
                         breaks = c(vpos$x[1]/2,vpos$x[1],
                                    vpos$x[2]-(vpos$width[2]/2),vpos$x[2],
                                    vpos$x[3]-(vpos$width[3]/2),vpos$x[3],
                                    vpos$x[4]-(vpos$width[4]/2),vpos$x[4]),
                         labels = c("","100","","100","","100","","100"),
                         limits = c(0,max(vpos$x)),
                         expand = c(0,50)) +
      coord_cartesian(ylim = c(5,-2)) +
      ggtitle(paste0("Species: ",model.versions$Species[j],"\nModel type: ",model.versions$Model_type[j])) +
      theme_bw() +
      geom_text(data = data.frame(x = vpos$x,y = c(-2,-2,-2,-2),
                                  labels = paste0(c("Strahler order 1",as.character(vpos$strahler[!vpos$strahler==1])))),
                aes(x = x, y = y, label = labels), size = 6, hjust = "inward") +
      theme(text = element_text(size = 20),plot.title = element_text(size = 12))
  }
  return(LP50.plot)
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
  load.period <- unique(time.period[,c("Year","Month","Day","Date")])

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

createResidenceTimeDepthByStrahlerPlot <- function(residence.times,reach.info){
  reach.info <- left_join(reach.info,aggregate(.~RchID,residence.times[,c("RchID","Residence_time","Depth")],FUN = median))

  R.residence.time.by.order <- ggplot(data = reach.info, aes(x = as.factor(strahler), y = Residence_time)) +
    geom_boxplot(outlier.shape = NA,fill = "grey70") +
    geom_jitter(width = 0.2,
                shape = 21,
                colour = "black",
                fill = "blue",
                alpha = 0.05,
                size = 2) +
    scale_y_continuous(breaks = c(36,360,900,1800,3600,10800)/3600,
                       labels = as.character(signif(c(36,360,900,1800,3600,10800)/3600,2))) +
    xlab("Strahler order") + ylab("Median residence time (hours)\nPeriod: May 1998 - 2017") +
    theme_bw() + coord_trans(y = "log10") + theme(title = element_text(size = 5),
                                                  legend.title = element_text(size = 10),
                                                  axis.text = element_text(size = 10),
                                                  axis.title = element_text(size = 10))


  R.depth.by.order <- ggplot(data = reach.info, aes(x = as.factor(strahler), y = log2(Depth))) +
    geom_boxplot(outlier.shape = NA,fill = "grey70") +
    geom_jitter(width = 0.2,
                shape = 21,
                colour = "black",
                fill = "blue",
                alpha = 0.05,
                size = 2) +
    scale_y_continuous(breaks = log2(c(0,0.1,0.2,0.3,0.4,0.5)),
                       labels = as.character(signif(c(0,0.1,0.2,0.3,0.4,0.5),2))) +
    xlab("Strahler order") + ylab("Median depth (m)\nPeriod: May 1998 - 2017") +
    theme_bw() + theme(title = element_text(size = 5),
                       legend.title = element_text(size = 10),
                       axis.text = element_text(size = 10),
                       axis.title = element_text(size = 10)) +
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape")
  return(R.depth.by.order + R.residence.time.by.order)
}

# Plot for residence times, loading, PECs, and LP50
createRtimeLoadingPECsLP50Plot <- function(focal.year,first.app.day,last.app.day,first.app.month,last.app.month,
                                           residence.times,loading.drift,max.pec,lp50,scenario.path){
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
    coord_equal() + guides(size = F) +
    labs(x = "X (m)", y = "Y (m)")+
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape") +
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
    coord_equal() + guides(size = F) +
    labs(x = "X (m)", y = "Y (m)")+
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape") +
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
    coord_equal() + guides(size = F) +
    labs(x = "X (m)", y = "Y (m)")+
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape") +
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
  riv.rummen@data$LP50[is.infinite(riv.rummen@data$LP50)] <- NA
  grad.colours <- seqfun(log10(range(riv.rummen@data$LP50,na.rm = T)),n.steps = 5)

  LP50.catchment.plot <-ggplot() + # define variables
    geom_path(data = Rivs_f,
              aes(x = long, y = lat, group = group,colour = log10(LP50)),size = 1.25) + # plot rivers
    # scale_size_manual(values = c(1,1.5,2,2.5)) +
    scale_colour_stepsn(paste0("LP50 estimate\nPeriod: ",first.app.day,"-",first.app.month,"/",
                               last.app.day,"-",last.app.month," in ",focal.year),
                        breaks = c(0,1,2),
                        colours = c("red","yellow","green3","green3"),
                        labels = c("1","10","100"),
                        # limits = c(grad.colours[1]*1.1,grad.colours[5]*1.1),
                        na.value = "green3") +
    coord_equal() + guides(size = F) +
    labs(x = "X (m)", y = "Y (m)")+
    ggtitle("Model: xAquatic v2.45\nHydrology: diffusive wave, T-shape") +
    theme_linedraw() + theme_light()+ theme(title = element_text(size = 5),
                                            legend.title = element_text(size = 10),
                                            axis.text = element_text(size = 10),
                                            axis.title = element_text(size = 10))
  LP50.catchment.plot
  return((R.catchment.plot + L.catchment.plot) / (P.catchment.plot + LP50.catchment.plot))
}

createLP50AggregationPlots <- function(lp50,output.folder){
  #########################################################################################################
  ##################################### LP50 analysis #####################################################
  #########################################################################################################
  # Exclude Inf values as no exposure then
  lp50 <- lp50[!lp50$LP50==Inf,]
  # Option 1
  agg.reach.species.modeltype <- aggregate(.~Species*RchID*Model_type,
                                           data = lp50[,c("LP50","Model_type","Species","RchID")],
                                           FUN = function(x) quantile(x,0.1))
  agg.reach.modeltype <- aggregate(.~RchID*Model_type,
                                   data = agg.reach.species.modeltype[,c("RchID","Model_type","LP50")],
                                   FUN = mean)
  lbl <- aggregate(.~Model_type*Species, data = agg.reach.species.modeltype[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p1 <- ggplot(agg.reach.species.modeltype)+
    geom_density(aes(x = LP50,fill = as.factor(Species)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(0.4,0.3,0.2),2),label = LP50,colour = as.factor(Species))) +
    guides(colour = F, fill = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p1,filename = paste0(output.folder,"Option1a_10ile_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type, data = agg.reach.modeltype[,c("LP50","Model_type")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p2 <- ggplot(agg.reach.modeltype)+
    geom_density(aes(x = LP50,fill = as.factor(Model_type)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(0.4,0.3),colour = as.factor(Model_type),label = LP50)) +
    guides(colour = F, fill = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p2,filename = paste0(output.folder,"Option1a_10ile_species_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type*Species, data = agg.reach.species.modeltype[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p3 <- ggplot(agg.reach.species.modeltype)+
    stat_ecdf(aes(x = LP50,colour = as.factor(Species)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(0.4,0.3,0.2),2),label = LP50,colour = as.factor(Species)),show.legend = F) +
    guides(colour = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p3,filename = paste0(output.folder,"Option1b_10ile_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type, data = agg.reach.modeltype[,c("LP50","Model_type")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p4 <- ggplot(agg.reach.modeltype)+
    stat_ecdf(aes(x = LP50,colour = as.factor(Model_type)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(0.4,0.3),colour = as.factor(Model_type),label = LP50), show.legend = F) +
    guides(colour = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p4,filename = paste0(output.folder,"Option1b_10ile_species_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)

  # Option 2
  agg.reach.species.modeltype <- aggregate(.~Species*Model_type,
                                           data = lp50[,c("LP50","Model_type","Species")],
                                           FUN = function(x) quantile(x,0.1))
  agg.reach.modeltype <- aggregate(.~Model_type,
                                   data = agg.reach.species.modeltype[,c("Model_type","LP50")],
                                   FUN = mean)
  lbl <- aggregate(.~Model_type*Species, data = lp50[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p1 <- ggplot(lp50[,c("LP50","Model_type","Species")])+
    geom_density(aes(x = LP50,fill = as.factor(Species)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(0.4,0.3,0.2),2),label = LP50,colour = as.factor(Species))) +
    guides(colour = F, fill = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p1,filename = paste0(output.folder,"Option2a_10ile_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- agg.reach.modeltype %>% mutate(LP50 = round(LP50,2))
  p2 <- ggplot(lp50[,c("LP50","Model_type","Species")])+
    geom_density(aes(x = LP50,fill = as.factor(Model_type)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(0.4,0.3),colour = as.factor(Model_type),label = LP50)) +
    guides(colour = F, fill = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p2,filename = paste0(output.folder,"Option2a_10ile_species_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type*Species, data = lp50[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p3 <- ggplot(lp50[,c("LP50","Model_type","Species")])+
    stat_ecdf(aes(x = LP50,colour = as.factor(Species)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(0.4,0.3,0.2),2),label = LP50,colour = as.factor(Species)),show.legend = F) +
    guides(colour = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p3,filename = paste0(output.folder,"Option2b_10ile_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- agg.reach.modeltype %>% mutate(LP50 = round(LP50,2))
  p4 <- ggplot(lp50[,c("LP50","Model_type","Species")])+
    stat_ecdf(aes(x = LP50,colour = as.factor(Model_type)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(0.4,0.3),colour = as.factor(Model_type),label = LP50), show.legend = F) +
    guides(colour = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p4,filename = paste0(output.folder,"Option2b_10ile_species_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)


  # Option 3
  agg.year.species.modeltype <- aggregate(.~Species*Year*Model_type,
                                          data = lp50[,c("LP50","Model_type","Species","Year")],
                                          FUN = function(x) quantile(x,0.1))
  agg.year.modeltype <- aggregate(.~Year*Model_type,
                                  data = agg.year.species.modeltype[,c("Year","Model_type","LP50")],
                                  FUN = mean)
  lbl <- aggregate(.~Model_type*Species, data = agg.year.species.modeltype[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p1 <- ggplot(agg.year.species.modeltype)+
    geom_density(aes(x = LP50,fill = as.factor(Species)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(4,3,2),2),label = LP50,colour = as.factor(Species))) +
    guides(colour = F, fill = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p1,filename = paste0(output.folder,"Option3a_10ile_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type, data = agg.year.modeltype[,c("LP50","Model_type")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p2 <- ggplot(agg.year.modeltype)+
    geom_density(aes(x = LP50,fill = as.factor(Model_type)),alpha = 0.5,stat="density")+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(4,3),colour = as.factor(Model_type),label = LP50)) +
    guides(colour = F, fill = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p2,filename = paste0(output.folder,"Option3a_10ile_species_time.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type*Species, data = agg.year.species.modeltype[,c("LP50","Model_type","Species")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p3 <- ggplot(agg.year.species.modeltype)+
    stat_ecdf(aes(x = LP50,colour = as.factor(Species)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Species)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = rep(c(0.4,0.3,0.2),2),label = LP50,colour = as.factor(Species)),show.legend = F) +
    guides(colour = guide_legend(title = "Species")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000)) +
    facet_wrap(.~Model_type,ncol = 1)
  ggsave(plot = p3,filename = paste0(output.folder,"Option3b_10ile_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)

  lbl <- aggregate(.~Model_type, data = agg.year.modeltype[,c("LP50","Model_type")],
                   FUN = function(x) round(quantile(x,0.1),2))
  p4 <- ggplot(agg.year.modeltype)+
    stat_ecdf(aes(x = LP50,colour = as.factor(Model_type)),lwd = 1)+
    geom_vline(data = lbl,aes(xintercept = LP50,colour = as.factor(Model_type)),lwd = 1) +
    geom_label(data = lbl,aes(x = LP50,y = c(0.4,0.3),colour = as.factor(Model_type),label = LP50), show.legend = F) +
    guides(colour = guide_legend(title = "Model type")) +
    scale_x_log10(breaks = c(0.1,1,10,100,1000,10000,100000),limits = c(0.01,100000))
  ggsave(plot = p4,filename = paste0(output.folder,"Option3b_10ile_species_time_cumulfreq.png"),width = 20,height = 15,units = "cm",dpi = 200)

}

createSpatialTemporalLP50Plots <- function(lp50,reach.info,output.folder,temporal.conditioning.percentile){
  lp50.mat <- lp50[grepl("Cascade", lp50$Group),c("LP50","Year","RchID","Species","Model_type")] %>% pivot_wider(.,names_from = "RchID",values_from = "LP50") %>% as.data.frame()  # todo
  lp50 <- left_join(lp50,reach.info,by = "RchID")

  combinations <- unique(lp50.mat[,c("Species","Model_type")])

  for (i in 1:nrow(combinations)){
    lp50.rank <- lp50.mat[lp50.mat$Species==combinations$Species[i]&lp50.mat$Model_type==combinations$Model_type[i],-c(2,3)]
    rownames(lp50.rank) <- lp50.rank$Year
    lp50.rank <- lp50.rank[,-1] %>% as.matrix()
    # give unexposed reaches NA and exclude from analysis
    lp50.rank[lp50.rank==Inf]<-NA
    no.exposure <- apply(lp50.rank,2,function(x) sum(is.na(x))==length(x))
    lp50.rank <- lp50.rank[,!no.exposure, drop = FALSE]

    # Matrix of reach weights
    col.weights <- lp50[,c("RchID","lenght")] %>% unique()
    col.weights <- col.weights[!no.exposure,]
    wg.mat <- matrix(rep(col.weights$lenght,nrow(lp50.rank)),nrow = nrow(lp50.rank), byrow = T)
    wg.mat[,] <- 1

    SG <- sum(col.weights$lenght) * nrow(lp50.rank)
    # matrix of ranks
    rank.a <- lp50.rank
    rank.a[]<- rank(rank.a,na.last = T)

    #matrix of cumsums weights
    sg <- as.data.frame(rank.a)
    sg$Year <- rownames(sg)
    sg <- pivot_longer(sg,cols = starts_with("R"),names_to = "RchID",values_to = "Rank") %>%
      as.data.frame() %>% left_join(.,col.weights) %>% .[order(.$Rank,decreasing = F,na.last = T),]
    sg$cumsum <- cumsum(sg$lenght)
    sg.mat <- apply(rank.a,2,FUN = function(x) sg$cumsum[x])
    if (!is.matrix(sg.mat)) sg.mat <- t(as.matrix(sg.mat))
    rownames(sg.mat) <- rownames(rank.a)

    for (j in temporal.conditioning.percentile){
      # matrix of ranked percentiles
      pg <- (100/SG)*(sg.mat - (wg.mat/2))
      # sort pg by years (within column)
      pg <- apply(pg,2,FUN = function(x) x[order(x,decreasing = F,na.last = T)])
      if (!is.matrix(pg)) pg <- t(as.matrix(pg))
      # then sort pg by location (within row) using a specific temporal percentile for conditioning
      pg <- pg[,order(pg[j,],decreasing = F,na.last = T)]
      if (!is.matrix(pg)) pg <- t(as.matrix(pg))


      # Now create plot
      spatial.perc <- data.frame(RchID = colnames(pg))
      spatial.perc <- left_join(spatial.perc,unique(lp50[,c("RchID","lenght")]))
      spatial.perc$lenght <- 1
      spatial.perc$cumsum <- cumsum(spatial.perc$lenght)
      spatial.perc$x <- (100/sum(spatial.perc$lenght))*(spatial.perc$cumsum - (spatial.perc$lenght/2))
      spatial.perc <- spatial.perc[,c(1,4)]

      temp.perc <- data.frame(Yr = 1:nrow(pg))
      temp.perc$y <- (100/nrow(pg))*(temp.perc$Yr - (1/2))
      temp.perc$Yr <- as.character(temp.perc$Yr)

      plot.df <- as.data.frame(pg)
      plot.df$Yr <- rownames(plot.df)
      plot.df <- plot.df %>% pivot_longer(.,cols = starts_with("R"),names_to = "RchID",values_to = "STLP50") %>% as.data.frame()
      plot.df <- left_join(plot.df,temp.perc)
      plot.df <- left_join(plot.df,spatial.perc)

      p<-ggplot(plot.df,aes(as.factor(round(x)),as.factor(round(y)))) + geom_tile(aes(fill = STLP50)) +
        scale_x_discrete("Spatial percentile",breaks = seq(0,100,by=10), labels = as.character(seq(0,100,by=10))) +
        scale_y_discrete("Temporal percentile") +
        scale_fill_binned("Spatiotemporal\npercentile (LP50)",
                          breaks = seq(10,90,by=10),
                          label = paste0(seq(10,90,by=10)," (",
                                         quantile(x = lp50.rank,probs = seq(0.1,0.9,0.1),na.rm = T) %>% signif(.,digits = 2) %>% round(.,1),
                                         ")"),
                          type = "viridis") +
        ggtitle(paste0("Temporal conditioning percentile = ",round(temp.perc$y[j]),"%-ile","\nSpecies = ",gsub("_"," ",combinations$Species[i]),
                       ", Model type = ",switch(combinations$Model_type[i],it="Individual threshold",
                                                sd = "Stochastic death"))) + theme(text = element_text(size = 12))
      ggsave(plot = p,paste0(output.folder,combinations$Species[i],"_",combinations$Model_type[i],"_conditioningPercentile",temp.perc$y[j],".png"),
             width = 20, height = 15,units = "cm",dpi = 200)
    }
  }
}
