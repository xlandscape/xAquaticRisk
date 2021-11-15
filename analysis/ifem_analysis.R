# Parse command-line arguments
library(optparse)
params <- parse_args(
  OptionParser(
    option_list = list(
      make_option("--source", type = "character", help = "The path to the source file containing predefined functions"),
      make_option("--hydrography", type = "character", help = "The path to the shapefile containing the hydrography"),
      make_option("--start", type = "character", help = "The first day of the simulation period"),
      make_option("--end", type = "character", help = "The last day of the simulation period"),
      make_option("--application_window", type = "character", help = "The application period"),
      make_option("--lib", type = "character", help = "Additional library path for R packages")
    )
  ),
  positional_arguments = TRUE
)
.libPaths(c(params$options$lib, .libPaths()))
params$first_year <- as.integer(substr(params$options$start, 1, 4))
params$last_year <- as.integer(substr(params$options$end, 1, 4))
params$first_app_month <- as.integer(substr(params$options$application_window, 1, 2))
params$first_app_day <- as.integer(substr(params$options$application_window, 4, 5))
params$last_app_month <- as.integer(substr(params$options$application_window, 10, 11))
params$last_app_day <- as.integer(substr(params$options$application_window, 13, 14))

source(params$options$source)
# Script for analysis of ifem outputs
checkAndLoadPackages()

reach.info <- getReachInfoFromScenario(scenario.path = params$options$hydrography)  # todo
############################################
################ PEC data analysis #########
############################################
# Read in PEC data from data store
max.pec <- readPECDataFromStore(data.store.fpath = params$args[1], first.year = params$first_year, last.year = params$last_year)

# Add reach.info for plotting
max.pec <- left_join(max.pec,reach.info)

# Calculate median values
medPEC <- medianPECValues(max.pec = max.pec,reach.info = reach.info)
# Plot PECS by strahler order
PEC.plot <- createPECbyStrahlerPlot(max.pec = max.pec,reach.info = reach.info, medPEC = medPEC)
ggsave(plot = PEC.plot, filename = file.path(params$args[2], paste0("PEC_", params$first_year, "-", params$last_year, "_medians_points.png")), width = 20, height = 15, units = "cm", dpi = 400)

############################################
################ LP50 analysis #############
############################################
lp50 <- readLP50DataFromStore(data.store.fpath = params$args[1], first.year = params$first_year, last.year = params$last_year, reach.info = reach.info)
# Calculate median values
medLP50 <- medianLP50Values(lp50 = lp50,reach.info = reach.info)
# Create list with plots for species and model type combinations
LP50.plot <- createLP50byStrahlerPlot(LP50 = lp50,reach.info = reach.info,medLP50 = medLP50)
# Save outputs
for(i in 1:length(LP50.plot)){
  file_path <- file.path(params$args[2], "Application_1_Week")
  dir.create(file_path)
  file_path <- file.path(file_path, names(LP50.plot[i]))
  dir.create(file_path)
  ggsave(plot = LP50.plot[[i]],filename = file.path(file_path, "_medians_points.png"), width = 20,height = 15,units = "cm",dpi = 400)
}

# these functions create a set of output figures in designated folder
file_path <- paste0(params$args[2], "/LP50_Risk_Stories/")
dir.create(file_path)
createLP50AggregationPlots(lp50 = lp50,output.folder = file_path)
file_path <- paste0(params$args[2], "/STLP50/")
dir.create(file_path)
createSpatialTemporalLP50Plots(lp50 = lp50,reach.info = reach.info, output.folder = file_path, temporal.conditioning.percentile = 1)
#####################################################################
################ Residende time and Loading drift ###################
#####################################################################
residence.times <- readResidenceTimeFromStore(data.store.fpath = params$args[1], first.year = params$first_year, last.year = params$last_year, reach.info = reach.info, first.app.month = params$first_app_month, last.app.month = params$last_app_month, first.app.day = params$first_app_day, last.app.day = params$last_app_day)
loading.drift <- readLoadingDriftFromStore(data.store.fpath = params$args[1], first.year = params$first_year, last.year = params$last_year, reach.info = reach.info, first.app.day = params$first_app_day, first.app.month = params$first_app_month, last.app.month = params$last_app_month, last.app.day = params$last_app_day)

ResidenceTime.Depth.Plot <- createResidenceTimeDepthByStrahlerPlot(residence.times = residence.times,reach.info = reach.info)
ggsave(plot = ResidenceTime.Depth.Plot,
       filename = file.path(params$args[2], "Application_1_Week", "Boxplot_median_Depth_residence_time_by_Strahler.png"),
       width = 20,height = 12, units = "cm",dpi = 400)

RtimePECLoadingLP50 <- createRtimeLoadingPECsLP50Plot(focal.year = params$first_year, first.app.day = params$first_app_day, first.app.month = params$first_app_month, last.app.month = params$last_app_month, last.app.day = params$last_app_day,
                                                      residence.times = residence.times, loading.drift = loading.drift, max.pec = max.pec, lp50 = lp50,
                                                      scenario.path = params$options$hydrography)
ggsave(plot = RtimePECLoadingLP50,
       filename = file.path(params$args[2], "Application_1_Week", "Catchment_plots_combined.png"),
       width = 30,height = 30, units = "cm",dpi = 400)
