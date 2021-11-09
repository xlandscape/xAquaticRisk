source("C:/Users/budde005/OneDrive - WageningenUR/Active projects/LandscapeModel/2021/data_and_analysis/ifem_functions.R")
# Script for analysis of ifem outputs
checkAndLoadPackages()

reach.info <- getReachInfoFromScenario(scenario.path = "D:/PesticideModel_BigDrive/xaquaticrisk/scenario/rummen-tdi_1.2/ReachList_shp.shp")
############################################
################ PEC data analysis #########
############################################
# Read in PEC data from data store
max.pec <- readPECDataFromStore(data.store.fpath = "D:/PesticideModel_BigDrive/xaquaticrisk/run/Rummen_1992_2017_LGUTS/mcs/X30BL3O4W502Q5C6MT/store/arr.dat",
                             first.year = 1998,last.year = 2017)

# Add reach.info for plotting
max.pec <- left_join(max.pec,reach.info)

# Calculate median values
medPEC <- medianPECValues(max.pec = max.pec,reach.info = reach.info)
# Plot PECS by strahler order
PEC.plot <- createPECbyStrahlerPlot(max.pec = max.pec,reach.info = reach.info, medPEC = medPEC)
ggsave(plot = PEC.plot,filename = "C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/Application_1_Week/PEC_1998-2017_medians_points.png",
       width = 20,height = 15,units = "cm",dpi = 400)

############################################
################ LP50 analysis #############
############################################
lp50 <- readLP50DataFromStore(data.store.fpath = "D:/PesticideModel_BigDrive/xaquaticrisk/run/Rummen_1992_2017_LGUTS/mcs/X30BL3O4W502Q5C6MT/store/arr.dat",
                              first.year = 1998,last.year = 2017,reach.info = reach.info)
# Calculate median values
medLP50 <- medianLP50Values(lp50 = lp50,reach.info = reach.info)
# Create list with plots for species and model type combinations
LP50.plot <- createLP50byStrahlerPlot(LP50 = lp50,reach.info = reach.info,medLP50 = medLP50)
# Save outputs
for(i in 1:length(LP50.plot)){
  ggsave(plot = LP50.plot[[i]],filename = paste0("C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/Application_1_Week/",names(LP50.plot[i]),"_LP50_1998-2017_medians_points.png"),
         width = 20,height = 15,units = "cm",dpi = 400)
}

# these functions create a set of output figures in designated folder
createLP50AggregationPlots(lp50 = lp50,output.folder = "C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/LP50_Risk_Stories/")
createSpatialTemporalLP50Plots(lp50 = lp50,reach.info = reach.info,
                               output.folder = "C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/LP50_Risk_Stories/STLP50/",
                               temporal.conditioning.percentile = 1)
#####################################################################
################ Residende time and Loading drift ###################
#####################################################################
residence.times <- readResidenceTimeFromStore(data.store.fpath = "D:/PesticideModel_BigDrive/xaquaticrisk/run/Rummen_1992_2017_LGUTS/mcs/X30BL3O4W502Q5C6MT/store/arr.dat",
                                              first.year = 1998,last.year = 2017,reach.info = reach.info,first.app.month = 4,last.app.month = 4,first.app.day = 20,last.app.day = 30)
loading.drift <- readLoadingDriftFromStore(data.store.fpath = "D:/PesticideModel_BigDrive/xaquaticrisk/run/Rummen_1992_2017_LGUTS/mcs/X30BL3O4W502Q5C6MT/store/arr.dat",
                                           first.year = 1998,last.year = 2017,reach.info = reach.info,first.app.day = 20,first.app.month = 4,last.app.month = 4,last.app.day = 30)

ResidenceTime.Depth.Plot <- createResidenceTimeDepthByStrahlerPlot(residence.times = residence.times,reach.info = reach.info)
ggsave(plot = ResidenceTime.Depth.Plot,
       filename = "C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/Application_1_Week/Boxplot_median_Depth_residence_time_by_Strahler.png",
       width = 20,height = 12, units = "cm",dpi = 400)

RtimePECLoadingLP50 <- createRtimeLoadingPECsLP50Plot(focal.year = 1998,first.app.day = 20,first.app.month = 4,last.app.month = 4,last.app.day = 30,
                                                      residence.times = residence.times,loading.drift = loading.drift,max.pec = max.pec,lp50 = lp50,
                                                      scenario.path = "D:/PesticideModel_BigDrive/xaquaticrisk/scenario/rummen-tdi_1.2/ReachList_shp.shp")
ggsave(plot = RtimePECLoadingLP50, 
       filename = "C:/Users/budde005/OneDrive - Wageningen University & Research/Active projects/LandscapeModel/2021/data_and_analysis/Figures/Application_1_Week/Catchment_plots_combined.png",
       width = 30,height = 30, units = "cm",dpi = 400)
