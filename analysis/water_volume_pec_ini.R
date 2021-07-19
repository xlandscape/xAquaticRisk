# This script displays the water volume that is available in reaches at the
# time of spray-applications

# Load packages
library(h5)
library(data.table)

# Specify some parameters
x3df_path <- "F:\\AquaticRiskLTI\\Experiments\\UC1-34-1\\mcs\\X3NMR8SS06K0YUYA1C\\processing\\sim.x3df\\arr.dat"

# Load the data
x3df <- h5file(x3df_path, "r")
deposition <- x3df["DepositionToReach/Deposition"][]
reaches_d <- x3df["DepositionToReach/Reaches"][]
volume <- x3df["Hydrology/Volume"][]
reaches_h <- x3df["Hydrology/Reaches"][]

# Check that reaches order is the same in both datasets
stopifnot(all.equal(reaches_d, reaches_h))

# Get the spatio-temporal coordinates of spray-drift depositions
coords_d <- which(deposition > 0, TRUE)

# Transpose days into hours, assuming 12:00 as time of aplication
coords_h <- coords_d
coords_h[, 1] <- coords_h[, 1] * 24 + 12

# Create a table containing all information
data <- as.data.table(coords_d)
setnames(data, c("day", "reach"))
data[, rate := deposition[coords_d]]
data[, volume := volume[coords_h]]

# Copy results to clipboard
write.table(data, "clipboard", row.names = FALSE, sep = ",")
