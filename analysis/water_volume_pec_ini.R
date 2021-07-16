# This script displays the water volume that is available in reaches at the
# time of spray-applications

# Load packages
library(h5)
library(data.table)

# Specify some parameters
x3df.path <- "F:\\AquaticRiskLTI\\Experiments\\UC1-34-1\\mcs\\X3NMR8SS06K0YUYA1C\\processing\\sim.x3df\\arr.dat"

# Load the data
x3df <- h5file(x3df.path, "r")
deposition <- x3df["DepositionToReach/Deposition"][]
reaches.d <- x3df["DepositionToReach/Reaches"][]
volume <- x3df["Hydrology/Volume"][]
reaches.h <- x3df["Hydrology/Reaches"][]

# Check that reaches order is the same in both datasets
stopifnot(all.equal(reaches.d, reaches.h))

# Get the spatio-temporal coordinates of spray-drift depositions
coords.d <- which(deposition > 0, TRUE)

# Transpose days into hours, assuming 12:00 as time of aplication
coords.h <- coords.d
coords.h[, 1] <- coords.h[, 1] * 24 + 12

# Create a table containing all information
data <- as.data.table(coords.d)
setnames(data, c("day", "reach"))
data[, rate := deposition[coords.d]]
data[, volume := volume[coords.h]]

# Copy results to clipboard
write.table(data, "clipboard", row.names = FALSE, sep = ",")
