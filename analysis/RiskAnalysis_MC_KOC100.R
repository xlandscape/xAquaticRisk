library(XRisk)
library(data.table)
library(ggplot2)
library(directlabels)

# Define script inputs
params <- list(
  x3df       = "F:\\dev\\Test_Run_aqRisk_Rummen_KOC100\\mcs\\X3IQZ36I2N94P5MWN1\\processing\\sim.x3df\\arr.dat",
  output.dir = "F:\\dev\\Test_Run_aqRisk_Rummen_KOC100\\reporting_koc100"
)

# Load spatio-temporal data (PEC per reach and hour)
x3df <- X3DF$new(params$x3df)
ds <- x3df$datasets$`/StepsRiverNetwork/PEC_SW`
data <- as.data.table(ds$data)
data <- melt(data, variable.name = "reach", id.vars = character(0))
data[, time := 1:ds$size[1]]
reaches <- x3df$datasets$`/StepsRiverNetwork/Reaches`$data

####
reaches <- data.table(reach = paste0("V", seq_along(reaches)), reach.id = paste0("r", reaches))


# Calculate the temporal percentiles
d <- data[, as.list(quantile(value, seq(.99, 1, .0001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")

# Calculate the spatial percentiles
d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")

# Plot the percentiles
d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
d[value < 0, value := 0]
brk <- c(.0001, 0.001, .01, .05, .1, .2, .5, 1)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "bottom.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  xlim(c(0, 1)) +
  ggtitle("PECsw [ug/l]") +
  xlab("Spatial percentile (100m segments)") +
  ylab("Temporal percentile (hours)") +
  theme(text = element_text(face = "bold")) +
  ylim(c(.99, 1))
ggsave(file.path(params$output.dir, "spatio-temporal percentiles v3.png"))

# 3d contours


# Daily scale
d <- data[, .(value = max(value)), .(reach, `%/%`(time, 24))]
d <- d[, as.list(quantile(value, seq(.99, 1, .0001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")
d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")
d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
d[value < 0, value := 0]
brk <- c(.0001, .01, .05, .1, .2, .5, 1)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "bottom.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  xlim(c(0, 1))
ggsave(file.path(params$output.dir, "spatio-temporal percentiles (daily max).png"))

# Weekly scale
d <- data[, .(value = max(value)), .(reach, `%/%`(time, (24 * 7)))]
d <- d[, as.list(quantile(value, seq(.90, 1, .001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")
d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")
d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
d[value < 0, value := 0]
brk <- c(.0001, .01, .05, .1, .2, .5, 1)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "bottom.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  xlim(c(0, 1))
ggsave(file.path(params$output.dir, "spatio-temporal percentiles (weekly max).png"))

# Four-weekly scale
d <- data[, .(value = max(value)), .(reach, `%/%`(time, (24 * 7 * 4)))]
d <- d[, as.list(quantile(value, seq(.90, 1, .001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")
d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")
d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
d[value < 0, value := 0]
brk <- c(.0001, .01, .05, .1, .2, .5, 1)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "top.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  xlim(c(0, 1))
ggsave(file.path(params$output.dir, "spatio-temporal percentiles (four-weekly max).png"))

# Twelve-weekly scale
d <- data[, .(value = max(value)), .(reach, `%/%`(time, (24 * 7 * 12)))]
d <- d[, as.list(quantile(value, seq(0, 1, .01))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")
d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")
d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
d[value < 0, value := 0]
brk <- c(.0001, .01, .05, .1, .2, .5, 1)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "top.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  xlim(c(0, 1)) +
  ylim(c(0, 1))
ggsave(file.path(params$output.dir, "spatio-temporal percentiles (twelve-weekly max).png"))

# Timeseries one reach
reaches <- sample(data[, max(value), reach][V1 > 0, unique(reach)], 10)
for (r in reaches) {
  x <- data[reach == r]
  ggplot(x, aes(time, value)) +
    geom_line() +
    theme_bw()
  ggsave(file.path(params$output.dir, paste("reach", r, "hourly.png")))
}

# Timeseries multiple reaches
d <- data[`%in%`(reach, reaches) & time > 3048 & time < 3120]
d <- merge(d, reaches, "reach")
saveRDS(d, file.path(params$output.dir, "10 reaches sample.rds"))
ggplot(d, aes(time, value, col =  reach.id)) +
  geom_line() +
  theme_bw() +
  ylab("PEC_SW [ug/l]") +
  xlab("time [hour]")
ggsave(file.path(params$output.dir, "10 reaches around first application.png"))
d <- readRDS(file.path(params$output.dir, "10 reaches sample.rds"))



###
ds_sed <- x3df$datasets$`/StepsRiverNetwork/PEC_SED`
data_sed <- as.data.table(ds_sed$data)
data_sed <- melt(data_sed, variable.name = "reach", id.vars = character(0))
data_sed[, time := 1:ds_sed$size[1]]
reaches <- x3df$datasets$`/StepsRiverNetwork/Reaches`$data
reaches <- data.table(reach = paste0("V", seq_along(reaches)), reach.id = paste0("r", reaches))
d_sed <- data_sed[time > 3048 & time < 3120]
d_sed <- merge(d_sed, reaches, "reach")
d_sed <- d_sed[`%in%`(reach, d[, unique(reach)])]
ggplot(d_sed, aes(time, value, col =  reach.id)) +
  geom_line() +
  theme_bw() +
  ylab("PEC_SED [mg/kg]") +
  xlab("time [hour]") +
  xlim(c(3050, 3090))
ggsave(file.path(params$output.dir, "10 reaches around first application (sediment, KOC100).png"))


ds_sed <- x3df$datasets$`/StepsRiverNetwork/PEC_SW`
data_sed <- as.data.table(ds_sed$data)
data_sed <- melt(data_sed, variable.name = "reach", id.vars = character(0))
data_sed[, time := 1:ds_sed$size[1]]
reaches <- x3df$datasets$`/StepsRiverNetwork/Reaches`$data
reaches <- data.table(reach = paste0("V", seq_along(reaches)), reach.id = paste0("r", reaches))
d_sed <- data_sed[time > 3048 & time < 3120]
d_sed <- merge(d_sed, reaches, "reach")
d_sed <- d_sed[`%in%`(reach, d[, unique(reach)])]
ggplot(d_sed, aes(time, value, col =  reach.id)) +
  geom_line() +
  theme_bw() +
  ylab("PEC_SED [mg/kg]") +
  xlab("time [hour]") +
  xlim(c(3050, 3090))
ggsave(file.path(params$output.dir, "10 reaches around first application (sediment, KOC100).png"))
