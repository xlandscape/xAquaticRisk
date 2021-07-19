library(XRisk)
library(data.table)
library(ggplot2)
library(directlabels)

# Define script inputs
params <- list(
  x3df       = "F:\\AquaticRiskLTI\\Experiments\\UC1-34-1\\mcs\\X3DHQ8ZO5OD4IMBCRI\\processing\\sim.x3df\\arr.dat",
  output.dir = "F:\\AquaticRiskLTI\\Experiments\\UC1-34-1\\reporting_UC1-34-1"
)

x3df <- X3DF$new(params$x3df)
dss <- x3df$datasets
ds <- dss$`/CascadeToxswa/ConLiqWatTgtAvg`
data <- as.data.table(ds$data)
data <- melt(data, variable.name = "reach", id.vars = character(0))
data[, time := 1:ds$size[1]]
reaches <- dss$`/CascadeToxswa/Reaches`$data
r_ids <- data.table(reach = paste0("V", 1:length(reaches)), reach.id = reaches)
data <- merge(data, r_ids, "reach")

d <- data[reach.id == 570 & time >= 2790 & time <= 2850]
ggplot(d, aes(time, value)) +
  geom_line() 


d <- data[, as.list(quantile(value, seq(0, 1, .001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")

d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")

d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
brk <- c(1e-8, 1e-7, .5e-7, 1e-6, .5e-6, 1e-5)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "bottom.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  #ggtitle("PECsw [ug/l]") +
  xlab("Spatial percentile (100m segments)") +
  ylab("Temporal percentile (hours)") +
  theme(text = element_text(face = "bold"))
ggsave(file.path(params$output.dir, "contour plot PECsw.png"))
