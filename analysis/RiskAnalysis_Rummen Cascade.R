library(XRisk)
library(data.table)
library(ggplot2)
library(directlabels)

# Define script inputs
params <- list(
  x3df       = "F:\\dev\\Test_Run_aqRisk Rummen Cascade\\mcs\\X328FFKLGYFRTNS2OW\\processing\\sim.x3df\\arr.dat",
  output.dir = "F:\\dev\\Test_Run_aqRisk Rummen Cascade\\reporting Rummen Cascade"
)

x3df <- X3DF$new(params$x3df)
dss <- x3df$datasets
ds <- dss$`/CascadeToxswa/ConLiqWatTgtAvg`
data <- as.data.table(ds$data)
data <- melt(data, variable.name = "reach", id.vars = character(0))
data[, time := 1:ds$size[1]]
reaches <- dss$`/CascadeToxswa/Reaches`$data
r_ids <- data.table(reach = paste0("V", 1:length(reaches)), reach_id = reaches)
data <- merge(data, r_ids, "reach")
data[, value := value * 1000]


d <- data[, as.list(quantile(value, seq(0, 1, .001))), reach]
d <- melt(d, id.vars = "reach", variable.name = "t_percentile")

d <- d[, as.list(quantile(value, seq(0, 1, .01))), t_percentile]
d <- melt(d, id.vars = "t_percentile", variable.name = "s_percentile")

d[, t_percentile := as.numeric(substr(t_percentile, 1, nchar(as.character(t_percentile)) - 1)) / 100]
d[, s_percentile := as.numeric(substr(s_percentile, 1, nchar(as.character(s_percentile)) - 1)) / 100]
brk <- 10^(-5:3)
ggplot(d, aes(s_percentile, t_percentile, z = value)) +
  geom_contour(breaks = brk) +
  geom_dl(aes(label = ..level..), method = "bottom.pieces", stat = "contour", breaks = brk) +
  theme_bw() +
  #ggtitle("PECsw [ug/l]") +
  xlab("Spatial percentile (100m segments)") +
  ylab("Temporal percentile (hours)") +
  theme(text = element_text(face = "bold")) +
  xlim(c(0, 1))
ggsave(file.path(params$output.dir, "contour plot PECsw.png"))


d <- data[`%in%`(reach_id, c(1558, 96, 1306)) & time >= 3050 & time <= 3100]
d[, reach_id := as.factor(reach_id)]
ggplot(d, aes(time, value, col = reach_id)) +
  geom_line() +
  theme_bw() +
  xlab("time [hours]") +
  ylab("PEC_SW [ug/l]")
ggsave(file.path(params$output.dir, "PECsw 1558,96,1306.png"))

d <- data[`%in%`(reach_id, c(1559, 1809, 1343)) & time >= 3050 & time <= 3100]
d[, reach_id := as.factor(reach_id)]
ggplot(d, aes(time, value, col = reach_id)) +
  geom_line() +
  theme_bw() +
  xlab("time [hours]") +
  ylab("PEC_SW [ug/l]")
ggsave(file.path(params$output.dir, "PECsw 1559,1809,1343.png"))

d <- data[`%in%`(reach_id, c(1028)) & time >= 3050 & time <= 3100]
d[, reach_id := as.factor(reach_id)]
ggplot(d, aes(time, value, col = reach_id)) +
  geom_line() +
  theme_bw() +
  xlab("time [hours]") +
  ylab("PEC_SW [ug/l]")
ggsave(file.path(params$output.dir, "PECsw 1028.png"))

d <- data[`%in%`(reach_id, c(1558, 96, 1306)) & time >= 3050 & time <= 10000]
d[, reach_id := as.factor(reach_id)]
ggplot(d, aes(time, value, col = reach_id)) +
  geom_line() +
  theme_bw() +
  xlab("time [hours]") +
  ylab("PEC_SW [ug/l]") +
  coord_cartesian(ylim = c(0, 0.01))

# CDF
d <- data[reach_id == 1558]
d[, ct := cut(value, c(-Inf, 10^(-8:-2), Inf))]
x <- d[, .(ct[1], .N), rleid(ct)]
ggplot(x, aes(V1, N)) + 
  theme_bw() +
  geom_text(aes(label=rleid)) +
  ylab("Duration [h]") +
  xlab("Concentration interval [ug/l]")
ggsave(file.path(params$output.dir, "CDF numbers.png"))
ggplot(x, aes(V1, N)) + 
  geom_point() +
  theme_bw() +
  ylab("Duration [h]") +
  xlab("Concentration interval [ug/l]")
ggsave(file.path(params$output.dir, "CDF dots.png"))

d <- data
d[, ct := cut(value, c(-Inf, 10^(-6:-2), Inf))]
x <- d[, .(ct[1], .N), rleid(ct)]
x[, ct := cut(N, c(-Inf, 10^(1:3), Inf))]
y <- x[, .N, .(V1, ct)]
ggplot(y, aes(V1, ct, z=N)) + 
  geom_tile(aes(fill = N)) +
  theme_bw() +
  ylab("Duration interval [h]") +
  xlab("Concentration interval [ug/l]") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = round(N / 1735, 2)))
ggsave(file.path(params$output.dir, "CDF all reaches.png"))

ggplot(y, aes(V1, ct, z=N)) + 
  geom_tile(aes(fill = N)) +
  theme_bw() +
  ylab("Duration interval [h]") +
  xlab("Concentration interval [ug/l]") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = N))
ggsave(file.path(params$output.dir, "CDF all reaches N.png"))

# hydro
flow <- as.data.table(dss$`/Hydrology/Flow`$data)
flow <- melt(flow, variable.name = "reach", id.vars = character(0))
flow[, time := 1:dss$`/Hydrology/Flow`$size[1]]
flow <- merge(flow, r_ids, "reach")
data <- merge(data, flow, c("reach", "time"))

# max
d <- data[, .SD[which.max(value.x)], reach_id.x]
ggplot(d, aes(log10(value.y), log10(value.x))) + 
  geom_point() +
  theme_bw() +
  xlab("log10(Water discharge [m³/d])") +
  ylab("log10(maxPEC_SW(reach)|t [ug/l])")
ggsave(file.path(params$output.dir, "maxPEC and flow.png"))

  