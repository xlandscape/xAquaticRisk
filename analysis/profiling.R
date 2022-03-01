library(data.table)
library(ggplot2)

args <- commandArgs(TRUE)
logfile <- args[1]
output_path <- args[2]
tmp_file <- file.path(output_path, "experiment.log")
file.copy(logfile, tmp_file)
logfile <- tmp_file

d <- fread(logfile, sep = "\t", header = FALSE)[startsWith(V1, "INFO  profile")]
d[, id := 1:.N]

if (nrow(d) > 0) {
  d <- d[,
    .(
      id = rep(id, each = 8),
      unlist(
        strsplit(
          gsub(
            paste0(
              "INFO  profile (.*?) - process (.*?), parent (.*?): (.*?), (.*?)% CPU \\(logical\\), ",
              "(.*?)MB memory usage, (.*?)MB read, (.*?)MB written.*"
              ),
            "\\1;\\2;\\3;\\4;\\5;\\6;\\7;\\8",
            V1
            ),
          ";",
          TRUE
        )
      ),
      rep(c("time", "pid", "parent", "name", "cpu", "ram", "read", "write"), .N)
    )
  ]
  d <- dcast(d, id ~ V3, NULL, value.var = "V2")
  d[
    ,
    c("cpu", "parent", "pid", "ram", "read", "time", "write") := .(
      as.numeric(cpu),
      as.integer(parent),
      as.integer(pid),
      as.integer(ram),
      as.integer(read),
      as.POSIXct(time),
      as.integer(write)
    )
  ]
  d[, interval := as.POSIXct(cut(time, breaks = "10 sec"))]

  d2 <- data.table(
    expand.grid(
      interval = seq(d[, min(interval)], d[, max(interval)], "10 sec"),
      name = d[, unique(name)]
    )
  )
  d2 <- merge(
    d2,
    d[, .(cpu = max(cpu), ram = max(ram), read = max(read), write = max(write)), .(interval, name)],
    c("interval", "name"),
    all.x = TRUE
  )
  d2[is.na(cpu), c("cpu", "ram", "read", "write") := .(0, 0, 0, 0)]


  g <- ggplot(
    d2, aes(interval, cpu, fill = name)) +
    geom_area() +
    labs(title = "Maximum CPU usage by runtime environment in 10-second intervals") +
    xlab("time") +
    ylab("CPU usage (%)") +
    theme_bw()
  ggsave(file.path(output_path, "Maximum CPU usage by runtime environment.png"))

  g <- ggplot(
    d2, aes(interval, ram, fill = name)) +
    geom_area() +
    labs(title = "Maximum RAM usage by runtime environment in 10-second intervals") +
    xlab("time") +
    ylab("RAM usage (MB)") +
    theme_bw()
  ggsave(file.path(output_path, "Maximum RAM usage by runtime environment.png"))

  g <- ggplot(
    d2, aes(interval, read, fill = name)) +
    geom_area() +
    labs(title = "Cumulative disk reading by runtime environment in 10-second intervals") +
    xlab("time") +
    ylab("read (MB)") +
    theme_bw()
  ggsave(file.path(output_path, "Cumulative disk reading by runtime environment.png"))

  g <- ggplot(
    d2, aes(interval, write, fill = name)) +
    geom_area() +
    labs(title = "Cumulative disk writing by runtime environment in 10-second intervals") +
    xlab("time") +
    ylab("read (MB)") +
    theme_bw()
  ggsave(file.path(output_path, "Cumulative disk writing by runtime environment.png"))

  d2 <- data.table(
    expand.grid(
      interval = seq(d[, min(interval)], d[, max(interval)], "10 sec"),
      pid = d[, .N, pid][N > 1, pid]
    )
  )
  d2 <- merge(
    d2,
    d[, .(cpu = max(cpu), ram = max(ram), read = max(read), write = max(write)), .(interval, pid)],
    c("interval", "pid"),
    all.x = TRUE
  )
  d2[is.na(cpu), c("cpu", "ram", "read", "write") := .(0, 0, 0, 0)]

  g <- ggplot(
    d2, aes(interval, cpu, fill = as.factor(pid))) +
    geom_area() +
    labs(title = "Maximum CPU usage by process in 10-second intervals") +
    xlab("time") +
    ylab("CPU usage (%)") +
    theme_bw()
  ggsave(file.path(output_path, "Maximum CPU usage by process.png"))

  d2 <- data.table(
    expand.grid(
      interval = seq(d[, min(interval)], d[, max(interval)], "10 sec"),
      parent = d[, unique(parent)]
    )
  )
  d2 <- merge(
    d2,
    d[, .(cpu = max(cpu), ram = max(ram), read = max(read), write = max(write)), .(interval, parent)],
    c("interval", "parent"),
    all.x = TRUE
  )
  d2[is.na(cpu), c("cpu", "ram", "read", "write") := .(0, 0, 0, 0)]

  g <- ggplot(
    d2, aes(interval, cpu, fill = as.factor(parent))) +
    geom_area() +
    labs(title = "Maximum CPU usage by parent process in 10-second intervals") +
    xlab("time") +
    ylab("CPU usage (%)") +
    theme_bw()
  ggsave(file.path(output_path, "Maximum CPU usage by parent process.png"))
}
