suppressWarnings(library(XRisk))
suppressWarnings(library(data.table))
suppressWarnings(library(pbapply))
suppressWarnings(library(ggplot2))
pboptions(type = "timer")

percentiles_table <- function(
  experiment_dir,
  dataset,
  unit_conversion_factor,
  reaches,
  simulation_start,
  hydrography_ids,
  strahler_order,
  output_file
) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Collect results per MC
  perc <- rbindlist(pblapply(mcs, function(mc) {

    # Load data
    x3df <- X3DF$new(mc)
    pecs <- x3df$datasets[[dataset]]$data * unit_conversion_factor
    reaches <- x3df$datasets[[reaches]]$data
    start_time <- as.integer(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))

    # Convert to long form table
    pecs <- as.data.table(pecs)

    # Add temporal reference
    pecs[, t := as.POSIXct(start_time + .I * 3600, origin = "1970-01-01 00:00:00")]

    # Bring data into long form
    pecs <- melt(pecs, "t", variable.name = "x", value.name = "pec")

    # Spatially referencing
    pecs <- merge(pecs, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))

    # Calculate percentiles
    perc <- pecs[, as.list(quantile(pec, c(.1, .25, .5, .75, .9, .95, .99, 1))), .(reach, year = year(t))]

    # Bring to long form
    melt(perc, c("reach", "year"), variable.name = "percentile", value.name = "pec")
  }))
  
  # Calculate expectancy values
  expectancy <- perc[,
    .(expectancy_value = mean(pec), confidence_95_normal = qnorm(.975) * sd(pec) / sqrt(.N)),
    .(reach, year, percentile)
  ]
  
  # Load and add Strahler order
  x3df <- X3DF$new(mcs[1])
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  expectancy <- merge(expectancy, strahler, by = "reach")
  
  # Save expectancy values
  fwrite(expectancy, output_file)
}

duration_exceedance_table <- function(
  experiment_dir,
  dataset,
  unit_conversion_factor,
  reaches,
  simulation_start,
  hydrography_ids,
  strahler_order,
  output_file
) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Collect results per MC
  durations <- rbindlist(pblapply(mcs, function(mc) {
  
    # Load data
    x3df <- X3DF$new(mc)
    pecs <- x3df$datasets[[dataset]]$data * unit_conversion_factor
    reaches <- x3df$datasets[[reaches]]$data
    start_time <- as.integer(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))
 
    # Convert to long form table
    pecs <- as.data.table(pecs)
  
    # Add temporal reference
    pecs[, t := as.POSIXct(start_time + .I * 3600, origin = "1970-01-01 00:00:00")]
  
    # Bring data into long form
    pecs <- melt(pecs, "t", variable.name = "x", value.name = "pec")
  
    # Spatially referencing
    pecs <- merge(pecs, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))
  
    # Build concentration bins
    pecs[,
      c("pec_0.001", "pec_0.005", "pec_0.01", "pec_0.1", "pec_1", "pec_10") :=
        .(pec >= .001, pec >= .005, pec >= .01, pec >= .1, pec >= 1, pec >= 10)
    ]
  
    # Count consecutive bins
    pecs[, duration_pec_0_001 := seq_len(.N), rleid(pec_0.001)]
    pecs[, duration_pec_0_005 := seq_len(.N), rleid(pec_0.005)]
    pecs[, duration_pec_0_01 := seq_len(.N), rleid(pec_0.01)]
    pecs[, duration_pec_0_1 := seq_len(.N), rleid(pec_0.1)]
    pecs[, duration_pec_1 := seq_len(.N), rleid(pec_1)]
    pecs[, duration_pec_10 := seq_len(.N), rleid(pec_10)]
 
    # Set durations outside bins to 0
    pecs[pec_0.001 == FALSE, duration_pec_0_001 := 0]
    pecs[pec_0.005 == FALSE, duration_pec_0_005 := 0]
    pecs[pec_0.01 == FALSE, duration_pec_0_01 := 0]
    pecs[pec_0.1 == FALSE, duration_pec_0_1 := 0]
    pecs[pec_1 == FALSE, duration_pec_1 := 0]
    pecs[pec_10 == FALSE, duration_pec_10 := 0]
  
    # Determine maximum durations per reach, year and bin
    durations <- pecs[,
      .(
        "0.001" = max(duration_pec_0_001),
        "0.005" = max(duration_pec_0_005),
        "0.01" = max(duration_pec_0_01),
        "0.1" = max(duration_pec_0_1),
        "1" = max(duration_pec_1),
        "10" = max(duration_pec_10)
      ),
      .(reach, year = year(t))
    ]
  
    # Bring to long form
    melt(durations, c("reach", "year"), variable.name = "threshold", value.name = "duration")
  }))

  # Calculate expectancy values
  expectancy <- durations[,
    .(expectancy_value = mean(duration), confidence_95_normal = qnorm(.975) * sd(duration) / sqrt(.N)),
    .(reach, year, threshold)
  ]

  # Load and add Strahler order
  x3df <- X3DF$new(mcs[1])
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  expectancy <- merge(expectancy, strahler, by = "reach")
  
  # Save expectancy values
  fwrite(expectancy, output_file)
}

frequency_exceedance_table <- function(
  experiment_dir,
  dataset,
  unit_conversion_factor,
  reaches,
  simulation_start,
  hydrography_ids,
  strahler_order,
  output_file
) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Collect results per MC
  frequencies <- rbindlist(pblapply(mcs, function(mc) {
  
    # Load data
    x3df <- X3DF$new(mc)
    pecs <- x3df$datasets[[dataset]]$data * unit_conversion_factor
    reaches <- x3df$datasets[[reaches]]$data
    start_time <- as.integer(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))
  
    # Convert to long form table
    pecs <- as.data.table(pecs)
  
    # Add temporal reference
    pecs[, t := as.POSIXct(start_time + .I * 3600, origin = "1970-01-01 00:00:00")]
  
    # Bring data into long form
    pecs <- melt(pecs, "t", variable.name = "x", value.name = "pec")
  
    # Spatially referencing
    pecs <- merge(pecs, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))
  
    # Build concentration bins
    pecs[,
      c("pec_0.001", "pec_0.005", "pec_0.01", "pec_0.1", "pec_1", "pec_10") :=
        .(pec >= .001, pec >= .005, pec >= .01, pec >= .1, pec >= 1, pec >= 10)
    ]
  
    # Count consecutive bins
    pecs[, "0.001" := c(rep(0L, .N - 1), .N), rleid(pec_0.001)]
    pecs[, "0.005" := c(rep(0L, .N - 1), .N), rleid(pec_0.005)]
    pecs[, "0.01" := c(rep(0L, .N - 1), .N), rleid(pec_0.01)]
    pecs[, "0.1" := c(rep(0L, .N - 1), .N), rleid(pec_0.1)]
    pecs[, "1" := c(rep(0L, .N - 1), .N), rleid(pec_1)]
    pecs[, "10" := c(rep(0L, .N - 1), .N), rleid(pec_10)]

    # Set durations ouside bins to 0
    pecs[pec_0.001 == FALSE, "0.001" := 0]
    pecs[pec_0.005 == FALSE, "0.005" := 0]
    pecs[pec_0.01 == FALSE, "0.01" := 0]
    pecs[pec_0.1 == FALSE, "0.1" := 0]
    pecs[pec_1 == FALSE, "1" := 0]
    pecs[pec_10 == FALSE, "10" := 0]
  
    # Bring to long form
    durations <- melt(pecs, c("reach", "t"), c("0.001", "0.005", "0.01", "0.1", "1", "10"), "threshold", "duration")

    # Save memory
    rm(pecs)
    durations <- durations[duration > 0]
  
    # Frequency bins
    frequencies <- durations[,
      .(
        `1` = sum(duration >= 1),
        `2` = sum(duration >= 2),
        `6` = sum(duration >= 6),
        `24` = sum(duration >= 24),
        `48` = sum(duration >= 48),
        `96` = sum(duration >= 96)
      ),
      .(reach, year = year(t), threshold)
    ]
    rm(durations)
  
    # Bring to long form
    melt(frequencies, c("reach", "year", "threshold"), variable.name = "duration", value.name = "frequency")
  }))

  # Calculate expectancy values
  expectancy <- frequencies[,
    .(expectancy_value = mean(frequency), confidence_95_normal = qnorm(.975) * sd(frequency) / sqrt(.N)),
    .(reach, year, threshold, duration)
  ]

  # Load and add Strahler order
  x3df <- X3DF$new(mcs[1])
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  expectancy <- merge(expectancy, strahler, by = "reach")
  
  # Save expectancy values
  fwrite(expectancy, output_file)
}

load_values_first_mc <- function(
  experiment_dir, dataset, unit_conversion_factor, reaches, simulation_start, hydrography_ids, strahler_order) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Get first MC run
  mc <- mcs[1]

  # Load data
  x3df <- X3DF$new(mc)
  pecs <- x3df$datasets[[dataset]]$data * unit_conversion_factor
  reaches <- x3df$datasets[[reaches]]$data
  start_time <- as.integer(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))

  # Convert to long form table
  pecs <- as.data.table(pecs)

  # Add temporal reference
  pecs[, t := as.POSIXct(start_time + .I * 3600, origin = "1970-01-01 00:00:00")]

  # Bring data into long form
  pecs <- melt(pecs, "t", variable.name = "x", value.name = "pec")

  # Spatially referencing
  pecs <- merge(pecs, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))
  pecs[, x := NULL]

  # Add year, month, day
  pecs[, c("year", "month", "mday", "yday") := .(year(t), month(t), mday(t), yday(t))]

  # Load and add Strahler order
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  pecs <- merge(pecs, strahler, by = "reach")

  # Return results
  return(pecs)
}

boxplot_global <- function(data, value, y_axis_label, output_file) {
  ggplot(data[get(value) > 0], aes(as.factor(year), log10(get(value)))) +
    geom_boxplot() +
    xlab("year") +
    ylab(y_axis_label) +
    theme_bw()
  ggsave(output_file)
}

boxplot_reaches <- function(data, value, reaches, y_axis_label, output_file) {
  ggplot(
    data[
      get(value) > 0 & `%in%`(reach, reaches), .(year = as.factor(year), val = get(value), reach = as.factor(reach))],
    aes(year, log10(val), fill = reach)
  ) +
    geom_boxplot() +
    xlab("year") +
    ylab(y_axis_label) +
    scale_y_continuous(
      breaks = data[get(value) > 0, floor(log10(min(get(value))))]:data[, ceiling(log10(max(get(value))))]
    ) +
    theme_bw()
  ggsave(output_file)
}

line_plot_reaches <- function(data, value, reaches, from_day_of_year, to_day_of_year, y_axis_label, output_file) {
  ggplot(
    data[
      get(value) > 0 & `%in%`(reach, reaches) & yday >= from_day_of_year & yday <= to_day_of_year,
      .(year = as.factor(year), val = get(value), reach = as.factor(reach), hour = (yday - 1) * 24 + hour(t))
    ],
    aes(hour, log10(val), col = reach)
  ) +
    geom_line() +
    scale_y_continuous(breaks=-10:0) +
    coord_cartesian(ylim=c(-10, 0)) +
    xlab("hour of year") +
    ylab(y_axis_label) +
    theme_bw() +
    facet_wrap(~ year)
  ggsave(output_file)
}

cdf <- function(data, value) {
  durations <- data[,
    .(
      reach,
      year,
      x_0 = get(value) < .001,
      x_0.001 = get(value) >= .001 & get(value) < .005,
      x_0.005 = get(value) >= .005 & get(value) < .01,
      x_0.01 = get(value) >= .01 & get(value) < .1,
      x_0.1 = get(value) >= .1 & get(value) < 1,
      x_1 = get(value) >= 1 & get(value) < 10,
      x_10 = get(value) >= 10
    )]
  durations[, "< 0.001" := c(rep(0L, .N - 1), .N), rleid(x_0)]
  durations[, "0.001-0.005" := c(rep(0L, .N - 1), .N), rleid(x_0.001)]
  durations[, "0.005-0.01" := c(rep(0L, .N - 1), .N), rleid(x_0.005)]
  durations[, "0.01-0.1" := c(rep(0L, .N - 1), .N), rleid(x_0.01)]
  durations[, "0.1-1" := c(rep(0L, .N - 1), .N), rleid(x_0.1)]
  durations[, "1-10" := c(rep(0L, .N - 1), .N), rleid(x_1)]
  durations[, ">= 10" := c(rep(0L, .N - 1), .N), rleid(x_10)]
  durations[x_0 == FALSE, "< 0.001" := 0]
  durations[x_0.001 == FALSE, "0.001-0.005" := 0]
  durations[x_0.005 == FALSE, "0.005-0.01" := 0]
  durations[x_0.01 == FALSE, "0.01-0.1" := 0]
  durations[x_0.1 == FALSE, "0.1-1" := 0]
  durations[x_1 == FALSE, "1-10" := 0]
  durations[x_10 == FALSE, ">= 10" := 0]
  durations <- melt(
    durations,
    c("reach", "year"),
    c("< 0.001", "0.001-0.005", "0.005-0.01", "0.01-0.1", "0.1-1", "1-10", ">= 10"),
    "threshold",
    "duration"
  )
  durations <- durations[,
    .(
      `1` = sum(duration == 1),
      `2-6` = sum(duration >= 2 & duration < 6),
      `6-24` = sum(duration >= 6 & duration < 24),
      `24-48` = sum(duration >= 24 & duration < 48),
      `48-96` = sum(duration >= 48 & duration < 96),
      `>= 96` = sum(duration >= 96)),
    .(reach, year, threshold)
  ]
  melt(durations, c("reach", "year", "threshold"), variable.name = "duration", value.name = "frequency")
}

cdf_plot_absolute <- function(frequencies, x_axis_label, output_file) {
  ggplot(
    frequencies[, .(frequency = sum(frequency)), .(threshold, duration)],
    aes(threshold, duration, fill = log10(frequency))
  ) +
    geom_tile() +
    geom_text(aes(label = frequency), col = "white") +
    xlab(x_axis_label) +
    ylab("duration threshold [h]") +
    theme_bw()
  ggsave(output_file)
}

cdf_plot_relative <- function(frequencies, x_axis_label, output_file) {
  ggplot(
    frequencies[, .(frequency = sum(frequency) / frequencies[, sum(frequency)]), .(threshold, duration)],
    aes(threshold, duration, fill = log10(frequency))
  ) +
    geom_tile() +
    geom_text(aes(label = paste(round(frequency * 100, 2), "%")), col = "white") +
    xlab(x_axis_label) +
    ylab("duration threshold [h]") +
    theme_bw()
  ggsave(output_file)
}

cumulative_plot <- function(data, value, y_axis_label, output_file) {
  data <- data[get(value) > 0, .(val = sort(get(value)))]
  data[, index := .I]
  data <- data[seq(1, .N, length.out = 1000)]
  ggplot(data, aes(index, log10(val))) +
    geom_line() +
    xlab("index") +
    ylab(y_axis_label) +
    theme_bw()
  ggsave(output_file)
}

cumulative_plot_strahler <- function(data, value, y_axis_label, output_file) {
  data <- data[get(value) > 0, .(val = get(value), strahler = as.factor(strahler))]
  setorder(data, val)
  data <- split(data, by = "strahler")
  data <- rbindlist(lapply(data, function(d) {
    d[, index := .I]
    d[seq(1, .N, length.out = 1000)]
  }))
  ggplot(data, aes(index, log10(val))) +
    geom_line() +
    xlab("index") +
    ylab(y_axis_label) +
    theme_bw() +
    facet_grid(. ~ strahler)
  ggsave(output_file)
}

survival_probability_table <- function(
  experiment_dir,
  datasets_prefix,
  datasets_suffix,
  reaches,
  simulation_start,
  species_names,
  hydrography_ids,
  strahler_order,
  output_file
) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Collect results per MC
  probabilities <- rbindlist(pblapply(mcs, function(mc) {

    # Load data
    x3df <- X3DF$new(mc)
    datasets <- names(x3df$datasets)[
      startsWith(names(x3df$datasets), datasets_prefix) & endsWith(names(x3df$datasets), datasets_suffix)]

    survival <- rbindlist(pblapply(datasets, function(x) {
      x3df$datasets[[x]]$extract(c(NA, NA, 11))
    }), idcol = "dataset")
    reaches <- x3df$datasets[[reaches]]$data
    start_year <- year(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))

    # Add model and species
    survival[,
      c("model", "species") := .(substr(datasets[dataset], 26, 27), as.integer(substr(datasets[dataset], 36, 36)))]
  
    # Reference species
    survival[, species := species_names[species]]
  
    # Add temporal reference
    survival[, year := start_year + `time/year` - 1]

    # Spatially referencing
    survival <- merge(survival, data.table(`space/base_geometry` = seq_along(reaches), reach = reaches))

    # Return only required columns
    survival[, .(reach, year, species, model, probability = value)]  
  }))
  
  # Calculate expectancy values
  expectancy <- probabilities[,
    .(expectancy_value = mean(probability), confidence_95_normal = qnorm(.975) * sd(probability) / sqrt(.N)),
    .(reach, year, species, model)
  ]
  
  # Load and add Strahler order
  x3df <- X3DF$new(mcs[1])
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  expectancy <- merge(expectancy, strahler, by = "reach")
  
  # Save expectancy values
  fwrite(expectancy, output_file)
}

lp50_table <- function(
  experiment_dir,
  datasets_prefix,
  datasets_suffix,
  reaches,
  simulation_start,
  species_names,
  hydrography_ids,
  strahler_order,
  output_file
) {
  # Determine MC runs
  mcs <- sapply(
    file.path(list.dirs(file.path(experiment_dir, "mcs"), recursive = FALSE)), file.path, "store", "arr.dat")

  # Collect results per MC
  lp50 <- rbindlist(pblapply(mcs, function(mc) {

    # Load data
    x3df <- X3DF$new(mc)
    start_year <- year(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))
    datasets <- names(x3df$datasets)[
      startsWith(names(x3df$datasets), datasets_prefix) & endsWith(names(x3df$datasets), datasets_suffix)]
    lp50 <- rbindlist(pblapply(datasets, function(x) {
      lp50 <- data.table(x3df$datasets[[x]]$data)
      lp50[, year := .I + start_year]
      melt(lp50, "year", variable.name = "x", value.name = "lp50")
    }), idcol = "dataset")
    reaches <- x3df$datasets[[reaches]]$data

    # Add model and species
    lp50[, c("model", "species") := .(substr(datasets[dataset], 26, 27), as.integer(substr(datasets[dataset], 36, 36)))]
  
    # Reference species
    lp50[, species := species_names[species]]

    # Spatially referencing
    lp50 <- merge(lp50, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))

    # Add NA
    lp50[lp50 < 0, lp50 := NA]

    # Return only required columns
    lp50[, .(reach, year, species, model, lp50)] 
  }))
  
  # Calculate expectancy values
  expectancy <- lp50[,
    .(expectancy_value = mean(lp50), confidence_95_normal = qnorm(.975) * sd(lp50) / sqrt(.N)),
    .(reach, year, species, model)
  ]
  
  # Load and add Strahler order
  x3df <- X3DF$new(mcs[1])
  strahler <- data.table(reach = x3df$datasets[[hydrography_ids]]$data)
  strahler[, strahler := x3df$datasets[[strahler_order]]$data]
  expectancy <- merge(expectancy, strahler, by = "reach")
  
  # Save expectancy values
  fwrite(expectancy, output_file)
}
