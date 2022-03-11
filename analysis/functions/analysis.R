library(XRisk)
library(data.table)
library(ggplot2)

lp50_strahler <- function(store, simulation_start, dataset, dataset_reaches, strahler, strahler_reaches, output_file) {
    x3df <- X3DF$new(store)
    start_year <- year(as.POSIXct(paste(x3df$datasets[[simulation_start]]$data, "00:00")))
    lp50 <- data.table(x3df$datasets[[dataset]]$data)
    lp50[, year := .I + start_year]
    lp50 <- melt(lp50, "year", variable.name = "x", value.name = "lp50")
    reaches <- x3df$datasets[[dataset_reaches]]$data
    lp50 <- merge(lp50, data.table(x = paste0("V", seq_along(reaches)), reach = reaches))
    lp50[lp50 < 0, lp50 := NA]
    lp50[lp50 > 100, lp50 := 100]    
    strahler_order <- data.table(
        reach = x3df$datasets[[strahler_reaches]]$data, strahler = x3df$datasets[[strahler]]$data)
    lp50 <- merge(lp50, strahler_order, by = "reach")
    lp50 <- lp50[, .(year, reach, strahler, lp50)]
    setorder(lp50, lp50)
    lp50[, index := 1:.N, strahler]
    p <- ggplot(lp50, aes(index, log10(lp50))) +
        geom_line(size = 1) +
        xlab("index") +
        ylab("log10(LP50)") +
        theme_bw() +
        facet_grid(. ~ strahler) +
        geom_hline(yintercept = 0, col = "red") +
        geom_hline(yintercept = 1, col = "orange")
    ggsave(output_file)
    p
}