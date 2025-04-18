#Setup----
#Updated April 2025
#Linked to GitHub
#Author: Hunter Quintal
#purpose: Produce three panel figure of clusters vs noaa events for each resolution

# Load Libraries & Set Project Root ----
library(here)
here::i_am("scripts/fig_s08_yearly_cluster_noaa.R")  # Adjust this file path as needed
source(here::here("scripts", "01_library.R"))

library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(stringr)
library(purrr)

plot_clusters_and_noaa_individual <- function(cluster_dirs, noaa_dir) {
  
  # 1) NOAA counts per year
  noaa_files <- list.files(noaa_dir, "\\.nc$", full.names = TRUE)
  noaa_dates <- str_extract(basename(noaa_files), "^\\d{4}-\\d{2}-\\d{2}")
  noaa_dates <- as.Date(noaa_dates, format = "%Y-%m-%d")
  noaa_years <- year(noaa_dates[!is.na(noaa_dates)])
  if (length(noaa_years) == 0) {
    stop("No valid NOAA dates found in ", noaa_dir)
  }
  noaa_counts <- tibble(year = noaa_years) %>%
    count(year, name = "count")
  
  # 2) Cluster counts per year
  cluster_counts <- imap_dfr(cluster_dirs, ~{
    files <- list.files(.x, "\\.nc$", full.names = TRUE)
    dates <- str_extract(basename(files),
                         "\\d{4}-\\d{2}-\\d{2}(?=_cluster)")
    dates <- as.Date(dates, format = "%Y-%m-%d")
    yrs   <- year(dates[!is.na(dates)])
    if (length(yrs) == 0) return(NULL)
    tibble(year = yrs) %>%
      count(year, name = "count") %>%
      mutate(cluster = .y)
  })
  if (nrow(cluster_counts) == 0) {
    stop("No valid cluster dates found in any of: ", paste(cluster_dirs, collapse = ", "))
  }
  
  # 3) Build the full, continuous year range
  all_years <- seq(
    min(union(cluster_counts$year, noaa_counts$year)),
    max(union(cluster_counts$year, noaa_counts$year))
  )
  
  # 4) Determine max cluster count for y-axis
  max_cluster <- max(cluster_counts$count, na.rm = TRUE)
  
  # 5) Palette for clusters
  pal3 <- hue_pal()(3)
  names(pal3) <- names(cluster_dirs)
  
  # 6) One plot per cluster
  cluster_plots <- map(names(cluster_dirs), function(nm) {
    df <- cluster_counts %>%
      filter(cluster == nm) %>%
      right_join(tibble(year = all_years), by = "year") %>%
      replace_na(list(count = 0))
    
    ggplot(df, aes(x = year, y = count)) +
      geom_col(fill = pal3[nm]) +
      scale_x_continuous(breaks = all_years) +
      ylim(0, max_cluster) +
      labs(title = nm, y = "Count of Events", x = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.title  = element_text(hjust = 0.5)
      )
  })
  
  # 7) NOAA plot (same x axis, own y scale)
  df_noaa <- tibble(year = all_years) %>%
    left_join(noaa_counts, by = "year") %>%
    replace_na(list(count = 0))
  
  p_noaa <- ggplot(df_noaa, aes(x = year, y = count)) +
    geom_col(fill = "grey70") +
    scale_x_continuous(breaks = all_years) +
    labs(title = "NOAA Excess Heat Episodes", y = "Count of Events", x = "Year") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      plot.title  = element_text(hjust = 0.5)
    )
  
  # 8) Stack: 3 cluster rows + NOAA row
  final_plot <- wrap_plots(
    c(cluster_plots, list(p_noaa)),
    ncol = 1
  )
  
  return(final_plot)
}

cluster_dirs <- c(
  "0.25°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.25","heat_index","county"),
  "0.31°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.3075","heat_index","county"),
  "0.39°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.39","heat_index","county")
)
noaa_dir <- here::here("data","output","04_noaa","southeast","excess_heat")

plots <- plot_clusters_and_noaa_individual(cluster_dirs, noaa_dir)

plot_noaa_vs_clusters_overlay <- function(cluster_dirs, noaa_dir) {
  
  # — 1) NOAA counts per year
  noaa_files <- list.files(noaa_dir, "\\.nc$", full.names = TRUE)
  noaa_dates <- str_extract(basename(noaa_files), "^\\d{4}-\\d{2}-\\d{2}")
  noaa_dates <- as.Date(noaa_dates, format = "%Y-%m-%d")
  noaa_years <- year(noaa_dates[!is.na(noaa_dates)])
  if (length(noaa_years) == 0) {
    stop("No valid NOAA dates found in ", noaa_dir)
  }
  df_noaa <- tibble(year = noaa_years) %>%
    count(year, name = "count")
  
  # — 2) Cluster counts per year (long format)
  df_clusters <- imap_dfr(cluster_dirs, ~{
    files <- list.files(.x, "\\.nc$", full.names = TRUE)
    dates <- str_extract(basename(files),
                         "\\d{4}-\\d{2}-\\d{2}(?=_cluster)")
    dates <- as.Date(dates, format = "%Y-%m-%d")
    yrs   <- year(dates[!is.na(dates)])
    if (length(yrs) == 0) return(NULL)
    
    tibble(year = yrs) %>%
      count(year, name = "count") %>%
      mutate(cluster = .y)
  })
  if (nrow(df_clusters) == 0) {
    stop("No valid cluster dates found in any of: ",
         paste(cluster_dirs, collapse = ", "))
  }
  
  # — 3) Build the full year sequence
  all_years <- seq(
    min(union(df_clusters$year, df_noaa$year)),
    max(union(df_clusters$year, df_noaa$year))
  )
  
  # — 4) Fill missing years with zeros
  df_noaa_full <- tibble(year = all_years) %>%
    left_join(df_noaa, by = "year") %>%
    replace_na(list(count = 0))
  
  df_clusters_full <- df_clusters %>%
    right_join(tibble(year = all_years), by = "year") %>%
    replace_na(list(count = 0))
  
  # — 5) Shared Y‑axis max
  y_max <- max(c(df_noaa_full$count, df_clusters_full$count), na.rm = TRUE)
  
  # — 6) Color palette for clusters
  pal3 <- hue_pal()(3)
  names(pal3) <- names(cluster_dirs)
  
  # build a sequence of 5‑year breaks
  year_breaks_5 <- seq(min(all_years), max(all_years), by = 5)
  
  # — 7) Cluster overlay plot, legend in top-left
  p_clusters <- ggplot(df_clusters_full,
                       aes(x = year, y = count, fill = cluster)) +
    geom_col(position = "identity", alpha = 0.5) +
    scale_fill_manual(values = pal3) +
    scale_x_continuous(breaks = year_breaks_5) +
    scale_y_continuous(limits = c(0, y_max)) +
    labs(title = "Clusters",
         x     = NULL,
         y     = "Count of Events",
         fill  = "Resolution") +
    theme_bw() +
    theme(
      axis.text.x         = element_text(angle = 90, vjust = 0.5),
      plot.title          = element_text(hjust = 0.5),
      legend.position     = c(0.01, 0.99),
      legend.justification= c("left", "top")
    )
  
  # — 8) NOAA plot (same y axis range)
  p_noaa <- ggplot(df_noaa_full, aes(x = year, y = count)) +
    geom_col(fill = "black", alpha = 0.5) +
    scale_x_continuous(breaks = year_breaks_5) +
    scale_y_continuous(limits = c(0, y_max)) +
    labs(title = "NOAA Excess Heat Episodes",
         x     = "Year",
         y     = "Count of Events") +
    theme_bw() +
    theme(
      axis.text.x    = element_text(angle = 90, vjust = 0.5),
      plot.title     = element_text(hjust = 0.5)
    )
  
  # — 9) Combine vertically
  final <- p_clusters / p_noaa + plot_layout(ncol = 1)
  return(final)
}


cluster_dirs <- c(
  "0.25°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.25","heat_index","county"),
  "0.31°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.3075","heat_index","county"),
  "0.39°/day" = here::here("data","output","03_cluster","02_cluster","advisory","points","0.39","heat_index","county")
)
noaa_dir <- here::here("data","output","04_noaa","southeast","excess_heat")

p <- plot_noaa_vs_clusters_overlay(cluster_dirs, noaa_dir)

# Save the figure
ggsave(here("figures","s08_time_series_noaa_cluster.png"),
       p, width = 9, height = 6, dpi = 300)
ggsave(here("figures","s08_time_series_noaa_cluster.svg"),
       p, width = 9, height = 6, device = "svg")

# For Kathie ----
plot_noaa_vs_single_cluster <- function(cluster_dir, noaa_dir) {

  # cluster_dir should be a named character vector of length 1, e.g.
  #   c("0.39°/day" = "path/to/0.39/folder")
  cluster_name <- names(cluster_dir)
  
  # — 1) NOAA counts per year
  noaa_files <- list.files(noaa_dir, "\\.nc$", full.names = TRUE)
  noaa_dates <- as.Date(str_extract(basename(noaa_files), "^\\d{4}-\\d{2}-\\d{2}"),
                        format = "%Y-%m-%d")
  df_noaa <- tibble(year = year(noaa_dates[!is.na(noaa_dates)])) %>%
    count(year, name = "count")
  
  # — 2) Single‐cluster counts per year
  files  <- list.files(cluster_dir, "\\.nc$", full.names = TRUE)
  dates  <- as.Date(str_extract(basename(files),
                                "\\d{4}-\\d{2}-\\d{2}(?=_cluster)"),
                    format = "%Y-%m-%d")
  df_cl <- tibble(year = year(dates[!is.na(dates)])) %>%
    count(year, name = "count")
  
  # — 3) Build full year seq and fill zeros
  all_years   <- seq(min(c(df_cl$year, df_noaa$year)),
                     max(c(df_cl$year, df_noaa$year)))
  df_noaa_f   <- tibble(year = all_years) %>%
    left_join(df_noaa, by="year") %>%
    replace_na(list(count=0))
  df_cl_f     <- tibble(year = all_years) %>%
    left_join(df_cl,   by="year") %>%
    replace_na(list(count=0))
  
  # — 4) Shared y‐max & 5‐year breaks
  y_max       <- max(c(df_noaa_f$count, df_cl_f$count))
  year_breaks <- seq(min(all_years), max(all_years), by = 5)
  
  # — 5) pick the one hue for this cluster
  pal1 <- hue_pal()(1)
  names(pal1) <- cluster_name
  
  # — 6) Cluster panel
  p_cl <- ggplot(df_cl_f, aes(x=year, y=count, fill=cluster_name)) +
    geom_col(alpha=0.5) +
    scale_fill_manual(values = pal1) +
    scale_x_continuous(breaks = year_breaks) +
    scale_y_continuous(limits = c(0,y_max)) +
    labs(title = "Clusters at 0.39°/day resolution",
         x     = NULL,
         y     = "Count of Events",
         fill  = NULL) +
    theme_bw() +
    theme(
      axis.text.x  = element_text(angle=90, vjust=0.5),
      plot.title   = element_text(hjust=0.5),
      legend.position = "none"
    )
  
  # — 7) NOAA panel
  p_noaa <- ggplot(df_noaa_f, aes(x=year, y=count)) +
    geom_col(fill="black", alpha=0.5) +
    scale_x_continuous(breaks = year_breaks) +
    scale_y_continuous(limits = c(0,y_max)) +
    labs(title = "NOAA Excess Heat Episodes",
         x     = "Year",
         y     = "Count of Events") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=90, vjust=0.5),
      plot.title  = element_text(hjust=0.5)
    )
  
  # — 8) Stack and return
  return(p_cl / p_noaa + plot_layout(ncol=1))
}

one39 <- c(
  "0.39°/day" = here::here(
    "data","output","03_cluster","02_cluster",
    "advisory","points","0.39","heat_index","county"
  )
)
noaa_dir <- here::here("data","output","04_noaa","southeast","excess_heat")

p2 <- plot_noaa_vs_single_cluster(one39, noaa_dir)

# Save the figure
ggsave(here("figures","s08_time_series_noaa_cluster_best.png"),
       p2, width = 9, height = 6, dpi = 300)
ggsave(here("figures","s08_time_series_noaa_cluster_best.svg"),
       p2, width = 9, height = 6, device = "svg")
