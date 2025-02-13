library(hrvester)
library(patchwork)
library(ggplot2)
library(furrr)
library(dplyr)
library(progressr)
library(stringr)
library(reticulate)

plan(multisession, workers = 12)
reticulate::use_virtualenv("path-to-venv")
handlers(global = TRUE)
handlers("progress")

if(Sys.info()['sysname'] == "Linux") {
  fit_dir = "path-for-fit-files" # set
 image_dir = "path-to-images" # set
 } else { # windows or macOS 
   fit_dir = "path-for-fit-files" # set
   image_dir = "path-to-images" # set
 }

# Check current .fit files
existing_fit_files <- list.files(fit_dir, pattern = "//.fit$", full.names = TRUE)

# Run Python script to download new files
py_run_file("./fit_export.py") # set as path to .py file

metrics = process_fit_directory(fit_dir) %>%
  filter(activity == "OST")

hrv_trend_plot(tail(metrics, n = 14), just_rssme = TRUE)
ggsave(paste0(image_dir, "ost.png"),
  width = 210*0.5,
  height = (297/4)*0.9,
  units = "mm",
  scale = 1.8)


plots <- plot_hrv_dashboard(metrics)
ggsave(
  paste0(image_dir,"hrv_dashboard.png"),
  plots,
  width = 12,
  height = 8,
  dpi = 300
)

weekly_status <- plot_weekly_heatmap(metrics)
ggsave(
  paste0(image_dir, "weekly_status.png"),
  weekly_status,
  width = 12,
  height = 8,
  dpi = 300
)

bjj_metrics <- plot_bjj_metrics(metrics)
ggsave(
  paste0(image_dir, "bjj_metrics.png"),
  bjj_metrics,
  width = 12,
  height = 8,
  dpi = 300
)

analyze_readiness(tail(metrics, n = 1), tail(metrics, n = 8)[2:7,])

training_recommendations(tail(calculate_neural_recovery(metrics)$neural_recovery_score, n = 1))

tail(metrics, n = 1)
