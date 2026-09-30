library(testthat)

heatmap_data <- data.frame(
  date = seq(as.Date("2015-12-28"), as.Date("2016-01-11"), by = "day"),
  laying_rmssd = rep(50, 15),
  laying_resting_hr = rep(60, 15),
  standing_hr = rep(75, 15),
  hrr_60s = rep(25, 15)
)

test_that("plot_weekly_heatmap uses locale-independent weekday keys (R17)", {
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)

  p <- expect_no_warning(plot_weekly_heatmap(heatmap_data))

  built <- ggplot2::ggplot_build(p)
  # All seven weekday keys are populated; none collapsed to NA
  weekdays_used <- built$data[[1]]$x
  expect_true(all(weekdays_used %in% seq_len(7)))
  expect_true(!any(is.na(weekdays_used)))
  # Labels are fixed English weekday abbreviations, Monday to Sunday
  expect_equal(
    as.character(unique(p$data$day_of_week)),
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  )
})

test_that("plot_weekly_heatmap groups ISO weeks across New Year (R17)", {
  p <- plot_weekly_heatmap(heatmap_data)
  weeks <- as.character(unique(p$data$week))

  # 2015-12-31 and 2016-01-01 both belong to ISO week 2015-W53
  expect_true("2015-W53" %in% weeks)
  expect_false("2016-W53" %in% weeks)
  # Week keys sort chronologically
  expect_true(!is.unsorted(match(weeks, sort(weeks))))
})

test_that("plot_weekly_heatmap shows missing recovery as No data (R17/R16)", {
  d <- heatmap_data
  d$hrr_60s <- NA # no usable component -> insufficient recovery score
  p <- plot_weekly_heatmap(d)
  expect_true("No data" %in% as.character(p$data$status))
})

test_that("rmssd_change heatmap method also works", {
  p <- expect_no_warning(plot_weekly_heatmap(heatmap_data, method = "rmssd_change"))
  expect_true(all(
    as.character(p$data$status) %in% c("Fresh", "Normal", "Caution", "Warning", "No data")
  ))
})
