test_that("get_HR handles empty fit object", {
  mock_fit <- structure(list(), class = "FitFile")
  expect_error(get_HR(mock_fit))
})

test_that("hrv_metrics handles valid FIT file", {
  mock_fit <- create_mock_fit_file()
  result <- hrv_metrics(mock_fit)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "morning", "laying_rMSSD", "standing_rMSSD",
                        "laying_mean_hr", "standing_mean_hr"))
})

test_that("cache_hrv_metrics handles new and existing files", {
  temp_dir <- tempdir()
  create_test_fit_files(temp_dir)
  
  result1 <- cache_hrv_metrics(temp_dir)
  expect_s3_class(result1, "data.frame")
  
  # Should use cache on second run
  result2 <- cache_hrv_metrics(temp_dir)
  expect_identical(result1, result2)
})
