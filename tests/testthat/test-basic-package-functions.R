library(NHSRplotthedots)

test_that("spc function can create a ggplot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  #act
  result <- spc(df, "data", "date")

  #assert
  expect_s3_class(result,"ggplot")
  expect_identical(result$labels$title, "SPC Chart")
  expect_identical(result$labels$x, "Date")
  expect_identical(result$labels$y, "Value")
})

test_that("spc function can create a faceted ggplot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  date <- rep(seq(as.Date("2021-03-22"), by = 1, length.out = 12), times = 2)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)

  #act
  result <- spc(df, "data", "date", "category")

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for faceted ggplot object
})

test_that("spc function returns a dataframe when options$outputChart is FALSE", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(outputChart = FALSE)

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"tbl")
})

test_that("ggplot title and axis labels can be modified with options", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    mainTitle = "New Plot Title",
    xAxisLabel = "New X Label",
    yAxisLabel = "New Y Label"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  expect_identical(result$labels$title, "New Plot Title")
  expect_identical(result$labels$x, "New X Label")
  expect_identical(result$labels$y, "New Y Label")
})

test_that("a target line can be added to the plot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  target <- rep(15, times = 12)
  df <- tibble(data, date, target)
  options = list(
    target = "target"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for target line in data frame and visible line on plot
})

test_that("limits can be rebased at an intervention point", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  intervention <- c(0,0,0,1,0,0,0,0,0,0,0,0)
  df <- tibble(data, date, intervention)
  options = list(
    rebase = "intervention"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertions for limit recalculations in data frame and visible lines on plot
})

test_that("limits can be rebased at multiple intervention points", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  intervention <- c(0,0,0,1,0,0,0,0,0,1,0,0)
  df <- tibble(data, date, intervention)
  options = list(
    rebase = "intervention"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertions for limit recalculations in data frame and visible lines on plot
})

test_that("plotting point size can be adjusted", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    pointSize = 4
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for line and point size changes
})

test_that("improvement direction can be set as 'decrease'", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    improvementDirection = "decrease"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for point colours
})

test_that("y axis values can be set as percentages", {
  #arrange
  data <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.8,0.7,0.6)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    percentageYAxis = TRUE
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion to confirm axis labels are percentages
})

test_that("a trajectory line can be added to the plot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  trajectory <- seq(from = 0.1, to = 5, length.out = 12)
  df <- tibble(data, date, trajectory)
  options = list(
    trajectory = "trajectory"
  )

  #act
  result <- spc(df, "data", "date", options = options)

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for trajectory line in data frame and visible line on plot
})

#TODO add test for remaining options:
# fixedXAxisMultiple
# fixedYAxisMultiple
# xAxisDateFormat
# xAxisBreaks
# yAxisBreaks
