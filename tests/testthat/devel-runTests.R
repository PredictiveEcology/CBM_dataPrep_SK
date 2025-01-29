
## SET TEST OPTIONS ----

  # Set custom input data location
  options("reproducible.inputPaths" = NULL)

  # Suppress warnings from calls to setupProject, simInit, and spades
  options("spades.test.suppressWarnings" = TRUE)


## RUN ALL TESTS ----

  # Run all tests
  testthat::test_local()

  # Run all tests with different reporters
  testthat::test_local(reporter = testthat::LocationReporter)
  testthat::test_local(reporter = testthat::SummaryReporter)


## RUN INDIVIDUAL TESTS ----

  ## Run module with defaults
  testthat::test_local(filter = "1-module_1-defaults")

  ## Run module with a smaller study area
  testthat::test_local(filter = "1-module_2-withAOI")




