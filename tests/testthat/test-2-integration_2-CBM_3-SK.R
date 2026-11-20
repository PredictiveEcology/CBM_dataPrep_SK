
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SK", {

  ## Run simInit and spades ----

  skip_if(as.logical(Sys.getenv("CI", FALSE)) & !"main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")))
  skip_if(is_testing()) # Test expensive: kept for interactive testing

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "2-intg_2-CBM_3-SK",
    times       = list(start = 1985, end = 1985),
    modules     = c(
      paste0("PredictiveEcology/CBM_defaults@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      "CBM_dataPrep_SK",
      paste0("PredictiveEcology/CBM_dataPrep@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_vol2biomass@", ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_core@",        ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development"))
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  # Check outputs
  expect_true(!is.null(simTest$emissionsProducts))

})


