
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SCANFI 2020 data", {

  ## Run simInit and spades ----

  skip_if(as.logical(Sys.getenv("CI", FALSE)) & !"main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")))
  skip_if(is_testing()) # Test expensive: kept for interactive testing

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "2-intg_3-SCANFI-2020",
    times       = list(start = 2020, end = 2020),
    modules     = c(
      paste0("PredictiveEcology/CBM_defaults@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      "CBM_dataPrep_SK",
      paste0("PredictiveEcology/CBM_dataPrep@",    ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_vol2biomass@", ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/CBM_core@",        ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development"))
    ),

    # Set study area
    masterRaster = terra::rast(
      crs  = "EPSG:3979",
      res  = 30,
      vals = 1L,
      xmin = -687696,
      xmax = -681036,
      ymin =  711955,
      ymax =  716183
    ),

    # Set input data sources
    ageLocator = "SCANFI-2020-age",
    spsLocator = "SCANFI-2020-LandR"
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

