
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM_dataPrep: SK test area (SPU 28) 2012", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "2-intg_1-dataPrep"
  times       <- list(start = 1998, end = 2000)

  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      modules = c(
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@", Sys.getenv("BRANCH_NAME", "development"))
      ),
      times   = times,
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),

      # Set required packages for project set up
      require = "terra",

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

      # Set disturbances
      disturbanceRastersURL = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt"
    )
  )

  # Run simInit
  simTestInit <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )

  expect_s4_class(simTest, "simList")


  ## Check output 'standDT' ----

  expect_true(!is.null(simTest$standDT))
  expect_true(inherits(simTest$standDT, "data.table"))

  for (colName in c("pixelIndex", "area", "spatial_unit_id")){
    expect_true(colName %in% names(simTest$standDT))
    expect_true(all(!is.na(simTest$standDT[[colName]])))
  }
  expect_identical(data.table::key(simTest$standDT), "pixelIndex")

  # Check number of valid pixels (no NAs in any column)
  expect_equal(nrow(simTest$standDT), 6753)

  # Check spatial units
  expect_equal(sort(unique(simTest$standDT$spatial_unit_id)), 28)


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex", "gcids", "age")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  # Check spinup ages are all >= 3
  expect_true("ageSpinup" %in% names(simTest$cohortDT))
  expect_equal(simTest$cohortDT$ageSpinup[simTest$cohortDT$age >= 3],
               simTest$cohortDT$age[simTest$cohortDT$age >= 3])
  expect_true(all(simTest$ageSpinup[simTest$cohortDT$age < 3] == 3))

  # Check number of valid cohorts (no NAs in any column)
  expect_equal(nrow(simTest$cohortDT), nrow(simTest$standDT))


  ## Check output 'curveID' ----

  expect_true(!is.null(simTest$curveID))
  expect_equal(simTest$curveID, c("species", "prodClass"))


  ## Check output 'userGcMeta' ----

  expect_true(!is.null(simTest$userGcMeta))
  expect_true(inherits(simTest$userGcMeta, "data.table"))

  for (colName in c(simTest$curveID, "species_id", "sw_hw")){
    expect_true(colName %in% names(simTest$userGcMeta))
    expect_true(all(!is.na(simTest$userGcMeta[[colName]])))
  }


  ## Check output 'userGcM3' ----

  expect_true(!is.null(simTest$userGcM3))
  expect_true(inherits(simTest$userGcM3, "data.table"))

  for (colName in c("curveID", "Age", "MerchVolume")){
    expect_true(colName %in% names(simTest$userGcM3))
    expect_true(all(!is.na(simTest$userGcM3[[colName]])))
  }


  ## Check output 'disturbanceEvents' -----

  expect_true(!is.null(simTest$disturbanceEvents))
  expect_true(inherits(simTest$disturbanceEvents, "data.table"))

  for (colName in c("pixelIndex", "year", "eventID")){
    expect_true(colName %in% names(simTest$disturbanceEvents))
    expect_true(is.integer(simTest$disturbanceEvents[[colName]]))
    expect_true(all(!is.na(simTest$disturbanceEvents[[colName]])))
  }

  expect_true(all(simTest$disturbanceEvents$year %in% 1985:2011))
  expect_equal(nrow(subset(simTest$disturbanceEvents, year %in% 1998:2000)), 1393)


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 5)
  expect_equal(simTest$disturbanceMeta$disturbance_type_id, c(1, 204, 7, 168, 168))

})

