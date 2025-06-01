
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM_dataPrep and disturbances", {

  ## Run simInit and spades ----

  # Set times
  times <- list(start = 1998, end = 2000)

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "2-intg_1-dataPrep")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set Github repo branch
  if (!nzchar(Sys.getenv("BRANCH_NAME"))) withr::local_envvar(BRANCH_NAME = "development")

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      times = times,

      modules = c(
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@", Sys.getenv("BRANCH_NAME"))
      ),
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),

      # Set required packages for project set up
      require = "terra",

      # Set study area
      masterRaster = terra::rast(
        extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        resolution = 30,
        crs        = "EPSG:3979",
        vals       = 1
      ),

      # Set disturbances
      ## Test matching user disturbances with CBM-CFS3 disturbances
      disturbanceMeta = rbind(
        data.frame(eventID = 1, wholeStand = 1, name = "Wildfire"),
        data.frame(eventID = 2, wholeStand = 1, name = "Clearcut harvesting without salvage"),
        data.frame(eventID = 3, wholeStand = 0, name = "Generic 20% mortality"),
        data.frame(eventID = 4, wholeStand = 1, name = "Deforestation"),
        data.frame(eventID = 5, wholeStand = 0, name = "Generic 20% mortality")
      ),
      disturbanceRasters = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt"
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
  expect_equal(nrow(simTest$standDT), 6763)


  ## Check output 'cohortDT' ----

  expect_true(!is.null(simTest$cohortDT))
  expect_true(inherits(simTest$cohortDT, "data.table"))

  for (colName in c("cohortID", "pixelIndex", "gcids", "ages")){
    expect_true(colName %in% names(simTest$cohortDT))
    expect_true(all(!is.na(simTest$cohortDT[[colName]])))
  }

  expect_identical(data.table::key(simTest$cohortDT), "cohortID")

  # Check spinup ages are all >= 3
  expect_true("ageSpinup" %in% names(simTest$cohortDT))
  expect_equal(simTest$cohortDT$ageSpinup[simTest$cohortDT$ages >= 3],
               simTest$cohortDT$ages[simTest$cohortDT$ages >= 3])
  expect_true(all(simTest$ageSpinup[simTest$cohortDT$ages < 3] == 3))

  # Check number of valid cohorts (no NAs in any column)
  expect_equal(nrow(simTest$cohortDT), nrow(simTest$standDT))


  ## Check output 'gcMeta' ----

  expect_true(!is.null(simTest$gcMeta))
  expect_true(inherits(simTest$gcMeta, "data.table"))

  for (colName in c("gcids", "species_id", "sw_hw")){
    expect_true(colName %in% names(simTest$gcMeta))
    expect_true(all(!is.na(simTest$gcMeta[[colName]])))
  }


  ## Check output 'userGcM3' ----

  expect_true(!is.null(simTest$userGcM3))
  expect_true(inherits(simTest$userGcM3, "data.table"))

  for (colName in c("gcids", "Age", "MerchVolume")){
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
  expect_equal(nrow(simTest$disturbanceEvents), 1374)


  ## Check output 'disturbanceMeta' ----

  expect_true(!is.null(simTest$disturbanceMeta))
  expect_true(inherits(simTest$disturbanceMeta, "data.table"))

  expect_equal(nrow(simTest$disturbanceMeta), 5)

  # Check that disturbances have been matched correctly
  rowsExpect <- rbind(
    data.frame(
      eventID             = 1,
      wholeStand          = 1,
      name                = "Wildfire",
      disturbance_type_id = 1
    ),
    data.frame(
      eventID             = 2,
      wholeStand          = 1,
      name                = "Clearcut harvesting without salvage",
      disturbance_type_id = 204
    ),
    data.frame(
      eventID             = 3,
      wholeStand          = 0,
      name                = "Generic 20% mortality",
      disturbance_type_id = 168
    ),
    data.frame(
      eventID             = 4,
      wholeStand          = 1,
      name                = "Deforestation",
      disturbance_type_id = 7
    ),
    data.frame(
      eventID             = 5,
      wholeStand          = 0,
      name                = "Generic 20% mortality",
      disturbance_type_id = 168
    )
  )

  for (i in 1:nrow(rowsExpect)){
    expect_equal(nrow(merge(simTest$disturbanceMeta, rowsExpect[i,], by = names(rowsExpect))), 1)
  }
})

