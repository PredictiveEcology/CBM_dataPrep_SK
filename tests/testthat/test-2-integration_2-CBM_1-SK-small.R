
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration: CBM: SK-small 1998-2000", {

  ## Run simInit and spades ----

  # Set times
  times <- list(start = 1998, end = 2000)

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "intg_2-CBM_1-SK-small")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set Github repo branch
  if (!nzchar(Sys.getenv("BRANCH_NAME"))) withr::local_envvar(BRANCH_NAME = "development")

  # Set up project
  simInitInput <- SpaDEStestMuffleOutput(

    SpaDES.project::setupProject(

      times = times,

      modules = c(
        paste0("PredictiveEcology/CBM_defaults@",    Sys.getenv("BRANCH_NAME")),
        "CBM_dataPrep_SK",
        paste0("PredictiveEcology/CBM_dataPrep@",    Sys.getenv("BRANCH_NAME")),
        paste0("PredictiveEcology/CBM_vol2biomass@", Sys.getenv("BRANCH_NAME")),
        paste0("PredictiveEcology/CBM_core@",        Sys.getenv("BRANCH_NAME"))
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
        crs        = "EPSG:3979",
        extent     = c(xmin = -687696, xmax = -681036, ymin = 711955, ymax = 716183),
        resolution = 30,
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
      disturbanceRasters = "https://drive.google.com/file/d/12YnuQYytjcBej0_kdodLchPg7z9LygCt",

      # Set outputs
      outputs = as.data.frame(expand.grid(
        objectName = c("cbmPools", "NPP"),
        saveTime   = sort(c(times$start, times$start + c(1:(times$end - times$start))))
      ))
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


  ## Check outputs ----

  expect_true(!is.null(simTest$spinupResult))

  expect_true(!is.null(simTest$cbmPools))

  expect_true(!is.null(simTest$NPP))

  expect_true(!is.null(simTest$emissionsProducts))

})


