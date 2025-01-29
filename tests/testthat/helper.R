
# Set up test directories
SpaDEStestSetUpDirectories <- function(tempDir = tempdir(), teardownEnv = testthat::teardown_env()){

  # List test paths and temporary directories
  testDirs <- .test_directories()

  # Create temporary directories
  for (d in testDirs$temp) dir.create(d)

  # Remove temporary directories on test teardown
  withr::defer({
    unlink(testDirs$temp$root, recursive = TRUE)
    if (file.exists(testDirs$temp$root)) warning(
      "Temporary test directory could not be removed: ", testDirs$temp$root, call. = F)
  }, envir = teardownEnv, priority = "last")

  # Copy module to temporary location
  .test_copyModule(testDirs$module, destDir = testDirs$temp$modules)

  # Sink output to file
  if (testthat::is_testing()) withr::local_output_sink(file.path(testDirs$temp$root, "local_output_sink.txt"))

  # Install "testthat" with dependencies into the R packages directory
  Require::Install("testthat", libPaths = testDirs$temp$libPath, verbose = -2)

  # Restore library paths after testing
  withr::local_libpaths(.libPaths(), .local_envir = teardownEnv)

  # Return test directories
  testDirs
}

# Set test paths and temporary directories
.test_directories <- function(tempDir = tempdir()){

  testDirs <- list()

  # Set source module location
  testDirs$module <- ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd())

  # Set input data path (must be absolute)
  ## Could maybe instead use system.file()
  testDirs$testdata <- file.path(testDirs$module, "tests/testthat", "testdata")

  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(testDirs$module)))
  )
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")   # For shared inputs
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")  # For test outputs
  testDirs$temp$modules  <- file.path(testDirs$temp$root, "modules")  # For modules
  testDirs$temp$libPath  <- file.path(testDirs$temp$root, "library")  # R package library
  testDirs$temp$projects <- file.path(testDirs$temp$root, "projects") # For project directories

  # Return
  testDirs
}

# Copy module files
.test_copyModule <- function(moduleDir, destDir, moduleName = basename(moduleDir)){

  # List module files
  modFiles <- file.info(list.files(moduleDir, full = TRUE))
  modFiles$name <- basename(row.names(modFiles))
  modFiles$path <- row.names(modFiles)

  # Create module directory
  modDir <- file.path(destDir, moduleName)
  dir.create(modDir)

  # Copy module files
  copyFiles <- list(
    files = paste0(basename(moduleDir), ".R"),
    dirs  = c("R", "data")
  )
  for (f in modFiles$path[!modFiles$isdir & modFiles$name %in% copyFiles$files]){
    file.copy(f, file.path(modDir, basename(f)))
  }
  for (d in modFiles$path[ modFiles$isdir & modFiles$name %in% copyFiles$dirs]){
    file.copy(d, modDir, recursive = TRUE)
  }
}


# Set global options
SpaDEStestLocalOptions <- function(teardownEnv = testthat::teardown_env()){

  # Set reproducible options:
  # - Silence messaging
  if (testthat::is_testing()) withr::local_options(list(reproducible.verbose = -2), .local_envir = teardownEnv)

  # Set Require package options:
  # - Clone R packages from user library
  # - Silence messaging
  withr::local_options(list(Require.cloneFrom = Sys.getenv("R_LIBS_USER")), .local_envir = teardownEnv)
  if (testthat::is_testing()) withr::local_options(list(Require.verbose = -2), .local_envir = teardownEnv)

  # Set SpaDES.core options:
  # - Do not rebuild package documentation
  withr::local_options(list(spades.moduleCodeChecks = FALSE), .local_envir = teardownEnv)
  withr::local_options(list(spades.moduleDocument   = FALSE), .local_envir = teardownEnv)

  # Set SpaDES.project options:
  # - Never update R profile
  withr::local_options(list(SpaDES.project.updateRprofile = FALSE), .local_envir = teardownEnv)
}


# Helper function: muffle messages and warnings
SpaDEStestMuffleConditions <- function(expr, suppressWarnings = getOption("spades.test.suppressWarnings", default = FALSE), ...){

  if (testthat::is_testing()){

    withCallingHandlers(
      expr,
      message               = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (suppressWarnings){
          tryInvokeRestart("muffleWarning")
        }else{
          if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
            tryInvokeRestart("muffleWarning")
          }
        }
      },
      ...
    )

  }else expr
}

