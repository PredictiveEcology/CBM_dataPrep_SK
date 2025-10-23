
# Map SCANFI species to SK curves
SCANFImatchSpeciesToCurves <- function(masterRaster, spsMatch, spsRast = NULL,
                                       inputDir = getOption("spades.inputPath", ".")){

  if (is.null(spsRast)){
    message("SCANFI species: reading species layers")
    spsRast <- CBMutils::CBMsourcePrepInputs(
      "SCANFI-2020-LandR", inputPath = inputDir, verbose = -1)$source
  }

  message("SCANFI species: cropping data to study area")

  adminMask <- sf::st_intersection(
    CBMutils::CBMsourcePrepInputs(
      "StatCan-admin", inputPath = inputDir, verbose = -1)$source |>
      subset(admin == "Saskatchewan") |>
      sf::st_segmentize(10000) |>
      sf::st_transform(sf::st_crs(spsRast)) |>
      sf::st_union(),
    sf::st_as_sfc(sf::st_bbox(masterRaster)) |>
      sf::st_segmentize(10000) |>
      sf::st_transform(sf::st_crs(spsRast))
  )
  spsRast <- terra::crop(spsRast, adminMask, mask = FALSE, snap = "out")

  message("SCANFI species: extracting leading species")

  spsNames <- names(spsRast)

  spsMax <- terra::app(spsRast, "max")
  spsMax <- terra::mask(spsMax, terra::vect(adminMask))
  spsMax <- terra::classify(spsMax, cbind(0, NA))

  cellIdx <- terra::cells(spsMax)

  spsRast <- data.table::data.table(terra::extract(spsRast, cellIdx))
  spsRast[, max := terra::extract(spsMax, cellIdx)]
  spsRast[, (spsNames) := lapply(.SD, `==`, max), .SDcols = spsNames]
  spsRast[, max := NULL]

  message("SCANFI species: matching leading species to SK curves")

  spsMatch[, id := match(curve, unique(spsMatch$curve))]
  spsMatch[, intersect(spsNames, names(spsMatch)) := lapply(.SD, as.logical),
           .SDcols = intersect(spsNames, names(spsMatch))]
  spsMatch[,   setdiff(spsNames, names(spsMatch)) := FALSE]
  spsRast <- data.table::merge.data.table(
    spsRast, spsMatch, by = spsNames, all.x = TRUE, sort = FALSE
  )

  if (any(is.na(spsRast$id))){
    notFound <- unique(spsRast[is.na(id), .SD, .SDcols = spsNames])
    stop("SCANFI leading species combination(s) have not been assigned a curve: ",
         paste(shQuote(apply(notFound, 1, function(r) paste(names(r)[r], collapse = ";"))),
               collapse = ", "))
  }

  spsRast <- spsRast$id
  terra::values(spsMax) <- 0
  terra::set.values(spsMax, cellIdx, spsRast)
  levels(spsMax) <- rbind(cbind(id = 0, curve = NA), unique(spsMatch[, .(id, curve)]))
  spsMax
}

