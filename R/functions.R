#' Guess Species
#'
#' @param x character, species
#'
#' @return string, standardized species name
#' @export
#'
#' @examples
#' guess_species("mouse")
guess_species <- function(x) {
  x <- tolower(x)
  x <- trimws(x)

  known_species <- c(
    "mouse" = "mouse",
    "mice" = "mouse",
    "mus musculus" = "mouse",
    "mus-musculus" = "mouse",
    "human" = "human",
    "homo sapiens" = "human",
    "homo-sapiens" = "human",
    "rat" = "rat",
    "rattus norvegicus" = "rat",
    "rattus-norvegicus" = "rat",
    "rhesus" = "rhesus",
    "rhesus monkey" = "rhesus",
    "rhesus macaque" = "rhesus",
    "macaca mulatta" = "rhesus",
    "macaca-mulatta" = "rhesus",
    "unknown" = "unknown"
  )

  out <- known_species[x]
  out[is.na(out)] <- "manual"
  out
}

#' Get location specific resources
#'
#' @param location string, name of computing location.
#'
#' @return list of resource locations
#' @export
get_resources <- function(location, species) {
  full_path <- function(x) {
    file.path(resource_base[[location]],
              species_specific[[species]][["path_ref"]],
              species_specific[[species]][[x]])
  }
  list(
    salmon_index = full_path("salmon_index"),
    T2G = full_path("T2G"),
    T3G = full_path("T3G"),
    whitelist = file.path(resource_base[[location]], whitelist)
  )
}

#' Run COMUNEQAIDR app
#'
#' @return Runs the shiny app
#' @export
run_comuneqaidr <- function() {
  appDir <- system.file("shiny-app", package = "COMUNEQAIDR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `COMUNEQAIDR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
