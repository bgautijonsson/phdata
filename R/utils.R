#' A helper function to fetch the CEDA username and password from the .Renviron file
#'
#' @return A string containing the username and password separated by a colon
#' @export
get_userpwd <- function(type = "private") {
  if (type == "private") {
    out <- stringr::str_c(
      Sys.getenv("CEDA_USR"),
      Sys.getenv("CEDA_PWD"),
      sep = ":"
    )
  }
  if (type == "public") {
    out <- stringr::str_c(
      "usr",
      "pwd",
      sep = ":"
    )
  }
  out
}


#' Given a filename, create a path to download the file from the CEDA FTP server
#'
#' @param filename
#'
#' @return A string containing the URL to download a file from the CEDA FTP server
#' @export
make_download_path <- function(filename, type = "private") {
  url |>
    stringr::str_replace("//", stringr::str_c("//", userpwd(type = type), "@")) |>
    stringr::str_c(filename)
}

#' Given a filename, create a path to download the file from the CEDA FTP server
#'
#' @param filename
#'
#' @return A string containing the URL to download a file from the CEDA FTP server
#' @export
make_url_from_list <- function(filename, type = "private") {
  url |>
    stringr::str_replace_all("variable", variable) |>
    stringr::str_replace("//", stringr::str_c("//", userpwd(type = type), "@")) |>
    stringr::str_c(filename)
}

#' A table containing the variables, their names in the FTP data and unit
#'
#' @return
#' @export
variable_lookup <- function() {
  dplyr::tribble(
    ~variable, ~name, ~unit,
    "Precipitation rate", "pr", "mm/day",
    "Mean air temperature", "tas", "degC",
    "Maximum air temperature", "tasmax", "degC",
    "Minimum air temperature", "tasmin", "degC"
  )
}


#' Create a base URL for passing to httr2
#'
#' @param variable
#'
#' @return
#' @export
make_base_url <- function(variable) {
  url <- "ftp://ftp.ceda.ac.uk/badc/ukcp18/data/land-cpm/uk/5km/rcp85/01/variable/1hr/v20210615/"
  stringr::str_replace(url, "variable", variable)
}
