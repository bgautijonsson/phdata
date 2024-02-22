run_request() <- function() {

  base_url <- "ftp://ftp.ceda.ac.uk/badc/ukcp18/data/land-cpm/uk/5km/rcp85/01/variable/1hr/v20210615/"

  make_url <- function(variable) {
    url <- "ftp://ftp.ceda.ac.uk/badc/ukcp18/data/land-cpm/uk/5km/rcp85/01/variable/1hr/v20210615/"
    stringr::str_replace(url, "variable", variable)
  }

  url <- make_url("pr")

  req <- httr2::request(url)


  resp <- req |>
    httr2::req_options(
      userpwd = get_userpwd(),
      dirlistonly = TRUE
    ) |>
    httr2::req_perform()

  filenames <- resp |>
    httr2::resp_body_string() |>
    strsplit("\\n") |>
    unlist()


}
