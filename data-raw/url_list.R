## code to prepare `url_list` dataset goes here

url <- make_base_url("pr")

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

from_to <- stringr::str_extract_all(filenames, "_[0-9]{8}-[0-9]{8}") |>
  stringr::str_replace("_", "") |>
  stringr::str_split("-", simplify = TRUE)

from <- as.Date(from_to[, 1], format = "%Y%m%d")
to <- from + lubridate::period(1, "month") - lubridate::period(1, "day")

file_paths <- make_download_path(filenames, type = "public") |>
  stringr::str_replace_all("pr", "variable")

url_list <- dplyr::tibble(
  from = from,
  to = to,
  url = file_paths
)

usethis::use_data(url_list, overwrite = TRUE)
