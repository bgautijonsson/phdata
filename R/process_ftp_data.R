#' Given the URL of a file, download the file and process it for later use
#'
#' @param filename
#'
#' @return
#' @export
process_data <- function(filename) {

  Sys.sleep(0.1)

  from_to <- stringr::str_extract_all(filename, "_[0-9]{8}-[0-9]{8}")[[1]] |>
    stringr::str_replace("_", "") |>
    stringr::str_split_1("-")

  from <- as.Date(from_to[1], format = "%Y%m%d")
  to <- from + lubridate::period(1, "month") - lubridate::period(1, "day")

  tmp <- tempfile()

  download.file(
    make_download_path(filename),
    tmp,
    mode = "wb",
    quiet = TRUE
  )

  temp_d <- ncdf4::nc_open(tmp)

  pr <- ncdf4::ncvar_get(temp_d, "pr")
  # apply(MARGIN = c(1, 2), FUN = max)

  tm <- ncdf4::ncvar_get(temp_d, "yyyymmddhh") |>
    stringr::str_squish() |>
    lubridate::as_datetime(
      format = "%Y%m%d%H"
    )

  lat <- ncdf4::ncvar_get(temp_d, "latitude")
  long <- ncdf4::ncvar_get(temp_d, "longitude")

  dplyr::tibble(
    time = tm
  )

  out <- tidyr::crossing(
    proj_x = 1:180,
    proj_y = 1:244
  ) |>
    dplyr::arrange(proj_y, proj_x) |>
    dplyr::mutate(
      longitude = as.numeric(long),
      latitude = as.numeric(lat),
      station = dplyr::row_number()
    ) |>
    tidyr::crossing(
      time = tm
    ) |>
    dplyr::arrange(proj_y, proj_x) |>
    dplyr::mutate(
      precip = as.numeric(pr),
    )  |>
    mutate(
      time = as_date(time)
    ) |>
    group_by(proj_x, proj_y, latitude, longitude, station, time) |>
    summarise(
      pr_max = max(precip),
      pr_sum = sum(precip),
      .groups = "drop"
    )

  out |>
    mutate(
      month = lubridate::floor_date(from, "month")
    ) |>
    group_by(month) |>
    write_dataset(
      path = "data",
      format = "parquet"
    )
}
