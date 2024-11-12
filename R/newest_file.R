

#' Get the newest file from a directory
#'
#' This function lists all files in a given directory and returns the newest file
#' that matches a specified pattern. The date is extracted from the filename and
#' used to determine the newest file.
#'
#' @param dir A character string specifying the directory to list the files from.
#' @param pattern An optional regular expression. Only file names which match the
#'   regular expression will be returned. Defaults to NULL.
#'
#' @return A character string with the path to the newest file that matches the
#'   specified pattern.
#'
#'
#' @export
newest_file <- function(dir, pattern = NULL){

  clean_path <- path <- NULL

  files <- list.files(dir, pattern = pattern)
  info <- fs::file_info(files) |>
    dplyr::mutate(clean_path = stringr::str_remove(path, "^\\d{4}-\\d{2}-\\d{2}_"),
           date = stringr::str_extract(path, "^\\d{4}-\\d{2}-\\d{2}")) |>
    dplyr::group_by(clean_path) |>
    dplyr::slice_max(order_by = date, n = 1) |>
    dplyr::pull(path)

  return(stringr::str_c(file.path(dir, info)))

}
