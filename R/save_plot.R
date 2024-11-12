#' Save a Plot with a Timestamped Filename
#'
#' This function saves a plot to a specified directory with a filename that includes the current date.
#' If no plot is specified, the last plot created is saved. The user can also provide a name for the plot.
#'
#' @param plot A plot object to save. Default is the last plot created.
#' @param dir A character string specifying the directory to save the plot.
#' @param name A character string to name the plot file. Default is "plot".
#' @param extension A character string for the file extension. Default is "png".
#' @param ... Additional arguments passed to \code{ggsave}.
#'
#' @return A character string of the path to the saved plot file.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' save_plot(p, dir = "plots", name = "scatterplot", width = 5, height = 5)
#' }
#'
#' @export
save_plot <- function(plot = ggplot2::last_plot(), dir, name = "plot", extension = "png", ...) {
  timestamp <- Sys.Date()
  filename <- paste0(timestamp, "_", name, ".", extension)
  filepath <- file.path(dir, filename)
  ggplot2::ggsave(filepath, plot = plot, ...)
  return(filepath)
}
