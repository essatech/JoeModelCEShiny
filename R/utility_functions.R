# General utility functions


#' Seconds to minutes and seconds
#'
#' @description Pretty print convert seconds to minutes and seconds
pretty_print_seconds <- function(x) {
  x <- round(x, 0)
  if(x < 60) {
    etime <- paste0(x, " seconds")
  } else {
    mins <- x / 60
    mins <- round(mins, 0)
    secs <- x %% 60
    etime <- paste0(mins, " minutes ", secs, " seconds")
  }
  return(etime)
}