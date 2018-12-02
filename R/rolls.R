#' @title Rolling a device
#' @description Simulates rolling the specified device.
#' @param device object of class "device", with at least two sides and appropriate probability values
#' @param times the number of times the device is to be rolled
#' @return The observed rolls
#' @export
roll <- function(device, times) {
  check_times(times)
  sample <- sample(device$sides,
                   size = times,
                   replace = TRUE,
                   prob = device$prob)
  l <- list("rolls" = sample,
       "sides" = device$sides,
       "prob" = device$prob,
       "total" = times)
  class(l) <- "rolls"
  return(l)
}

#' @export
check_times <- function(times) {
  if((times %% 1) != 0) {
    stop("times must be an integer")
  } else if(times < 1) {
    stop("sides must be a positive integer")
  } else {
    return(TRUE)
  }
}

#' @export
print.rolls <- function(x) {
  cat("object 'rolls' \n\n")
  list = list(
    rolls = x$"rolls"
  )
  print(list)
  invisible(x)
}

#####################################
########### summary.rolls ###########
#####################################


#' @export
summary.rolls <- function(x) {
  proportions <- as.vector(table(x$rolls) / x$total)
  freqs <- data.frame(side = x$sides,
                      count = as.vector(table(x$rolls)),
                      prop = proportions)
  obj <- list(freqs = freqs)
  class(obj) <- "summary.rolls"
  obj
}

#' @export
print.summary.rolls <- function(x) {
  cat('summary "rolls"\n\n')
  print(x$freqs)
  invisible(x)
}
