#' @export
"[.rolls" <- function(x, i) {
  x$roll[i]
}

#' @export
"[<-.rolls" <- function(x, i, value) {
  if (value != x$sides[1] & value != x$sides[2]) {
    stop(sprintf('\nreplacing value must be %s or %s', x$sides[1], x$coin[2]))
  }
  if (i > x$total) {
    stop("\nindex out of bounds")
  }
  x$rolls[i] <- value
  x
}

#' @export
"+.rolls" <- function(device, increment) {
  if (length(increment) != 1 | increment <= 0) {
  stop("\ninvalid increment")
}
  more <- roll(device, times = increment)
  r <- c(device, more)
  r$sides <- device$sides
  r$rolls <- c(device$rolls, more$rolls)
  r$prob <- device$prob
  r$total <- device$total + more$total
  class(r) <- "rolls"
  r
}
