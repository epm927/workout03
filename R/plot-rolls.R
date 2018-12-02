#' @export
plot <- function(x, number = x$total) {
  barplot(table(x$rolls) / x$total,
          border = "grey",
          xlab = "sides of device",
          ylab = "relative frequencies",
          main = paste("Relative Frequencies in a series of", number, "rolls")
  )
}
