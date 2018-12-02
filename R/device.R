#' @title device
#' @description Creates an object of class device
#' @param sides vector of (at least 2) sides, default of 1 and 2
#' @param prob probability for each side
#' @return device object
#' @export
device <- function(sides = c(1, 2), prob = c(0.5, 0.5)) {
  if(check_sides(sides) == FALSE) {
    stop("invalid sides value")
  } else if(check_prob(prob) == FALSE) {
    stop ("invalid probability value")
  } else if(length(sides) != length(prob)) {
    stop("sides and prob must be of the same length")
  } else {
    obj <- list(
      sides = sides,
      prob = prob
    )
    class(obj) <- "device"
    return(obj)
  }
}

#' @export
print.device <- function(x) {
  cat('object "device"\n\n')
  device_df <- data.frame(
    sides = x$sides,
    prob = x$prob
  )
  print(device_df)
  invisible(x)
}

#' @export
is.device <- function(x) {
  if((class(x) == "device")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#####################################
######## Auxiliary Functions ########
#####################################

#' @title Checking "Sides"
#' @description Checks if the given input is a correct "sides" argument
#' @param sides vector of (at least 2) sides, default of 1 and 2e
#' @return logical value; TRUE or FALSE.
#' @export
check_sides <- function(sides) {
  if(length(sides) < 2) {
    stop("'sides' must be a vector of length greater than 1")
  } else if (sum(duplicated(sides)) >= 1) {
    stop("'sides' cannot have duplicated elements")
  } else {
    return(TRUE)
  }
}


#' @title Checking "Prob"
#' @description Checks if the given input is a correct "sides" argument
#' @param prob vector of (at least 2) probability values, between 0 and 1, summing to 1
#' @return logical value; TRUE or FALSE.
#' @export
check_prob <- function(prob) {
  if(sum(prob) != 1) {
    stop("elements in 'prob' must add up to 1")
  } else if(sum((prob < 0) & (prob > 1)) >= 1) {
    stop("elements in prob must be between 0 and 1")
  } else {
    return(TRUE)
  }
}
