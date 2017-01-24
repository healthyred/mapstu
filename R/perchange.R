#' @title Percent Change
#'
#' @description Provides a dataframe of percent change that can also be mapped
#'     The percent change function ignores NaN values, and Inf Values.
#' @param year1 The data vector that you want to subtract from
#' @param year2 The data vector is that will be subtracted from
#' @return a dataframe of percent change for each country
#'
#' @export


perchange <- function(year1, year2){

  ##creating a copy matrix of a 2 by 207 matrix and filling in the first columnn with the countries
  copy <- as.data.frame(matrix(0, ncol = 1, nrow =207))
  copy$V1 <- yearsdata$State.Countries
  colnames(copy)[1] <- "NAME"

  ##Creates a percent change function and returns the percent change for every single country
  copy$Perchange <- ((year1 - year2)/ year2) * 100

  ##Makes all the Inf values into Na, and then all of the NA into 0
  is.na(copy) <- sapply(copy, is.infinite)
  copy[is.na(copy)] <- 0

  ##Makes all of the NaN values in the set 0
  is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
  copy[is.nan(copy)] <- 0

  ##returns the dataframe with percent change
  return(copy)
}
