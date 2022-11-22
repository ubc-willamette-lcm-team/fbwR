#' Find the closet value to x in a table, given a provided tolerance value.
#' Taken from the R MALDIQuant library
#' @param x Value to look up for matching in the table
#' @param table Table of values with columns to search/lookup
#' @param tolerance Accepted tolerance between the value to look up and the 
#' closest value in the lookup table. If distance between x and the closest
#' value in table is greater than tolerance, nomatch is returned. Defaults to
#' infinity, such that no tolerance is applied.
#' @param nomatch The value to return if the difference between x and the
#' closest value in the table is greater than the tolerance value
#' @return Closest value in table to x.
#' @export

match_closest <- function(x, table, tolerance = Inf, nomatch = NA_integer_) {
  lIdx <- findInterval(x, table, rightmost.closed = FALSE, all.inside = TRUE)
  rIdx <- lIdx + 1L # L = integer
  lIdx[lIdx == 0L] <- 1L
  lDiff <- abs(table[lIdx] - x)
  rDiff <- abs(table[rIdx] - x)
  d <- which(lDiff >= rDiff)
  lIdx[d] <- rIdx[d]
  if (any(is.finite(tolerance))) {
    if (any(tolerance < 0L)) {
      warning(sQuote("tolerance"), " < 0 is meaningless. Set to zero.")
      tolerance[tolerance < 0L] <- 0L
    }
    if (length(nomatch) != 1L) {
      stop("Length of ", sQuote("nomatch"), " has to be one.")
    }
    tolerance <- rep_len(tolerance, length(table))
    lDiff[d] <- rDiff[d]
    lIdx[lDiff > tolerance[lIdx]] <- nomatch
  }
  lIdx
}
