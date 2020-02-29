

#' Most typical case
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most typical case having the smallest absolute
#' residual of all cases. Originally proposed by Seawright and Gerring
#' \url{https://doi.org/10.1177/1065912907313077}.
#'
#' @importFrom stats lm residuals
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_typical(df)
#' @export
most_typical <- function(lmobject){
  absresid <- sort(abs(residuals(lmobject)))
  return(absresid[1])
}
