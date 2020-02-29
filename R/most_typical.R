

#' Most typical case
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most typical case having the smallest absolute
#' residual of all cases.
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
