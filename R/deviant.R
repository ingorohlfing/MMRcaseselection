

#' Most deviant case
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most deviant case having the largest absolute
#' residual of all cases. Originally proposed by Seawright and Gerring
#' \url{https://doi.org/10.1177/1065912907313077}.
#'
#' @importFrom stats lm residuals
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_deviant(df)
#' @examples
most_deviant <- function(lmobject){
  if(class(lmobject) == "lm"){
    absresid <- sort(abs(residuals(lmobject)))
    return(absresid[length(absresid)])
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}
