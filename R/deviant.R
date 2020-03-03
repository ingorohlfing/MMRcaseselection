

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
#' @export
most_deviant <- function(lmobject){
  if(class(lmobject) == "lm"){
    absresid <- sort(abs(residuals(lmobject)))
    return(absresid[length(absresid)])
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}

#' Most overpredicted case
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#'
#' @return The most overpredicted case with the largest negative residual
#' (the most negative residual).
#' @importFrom stats lm residuals
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' most_overpredicted(df)
#' @export
most_overpredicted <- function(lmobject){
  if(class(lmobject) == "lm"){
    resid <- sort(residuals(lmobject))
    return(resid[1])
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}
