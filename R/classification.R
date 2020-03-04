

#' Classification of cases as typical and deviant using the prediction
#' interval.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param piwidth Width of the prediction interval.
#'
#' @return A dataframe with observed outcome, fitted outcome values,
#' upper and lower bound of the prediction interval and classification
#' of cases as typical or deviant.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' predint(df, piwidth = 0.9)
#'
#' @export
predint <- function(lmobject, piwidth = 0.95){
  if(class(lmobject) == "lm"){
    temp <- as.data.frame(suppressWarnings(predict.lm(df,
                                                      interval = "prediction",
                                                      level = piwidth)))
    outcome <- lmobject$model[, 1]
    comb <- cbind(temp, outcome)
    comb$status <- ifelse(comb$outcome < comb$lwr |
                            comb$outcome > comb$upr,
                          "deviant", "typical")
    return(comb)
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}
