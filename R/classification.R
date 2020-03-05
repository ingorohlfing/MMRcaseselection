

#' Classification of cases as typical and deviant using the prediction
#' interval.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param piwidth Width of the prediction interval.
#'
#' @return A dataframe with observed outcome, fitted outcome values,
#' upper and lower bound of the prediction interval and classification
#' of cases as typical or deviant. Originally proposed by Rohlfing and
#' Starke (\url{https://doi.org/10.1111/spsr.12052}).
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
    if(piwidth >= 0 & piwidth <= 1){
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
      stop('Prediction interval needs to be between 0 and 1')
    }
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}
