

#' Classification of cases as typical and deviant using the prediction
#' interval.
#'
#' Case are designated as typical (= well predicted) and deviant
#' (= badly predicted) based on the prediction interal. The x\% prediction
#' interval represents the range of outcome values that we expect to include
#' x\% of outcome values in future samples. For example, a 95\% prediction
#' interval ranging from 0-5 conveys that 95\% of future outcome values of
#' similar cases will be in the range of 0-5. If the observed outcome is
#' inside of the interval, the case counts as typical and as deviant
#' otherwise.
#'
#' Proposed by Rohlfing, Ingo and Peter Starke (2013):
#' Building on Solid Ground: Robust Case Selection in Multi-Method Research.
#' *Swiss Political Science Review* 19 (4): 492-512.
#' (\url{https://doi.org/10.1111/spsr.12052})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param piwidth Width of the prediction interval.
#'
#' @return A dataframe with the observed outcome, fitted outcome,
#' upper and lower bound of the % prediction interval and classification
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
