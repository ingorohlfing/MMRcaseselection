

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
      # calculating prediction interval
      temp <- as.data.frame(suppressWarnings(predict.lm(df,
                                                        interval = "prediction",
                                                        level = piwidth)))
      # extracting outcome values
      outcome <- lmobject$model[, 1]
      # merging outcome values into dataframe
      comb <- cbind(temp, outcome)
      # classification of cases
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

#' Plot of typical and deviant cases based on prediction interval
#'
#' Presented in Rohlfing, Ingo and Peter Starke (2013):
#' Building on Solid Ground: Robust Case Selection in Multi-Method Research.
#' \emph{Swiss Political Science Review} 19 (4): 492-512.
#' (\url{https://doi.org/10.1111/spsr.12052})
#'
#' @param pred.df A dataframe created with \code{predint}.
#'
#' @return A plot of the observed outcome against the fitted outcome with
#' prediction intervals and case classifications. Created with
#' \code{\link{ggplot2}}.
#
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' predintstatus <- predint(df, piwidth = 0.9)
#' predint_plot(predintstatus)
#'
#' @export
predint_plot <- function(pred.df){
  ggplot2::ggplot(data = pred.df) +
    geom_point(mapping = aes(x = fit, y = outcome, color = status)) +
    geom_abline(intercept = 0, slope = 1, linetype = 5) +
    geom_errorbar(mapping = aes(x = fit, ymin = lwr, ymax = upr,
                                color = status)) +
    scale_color_viridis_d() +
    theme_classic() +
    theme(legend.title = element_blank())
}

#' Classification of cases as typical and deviant using the standard
#' deviation of the residuals.
#'
#' The share of the standard deviation of the residuals is used to
#' designate cases as typical or deviant.
#'
#' Proposed by Lieberman, Evan S. (2005): Nested Analysis as a Mixed-Method
#' Strategy for Comparative Research. *American Political Science Review*
#' 99 (3): 435-452. \url{https://doi.org/10.1017/S0003055405051762}.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param stdshare Share of standard deviation of residuals distinguishing
#' between typical and deviant cases.
#'
#' @return A dataframe with the observed outcome, fitted outcome,
#' residual standard deviation and classification of cases as typical
#' or deviant.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' resid_std(df, stdshare = 1)
#'
#' @export
resid_std <- function(lmobject, stdshare = 1){
  if(class(lmobject) == "lm"){
    if(stdshare >= 0){
      # calculating standard deviation of residuals
      tempsd <- as.data.frame(suppressWarnings(predict.lm(df, se.fit = T)))
      # removing irrelevant columns
      tempsd <- tempsd[, c("fit", "residual.scale")]
      # extracing outcome values
      outcome <- df$model[, 1]
      # merging outcome values into dataframe
      comb <- cbind(tempsd, outcome)
      # classification of cases
      comb$status <- ifelse(comb$outcome < comb$fit-stdshare*comb$residual.scale|
                              comb$outcome > comb$fit+stdshare*comb$residual.scale,
                            "deviant", "typical")
      return(comb)
      }
    else{
      stop('Standard deviation should not be negative')
    }
  }
  else{
    stop('Input into function is not of class "lm"')
  }
}

