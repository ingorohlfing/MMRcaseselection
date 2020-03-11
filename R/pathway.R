

#' Pathway case
#'
#' Calculation of difference between residuals of full model and reduced
#' model lacking the focal variable. The larger the difference, the more
#' a case qualifies as a pathway case suitable for the analysis of
#' mechanisms.
#'
#' The plain difference follows the approach developed by Weller and Barnes
#' (Weller, Nicholas and Jeb Barnes (2014): *Finding Pathways: Mixed-Method
#' Research for Studying Causal Mechanisms.* Cambridge: Cambridge University
#' Press. \url{https://doi.org/10.1017/CBO9781139644501}).
#'
#' The calculation of the absolute residual if the reduced-model
#' residual is larger than the full-model residual follows the original
#' proposal by Gerring (Gerring, John (2007): Is There a (Viable)
#' Crucial-Case Method? *Comparative Political Studies* 40 (3): 231-253.
#' \url{https://journals.sagepub.com/doi/10.1177/0010414006290784})
#'
#' @param full.model Full model including covariate of interest
#' @param reduced.model Reduced model excluding covariate of interest
#'
#' @return A dataframe with pathway case residuals. The value for
#' \code{pathway.abs} is \code{NA} if the full-model residual is equal
#' to or larger than the reduced-model residual.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway(df_full, df_reduced)
#'
#' @export
pathway <- function(full.model, reduced.model){
  if(class(full.model) == "lm"){
    if(class(reduced.model) == "lm"){
      full.resid <- residuals(full.model) # full model
      reduced.resid <- residuals(reduced.model) # reduced model
      pathway <- full.resid-reduced.resid # difference
      pathway.abs <- ifelse(reduced.resid > full.resid,
                            abs(reduced.resid-full.resid),
                            NA)
      comb <- cbind(full.model$model, full.resid, reduced.resid,
                    pathway, pathway.abs)
      #comb <- comb[order(pathway.resid), ] # descending order
      return(comb)
    }
    else{
      stop('Reduced model object is not of class "lm"')
    }
  }
  else{
    (stop('Full model object is not of class "lm"'))
  }
}
