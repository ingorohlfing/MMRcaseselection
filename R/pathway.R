

#' Pathway case
#'
#' @param full.model Full model including covariate of interest
#' @param reduced.model Reduced model excluding covariate of interest
#'
#' @return A dataframe with pathway case residuals.
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
