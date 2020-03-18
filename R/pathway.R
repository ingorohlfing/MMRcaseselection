

#' Pathway case
#'
#' Calculation of difference between residuals of full model and reduced
#' model lacking the focal variable. The larger the difference, the more
#' a case qualifies as a pathway case suitable for the analysis of
#' mechanisms.
#'
#' The plain difference follows the approach developed by Weller and Barnes
#' (Weller, Nicholas and Jeb Barnes (2014): \emph{Finding Pathways: Mixed-Method
#' Research for Studying Causal Mechanisms.} Cambridge: Cambridge University
#' Press. \url{https://doi.org/10.1017/CBO9781139644501}).
#'
#' The calculation of the absolute residual if the reduced-model
#' residual is larger than the full-model residual follows the original
#' proposal by Gerring (Gerring, John (2007): Is There a (Viable)
#' Crucial-Case Method? \emph{Comparative Political Studies} 40 (3): 231-253.
#' \url{https://journals.sagepub.com/doi/10.1177/0010414006290784})
#'
#' @param full.model Full model including covariate of interest
#' @param reduced.model Reduced model excluding covariate of interest
#'
#' @return A dataframe with pathway case residuals (pathway values) for
#' both definitions of pathway values.
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
      pathway.wb <- abs(reduced.resid)-abs(full.resid) # difference
      pathway.gvalue <- abs(reduced.resid-full.resid)
      pathway.gtype <- ifelse(reduced.resid > full.resid, "yes", "no")
      comb <- cbind(full.model$model, full.resid, reduced.resid,
                    pathway.wb, pathway.g)
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

#' Plot of residuals against pathway variable
#'
#' @param full.model Full model including covariate of interest
#' @param reduced.model Reduced model excluding covariate of interest
#' @param pathway.var Variable that is dropped from full model
#' @param pathway.type Type of pathway residual. \code{pathway.wb} are
#' residuals proposed by Weller and Barnes. \code{pathway.g} are residuals
#' as calculated by Gerring.
#'
#' @return A plot of the chosen type of residuals against the pathway variable
#' created with \code{\link{ggplot2}}.
#'
#' @examples
#' #' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway_xvr(full.model, reduced.model, pathway.var = "disp",
#' pathway.type = "pathway.wb")
#'
#' @export
pathway_xvr <- function(full.model, reduced.model,
                        pathway.var = "variable", pathway.type = "residual"){
  pwdf <- pathway(df_full, df_reduced)
  pwplot <- ggplot2::ggplot() +
    geom_point(data = pwdf, mapping = aes_string(x = pathway.var, y = pathway.type)) +
    geom_hline(yintercept = 0, linetype = 5) +
    scale_y_continuous("Residuals") +
    theme_classic() -> pwplot
  return(pwplot)
}
