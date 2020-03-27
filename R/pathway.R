

#' Pathway case
#'
#' Calculation of pathway values, defined as the difference between residuals of
#' full model and reduced model lacking the pathway variable. The larger the
#' difference, the more a case qualifies as a pathway case suitable for the
#' analysis of mechanisms.
#'
#' The difference between the absolute residuals of the full and reduced model
#' follows the approach developed by Weller and Barnes (2014): \emph{Finding
#' Pathways: Mixed-Method Research for Studying Causal Mechanisms.}
#' Cambridge: Cambridge University Press.
#' \url{https://doi.org/10.1017/CBO9781139644501}).
#'
#' The calculation of the absolute difference between the full and reduced model
#' if the reduced-model residual is larger than the full-model residual follows
#' the original proposal by Gerring (2007): Is There a (Viable)
#' Crucial-Case Method? \emph{Comparative Political Studies} 40 (3): 231-253.
#' \url{https://journals.sagepub.com/doi/10.1177/0010414006290784})
#'
#' @param full.model Full model including covariate of interest
#' (=pathway variable)
#' @param reduced.model Reduced model excluding covariate of interest
#'
#' @return A dataframe with
#'
#' - all full model variables,
#'
#' - full model residuals (\code{full.resid}),
#'
#' - reduced model residuals (\code{reduced.resid}),
#'
#' - pathway values following Weller/Barnes (\code{pathway.wb}),
#'
#' - pathway values following Gerring (\code{pathway.gvalue}),
#'
#' - variable showing whether Gerring's criterion for a pathway
#' case is met (\code{pathway.gstatus})
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway(df_full, df_reduced)
#'
#' @export
pathway <- function(full_model, reduced_model) {
  if (class(full_model) == "lm") {
    if (class(reduced_model) == "lm") {
      # full model
      full.resid <- residuals(full_model)
      # reduced model
      reduced.resid <- residuals(reduced_model)
      # difference between absolute residuals
      pathway.wb <- abs(reduced.resid) - abs(full.resid)
      # absolute difference between residuals
      pathway.gvalue <- abs(reduced.resid - full.resid)
      # check for Gerring's criterion for pathway values
      pathway.gtype <- ifelse(reduced.resid > full.resid, "yes", "no")
      comb <- cbind(full_model$model, full.resid, reduced.resid,
                    pathway.wb, pathway.gvalue, pathway.gtype)
      return(comb)
    }
    else{
      stop("Reduced model object is not of class lm")
    }
  }
  else{
    (stop("Full model object is not of class lm"))
  }
}

#' Plot of residuals against pathway variable
#'
#' @param full.model Full model including covariate of interest
#' (=pathway variable)
#' @param reduced.model Reduced model excluding covariate of interest
#' @param pathway.var Pathway variable dropped from full model
#' @param pathway.type Type of pathway values. \code{pathway.wb} are
#' pathway values proposed by Weller and Barnes. \code{pathway.g} are values
#' as calculated by Gerring.
#'
#' @return A plot of the chosen type of pathway values against the pathway
#' variable created with \code{\link{ggplot2}}.
#'
#' @examples
#' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway_xvr(full_model, reduced_model, pathway_var = "disp",
#' pathway_type = "pathway.wb")
#'
#' @export
pathway_xvr <- function(full_model, reduced_model,
                        pathway_var = "variable", pathway_type = "residual") {
  pwdf <- pathway(df_full, df_reduced)
  if (pathway_type == "pathway.wb") {
    pwplot <- ggplot2::ggplot() +
      geom_point(data = pwdf, mapping = aes_string(x = pathway_var,
                                                   y = pathway_type)) +
      geom_hline(yintercept = 0, linetype = 5) +
      scale_y_continuous("Pathway values") +
      theme_classic() -> pwplot
  }
  else{
    pwplot <- ggplot2::ggplot() +
      geom_point(data = pwdf, mapping = aes_string(x = pathway_var,
                                                   y = pathway_type,
                                                   color = "pathway.gtype")) +
      geom_hline(yintercept = 0, linetype = 5) +
      scale_y_continuous("Pathway values") +
      scale_color_viridis_d("Reduced > full residuals") +
      theme_classic() +
      theme(legend.position = "bottom") -> pwplot
  }
  return(pwplot)
}
