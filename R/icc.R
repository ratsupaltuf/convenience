
#' Calculate ICC
#'
#' @param m A random effects model of class merMod
#' @return Returns the intraclass correlation coefficient at each level
#' @importFrom lme4 VarCorr
#' @importFrom dplyr tibble
#' @importFrom MuMIn r.squaredGLMM
#' @export
#'
#' @note
#' adapted from from
#' https://gist.github.com/benwhalley/1297dc3b1d616de41837ca015f338b10
#'
#' @examples
#' icc(m)
icc <- function(m){
  vc <- as.data.frame((VarCorr(m)))
  l <- vc$vcov
  tibble(grp=vc$grp, icc=sapply(l, function(x){x/sum(l)}))
}

#' Extract and compare variance components of lmer models
#'
#' @param Model A multilevel model of class merMod
#' @param Nullmodel A multilevel model of class merMod \code{Model} is compared to.
#' Typically the Nullmodel is the intercept-only model.
#'
#' @return Returns the variance reduction in achieved by \code{Model} compared to \code{Nullmodel} in percent.
#' @importFrom lme4 VarCorr
#' @importFrom dplyr tibble
#' @importFrom MuMIn r.squaredGLMM
#'
#' @note
#' The variance explained at each level is a nice way to compare the relevance
#' of predictors in multilevel models. However, this function needs to be
#' updated in order to be more generally applicable
#'
#' @export
#'
#' @examples vcs(m1, m0)
vcs <- function(Model, Nullmodel=M0) {
  vc <- as.data.frame((VarCorr(Nullmodel)))
  l <- vc$vcov
  baseline <- tibble(grp=vc$grp, var=l)
  vc <- as.data.frame((VarCorr(Model)))
  l <- vc$vcov
  comparison <- tibble(grp=vc$grp, var=l)
  iccs <- cbind(baseline,comparison[,2])
  names(iccs) <- c("Level", "Nullmodel", "Comparison_Model")
  iccs <- iccs %>% mutate(Variance_Change = Comparison_Model-Nullmodel,
                          PCV = (Variance_Change/Nullmodel)*100,
                          PCV = round(PCV, 2)
  )   %>% select(Level, PCV)
  m <- c("Marginal Rsq", round(r.squaredGLMM(Model)[1],3))
  c <- c("Conditional Rsq", round(r.squaredGLMM(Model)[2],3))
  df <- rbind(iccs,m,c)
  names(df) <- c("Level", "Model")
  df$Level[1:3] <- paste("\\% Change Variance", df$Level[1:3])
  df$Level[4:5] <- c("Marginal $R^2$", "Conditional $R^2$")
  return(df)
}
