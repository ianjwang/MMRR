#' Plot MMMR
#'
#' Plots the results of an MMRR analysis
#'
#' @param reg The fitted MMRR model
#' @param Y The dependent variable in the form of a distance matrix
#' @param X A list of independent variables in the form of distance matrices (with optional names)
#' @param scale If TRUE, all variables are scaled
#' @param lineCol Color for regression line
#' @examples
#' Xmats <- list(geography = simData$geoMat, ecology = simData$ecoMat)
#' reg <- MMRR(simData$genMat, Xmats, nperm = 99)
#' plotMMRR(reg, simData$genMat, Xmats)
#' @details
#' The objects supplied for Y and X should be the same variables used to fit the MMRR model.  The paramter 'scale' should be the same as used to fit the model.
#' @export
plotMMRR <- function(reg, Y, X, scale = TRUE, lineCol = "blue"){
  y <- unfold(Y, scale)
  x <- matrix(nrow=length(X), ncol = length(y))
  for(i in 1:length(X)){
    x[i,] <- unfold(X[[i]], scale)
    x[i,] <- reg$coefficients[i+1] * x[i,]
  }
  xvars <- paste0("x[[", 1:length(X), "]]")
  lm.reg <- lm(as.formula(paste("y ~ ", paste(xvars, collapse = "+"))))
  plot(y, colSums(x))
  abline(reg = lm.reg, col = "blue")
}
