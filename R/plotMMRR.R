#' Plot MMMR
#'
#' Plots the results of an MMRR analysis
#'
#' @param reg The fitted MMRR model
#' @param Y The dependent variable in the form of a distance matrix
#' @param X A list of independent variables in the form of distance matrices (with optional names)
#' @param scale If TRUE, all variables are scaled
#' @param varNames A vector of names for the variables in the model (optional)
#' @param lineCol Color for regression line
#' @examples
#' Xmats <- list(geography = simData$geoMat, ecology = simData$ecoMat)
#' reg <- MMRR(simData$genMat, Xmats, nperm = 99)
#' plotMMRR(reg, simData$genMat, Xmats, varNames = c("Gen. Dist.", "Geo. Dist.", "Env. Dist."))
#' @details
#' The objects supplied for Y and X should be the same variables used to fit the MMRR model.  The paramater 'scale' should be the same as used to fit the model.
#' The varNames argument can be used to specify variable names for labeling the plot axes.  The first name is for the dependent variable; additional names should be supplied in the same order as the independent variables.
#' @export
plotMMRR <- function(reg, Y, X, scale = TRUE, varNames = NULL, lineCol = "blue", ...){
  y <- unfold(Y, scale)
  if(length(varNames) > 0){
    name.Y <- varNames[1]
  } else {
    name.Y <- substitute(Y)
  }
  # Plot single variable relationships
  for(i in 1:length(X)){
    x <- unfold(X[[i]], scale = scale)
    if(length(varNames) >= i + 1){
      name.X <- varNames[i + 1]
    } else {
      name.X <- names(X)[i]
    }
    plot(x, y, ylab = name.Y, xlab = name.X, ...)
    lm.reg <- lm(y ~ x)
    abline(reg = lm.reg, col = lineCol)
  }
  # Plot fitted relationship
  x <- matrix(nrow=length(X), ncol = length(y))
  for(i in 1:length(X)){
    x[i,] <- unfold(X[[i]], scale)
    x[i,] <- reg$coefficients[i+1] * x[i,]
  }
  plot(colSums(x), y, ylab = substitute(Y), xlab = "Predicted Distance", ...)
  lm.reg <- lm(y ~ colSums(x))
  abline(reg = lm.reg, col = lineCol)
  # Plot covariances
  cmb <- combn(1:length(X), 2)
  for(i in 1:ncol(cmb)){
    x <- unfold(X[[cmb[1, i]]], scale = scale)
    y <- unfold(X[[cmb[2, i]]], scale = scale)
    if(length(varNames) > cmb[1, i]){
      name.x <- varNames[cmb[1, i] + 1]
    } else {
      name.x <- names(X[cmb[1, i]])
    }
    if(length(varNames) > cmb[2, i]){
      name.y <- varNames[cmb[2, i] + 1]
    } else {
      name.y <- names(X[cmb[2, i]])
    }
    plot(x, y, ylab = name.y, xlab = name.x, ...)
  }
}
