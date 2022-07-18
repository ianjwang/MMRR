#' MMMR
#'
#' MMRR performs Multiple Matrix Regression with Randomization analysis
#'
#' @param Y The dependent variable in the form of a distance matrix
#' @param X A list of independent variables in the form of distance matrices (with optional names)
#' @param nperm The number of permutations to perform for significance testing
#' @param scale If TRUE, all variables are scaled
#' @examples
#' Xmats <- list(geography = simData$geoMat, ecology = simData$ecoMat)
#' MMRR(simData$genMat, Xmats, nperm = 99)
#' @export
MMRR<-function(Y, X, nperm = 999, scale = TRUE){
  #compute regression coefficients and test statistics
  nrowsY <- nrow(Y)
  y <- unfold(Y, scale = scale)
  if(is.null(names(X))) names(X) <- paste("X", 1:length(X), sep="")
  Xmats <- sapply(X, unfold)
  fit <- lm(y~Xmats)
  coeffs <- fit$coefficients
  summ <- summary(fit)
  r.squared <- summ$r.squared
  tstat <- summ$coefficients[,"t value"]
  Fstat <- summ$fstatistic[1]
  tprob <- rep(1,length(tstat))
  Fprob <- 1

  #perform permutations
  for(i in 1:nperm){
    rand <- sample(1:nrowsY)
    Yperm <- Y[rand, rand]
    yperm <- unfold(Yperm)
    fit <- lm(yperm ~ Xmats)
    summ <- summary(fit)
    Fprob <- Fprob+as.numeric(summ$fstatistic[1] >= Fstat)
    tprob <- tprob+as.numeric(abs(summ$coefficients[,"t value"]) >= abs(tstat))
  }

  #return values
  tp <- tprob/(nperm+1)
  Fp <- Fprob/(nperm+1)
  names(r.squared) <- "r.squared"
  names(coeffs) <- c("Intercept",names(X))
  names(tstat) <- paste(c("Intercept",names(X)),"(t)",sep="")
  names(tp) <- paste(c("Intercept",names(X)),"(p)",sep="")
  names(Fstat) <- "F-statistic"
  names(Fp) <- "F p-value"
  return(list(r.squared=r.squared,
              coefficients=coeffs,
              tstatistic=tstat,
              tpvalue=tp,
              Fstatistic=Fstat,
              Fpvalue=Fp))
}
