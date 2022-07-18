#' Unfold matrix
#'
#' Converts the lower diagonal elements of a matrix into a vector
#'
#' @param X A matrix
#' @param scale If TRUE, the resulting vector is scaled and centered
#' @example
#' unfold(simData$geoMat)
#' @export
unfold<-function(X, scale = TRUE){
  x<-vector()
  for(i in 2:nrow(X)) x <- c(x, X[i, 1:i-1])
  if(scale == TRUE) x <- scale(x, center = TRUE, scale = TRUE)
  return(x)
}
