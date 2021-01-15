#' @include jd3_ts.R jd3_rslts.R
#' @checkmate
NULL

#' Title
#'
#' @param y 
#' @param level -1 = no level, 0 = fixed level, 1 = sotchastic level
#' @param slope 
#' @param noise 
#' @param seasonal Seasonal model
#' @param X Regression variables (same length as y) or NULL
#' @param X.td Specification for trading days clustering. 
#' Contains thr group id fo Mondays... Sundays 
#' (for instance (1,1,1,1,1,0,0) for week days or (1,1,1,1,1,2,0) for week.saturdays/Sundays variables).
#' Contrasts are used. Can be NULL 
#'
#' @return
#' @export
#'
#' @examples
seasonalbreaks<-function(y, level=1, slope=1, noise=1, seasonal=c("HarrisonStevens", "Trigonometric", "Dummy", "Crude", "Fixed", "Unused"),
                       X=NULL,X.td=NULL){
  
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  seasonal<-match.arg(seasonal)

  
  if (! is.null(X.td)){
    sy<-start(y)
    td<-tradingdays(X.td, frequency(y), sy[1], sy[2], length(y))
    X<-cbind(X, td)
  }

  so<-.jcall("demetra/sts/r/StsOutliersDetection", "[D", "seasonalBreaks", ts_r2jd(y), 
               as.integer(level), as.integer(slope), as.integer(noise), seasonal, matrix_r2jd(X))
  
  return (ts(so, frequency = frequency(y), start=start(y)))
}
  