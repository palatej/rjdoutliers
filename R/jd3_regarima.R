#' @include jd3_ts.R jd3_rslts.R
#' @checkmate
NULL

#' Title
#'
#' @param y Series
#' @param order Regular orders (p,d,q)
#' @param seasonal Seasonal orders (bp, bd, bq)
#' @param mean Mean correction
#' @param X Regression variables
#' @param X.td Trading days ( groups of days: for instance X.td=c(1,1,1,1,1,2,0))
#' @param ao Detection of additive outliers
#' @param ls Detection of level shifts
#' @param so Detection of seasonal outliers
#' @param tc Detection of transitory changes
#' @param cv Critical value 
#'
#' @return
#' @export
#'
#' @examples
regarimaoutliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                        X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    sy<-start(y)
    td<-tradingdays(X.td, frequency(y), sy[1], sy[2], length(y))
    X<-cbind(X, td)
  }
  jregarima<-.jcall("demetra/x13/r/RegArimaOutliersDetection", "Ldemetra/x13/r/RegArimaOutliersDetection$Results;", "process", ts_r2jd(y), 
                 as.integer(order), as.integer(seasonal), mean, matrix_r2jd(X),
                 ao, ls, tc, so, cv)
  
  q<-.jcall(jregarima, "[B", "buffer")
  p<-RProtoBuf::read(outliers.RegArimaSolution, q)
  
  cov<-p2r_matrix(p$covariance) 
  
  return (structure(list(
    outliers=p2r_outliers(p$outliers),
    variables=p2r_x(p, cov),
    initialarima=p$arima_initial,
    finalarima=p$arima_final,
    initiallikelihood=p2r_likelihood(p$likelihood_initial),
    finallikelihood=p2r_likelihood(p$likelihood_final),
    coefficients=p$coefficients,
    covariance=p2r_matrix(p$covariance) 
  ), class = "JD3REGARIMAOUTLIERS"))

}

