#' @include jd3_ts.R jd3_rslts.R util.R
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
#' @param ml Use of maximum likelihood (otherwise approximation by means of Hannan-Rissanen)
#'
#' @return
#' @export
#'
#' @examples
tramooutliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                      X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0, ml=F){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    sy<-start(y)
    td<-tradingdays(X.td, frequency(y), sy[1], sy[2], length(y))
    X<-cbind(X, td)
  }
  
  
  jtramo<-.jcall("demetra/tramoseats/r/TramoOutliersDetection", "Ldemetra/tramoseats/r/TramoOutliersDetection$Results;", "process", ts_r2jd(y), 
               as.integer(order), as.integer(seasonal), mean, matrix_r2jd(X),
               ao, ls, tc, so, cv, ml)
  
  q<-.jcall(jtramo, "[B", "buffer")
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
                         covariance=cov
                    ), class = "JD3REGARIMAOUTLIERS"))
}

terror_names<-c("actual", "forecast", "error", "rel. error", "raw", "fraw", "efraw")
forecast_names<-c("forecast", "error", "fraw", "efraw")

#' Title
#'
#' @param ts
#' @param spec
#' @param nback
#'
#' @return
#' @export
#'
#' @examples
terror<-function(ts, spec, nback=1){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/MatrixType;", "process", jts, spec, as.integer(nback))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Terror", "Ldemetra/math/matrices/MatrixType;", "process", jts, jspec, jcontext, as.integer(nback))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-matrix_jd2r(jrslt)
    colnames(rslt)<-terror_names
    return (rslt)
  }
}

#' Title
#'
#' @param ts
#' @param spec
#' @param nf
#'
#' @return
#' @export
#'
#' @examples
forecast<-function(ts, spec="trfull", nf=-1){
  # TODO : check parameters
  jts<-ts_r2jd(ts)
  if (nf<0) nf<-frequency(ts)*(-nf)
  
  if (is.character(spec)){
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/MatrixType;", "forecast", jts, spec, as.integer(nf))
  }else{
    jspec<-r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext<-.jnull("demetra/util/r/Dictionary")
    }
    jrslt<-.jcall("demetra/tramoseats/r/Tramo", "Ldemetra/math/matrices/MatrixType;", "forecast", jts, jspec, jcontext, as.integer(nf))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-matrix_jd2r(jrslt)
    colnames(rslt)<-forecast_names
    return (rslt)
  }
}
