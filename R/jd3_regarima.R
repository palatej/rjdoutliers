#' @include jd3_ts.R jd3_rslts.R
#' @checkmate
NULL

#' Title
#'
#' @param y 
#' @param order 
#' @param seasonal 
#' @param mean 
#' @param X 
#' @param X.td 
#' @param ao 
#' @param ls 
#' @param so 
#' @param tc 
#' @param cv 
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
  model<-list(
    y=as.numeric(y),
    variables=proc_vector(jregarima, "variables"),
    X=proc_matrix(jregarima, "regressors"),
    b=proc_vector(jregarima, "b"),
    bcov=proc_matrix(jregarima, "bvar"),
    linearized=proc_vector(jregarima, "linearized")
  )
  
  ll0<-proc_likelihood(jregarima, "initiallikelihood.")
  ll1<-proc_likelihood(jregarima, "finallikelihood.")
  
  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}
