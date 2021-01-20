#' @include jd3_ts.R util.R
#' @checkmate
NULL

#' Title
#'
#' @param y 
#' @param level 
#' @param slope 
#' @param noise 
#' @param seasonal 
#' @param X 
#' @param X.td 
#' @param ao 
#' @param ls 
#' @param so 
#' @param cv 
#' @param tcv 
#' @param estimation.forward 
#' @param estimation.backward 
#'
#' @return
#' @export
#'
#' @examples
stsoutliers<-function(y, level=1, slope=1, noise=1, seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused"),
              X=NULL, X.td=NULL, ao=T, ls=T, so=F, 
              cv=0, tcv=0, estimation.forward=c("Score", "Point", "Full"), 
              estimation.backward=c("Point", "Score", "Full")){
  
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  seasonal<-match.arg(seasonal)
  estimation.forward<-match.arg(estimation.forward)
  estimation.backward<-match.arg(estimation.backward)
  
  
  if (! is.null(X.td)){
    sy<-start(y)
    td<-tradingdays(X.td, frequency(y), sy[1], sy[2], length(y))
    X<-cbind(X, td)
  }
      
  
  jsts<-.jcall("demetra/sts/r/StsOutliersDetection", "Ldemetra/sts/r/StsOutliersDetection$Results;", "process", ts_r2jd(y), 
              as.integer(level), as.integer(slope), as.integer(noise), seasonal, matrix_r2jd(X),
              ao, ls, so, cv, tcv, estimation.forward, estimation.backward)
  
  q<-.jcall(jsts, "[B", "buffer")
  p<-RProtoBuf::read(outliers.StsSolution, q)
  
  cov<-p2r_matrix(p$covariance) 
  
  return (structure(list(
    outliers=p2r_outliers(p$outliers),
    variables=p2r_x(p, cov),
    initialbsm=p$bsm_initial,
    finalbsm=p$bsm_final,
    initiallikelihood=p2r_likelihood(p$likelihood_initial),
    finallikelihood=p2r_likelihood(p$likelihood_final),
    coefficients=p$coefficients,
    covariance=p2r_matrix(p$covariance), 
    components=p2r_matrix(p$components),
    initialtau=p2r_matrix(p$tau_initial),
    finaltau=p2r_matrix(p$tau_final)
   ), class = "JD3STSOUTLIERS"))
  
}

