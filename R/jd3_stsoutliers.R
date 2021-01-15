#' @include jd3_ts.R jd3_rslts.R
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
  model<-list(
    y=as.numeric(y),
    variables=proc_vector(jsts, "variables"),
    X=proc_matrix(jsts, "regressors"),
    b=proc_vector(jsts, "b"),
    bcov=proc_matrix(jsts, "bvar"),
    components=proc_matrix(jsts, "cmps"),
    linearized=proc_vector(jsts, "linearized")
  )
  
  l0<-proc_numeric(jsts, "initialbsm.levelvar")
  s0<-proc_numeric(jsts, "initialbsm.slopevar")
  seas0<-proc_numeric(jsts, "initialbsm.seasvar")
  n0<-proc_numeric(jsts, "initialbsm.noisevar")
  tau0=proc_matrix(jsts, "initialtau")
  
  
  l1<-proc_numeric(jsts, "finalbsm.levelvar")
  s1<-proc_numeric(jsts, "finalbsm.slopevar")
  seas1<-proc_numeric(jsts, "finalbsm.seasvar")
  n1<-proc_numeric(jsts, "finalbsm.noisevar")
  tau1=proc_matrix(jsts, "finaltau")
  
  ll0<-proc_likelihood(jsts, "initiallikelihood.")
  ll1<-proc_likelihood(jsts, "finallikelihood.")
  
  return(structure(list(
    model=model,
    bsm=list(
      initial=list(
        level=l0,
        slope=s0,
        seasonal=seas0,
        noise=n0,
        tau=tau0
      ),
      final=list(
        level=l1,
        slope=s1,
        seasonal=seas1,
        noise=n1,
        tau=tau1
      )
    ),
    likelihood=list(initial=ll0, final=ll1)),
    class="JDSTS"))
}

#' Title
#'
#' @param clustering 
#' @param freq 
#' @param startyear 
#' @param startperiod 
#' @param len 
#'
#' @return
#' @export
#'
#' @examples
tradingdays<-function(clustering, freq, startyear, startperiod=1, len, contrasts=T){
  jdom<-.jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsDomain;", "of", as.integer(freq), 
               as.integer(startyear), as.integer(startperiod), as.integer(len))
  jtd<-.jcall("demetra/calendar/r/GenericCalendars", "Ldemetra/math/matrices/MatrixType;", "td", jdom, as.integer(clustering), as.logical(contrasts))
  return (matrix_jd2r(jtd))
}
