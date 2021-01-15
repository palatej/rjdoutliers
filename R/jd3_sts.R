#' @include jd3_ts.R jd3_rslts.R
#' @checkmate
NULL

#' Structural time series
#'
#' @param y 
#' @param level 
#' @param slope 
#' @param cycle 
#' @param noise 
#' @param seasonal 
#'
#' @return
#' @export
#'
#' @examples
sts<-function(y, level=1, slope=1, cycle=-1, noise=1
              , seasonal=c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused")){
  
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  seasonal<-match.arg(seasonal)
  jsts<-.jcall("demetra/sts/r/StsEstimation", "Ldemetra/sts/r/StsEstimation$Results;", "process", ts_r2jd(y), 
              as.integer(level), as.integer(slope), as.integer(cycle), as.integer(noise), seasonal)
  l<-proc_numeric(jsts, "levelvar")
  s<-proc_numeric(jsts, "slopevar")
  seas<-proc_numeric(jsts, "seasvar")
  n<-proc_numeric(jsts, "noisevar")
  #  c<-proc_numeric(jts, "cyclevar")
  #  cfactor<-proc_numeric(jts, "cycledumpingfactor")
  #  clength<-proc_numeric(jts, "cycleLength")
  model<-list(
    level=l,
    slope=s,
    seasonal=seas,
    noise=n
  )
  estimation<-list(
    score=proc_vector(jsts, "score"),
    covariance=proc_matrix(jsts, "pcov")
  )
  likelihood<-proc_likelihood(jsts, "likelihood.")
  
  return(structure(list(
    model=model,
    estimation=estimation,
    likelihood=likelihood),
    class="JDSTS"))
}


#' Title
#'
#' @param m 
#'
#' @return
#' @export
#'
#' @examples
print.JDSTS<-function(m){
  cat("Structural time series", "\n\n")
  cat("Variances:\n")
  s<-m$model$level
  if (! is.na(s) && s >=0) cat("level: ", format(round(s, 6), scientific = FALSE), "\n")
  s<-m$model$slope
  if (! is.na(s) && s >=0) cat("slope: ", format(round(s, 6), scientific = FALSE), "\n")
  s<-m$model$seas
  if (! is.na(s) && s >=0) cat("seas: ", format(round(s, 6), scientific = FALSE), "\n")
  s<-m$model$n
  if (! is.na(s) && s >=0) cat("noise: ", format(round(s, 6), scientific = FALSE), "\n\n")
  s<-m$likelihood$ll
  cat("LogLikelihood: ", format(round(s, 5), scientific = FALSE), "\n")
  s<-m$estimation$score
  cat("Scores: ", format(round(s, 5), scientific = FALSE), "\n")
  
  #      ll<-proc_numeric(object@internal,"likelihood.ll")
  #      cat("Log likelihood = ", format(round(ll, 4), scientific = FALSE), "\n")
}