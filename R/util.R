identical_na <- function(x){
  identical(x, NA) ||
    identical(x, NA_character_) ||
    identical(x, NA_complex_) ||
    identical(x, NA_integer_) ||
    identical(x, NA_real_) ||
    identical(x, NaN)
}


p2r_likelihood<-function(p){
  return (list(ll=p$log_likelihood,
               aic=p$aic, bic=p$bic, ssq=p$ssq))
}

p2r_matrix<-function(p){
  m<-matrix(data=p$values, nrow = p$nrows, ncol = p$ncols)
  `attr<-`(m, "name", p$name)
  return (m)
}

p2r_outliers<-function(p){
  if (length(p) == 0)
    return (NULL)
  codes<-sapply(p, function(o){o$code})
  pos<-sapply(p, function(o){o$position})
  vals<-sapply(p, function(o){o$coefficient})
  err<-sapply(p, function(o){o$stde})
  tstat<-vals/err 
  
  return (data.frame(code=codes, pos=1+pos, val=vals, err=err,tstat=tstat))
}

p2r_x<-function(p, cov){
  c<-p$coefficients
  nc=length(c)
  if (nc == 0)
    return (NULL)
  no<-length(p$outliers)
  if (nc == no)
    return (NULL);
  nx<-nc-no
  idx<-1:nx
  vars<-paste("x", idx, sep="-")
  vals<-p$coefficients[idx]
  err<-sqrt(diag(cov)[idx])
  tstat<-vals/err 
  
  return (data.frame(var=vars, val=vals, err=err,tstat=tstat))
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
