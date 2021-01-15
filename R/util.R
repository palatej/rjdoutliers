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