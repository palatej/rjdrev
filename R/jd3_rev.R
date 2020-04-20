#' @include jd3_procresults.R jd3_rslts.R jd3_ts.R
NULL

#' Title
#'
#' @param periodicity 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
revisions <- function(periodicity, input) {
  
  jrev<-.jnew("demetra/revisions/r/VintagesFactory", as.integer(periodicity))
  
  allowedRevArgs <- c("referencePeriod","registrationDate","value")
  
  addToRevisions <- function(x) {
    # check item and add to JD3 Revisions
    if (is.list(x)) {
      if (length(x)!=3) {
        warning("Wrong element")
        return(NULL)
      }
    }
    refdate<-x[[1]]
    if (! inherits(refdate, "Date")){
      warning("Wrong reference period")
      return(NULL)
    }
    regdate<-x[[2]]
    if (! inherits(regdate, "Date")){
      warning("Wrong registration period")
      return(NULL)
    }
    val<-x[[3]]
    if ( !is.numeric(val)){
      warning("Wrong value")
      return(NULL)
    }
    .jcall(jrev, "V", "add", as.character(refdate), as.character(regdate), val)
    return (NULL)
  }
  
  lapply(input, FUN=addToRevisions)
  
  preliminary<- ts_jd2r(.jcall(jrev,  "Ldemetra/timeseries/TsData;", "preliminary"))
  current<- ts_jd2r(.jcall(jrev,  "Ldemetra/timeseries/TsData;", "current"))

    return(structure(list(
    internal=jrev,
    preliminary=preliminary,
    current=current),
    class="JDRevisions"))
  
}

ymd<-function(y, m, d=1){
  return (as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}

yq<-function(y, q){
  return (as.Date(sprintf("%04i-%02i-%02i", y, q*3-2, 1)))
}

yp<-function(s){
  y<-as.integer(substr(s, 1, 4))
  p=substr(s,5,5)
  if (p == 'Q' || p == 'q'){
    q<-as.integer(substr(s, 6, 6))
    return (yq(y,q))
  }
  if (p == 'M' || p == 'm'){
    m<-as.integer(substr(s, 6, length(s)))
    return (ymd(y,m))
  }
  return (NULL)
}


#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
revisionsFromCsv<-function(file, periodicity, regDateFormat= "%Y.%m.%d"){
  z<-read.csv(file, stringsAsFactors = F)  

  todate<-function(x){
    x<-substr(x, 2, 11)
    return (as.Date(x, regDateFormat))
  }
  
  regdates<-lapply(colnames(z)[-1], todate)
  
  refdates<-lapply(z$time,yp)
  
  jrev<-.jnew("demetra/revisions/r/VintagesFactory", as.integer(periodicity))
  
  # Not optimal
  for (row in 1:length(refdates)){
    for (col in 1:length(regdates)){
      val<-z[row, col+1]
      if (! is.na(val)){
        .jcall(jrev, "V", "add", as.character(refdates[[row]]), as.character(regdates[[col]]), val)
      }
    }
  }
  
  preliminary<- ts_jd2r(.jcall(jrev,  "Ldemetra/timeseries/TsData;", "preliminary"))
  current<- ts_jd2r(.jcall(jrev,  "Ldemetra/timeseries/TsData;", "current"))

    return(structure(list(
    internal=jrev,
    preliminary=preliminary,
    current=current),
    class="JD.Revisions"))
}


#' Title
#'
#' @param revisions 
#' @param nrevs 
#'
#' @return
#' @export
#'
#' @examples
regressionAnalysis<-function(revisions, nrevs=3){
  if (class(revisions) != "JD.Revisions"){
    warning("Invalid argument. Should be a revisions object")
    return(NULL)
  }

  jrevanalysis<-.jcall(revisions$internal, "Ldemetra/revisions/parametric/RegressionBasedAnalysis;", 
                       "regressionBasedAnalysis", as.integer(nrevs) )
  
  
  jbias<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "biasInformation", jrevanalysis)
  bias<-matrix_jd2r(jbias)
  fbias<-as.data.frame(bias)
  fbias<-`colnames<-`(fbias, c("N", "mu", "stdev", "T", "p-value", "ar-parameter", "adj. stdev", "adj. T", "adj. p-value"))
  nr<-dim(bias)[1]
  rn<-array(nr)
  rn[1]<-"Current"
  for (j in 2:nr){
    rn[j]<-paste0("Rev-", j)
  }
  fibais<-`rownames<-`(fbias, rn)
  
  return(structure(
    list(bias=fbias),
    class="JD.Revisions.RegressionBasedAnalysis"))
}


