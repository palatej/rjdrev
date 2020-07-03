#' @include jd3_rslts.R jd3_ts.R
#' @import knitr
NULL



OlsNames<-c("N", "R2", "intercept.estimate", "intercept.stdev", "intercept.tstat", "intercept.pvalue",
            "slope.estimate", "slope.stdev", "slope.tstat", "slope.pvalue",
            "JarqueBera.value", "JarqueBera.pvalue",
            "BreuschPagan.value", "BreuschPagan.pvalue",
            "White.value", "White.pvalue")

readanalysis<-function(janalysis){
  n<-.jcall(janalysis, "I", "size")
  theil<-sapply(1:n, function(i){.jcall("demetra/revisions/r/Utility", "D", "theil", janalysis, as.integer(i))})
  ols<-matrix(ncol=length(OlsNames), nrow=n)
  for (i in 1:n){
    ols[i,]<-.jcall("demetra/revisions/r/Utility", "[D", "olsInformation", janalysis, as.integer(i))
  }
  ols<-as.data.frame(ols)
  ols<-`colnames<-`(ols, OlsNames)
  
  
  revisions<-list(theil=data.frame(theil=theil), ols=ols)
  
  return (structure(revisions,class="JD.Revisions.RegressionBasedAnalysis") )
}

#' Title
#'
#' @param rev 
#'
#' @return
#' @export
#'
#' @examples
print.JD.Revisions.RegressionBasedAnalysis<-function(rev, maxcols=10){
  o<-as.data.frame(t(as.matrix(rev$ols)))
  if (dim(o)[2]>maxcols) o<-o[,1:maxcols]
  print(knitr::kable(o, "pandoc", digits=3))
}



makerevisions<-function(jfac, selection.nrevisions, selection.start, selection.end, analysis.vertical){
  #creates the Vintages
  
  jvintages<-.jcall(jfac, "Ldemetra/revisions/r/Vintages;", "build")
  
  if (selection.nrevisions>0){
    janalysis<-.jcall(jvintages, "Ldemetra/revisions/parametric/RegressionBasedAnalysis;", "diagonalAnalysis", as.integer(0), as.integer(selection.nrevisions))
  }else if (! is.null(selection.start) && ! is.null(selection.end) ){
    if (inherits(selection.start, "Date") && inherits(selection.end, "Date")){
      janalysis<-.jcall(jvintages, "Ldemetra/revisions/parametric/RegressionBasedAnalysis;", "verticalAnalysis", as.character(selection.start), as.character(selection.end))
    }
    else{
      warning("Wrong registration period")
    }  
  }
  
  return(readanalysis(janalysis))
}


#' Title
#'
#' @param periodicity 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
revisions <- function(periodicity, input, selection.nrevisions=0, selection.firstVintage=NULL, selection.lastVintage=NULL) {
  
  jfac<-.jnew("demetra/revisions/r/VintagesFactory", as.integer(periodicity))
  
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
    .jcall(jfac, "V", "add", as.character(refdate), as.character(regdate), val)
    return (NULL)
  }
  
  lapply(input, FUN=addToRevisions)
  
  #creates the Vintages
  
  return(makerevisions(jfac, selection.nrevisions, selection.start, selection.end, analysis.vertical))
  
  
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
revisionsFromCsv<-function(file, periodicity, regDateFormat= "%Y.%m.%d", selection.nrevisions=0, selection.start=NULL, selection.end=NULL, analysis.vertical=T){
  z<-read.csv(file, stringsAsFactors = F)  

  todate<-function(x){
    x<-substr(x, 2, 11)
    return (as.Date(x, regDateFormat))
  }
  
  regdates<-lapply(colnames(z)[-1], todate)
  
  refdates<-lapply(z$time,yp)
  
  jfac<-.jnew("demetra/revisions/r/VintagesFactory", as.integer(periodicity))
  
  # Not optimal
  for (row in 1:length(refdates)){
    for (col in 1:length(regdates)){
      val<-z[row, col+1]
      if (! is.na(val)){
        .jcall(jfac, "V", "add", as.character(refdates[[row]]), as.character(regdates[[col]]), val)
      }
    }
  }
  
  
  return(makerevisions(jfac, selection.nrevisions, selection.start, selection.end, analysis.vertical))
  
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



#' Title
#'
#' @param file 
#' @param periodicity 
#' @param regDateFormat 
#'
#' @return
#' @export
#'
#' @examples
vintageTableFromCsv<-function(file, periodicity, regDateFormat= "%Y.%m.%d"){
  z<-read.csv(file, stringsAsFactors = F)  
  
  todate<-function(x){
    x<-substr(x, 2, 11)
    return (as.Date(x, regDateFormat))
  }
  
  regdates<-lapply(colnames(z)[-1], todate)
  
  refdates<-lapply(z$time,yp)
  
  jfac<-.jnew("demetra/revisions/r/VintagesFactory", as.integer(periodicity))
  
  # Not optimal
  for (row in 1:length(refdates)){
    for (col in 1:length(regdates)){
      val<-z[row, col+1]
      if (! is.na(val)){
        .jcall(jfac, "V", "add", as.character(refdates[[row]]), as.character(regdates[[col]]), val)
      }
    }
  }
  
  return (vintageTableFromFactory(jfac))
}

vintageTableFromFactory<-function(jfac){

  jvn<-.jcall(jfac, "Ldemetra/revisions/r/Vintages;", "build")
  jmat<-.jcall(jvn, "Ldemetra/revisions/timeseries/TsMatrix;", "vtable")
  data<-matrix_jd2r(.jcall(jmat, "Ldemetra/math/matrices/MatrixType;", "getMatrix"))
  cols<-.jcall(jmat, "[S", "getFields")
  jstart<-.jcall(jmat, "Ldemetra/timeseries/TsPeriod;", "getStart")
  pstart<-.jcall("demetra/timeseries/r/TsUtility", "[I", "of", jstart)
  
  data[is.nan(data)]<-NA
  tsm<-ts(data, frequency = pstart[1], start = pstart[-1])
  tsm<-`colnames<-`(tsm, cols)
  return (tsm)
}

#' Title
#'
#' @param tsm 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
verticalRevisions<-function(tsm, lag=1){
  q<-tsm
  q[is.na(q)]<-0
  n<-dim(q)[2]
  
  idx1<-(lag+1):n
  idx0<-1:(n-lag)
  
  rev<-q[,idx1]-q[,idx0]
  
  w<-sapply(colnames(tsm), function(s){paste0('[', s, ']')})
  rw<-mapply(function(a,b){paste(a,b,sep='-')}, w[idx1],w[idx0])
  
  rev<-`colnames<-`(rev, rw)
  
  jq<-matrix_r2jd(q)
  theil<-.jcall("demetra/revisions/r/Utility", "[D", "theil", jq)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "slopeAndDrift", jq)
  slopeAndDrift=matrix_jd2r(jsd)
  parametric<-list(theil=theil, slopeAndDrift=slopeAndDrift)

  return (structure(list(vintageTable=tsm, revisions=rev, parametric=parametric), class="JD3_RevisionAnalysis"))
}

