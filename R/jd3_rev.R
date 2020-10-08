#' @include jd3_rslts.R jd3_ts.R
#' @import knitr
NULL

biasNames<-c("N", "estimate", "stderr", "tstat", "pvalue", "ar(1)", "stderr.adjusted", "tstat.adjusted", "pvalue.adjusted")

OlsNames<-c("N", "R2", "F", "intercept.estimate", "intercept.stderr", "intercept.pvalue",
            "slope.estimate", "slope.stderr", "slope.pvalue",
            "skewness", "kurtosis", "JarqueBera.value", "JarqueBera.pvalue", 
            "BreuschPagan.R2", "BreuschPagan.value", "BreuschPagan.pvalue",
            "White.R2", "White.value", "White.pvalue", 
            "arch.R2", "arch.value", "arch.pvalue")
            

OlsTestNames<-c(
              "skewness", "kurtosis", "JarqueBera.value", "JarqueBera.pvalue", 
            "BreuschPagan.R2", "BreuschPagan.value", "BreuschPagan.pvalue",
            "White.R2", "White.value", "White.pvalue", 
            "arch.R2", "arch.value", "arch.pvalue")

OlsAdjNames<-c("N", "R2", "F")

acNames<-c("BreuschGodfrey.R2", "BreuschGodfrey.value", "BreuschGodfrey.pvalue", 
           "LungBox.value", "LungBox.pvalue")
            
urNames<-c("DF.value", "DF.stderr", "DF.statistic", "DF.pvalue",
           "ADF.value", "ADF.stderr", "ADF.statistic", "ADF.pvalue",
           "DFCT.value", "DFCT.stderr", "DFCT.statistic", "DFCT.pvalue",
           "PP.value", "PP.stderr", "PP.statistic", "PP.pvalue"
           )

egNames<-c("value", "stderr", "statistic", "pvalue")

vecmNames<-c("trace(2)", "trace(1)", "max(2)", "max(1)")

snNames<-c("News.R2", "News.F", "News.pvalue", "Noise.R2", "Noise.F", "Noise.pvalue")


OlsCNames<-function(nregs){
  n<-c("intercept.estimate", "intercept.stderr", "intercept.pvalue") 
  for (i in 1:nregs){
    cur<-paste0("x(", i, ")")
    n<-c(n, paste0(cur, ".estimate"), paste0(cur, ".stderr"), paste0(cur, ".pvalue")) 
  }
  return (n)
} 

OlsAllNames<-function(nregs){
  return (c(OlsAdjNames, OlsCNames(nregs), OlsTestNames))
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
vintages<- function(periodicity, input) {
  
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
  
  return (vintageTableFromFactory(jfac))
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
verticalRevisions<-function(tsm, gap=1){
  q<-tsm
  q[is.na(q)]<-0
  n<-dim(q)[2]
  
  idx1<-(gap+1):n
  idx0<-1:(n-gap)
  
  rev<-q[,idx1]-q[,idx0]
  
  w<-sapply(colnames(tsm), function(s){paste0('[', s, ']')})
  rw<-mapply(function(a,b){paste(a,b,sep='-')}, w[idx1],w[idx0])
  
  rev<-`colnames<-`(rev, rw)
  
  jq<-matrix_r2jd(q)
  jrev<-matrix_r2jd(rev)
  theil<-.jcall("demetra/revisions/r/Utility", "[D", "theil", jq, as.integer(gap))
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "slopeAndDrift", jq, as.integer(gap))
  slopeAndDrift=matrix_jd2r(jsd)
  jbias<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "bias", jrev)
  bias=matrix_jd2r(jbias)
  jef1<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "efficiencyModel1", jq, as.integer(gap))
  slopeAndDrift=matrix_jd2r(jsd)
  jef2<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "efficiencyModel2", jq, as.integer(gap))
  slopeAndDrift=matrix_jd2r(jsd)
  efficiencyModel1=matrix_jd2r(jef1)
  efficiencyModel2=matrix_jd2r(jef2)
  parametric<-list(theil=theil, 
                   bias=`colnames<-`(bias, biasNames),
                   slopeAndDrift=`colnames<-`(slopeAndDrift, OlsNames), 
                   efficiencyModel1=`colnames<-`(efficiencyModel1, OlsNames),
                   efficiencyModel2=`colnames<-`(efficiencyModel2, OlsNames))

  return (structure(list(vintageTable=tsm, revisions=rev, parametric=parametric), class="JD3_RevisionAnalysis"))
}

#' Title
#'
#' @param vintages 
#' @param gap 
#'
#' @return
#' @export
#'
#' @examples
revisions<-function(vintages, gap=1){
  q<-vintages
  q[is.na(q)]<-0
  n<-dim(q)[2]
  
  idx1<-(gap+1):n
  idx0<-1:(n-gap)
  
  rev<-q[,idx1]-q[,idx0]
  
  w<-sapply(colnames(tsm), function(s){paste0('[', s, ']')})
  rw<-mapply(function(a,b){paste(a,b,sep='-')}, w[idx1],w[idx0])
  
  rev<-`colnames<-`(rev, rw)
  return (rev)
}
  
#' Title
#'
#' @param vintages 
#'
#' @return
#' @export
#'
#' @examples
theil<-function(vintages, gap=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  theil<-.jcall("demetra/revisions/r/Utility", "[D", "theil", jq, as.integer(gap))
  return (theil)
}

#' Title
#'
#' @param vintages 
#'
#' @return
#' @export
#'
#' @examples
slopeAndDrift<-function(vintages, gap=1){
  q<-tsm
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "slopeAndDrift", jq, as.integer(gap))
  slopeAndDrift=matrix_jd2r(jsd)
  return (`colnames<-`(slopeAndDrift, OlsNames))
}
                   
#' Title
#'
#' @param vintages 
#' @param reference 
#' @param nbreuschgodfrey 
#' @param nljungbox 
#'
#' @return
#' @export
#'
#' @examples
autoCorrelation<-function(vintages, nbreuschgodfrey=1, nljungbox=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "autoCorrelation", jq
              , as.integer(nbreuschgodfrey), as.integer(nljungbox))
  ac<-matrix_jd2r(jsd)
  return (`colnames<-`(ac, acNames))
}

#' Title
#'
#' @param vintages 
#' @param adfk 
#'
#' @return
#' @export
#'
#' @examples
unitroot<-function(vintages, adfk=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "unitroot", jq, as.integer(adfk))
  ur<-matrix_jd2r(jsd)
  return (`colnames<-`(ur, urNames))
}

#' Cointegration tests (Engle-Granger)
#'
#' @param vintages 
#' @param adfk 
#'
#' @return
#' @export
#'
#' @examples
cointegration<-function(vintages, adfk=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "cointegration", jq, as.integer(adfk))
  eg<-matrix_jd2r(jsd)
  return (`colnames<-`(eg, egNames))
}

#' Title
#'
#' @param vintages 
#' @param lag 
#' @param model 
#'
#' @return
#' @export
#'
#' @examples
vecm<-function(vintages, lag=2, model = c("none", "cnt", "trend")){
  model<-match.arg(model)
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "vecm", jq, as.integer(lag), model)
  vecm<-matrix_jd2r(jsd)
  return (`colnames<-`(vecm, vecmNames))
}


#' Title
#'
#' @param vintages 
#' @param gap 
#'
#' @return
#' @export
#'
#' @examples
signalnoise<-function(vintages, gap=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jsd<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "signalNoise", jq, as.integer(gap))
  sn<-matrix_jd2r(jsd)
  return (`colnames<-`(sn, snNames))
}

#' Title
#'
#' @param revisions 
#'
#' @return
#' @export
#'
#' @examples
bias<-function(revisions){
  jrevs<-matrix_r2jd(revisions)
  jbias<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "bias", jrevs)
  bias=matrix_jd2r(jbias)
  return (`colnames<-`(bias, biasNames))
}

#' Title
#'
#' @param vintages 
#'
#' @return
#' @export
#'
#' @examples
efficiencyModel1<-function(vintages, gap=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jef1<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "efficiencyModel1", jq, as.integer(gap))
  efficiencyModel1=matrix_jd2r(jef1)
   return (`colnames<-`(efficiencyModel1, OlsNames))
}

#' Title
#'
#' @param vintages 
#'
#' @return
#' @export
#'
#' @examples
efficiencyModel2<-function(vintages, gap=1){
  q<-vintages
  q[is.na(q)]<-0
  jq<-matrix_r2jd(q)
  jef2<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "efficiencyModel2", jq, as.integer(gap))
  efficiencyModel2=matrix_jd2r(jef2)
  return (`colnames<-`(efficiencyModel2, OlsNames))
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
orthogonallyModel1<-function(revisions, nrevs=1){
  jr<-matrix_r2jd(as.matrix(revisions))
  jom<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "orthogonallyModel1", jr, as.integer(nrevs))
  om=matrix_jd2r(jom)
  return (`colnames<-`(om, OlsAllNames(nrevs)))
}

#' Title
#'
#' @param revisions 
#' @param reference 
#'
#' @return
#' @export
#'
#' @examples
orthogonallyModel2<-function(revisions, reference=1){
  jr<-matrix_r2jd(as.matrix(revisions))
  jom<-.jcall("demetra/revisions/r/Utility", "Ldemetra/math/matrices/MatrixType;", "orthogonallyModel2", jr, as.integer(reference))
  om=matrix_jd2r(jom)
  return (`colnames<-`(om, OlsNames))
}
