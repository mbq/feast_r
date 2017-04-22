fixX<-function(X){
 #Convert matrices into data.frame
 if(is.matrix(X)) X<-as.data.frame(X)
 nam<-names(X)
 if(!is.data.frame(X)) stop("X must be matrix-alike with factors or logical colums.");
 for(e in which(sapply(X,is.logical))){
  X[[e]]<-factor(X[[e]]);
 }
 if(any(!sapply(X,is.factor)))
  stop("Every attribute in X must be factor or logical.");
 if(any(is.na(X)))
  stop("NAs not allowed in X.")
 #0-based integers
 sapply(X,function(x) as.integer(x)-1L)->X
 dim(X)->dX
 as.integer(X)->X
 dim(X)<-dX
 colnames(X)<-nam
 X
}

fixY<-function(Y){
 #Reject non-factor-alikes
 if(!(is.factor(Y)||is.logical(Y)))
  stop("Y must be either factor or logical.")
 if(any(is.na(Y)))
  stop("NAs not allowed in Y.")
 as.integer(factor(Y))-1L
}

#' Minimum Relevance Maximum Redundancy selection using the difference variant by H. Peng et al.
#' @references "Feature Selection Based on Mutual Information: Criteria of Max-Dependency, Max-Relevance, and Min-Redundancy" H. Peng et al. IEEE Pattern Analysis and Machine Intelligence (PAMI) (2005)
#' @template generic
#' @examples
#' mRMR_D(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_mRMR
#' @export
mRMR_D<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_mRMR,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}

#' Discrete version of the Conditional Mutual Information Maximisation selection, using the fast exact implementation by F. Fleuret.
#' @references "Fast Binary Feature Selection using Conditional Mutual Information Maximisation" F. Fleuret, JMLR (2004)
#' @template generic
#' @examples
#' CMIM(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_CMIM
#' @export
CMIM<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_CMIM,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}


#' Joint Mutual Information selection.
#' @references "Data Visualization and Feature Selection: New Algorithms for Nongaussian Data H. Yang and J. Moody, NIPS (1999)
#' @template generic
#' @examples
#' JMI(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_JMI
#' @export
JMI<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_JMI,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}

#' Double Input Symmetrical Relevance selection.
#' @references "On the Use of Variable Complementarity for Feature Selection in Cancer Classification" P. Meyer and G. Bontempi, (2006)
#' @template generic
#' @examples
#' DISR(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_DISR
#' @export
DISR<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_DISR,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}

#' Interaction Capping selction.
#' @references "Machine Learning Based on Attribute Interactions" A. Jakulin, PhD Thesis (2005)
#' @template generic
#' @examples
#' ICAP(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_ICAP
#' @export
ICAP<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_ICAP,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}


#' CMI selection using a greedy forward search.
#' @template generic
#' @examples
#' CondMI(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_CondMI
#' @export
CondMI<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_CondMI,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}

#' MIM selection using a greedy forward search.
#' @template generic
#' @examples
#' MIM(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4)
#' @useDynLib praznik C_MIM
#' @export
MIM<-function(X,Y,k){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_MIM,k,X,Y)->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}

#' Generic selector using the Beta-Gamma space by Brown et al.
#' @references "Conditional Likelihood Maximisation: A Unifying Framework for Mutual Information Feature Selection" G. Brown, A. Pocock, M.-J. Zhao, M. Lujan, JMLR (2011)
#' @template generic
#' @param beta Beta parameter; see the reference for details.
#' @param gamma Gamma parameter; see the reference for details.
#' @examples
#' BetaGamma(data.frame(lapply(iris[,-5],cut,10)),iris[,5],4,0.1,0.1)
#' @useDynLib praznik C_BetaGamma
#' @export
BetaGamma<-function(X,Y,k,beta,gamma){
 X<-fixX(X); fixY(Y)->Y;
 k<-as.integer(k);

 .Call(C_BetaGamma,
  k,X,Y,
  as.numeric(c(beta[1],gamma[1])))->ans
 list(selection=colnames(X)[ans[[1]]+1],scores=ans[[2]])
}
