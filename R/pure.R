pureMIM<-function(X,Y,k=3){
 apply(X,2,mutinformation,Y)/log(2)->mim
 sort(mim,decreasing=TRUE)[1:k]->ans
 list(
  selection=names(ans),
  scores=ans
 )
}

#Tool
mergef<-function(x,y)
 factor(as.numeric(y)*length(levels(x))+as.numeric(x))

#Seems working, yet the scores are wrong
pureCondMI<-function(X,Y,k=3){
 X<-data.frame(X)
 S<-factor(rep(1,nrow(X)))
 selection<-c()
 ascores<-c()
 for(e in 1:k){
  sort(apply(X,2,condinformation,Y,S)/log(2),decreasing=TRUE)->scores
  if(scores[1]==0) break
  sel<-names(scores)[1]
  selection<-c(selection,sel)
  ascores<-c(ascores,scores[1])
  S<-mergef(S,factor(X[,sel]))
  X[,colnames(X)!=sel]->X
 }
 list(
  selection=selection,
  scores=ascores
 )
}

pureJMI<-function(X,Y,k=3){
 X<-data.frame(X)
 ascores<-apply(X,2,mutinformation,Y)/log(2)
 selection<-names(which.max(ascores))
 fscores<-max(ascores)
 scores<-rep(0,ncol(X))
 for(e in 1:(k-1)){
  factor(X[,tail(selection,1)])->x
  scores[colnames(X)!=tail(selection,1)]->scores
  X[,colnames(X)!=tail(selection,1)]->X
  
  scores+apply(X,2,function(xx) mutinformation(mergef(x,factor(xx)),Y)/log(2))->scores
  
  if(max(scores)==0) break

  selection<-c(selection,names(which.max(scores)))
  fscores<-c(fscores,max(scores))
 }
 list(
  selection=selection,
  scores=fscores
 )
}

#Can't be verified
pureMIFS<-function(X,Y,k=3,beta=1){
 X<-data.frame(X)
 ascores<-apply(X,2,mutinformation,Y)/log(2)
 selection<-names(which.max(ascores))
 fscores<-max(ascores)
 for(e in 1:(k-1)){
  factor(X[,tail(selection,1)])->x
  ascores[colnames(X)!=tail(selection,1)]->ascores
  X[,colnames(X)!=tail(selection,1)]->X

  apply(X,2,function(xx) mutinformation(x,factor(xx))/log(2))->scores
  ascores<-ascores-beta*scores
  if(max(ascores)<=0) break

  selection<-c(selection,names(which.max(ascores)))
  fscores<-c(fscores,max(ascores))
 }
 list(
  selection=selection,
  scores=fscores
 )
}

#Works wrong
puremRMR_D<-function(X,Y,k=3){
 X<-data.frame(X)
 jscores<-apply(X,2,mutinformation,Y)/log(2)
 bscores<-rep(0,ncol(X))
 selection<-names(which.max(jscores))
 fscores<-max(jscores)
 for(e in 1:(k-1)){
  factor(X[,tail(selection,1)])->x
  jscores[colnames(X)!=tail(selection,1)]->jscores
  bscores[colnames(X)!=tail(selection,1)]->bscores
  X[,colnames(X)!=tail(selection,1)]->X

  apply(X,2,function(xx) mutinformation(x,factor(xx))/log(2))->scores
  bscores<-bscores+scores
  ascores<-jscores-bscores/(e+1)
  if(max(ascores)<=0) break

  selection<-c(selection,names(which.max(ascores)))
  fscores<-c(fscores,max(ascores))
 }
 list(
  selection=selection,
  scores=fscores
 )
}

pureDISR<-function(X,Y,k=3){
 X<-data.frame(X)
 ascores<-apply(X,2,mutinformation,Y)/log(2)
 selection<-names(which.max(ascores))
 fscores<-max(ascores)
 rep(0,ncol(X))->scores
 for(e in 1:(k-1)){
  factor(X[,tail(selection,1)])->x
  scores[colnames(X)!=tail(selection,1)]->scores
  X[,colnames(X)!=tail(selection,1)]->X
  
  scores+apply(X,2,function(xx) 
   mutinformation(mergef(x,factor(xx)),Y)/
   entropy(mergef(mergef(x,factor(xx)),Y))
  )->scores
  if(max(scores)==0) break

  selection<-c(selection,names(which.max(scores)))
  fscores<-c(fscores,max(scores))
 }
 list(
  selection=selection,
  scores=fscores
 )
}

