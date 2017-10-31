context("test-correctness.R")

set.seed(7)

#Generate iris with nonsense data
X<-cbind(iris[,-5],apply(iris[,rep(1:4,each=3)],2,sample))
names(X)<-c(names(iris)[-5],sprintf("Nonsense%d",1:(ncol(X)-4)))
Y<-iris$Species
#Make it discrete
X<-data.frame(apply(X,2,cut,10))

#Value of k for all tests
K<-7

#There may be false positives in case of ties -- FEAST returns last max, pure first max

test_that("Pure MIM works like MIM",{
 pureMIM(X,Y,k=K)->pure
 MIM(X,Y,k=K)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure CondMI works like CondMI",{
 pureCondMI(X,Y,k=K)->pure
 CondMI(X,Y,k=K)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure JMI works like JMI",{
 pureJMI(X,Y,k=K)->pure
 JMI(X,Y,k=K)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure mRMR_D works like mRMR_D",{
 puremRMR_D(X,Y,k=K)->pure
 mRMR_D(X,Y,k=K)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure DISR works like DISR",{
 pureDISR(X,Y,k=K)->pure
 DISR(X,Y,k=K)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure BetaGamma works like BetaGamma",{
 runif(1,0,2)->beta
 runif(1,0,2)->gamma

 pureBetaGamma(X,Y,k=K,beta=beta,gamma=gamma)->pure
 BetaGamma(X,Y,k=K,beta=beta,gamma=gamma)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure MIFS works like BetaGamma(g=0)",{
 runif(1,0,2)->beta

 pureMIFS(X,Y,k=K,beta=beta)->pure
 BetaGamma(X,Y,k=K,beta=beta,gamma=0)->feast

 expect_equal(pure$scores,feast$scores)
 expect_equal(pure$selection,feast$selection)
})

test_that("Pure MIM can pull all",{
 pureMIM(X[,1:4],Y,k=4)
})

test_that("Pure CondMI can pull all",{
 pureCondMI(X[,1:4],Y,k=4)
})

test_that("Pure JMI can pull all",{
 pureJMI(X[,1:4],Y,k=4)
})

test_that("Pure MIFS can pull all",{
 pureMIFS(X[,1:4],Y,k=4)
})

test_that("Pure BetaGamma can pull all",{
 pureBetaGamma(X[,1:4],Y,k=4)
})

test_that("Pure mRMR_D can pull all",{
 puremRMR_D(X[,1:4],Y,k=4)
})

test_that("Pure DISR can pull all",{
 pureDISR(X[,1:4],Y,k=4)
})
