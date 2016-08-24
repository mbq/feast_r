#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

//Circumvent MEX/so compilation of FEAST and MIToolbox;
// ...first, silence their header files...
#define __FSToolbox_H
#define __MIToolbox_H

// ...kill exit...
#define exit(a) //Noop
//...fprintf (only used for alloc failure)...
#undef fprintf
#define fprintf(a,b,c) //Noop
//...and printf (in MIToolbox)
#undef printf
#define printf(a,b,c) //Noop

// ...yet duplicate what they define...
#define COMPILE_C 1
#define C_IMPLEMENTATION
#define BASE_TWO 2.0
#define BASE_E M_E
#define LOG_BASE BASE_TWO

// ...provide alloc/free handlers using R GCed pool...
//NOTE: zeroing this memory is crucial; do not change to R_alloc.
#define CALLOC_FUNC(a,b) Calloc((b)*(a),char)
#define FREE_FUNC(a) Free((a))

// ...finally, statically inline all the required code.
#include "FEAST/mRMR_D.c"
#include "FEAST/CMIM.c"
#include "FEAST/JMI.c"
#include "FEAST/DISR.c"
#include "FEAST/ICAP.c"
#include "FEAST/CondMI.c"
#include "FEAST/MIM.c"
#include "FEAST/BetaGamma.c"
#include "MIToolbox/ArrayOperations.c"
#include "MIToolbox/MutualInformation.c"
#include "MIToolbox/CalculateProbability.c"
#include "MIToolbox/Entropy.c"

//Shared code to validate input
void processInput(SEXP k,SEXP X,SEXP Y,int *numSel,int *nObj,int *nAtt){
 numSel[0]=INTEGER(k)[0];
 SEXP dimX=getAttrib(X,R_DimSymbol);
 nObj[0]=INTEGER(dimX)[0];
 if(nObj[0]!=length(Y))
  error("Decision vector length must be equal to the number of objects.");
 nAtt[0]=INTEGER(dimX)[1];
 if(numSel[0]>nAtt[0])
  error("You cannot select more than features than the set has.");
}

//Interfaces to the FEAST-implemented algorithms
SEXP C_mRMR(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 mRMR_D(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_CMIM(SEXP k,SEXP X,SEXP Y,...){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 CMIM(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_JMI(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 JMI(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_DISR(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 DISR(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_ICAP(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 ICAP(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_CondMI(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 CondMI(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_MIM(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 MIM(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans));

 UNPROTECT(1);
 return(Ans);
}

SEXP C_BetaGamma(SEXP k,SEXP X,SEXP Y,SEXP bg){
 int numSel,nObj,nAtt;
 processInput(k,X,Y,&numSel,&nObj,&nAtt);
 if(length(bg)!=2)
  error("Incorrect beta-gamma parameter!");
 double *betaGamma=REAL(bg);

 SEXP Ans; PROTECT(Ans=allocVector(REALSXP,numSel));

 BetaGamma(numSel,nObj,nAtt,REAL(X),REAL(Y),REAL(Ans),
  betaGamma[0],betaGamma[1]);

 UNPROTECT(1);
 return(Ans);
}
