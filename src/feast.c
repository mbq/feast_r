#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>

//Circumvent MEX/so compilation of FEAST and MIToolbox;
// ...first, silence their header files...
#define __FSToolbox_H
#define __MIToolbox_H

// ...kill exit and fprintf (only used for alloc failure)...
#undef fprintf
#define fprintf(...) //Noop
#define exit(a) //Noop

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
#include "FEAST/src/mRMR_D.c"
#include "FEAST/src/CMIM.c"
#include "FEAST/src/JMI.c"
#include "FEAST/src/DISR.c"
#include "FEAST/src/ICAP.c"
#include "FEAST/src/CondMI.c"
#include "FEAST/src/MIM.c"
#include "FEAST/src/BetaGamma.c"
#include "MIToolbox/src/ArrayOperations.c"
#include "MIToolbox/src/MutualInformation.c"
#include "MIToolbox/src/CalculateProbability.c"
#include "MIToolbox/src/Entropy.c"

//Shared code to validate input
void processInput(SEXP k,SEXP X,SEXP Y,int *numSel,int *nObj,int *nAtt,uint ***x){
 numSel[0]=INTEGER(k)[0];
 SEXP dimX=getAttrib(X,R_DimSymbol);
 nObj[0]=INTEGER(dimX)[0];
 if(nObj[0]!=length(Y))
  error("Decision vector length must be equal to the number of objects.");
 nAtt[0]=INTEGER(dimX)[1];
 if(numSel[0]>nAtt[0])
  error("You cannot select more than features than the set has.");

 //Populate the column map
 x[0]=(uint**)Calloc(nAtt[0],uint*);
 for(int e=0;e<nAtt[0];e++)
  x[0][e]=((uint*)INTEGER(X))+(nObj[0]*e);
}

//Interfaces to the FEAST-implemented algorithms
SEXP C_mRMR(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 //Init place for the results
 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 mRMR_D(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 Free(x);
 UNPROTECT(3);
 return(Ans);
}

SEXP C_CMIM(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 CMIM(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_JMI(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 JMI(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_DISR(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 DISR(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_ICAP(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 ICAP(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_CondMI(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 CondMI(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_MIM(SEXP k,SEXP X,SEXP Y){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 MIM(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score));

 UNPROTECT(3);
 return(Ans);
}

SEXP C_BetaGamma(SEXP k,SEXP X,SEXP Y,SEXP bg){
 int numSel,nObj,nAtt;
 uint **x;
 processInput(k,X,Y,&numSel,&nObj,&nAtt,&x);

 if(length(bg)!=2)
  error("Incorrect beta-gamma parameter!");
 double *betaGamma=REAL(bg);

 SEXP Ans; PROTECT(Ans=allocVector(VECSXP,2));
 SEXP Sel; PROTECT(Sel=allocVector(INTSXP,numSel));
 SET_VECTOR_ELT(Ans,0,Sel);
 SEXP Score; PROTECT(Score=allocVector(REALSXP,numSel));
 SET_VECTOR_ELT(Ans,1,Score);

 BetaGamma(numSel,nObj,nAtt,x,INTEGER(Y),INTEGER(Sel),REAL(Score),
  betaGamma[0],betaGamma[1]);

 UNPROTECT(3);
 return(Ans);
}
