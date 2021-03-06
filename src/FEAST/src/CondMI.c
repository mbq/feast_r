/*******************************************************************************
** CondMI.c, implements the CMI criterion using a greedy forward search
**
** Initial Version - 19/08/2010
** Updated - 23/06/2011
**           22/02/2014 - Patched calloc.
**           12/03/2016 - Changed initial value of maxMI to -1.0 to prevent segfaults when I(X;Y) = 0.0 for all X.
**           17/12/2016 - Added feature scores.
**
** Author - Adam Pocock
** 
** Part of the FEAture Selection Toolbox (FEAST), please reference
** "Conditional Likelihood Maximisation: A Unifying Framework for Information
** Theoretic Feature Selection"
** G. Brown, A. Pocock, M.-J. Zhao, M. Lujan
** Journal of Machine Learning Research (JMLR), 2012
**
** Please check www.github.com/Craigacp/FEAST for updates.
** 
** Copyright (c) 2010-2017, A. Pocock, G. Brown, The University of Manchester
** All rights reserved.
** 
** Redistribution and use in source and binary forms, with or without modification,
** are permitted provided that the following conditions are met:
** 
**   - Redistributions of source code must retain the above copyright notice, this 
**     list of conditions and the following disclaimer.
**   - Redistributions in binary form must reproduce the above copyright notice, 
**     this list of conditions and the following disclaimer in the documentation 
**     and/or other materials provided with the distribution.
**   - Neither the name of The University of Manchester nor the names of its 
**     contributors may be used to endorse or promote products derived from this 
**     software without specific prior written permission.
** 
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
** ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
** ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
*******************************************************************************/

#include "FEAST/FSAlgorithms.h"
#include "FEAST/FSToolbox.h"

/* for memcpy */
#include <string.h>

/* MIToolbox includes */
#include "MIToolbox/MutualInformation.h"
#include "MIToolbox/ArrayOperations.h"

int* CondMI(uint k, uint noOfSamples, uint noOfFeatures, uint **featureMatrix, uint *classColumn, int *outputFeatures, double *featureScores) {
    char *selectedFeatures = (char *) checkedCalloc(noOfFeatures,sizeof(char));

    /*holds the class MI values*/
    double *classMI = (double *) checkedCalloc(noOfFeatures,sizeof(double));

    /*Changed to ensure it always picks a feature*/
    double maxMI = -1.0;
    int maxMICounter = -1;

    double score, currentScore;
    int currentHighestFeature;

    uint *conditionVector = (uint *) checkedCalloc(noOfSamples,sizeof(uint));

    int i, j;

    for (i = 0; i < k; i++) {
        outputFeatures[i] = -1;
    }

    for (i = 0; i < noOfFeatures; i++) {
        /*calculate mutual info
         **double calcMutualInformation(uint *firstVector, uint *secondVector, int vectorLength);
         */
        classMI[i] = calcMutualInformation(featureMatrix[i], classColumn, noOfSamples);

        if (classMI[i] > maxMI) {
            maxMI = classMI[i];
            maxMICounter = i;
        }/*if bigger than current maximum*/
    }/*for noOfFeatures - filling classMI*/

    selectedFeatures[maxMICounter] = 1;
    outputFeatures[0] = maxMICounter;
    featureScores[0] = maxMI;

    memcpy(conditionVector, featureMatrix[maxMICounter],sizeof(int)*noOfSamples);

    /*****************************************************************************
     ** We have populated the classMI array, and selected the highest
     ** MI feature as the first output feature
     ** Now we move into the CondMI algorithm
     *****************************************************************************/

    for (i = 1; i < k; i++) {
        score = 0.0;
        currentHighestFeature = -1;
        currentScore = 0.0;

        for (j = 0; j < noOfFeatures; j++) {
            /*if we haven't selected j*/
            if (selectedFeatures[j] == 0) {
                currentScore = 0.0;

                /*double calcConditionalMutualInformation(uint *firstVector, uint *targetVector, uint *conditionVector, int vectorLength);*/
                currentScore = calcConditionalMutualInformation(featureMatrix[j], classColumn, conditionVector, noOfSamples);

                if (currentScore > score) {
                    score = currentScore;
                    currentHighestFeature = j;
                }
            }/*if j is unselected*/
        }/*for number of features*/

        outputFeatures[i] = currentHighestFeature;
        featureScores[i] = score;

        if (currentHighestFeature != -1) {
            selectedFeatures[currentHighestFeature] = 1;
            mergeArrays(featureMatrix[currentHighestFeature], conditionVector, conditionVector, noOfSamples);
        }
    }/*for the number of features to select*/

    FREE_FUNC(classMI);
    FREE_FUNC(conditionVector);
    FREE_FUNC(selectedFeatures);

    classMI = NULL;
    conditionVector = NULL;
    selectedFeatures = NULL;

    return outputFeatures;
}/*CondMI(uint,uint,uint,uint[][],uint[],int[],double[])*/

double* discCondMI(uint k, uint noOfSamples, uint noOfFeatures, double **featureMatrix, double *classColumn, double *outputFeatures, double *featureScores) {
    uint *intFeatures = (uint *) checkedCalloc(noOfSamples*noOfFeatures,sizeof(uint));
    uint *intClass = (uint *) checkedCalloc(noOfSamples,sizeof(uint));
    int *intOutputs = (int *) checkedCalloc(k,sizeof(int));

    uint **intFeature2D = (uint**) checkedCalloc(noOfFeatures,sizeof(uint*));

    int i;

    for (i = 0; i < noOfFeatures; i++) {
        intFeature2D[i] = intFeatures + i*noOfSamples;
        normaliseArray(featureMatrix[i],intFeature2D[i],noOfSamples);
    }

    normaliseArray(classColumn,intClass,noOfSamples);

    CondMI(k, noOfSamples, noOfFeatures, intFeature2D, intClass, intOutputs, featureScores);

    for (i = 0; i < k; i++) {
        outputFeatures[i] = intOutputs[i];
    }

    FREE_FUNC(intFeatures);
    FREE_FUNC(intClass);
    FREE_FUNC(intOutputs);
    FREE_FUNC(intFeature2D);

    intFeatures = NULL;
    intClass = NULL;
    intOutputs = NULL;
    intFeature2D = NULL;

    return outputFeatures;
}/*discCondMI(int,int,int,double[][],double[],double[],double[])*/
