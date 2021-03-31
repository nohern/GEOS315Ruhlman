#GEOS 315 Ruhlman 2021
#Data Analysis for Hair Sampling
#Last Updated: 3/29/2021
#Last Updated By: Hope D'Erasmo (hderasmo@wellesley.edu)
#Set your working directory!

#Data Wrangling

##Load Required Packages
library(readxl)
library(ggplot2)
library(readr)
library(tidyverse)

##Load the Data from the Local Computer
PrelimHairData_1.0 <- read.csv("PrelimHairData_3-19.csv")
HairData_1.0 <- read.csv("Test Hair Sample Data - XEPOS Data.csv")

##Create an Object that is just the percents for each element

###Get rid of columns that aren't percentages (we can add them later)
HairData_1.0_Percents <- select(HairData_1.0,
                                      c(X, X.7, X.10,
                                        X.13, X.16, X.19,
                                        X.22, X.25, X.28,
                                        X.31, X.34, X.37,
                                        X.40, X.43, X.46,
                                        X.49, X.52, X.55,
                                        X.58, X.61, X.64,
                                        X.67, X.70, X.73,
                                        X.76, X.79, X.82,
                                        X.85, X.88, X.91,
                                        X.94, X.97, X.100,
                                        X.103, X.106, X.109,
                                        X.112, X.115, X.118,
                                        X.121, X.124, X.127,
                                        X.130, X.133, X.136,
                                        X.139, X.142, X.145,
                                        X.148, X.151, X.154,
                                        X.157, X.160, X.163,
                                        X.166, X.169, X.172,
                                        X.175, X.178, X.181,
                                        X.184, X.187, X.190,
                                        X.193, X.196, X.199,
                                        X.202, X.205, X.208,
                                        X.211, X.214, X.217))

##Rename columns to be their respective elements
HairData_1.0_Percents_Labeled <- rename(HairData_1.0_Percents,
                                      SampleName = X, Na = X.7,
                                      Mg = X.10,
                                      Al = X.13, Si = X.16, P = X.19,
                                      S = X.22, Cl = X.25, K = X.28,
                                      Ca = X.31, Sc = X.34, Ti = X.37,
                                      V = X.40, Cr = X.43, Mn = X.46,
                                      Fe = X.49, Co = X.52, Ni = X.55,
                                      Cu = X.58, Zn = X.61, Ga = X.64,
                                      Ge = X.67, As = X.70, Se = X.73,
                                      Br = X.76, Rb = X.79, Sr = X.82,
                                      Y = X.85, Zr = X.88, Nb = X.91,
                                      Mo = X.94, Pd = X.97, Ag = X.100,
                                      Cd = X.103, In = X.106, Sn = X.109,
                                      Sb = X.112, Te = X.115, I = X.118,
                                      Cs = X.121, Ba = X.124, La = X.127,
                                      Ce = X.130, Pr = X.133, Nd = X.136,
                                      Sm = X.139, Eu = X.142, Gd = X.145,
                                      Tb = X.148, Dy = X.151, Ho = X.154,
                                      Er = X.157, Tm = X.160, Yb = X.163,
                                      Lu = X.166, Hf = X.169, Ta = X.172,
                                      W = X.175, Os = X.178, Au = X.181,
                                      Hg = X.184, Tl = X.187, Pb = X.190,
                                      Bi = X.193, Po = X.196, At = X.199,
                                      Th = X.202, Pa = X.205, U = X.208,
                                      Np = X.211, Pu = X.214, Am = X.217)

##Remove Rows 1, 2, 3
HairData_1.0_Percents_Labeled <- HairData_1.0_Percents_Labeled[-c(1,2,3),] 

##Rename to a easier to use variable name
HairData_1.0 <- HairData_1.0_Percents_Labeled


##Seperate Out Standards & Samples

HairSamples_1.0 <- HairData_1.0[-c(1, 61, 62, 63, 64, 65, 66, 67, 68,
                                   69, 70, 71, 72, 73, 74, 75,
                                   76, 77, 78, 79, 80, 81),]

HairStandards_1.0 <- HairData_1.0[c(1, 61, 62, 63, 64, 65, 66, 67, 68,
                                    69, 70, 71, 72, 73, 74, 75,
                                    76, 77, 78, 79, 80, 81),]

HairStandards_1.0 <- HairStandards_1.0[-c(3, 4, 5, 6, 7, 8, 9), ]

##Convert samples and standards to ppm instead of percent for samples

###Convert measurements at LOD to NAs

numRow <- nrow(HairSamples_1.0)
numCol <- ncol(HairSamples_1.0) 
HairSamples.NAs_1.0 <- HairSamples_1.0


for (r in 1:numRow){
  
  for(c in 1:numCol){
    
    if (grepl("<",HairSamples_1.0[r, c], fixed = TRUE) == TRUE) {
      
      HairSamples.NAs_1.0[r,c] <- NA ##If you want to not exclude them, change this
    }
    
    else{
      
      HairSamples.NAs_1.0[r,c] <- HairSamples_1.0[r,c]
    }
    
  }
  
}

###Convert percentages to ppm for samples

####Temporarily remove the SampleName column for this to work
HairSamplesCont <- select(HairSamples.NAs_1.0, -c(SampleName))

####Convert these data to numeric types hopefully finally
HairSamplesNum <- as.data.frame(lapply(HairSamplesCont, as.numeric))


####Set up the loops to convert the data to numeric type and convert to ppm

numRow <- nrow(HairSamplesNum)
numCol <- ncol(HairSamplesNum) 


HairSamples.ppm_1.0 <- HairSamplesNum


for (r in 1:numRow){
  
  for(c in 1:numCol){
    
    
      HairSamples.ppm_1.0[r,c] <- HairSamplesNum[r,c] * 10000
    
    
  }
  
}




##Convert samples and standards to ppm instead of percent for standards

###Convert measurements at LOD to NAs

numRow <- nrow(HairStandards_1.0)
numCol <- ncol(HairStandards_1.0) 
HairStandards.NAs_1.0 <- HairStandards_1.0


for (r in 1:numRow){
  
  for(c in 1:numCol){
    
    if (grepl("<",HairStandards_1.0[r, c], fixed = TRUE) == TRUE) {
      
      HairStandards.NAs_1.0[r,c] <- NA ##If you want to not exclude them, change this
    }
    
    else{
      
      HairStandards.NAs_1.0[r,c] <- HairStandards_1.0[r,c]
    }
    
  }
  
}

###Convert percentages to ppm for samples

####Temporarily remove the SampleName column for this to work
HairStandardsCont <- select(HairStandards.NAs_1.0, -c(SampleName))

####Convert these data to numeric types hopefully finally
HairStandardsNum <- as.data.frame(lapply(HairStandardsCont, as.numeric))


####Set up the loops to convert the data to numeric type and convert to ppm

numRow <- nrow(HairStandardsNum)
numCol <- ncol(HairStandardsNum) 


HairStandards.ppm_1.0 <- HairStandardsNum


for (r in 1:numRow){
  
  for(c in 1:numCol){
    
    
    HairStandards.ppm_1.0[r,c] <- HairStandardsNum[r,c] * 10000
    
    
  }
  
}

#Linear Regression Table Construction

##Load required packages:
library(corrplot)
library(Hmisc)
library(factoextra)


##Step 1: Select Only Continuous Variables from the Dataset

PrelimHairCont <- select(PrelimHairData_1.0, -c(SampleName)) %>%
  drop_na()

##Step 2: Drop any rows with NA values or Non-numeric values

numRow <- nrow(PrelimHairCont)
numCol <- ncol(PrelimHairCont)
PrelimHairContNAs <-PrelimHairCont

for (r in 1:numRow){
  
  for(c in 1:numCol){
  
    if (grepl("<",PrelimHairCont[r, c], fixed = TRUE) == TRUE) {
      
      PrelimHairContNAs[r,c] <- NA ##If you want to not exclude them, change this
    }
    
    else{
      
      PrelimHairContNAs[r,c] <- PrelimHairCont[r,c]
    }
  
}
  
}

#PrelimHairContFinal <- drop_na(PrelimHairContNAs)

##Step 3: Calculate the Correlation Matrix (including p-values)

PrelimHairCont_cor <- rcorr(as.matrix(PrelimHairContNAs), type="pearson")
PrelimHairCont_cor$r # look at correlation coefficients (r)
PrelimHairCont_cor$P # look at p-values for each pairwise comparison

##Step 4 (Optional): Create a correlogram to better visualize the correlations

corrplot(PrelimHairCont_cor$r, type = "upper", 
         tl.col = "black", 
         tl.srt = 45, # tilt top row to 45-degree angle
         addCoef.col = "black") 



#PCA Construction 

##Download Requisite Packages
library(ggfortify)
library(dyplyr)
library(colorspace)
library(readxl)

##Following Dan's SOP:

PrelimHair_pca <- PrelimHairData_1.0

# See if the data needs to be normalized using a log function (?)

##Steps that Hope knows:
# 1. Create a dataframe with only columns for the PCA

# 2. Specify categorical variables to be their own objects and remove
#    them from the data frame

# 3. Estimate the PCA Model

PrelimHair_pca_est <- prcomp(PrelimHair_pca, center= TRUE, scale.=TRUE)

# 4. Look at summary output of the pricipal components

summary(PrelimHair_pca_est)

# 5. Summary plot showing percent variation in each principal component

fviz_eig(PrelimHair_pca_est)

# 6. PCA plot mapping variables in relation to each other on PC1 & PC2
fviz_pca_var(PrelimHair_pca_est,
             geom.ind = "point", # show points only (but not "text")
             mean.point = FALSE, # Remove point that represents the mean of each group
             addEllipses = FALSE, # add ellipses
             col.var = "black") + # make variables & arrows black (default is blue)
  theme_bw()

# 7. You can plot PCA also grouped based on one of your categorical variables
fviz_pca_biplot(PrelimHair_pca_est,
                geom.ind = "point", # show points only (but not "text")
                col.ind = CATEGORICAL VARIABLE, # color by categorical variable
                mean.point = FALSE, # Remove point that represents the mean of each group
                addEllipses = TRUE, # add ellipses
                col.var = "black", # make variables & arrows black (default is blue)
                legend.title = "CATEGORICAL VARIABLE")  +
  theme_bw()


