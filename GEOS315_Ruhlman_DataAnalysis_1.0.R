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
PrelimHairData_1.0_Percents <- select(PrelimHairData_1.0,
                                      c(ï..Column1, Column7, Column10,
                                         Column13, Column16, Column19,
                                         Column22, Column25, Column28,
                                         Column31, Column34, Column37,
                                         Column40, Column43, Column46,
                                         Column49, Column52, Column55,
                                         Column58, Column61, Column64,
                                         Column67, Column70, Column73,
                                         Column76, Column79, Column82,
                                         Column85, Column88, Column91,
                                         Column94, Column97, Column100,
                                         Column103, Column106, Column109,
                                         Column112, Column115, Column118,
                                         Column121, Column124, Column127,
                                         Column130, Column133, Column136,
                                         Column139, Column142, Column145,
                                         Column148, Column151, Column154,
                                         Column157, Column160, Column163,
                                         Column166, Column169, Column172,
                                         Column175, Column178, Column181,
                                         Column184, Column187, Column190,
                                         Column193, Column196, Column199,
                                         Column202, Column205, Column208,
                                         Column211, Column214, Column217))

##Rename columns to be their respective elements
PrelimHairData_1.0_Percents_Labeled <- rename(PrelimHairData_1.0_Percents,
                                      SampleName = ï..Column1, Na = Column7,
                                      Mg = Column10,
                                      Al = Column13, Si = Column16, P = Column19,
                                      S = Column22, Cl = Column25, K = Column28,
                                      Ca = Column31, Sc = Column34, Ti = Column37,
                                      V = Column40, Cr = Column43, Mn = Column46,
                                      Fe = Column49, Co = Column52, Ni = Column55,
                                      Cu = Column58, Zn = Column61, Ga = Column64,
                                      Ge = Column67, As = Column70, Se = Column73,
                                      Br = Column76, Rb = Column79, Sr = Column82,
                                      Y = Column85, Zr = Column88, Nb = Column91,
                                      Mo = Column94, Pd = Column97, Ag = Column100,
                                      Cd = Column103, In = Column106, Sn = Column109,
                                      Sb = Column112, Te = Column115, I = Column118,
                                      Cs = Column121, Ba = Column124, La = Column127,
                                      Ce = Column130, Pr = Column133, Nd = Column136,
                                      Sm = Column139, Eu = Column142, Gd = Column145,
                                      Tb = Column148, Dy = Column151, Ho = Column154,
                                      Er = Column157, Tm = Column160, Yb = Column163,
                                      Lu = Column166, Hf = Column169, Ta = Column172,
                                      W = Column175, Os = Column178, Au = Column181,
                                      Hg = Column184, Tl = Column187, Pb = Column190,
                                      Bi = Column193, Po = Column196, At = Column199,
                                      Th = Column202, Pa = Column205, U = Column208,
                                      Np = Column211, Pu = Column214, Am = Column217)

##Remove Row 1
PrelimHairData_1.0_Percents_Labeled <- PrelimHairData_1.0_Percents_Labeled[-c(1,2),] 

##Rename to a easier to use variable name
PrelimHairData_1.0 <- PrelimHairData_1.0_Percents_Labeled

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


