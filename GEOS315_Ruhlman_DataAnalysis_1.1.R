#GEOS 315 Ruhlman 2021
#Data Analysis for Hair Sampling
#Last Updated: 3/31/2021
#Last Updated By: Hope D'Erasmo (hderasmo@wellesley.edu)
#Set your working directory!

#Data Wrangling

##Load Required Packages
library(readxl)
library(ggplot2)
library(readr)
library(tidyverse)

##Load the Data from the Local Computer
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


##Separate Out Standards & Samples

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
HairSampleNames <- select(HairSamples.NAs_1.0, c(SampleName))
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

###Rename rows to be the sample names

rownames(HairSamples.ppm_1.0) <- c("H1-3.14.21-03",
                                   "H1-3.14.21-02",
                                   "H1-3.14.21-01",
                                   "H2-3.25.21-01",
                                   "H2-3.25.21-02",
                                   "H2-3.25.21-03",
                                   "H3-03.25.21-01",
                                   "H3-03.25.21-02",
                                   "H3-03.25.21-03",
                                   "H4-03.25.21-01", 
                                   "H4-03.25.21-02", 
                                   "H4-03.25.21-03", 
                                   "H5-03.25.21-01", 
                                   "H5-03.25.21-02", 
                                   "H5-03.25.21-03",
                                   "H6-03.25.21-01",
                                   "H6-03.25.21-02",
                                   "H6-03.25.21-03",
                                   "H7-03.24.21-01",
                                   "H7-03.24.21-02",
                                   "H8-03.21.21-01",
                                   "H8-03.21.21-02",
                                   "H8-03.21.21-03",
                                   "H9-03.25.21-02",
                                   "H9-03.25.21-03",
                                   "H10-03.26.21-01",
                                   "H10-03.26.21-02",
                                   "H10-03.26.21-03",
                                   "H11-02.26.21-01", 
                                   "H11-02.26.21-02", 
                                   "H12-03.26.21-01",
                                   "H12-03.26.21-02",
                                   "H12-03.26.21-03",
                                   "H13-03.26.21-01",
                                   "H13-03.26.21-02",
                                   "H13-03.26.21-03",
                                   "H14-03.26.21-01",
                                   "H14-03.26.21-02",
                                   "H14-03.26.21-03",
                                   "H15-03.26.21-01",
                                   "H15-03.26.21-02",
                                   "H15-03.26.21-03",
                                   "H16-03.27.21-01", 
                                   "H16-03.27.21-02",
                                   "H17-03.26.21-01",
                                   "H17-03.26.21-02",
                                   "H17-03.26.21-03",
                                   "H18-03.28.21-01",
                                   "H18-03.28.21-02",
                                   "H18-03.28.21-03",
                                   "H20-03.28.21-01",
                                   "H20-03.28.21-02",
                                   "H20-03.28.21-03",
                                   "H21-03.28.21-01",
                                   "H21-03.28.21-02",
                                   "H21-03.28.21-03",
                                   "H22-03.29.21-01",
                                   "H22-03.29.21-02",
                                   "H22-03.29.21-03")



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

###Rename rows to be the standard names
rownames(HairStandards.ppm_1.0) <- c("H Stuffing Standard (1)",
                                     "H-Stuffing Standard (2)",
                                     "NIST 1515 (1)",
                                     "NIST 2709 (2)",
                                     "NIST 2709 (3)",
                                     "NIST 2709 (4)", 
                                     "NIST 1515 (2)",
                                     "NIST 2709 (5)",
                                     "NIST 1515 (3)",
                                     "NIST 2709 (6)",
                                     "NIST 1515 (4)",
                                     "NIST 2709 (7)",
                                     "NIST 1515 (5)",
                                     "NIST 2709 (8)",
                                     "NIST 1515 (6)"
                                     
)

#Linear Regression Table Construction

##Load required packages:
library(corrplot)
library(Hmisc)
library(factoextra)


##Step 1: Select Only Continuous Variables from the Sample Dataset

HairSamplesCont <- HairSamples.ppm_1.0

##Step 2: Select out only EOIs
HairSamplesContEOI <- select(HairSamplesCont, c(Zn,Fe, Cu, Ni, #Mo,
                                                Pb, Sb,
                                                Ca, Ti, S, Si, P, Br))


##Step 3: Calculate the Correlation Matrix (including p-values)

HairSamplesCont_cor <- rcorr(as.matrix(HairSamplesContEOI), type="pearson")
HairSamplesCont_cor$r # look at correlation coefficients (r)
HairSamplesCont_cor$P # look at p-values for each pairwise comparison

##Step 4 (Optional): Create a correlogram to better visualize the correlations

corrplot(HairSamplesCont_cor$r, type = "upper", 
         tl.col = "black", 
         tl.srt = 45, # tilt top row to 45-degree angle
         addCoef.col = "black") 

##Step 5 (optional): Create a coreelogram with coloring based on p-values
corrplot(HairSamplesCont_cor$r, type = "upper", 
         tl.col = "black", 
         tl.srt = 45, # tilt top row to 45-degree angle
         p.mat = LCE_cor$P, # p-values from matrix
         sig.level = 0.05, # alpha value for signficance
         addCoef.col = "black",
         insig = "blank")
##tbh this isn't displaying the p-values and I'm not totally sure why :/



#PCA Construction 

##Download Requisite Packages
library(ggfortify)
library(dyplyr)
library(colorspace)
library(readxl)

##Creating a dataframe that combines XEPOS data and survey data
HairSamplesContEOI #This is the object that's all of the EOI values in ppm

###Add the column of sample names back to that object
HairSamplesEOINames <- select(HairSamplesContEOI, Zn, Fe, Cu, Ni, #Mo, 
                              Pb, Sb, Ca, Ti, S, Si, P, Br) %>%
  mutate(Sample.ID = c("H1-03.14.21-03",
                      "H1-03.14.21-02",
                      "H1-03.14.21-01",
                      "H2-03.25.21-01",
                      "H2-03.25.21-02",
                      "H2-03.25.21-03",
                      "H3-03.25.21-01",
                      "H3-03.25.21-02",
                      "H3-03.25.21-03",
                      "H4-03.25.21-01", 
                      "H4-03.25.21-02", 
                      "H4-03.25.21-03", 
                      "H5-03.25.21-01", 
                      "H5-03.25.21-02", 
                      "H5-03.25.21-03",
                      "H6-03.25.21-01",
                      "H6-03.25.21-02",
                      "H6-03.25.21-03",
                      "H7-03.24.21-01",
                      "H7-03.24.21-02",
                      "H8-03.21.21-01",
                      "H8-03.21.21-02",
                      "H8-03.21.21-03",
                      "H9-03.25.21-02",
                      "H9-03.25.21-03",
                      "H10-03.26.21-01",
                      "H10-03.26.21-02",
                      "H10-03.26.21-03",
                      "H11-02.26.21-01", 
                      "H11-02.26.21-02", 
                      "H12-03.26.21-01",
                      "H12-03.26.21-02",
                      "H12-03.26.21-03",
                      "H13-03.26.21-01",
                      "H13-03.26.21-02",
                      "H13-03.26.21-03",
                      "H14-03.26.21-01",
                      "H14-03.26.21-02",
                      "H14-03.26.21-03",
                      "H15-03.26.21-01",
                      "H15-03.26.21-02",
                      "H15-03.26.21-03",
                      "H16-03.27.21-01", 
                      "H16-03.27.21-02",
                      "H17-03.26.21-01",
                      "H17-03.26.21-02",
                      "H17-03.26.21-03",
                      "H18-03.28.21-01",
                      "H18-03.28.21-02",
                      "H18-03.28.21-03",
                      "H20-03.28.21-01",
                      "H20-03.28.21-02",
                      "H20-03.28.21-03",
                      "H21-03.28.21-01",
                      "H21-03.28.21-02",
                      "H21-03.28.21-03",
                      "H22-03.29.21-01",
                      "H22-03.29.21-02",
                      "H22-03.29.21-03"))

##Load in Survey data
SurveyData_1.0 <- read.csv("Survey Data Polished - Data Reorganized.csv")

##Merge the two dataframes
SampleSurvey_ppm <- right_join(SurveyData_1.0, HairSamplesEOINames,
                                by = "Sample.ID")

##Remove rows we don't care about
SampleSurvey_ppm <- select(SampleSurvey_ppm, -c(n, Sample.., Date.Form.was.filled.out, 
                                                Date.Sample.was.taken..if.different.
                                                    ))



##Following Dan's SOP:


# See if the data needs to be normalized using a log function (?)

##Steps that Hope knows:

# 1. Specify categorical variables to be their own objects and remove
#    them from the data frame

SampleSurvey_pca <- SampleSurvey_ppm %>%
  drop_na()

age_vector <- as.vector(SampleSurvey_pca$Age)
curResidence_vector <- as.vector(SampleSurvey_pca$Current.Residence)
childResidence_vector <- as.vector(SampleSurvey_pca$Childhood.Residence)
demographic_vector <- as.vector(SampleSurvey_pca$Demographic)
diet_vector <- as.vector(SampleSurvey_pca$Diet)
chemicalTreatment_vector <- as.vector(SampleSurvey_pca$Chemical.Treatment)
hairColor_vector <- as.vector(SampleSurvey_pca$Natural.Hair.Color)
hairTexture_vector <- as.vector(SampleSurvey_pca$Hair.Texture)
hairDensity_vector <- as.vector(SampleSurvey_pca$Hair.Density)
productsUsed_vector <- as.vector(SampleSurvey_pca$Products.Used)
featuresOfProduct_vector <- as.vector(SampleSurvey_pca$Features.of.Product.)


# 2. Create a dataframe with only columns for the PCA

SampleSurvey_pca <- HairSamplesContEOI %>%
  drop_na()

# 3. Look at the distributions of the continuous variables to see if you should
#    use a log scale, etc.

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Zn))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Fe))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Cu))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Ni))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Pb))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Sb))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Ca))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Ti))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = S))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Si))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = P))

ggplot(data = SampleSurvey_pca) + 
  geom_freqpoly(aes(x = Br))

##Log transform the dataframe (CHECK WITH DAN ABOUT THIS)
#SampleSurvey_pca_log <- data.frame(log(SampleSurvey_pca)) %>%
 # drop_na()


# 4. Estimate the PCA Model

SampleSurvey_pca_est <- prcomp(SampleSurvey_pca, center= TRUE, scale.=TRUE)

# 5. Look at summary output of the pricipal components

summary(SampleSurvey_pca_est)

# 6. Summary plot showing percent variation in each principal component

fviz_eig(SampleSurvey_pca_est)

# 7. PCA plot mapping variables in relation to each other on PC1 & PC2
fviz_pca_var(SampleSurvey_pca_est,
             geom.ind = "point", # show points only (but not "text")
             mean.point = FALSE, # Remove point that represents the mean of each group
             addEllipses = FALSE, # add ellipses
             col.var = "black") + # make variables & arrows black (default is blue)
  theme_bw()

# 8. You can plot PCA also grouped based on one of your categorical variables
fviz_pca_biplot(SampleSurvey_pca_est,
                geom.ind = "point", # show points only (but not "text")
                col.ind = featuresOfProduct_vector, # color by categorical variable
                mean.point = FALSE, # Remove point that represents the mean of each group
                addEllipses = TRUE, # add ellipses
                col.var = "black", # make variables & arrows black (default is blue)
                legend.title = "Features of Products Used")  +
  theme_bw()


