#GEOS 315 Ruhlman 2021
#Data Analysis for Hair Sampling
#Last Updated: 4/1/2021
#Last Updated By: Hope D'Erasmo (hderasmo@wellesley.edu)
#Set your working directory!

#Data Wrangling

##Load Required Packages
library(readxl)
library(ggplot2)
library(readr)
library(tidyverse)

##Load the Data from the Local Computer
HairData_2.0 <- read.csv("XEPOS Data Real Final Format.csv")

HairData_2.0Cont <- select(HairData_2.0, c(Si, P, S, Ca, Ti,
                                           Fe, Ni, Cu, Zn, Br, Sr, Mo, 
                                           Sb, Pb))
####Convert these data to numeric types hopefully finally
HairSamplesNum <- as.data.frame(lapply(HairData_2.0Cont, as.numeric))


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


