
##  _____________________
##
## Script: NLCD_COLONY_PCA
##
## Purpose of script: Practice a PCA
#   - Use any dataset that contains categorical and continuous variables
#   - Read file into R, specify factor/categorical variables
#   - Replace any NaN or Inf values with NA
#   - Run a PCA using pcrcomp()
#   - Create a screeplot and a biplot
#   - Plot components of pcs with it colored by a categorical variable (Substrate, species, year)
#   - Optional: Make a plot with 3 pricipal components and record the image
#   
#   
## Included dataset:
##    - found here: https://github.com/laci-cartmell/Cartmell_Lesson_PCA/blob/main/Cartmell_NLCD_Rlesson.xlsx, 
##   is from my own research and it contains information on the name, substrate, year, and the proportion
##   of the 11 land use categories w/in a 1km radius of multiple cliff swallow colony sites.
##  
## A PCA works well with datasets like this one as it has multicollinearity btw variables and 
##  categorical variables that we want to understand how they influence the continuous variables.
## But feel free to use your own data or any other datasets if at least 1 cat. var. you can group by
## 
##
## Author: Laci Cartmell
##
## Date Created: 2022-11-03
##
##
## _____________________
## Notes: The below script is setup for Cartmell_NLCD_Rlesson.xlsx
##        The top section of the script is preparing the dataset for the pca
##        PCA is done in the bottom half
##        If unable to save images, clips of images works okay too.
##        
## _____________________
## Happy coding!
##


## load the packages needed: 

install.packages("ggfortify")
install.packages("devtools")
install.packages("remotes") 
install.packages("rgl")
install.packages("pca3d")
install.packages("devtools")
library(devtools)

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
install_github("vqv/ggbiplot")


##################
#### First, we must organize 
##################

 
# read in data
library(readxl)

Cartmell_NLCD_Rlesson <- read_excel("Cartmell_NLCD_Rlesson.xlsx")
View(Cartmell_NLCD_Rlesson)

# rename something easy to type and applicable
datatable <- Cartmell_NLCD_Rlesson

#check for any issues
summary(datatable)

# as.factor on categorical variables
#  SPECIFY SUBSTRATE/YEAR/COLONYNAME AS FACTOR VARS

# substrate has 4 categories - refers to structure the nest is built on
substrates <- c("1", "2", "3", "4")
datatable$Substrate <- as.factor(datatable$Substrate)

#year has 7 categories
years <- c("2001", "2004", "2006", "2008", "2013", "2016", "2019")
datatable$Year <- as.factor(datatable$Year)

#not quite a categorical var, but will still treat as factor
colonysites <- unique(datatable$COLONYNAME)
datatable$COLONYNAME = factor(datatable$COLONYNAME,
                              levels = unique(datatable$COLONYNAME))

#check factor(datatable$var) worked
summary(datatable)


#Check for NaN or Inf and replace with NA if present
#is all a variable NA
all(is.na(datatable$Size))
#Replace NaN & Inf with NA
datatable[is.na(datatable) | datatable=="-Inf"] = NA

#check for replacement
summary(datatable)

# Check correlation if you're feeling adventurous
round(cor(iris[,1:4]), 2) #iris data
round(cor(datatable[,5:11]), 2) #nlcd data


##################
#### PCA -   
##################

#remember our og
summary(datatable)

# run a pca
data.pca <- prcomp(datatable[,c(5:11)], center = TRUE, scale. = TRUE) #5-11 columns
summary(data.pca)
#PC1 explains 22% of the total variance
#PC2 - % of total varaince
# PC1,2,3 make up % of the total variance

#look at PCA object
str(data.pca)

#loadings
data.pca

#plotting pca
# loading library
library(ggfortify)

# biplots!

biplot.data.pca <- biplot(data.pca)
biplot.data.pca

#how many components

library(ggbiplot)

ggbiplot(data.pca)

#label each point with year
ggbiplot(data.pca, label = datatable$Year)

data.pca.plot <- autoplot(data.pca,
                          data = datatable,
                          colour = 'catsize')

data.pca.plot
data.pca.plot2 <- autoplot(data.pca,
                           data = datatable,
                           colour = 'Substrate')
data.pca.plot2


##################
#### PCA - Principal component analysis  
##################

# if not already, install
install.packages("ggfortify")
install.packages("devtools")
install.packages("remotes") 
install.packages("rgl")
install.packages("pca3d")

library(pca3d)

#
pca3d(data.pca)
pca3d

#create group of year
pca3d(data.pca, components = 1:3, group=datatable$Year)

#visualize with interactive plot
snapshotPCA3d(file="first3pcs.png")

#create figure for saving
pca2d(colony_pca, group=, legend="topright")


