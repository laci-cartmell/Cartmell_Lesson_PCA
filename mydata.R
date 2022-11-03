

##################### 
# Practice with my work research data
#####################

# read in data
library(readxl)
Cartmell_NLCD_Rlesson <- read_excel("Cartmell_NLCD_Rlesson.xlsx")
View(Cartmell_NLCD_Rlesson)
datatable <- Cartmell_NLCD_Rlesson


#list of colonynames

print(group_colony)

#  SPECIFY SUBSTRATE/YEAR/COLONYNAME AS FACTOR VARS, INDICATE BASE FOR SUBSTRATE AND YEAR
datatable$Substrate <- as.factor(datatable$Substrate)
#contrasts(datatable$Substrate) <- contr.treatment(c(".1", ".2", ".3", ".4"), base=4)

datatable$Year <- as.factor(datatable$Year)
#contrasts(datatable$Year) <- contr.treatment(c("2001", "2004", "2006", "2008", "2013", "2016", "2019"), base=1)

datatable$COLONYNAME = factor(datatable$COLONYNAME,
                              levels = unique(datatable$COLONYNAME))

summary(datatable)


#Check for NaN or Inf and replace with NA if present
#is all a variable NA
all(is.na(datatable$Size))
#Replace NaN & Inf with NA
datatable[is.na(datatable) | datatable=="-Inf"] = NA

summary(datatable)


##################
#### PCA - Principal component analysis - 
##################

data <- datatable[,c(18:28)] # only continuous
str(data)
head(data)
summary(data)

data.pca <- prcomp(datatable[,c(5:11)], center = TRUE, scale. = TRUE)
summary(data.pca)
#PC1 explains 22% of the total variance

#look at PCA object
str(data.pca)

#plotting pca
# loading library
install.packages("ggfortify")
library(ggfortify)


summary(datatable)
biplot.data.pca <- biplot(data.pca)
biplot.data.pca

#how many components

install.packages("devtools")
library(devtools)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(data.pca)

#label each point with year
ggbiplot(data.pca, label = datatable$Year)

data.pca.plot <- autoplot(data.pca,
                          data = datatable,
                          colour = 'Size')

data.pca.plot
data.pca.plot2 <- autoplot(data.pca,
                           data = datatable,
                           colour = 'Substrate')
data.pca.plot2

######################################################################

#### 3-dimensions
install.packages("ggfortify")
install.packages("devtools")
install.packages("remotes") 
install.packages("rgl")
install.packages("pca3d")

library(pca3d)

pca3d(data.pca)

#create group of year
pca3d(data.pca, components = 1:3, group=datatable$Year)

#visualize with interactive plot
snapshotPCA3d(file="first3pcs.png")

#create figure for saving
pca2d(Iris_pca, group=group_species, legend="topright")


