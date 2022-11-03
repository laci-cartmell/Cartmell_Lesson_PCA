

##################### 
# my research data
#####################

library(readxl)
NLCD_Land_cover_gisanalysis <- read_excel("F:/nlcd_land_cover_l48_20210604/NLCD_Land_cover_gisanalysis.xlsx", sheet ="Sheet5")
summary(NLCD_Land_cover_gisanalysis)
datatable <- NLCD_Land_cover_gisanalysis

#list of colonynames
library(readxl)
colonynames <- read_excel("F:/nlcd_land_cover_l48_20210604/colonynames_analysis.xlsx", 
                          sheet = "colonynames_analysis")
View(colonynames)

#  SPECIFY SUBSTRATE/YEAR/COLONYNAME AS FACTOR VARS, INDICATE BASE FOR SUBSTRATE AND YEAR
datatable$Substrate <- as.factor(datatable$Substrate)
#contrasts(datatable$Substrate) <- contr.treatment(c(".1", ".2", ".3", ".4"), base=4)

datatable$Year <- as.factor(datatable$Year)
#contrasts(datatable$Year) <- contr.treatment(c("2001", "2004", "2006", "2008", "2013", "2016", "2019"), base=1)

datatable$COLONYNAME = factor(datatable$COLONYNAME,
                              levels = unique(datatable$COLONYNAME))
summary(datatable)
install.packages("writexl")
require(writexl)

write_xlsx(datatable, "C:\\MyLocation\\datatable2.xlsx")

#Check for NaN or Inf and replace with NA if present
#is all a variable NA
all(is.na(datatable$Size))
#Replace NaN & Inf with NA
datatable[is.na(datatable) | datatable=="-Inf"] = NA

# rank in r - ranking our df by H_P 
df %>%
  +   group_by(COLONYNAME) %>%
  +   mutate(good_ranks = order(order(Hay_Pasture, decreasing=TRUE)))
rank(datatable,ties.method = "first")


##################
#### PCA - Principal component analysis - 
##################
data <- datatable[,c(18:28)]
str(data)
head(data)
summary(data)

data.pca <- prcomp(datatable[,c(16:26)], center = TRUE, scale. = TRUE)
summary(data.pca)
#PC1 explains 22% of the total variance

#look at PCA object
str(data.pca)

#plotting pca
# loading library
install.packages("ggfortify")
library(ggfortify)
data.pca.plot <- autoplot(data.pca,
                          data = datatable,
                          colour = 'Size')
data.pca.plot2 <- autoplot(data.pca,
                           data = datatable,
                           colour = 'Substrate')
data.pca.plot
summary(datatable)
biplot.data.pca <- biplot(data.pca)
biplot.data.pca

#how many components
plot.data.pca <- plot(data.pca, type="1")
plot.data.pca
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


```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.