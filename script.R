install.packages("ggfortify")
install.packages("devtools")
install.packages("remotes") 
install.packages("rgl")
install.packages("pca3d")

# Iris dataset - 3 species, 50 samples each
str(iris)
summary(iris)

# Create factor var w/levels for species category
group_species <- factor(iris$Species,
                        levels = c("versicolor", "virginica", "setosa"))
summary(group_species)

# Create factor var w/levels for species category
#iris$Species <- factor(iris$Species,
 #                      levels = c("versicolor", "virginica", "setosa"))


# Check correlation (usually done earlier)
round(cor(iris[,1:4]), 2)

# PCA
Iris_pca <- prcomp(iris[,1:4],
                   center = TRUE,   # mean of 0
                   scale. = TRUE,   # STD of 1 
                   cor=TRUE,
                   scores=TRUE)

summary(Iris_pca)
str(Iris_pca)


#Plotting a PCA - checks for linearity and normality
library(ggfortify)

#plots PC1,PC2 automatically
iris_pca_plot <- autoplot(Iris_pca,
                          data = iris,
                          #  colour = 'Species'
)
iris_pca_plot


# screeplot - how many components to keep
plot_iris_pca <- plot(Iris_pca, type="lines")


# biplot - how components were combined in 2-d
biplot_iris_pca <- biplot(Iris_pca)

#####
library(pca3d)

pca3d(Iris_pca, components = 2:4, group=group_species)

#visualize with interactive plot
snapshotPCA3d(file="first3pcs.png")

#create figure for saving
pca2d(Iris_pca, group=group_species, legend="topright")









## 3 DIMENSIONAL

# Different pca set-up - princomp

Iris_pca <- princomp(iris[,1:4],
                   center = TRUE,   # mean of 0
                   scale. = TRUE,   # STD of 1 
                   cor=TRUE,
                   scores=TRUE)

library(rgl)

plot3d(Iris_pca$scores[,1:3])
text3d(Iris_pca$scores[,1:3],texts=rownames(iris))
text3d(Iris_pca$loadings[,1:3], texts=rownames(Iris_pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(Iris_pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),Iris_pca$loadings[i,1:3]))
}
