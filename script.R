install.packages("ggfortify")
install.packages("devtools")
install.packages("remotes") 
install.packages("rgl")
# Iris dataset - 3 species, 50 samples each
str(iris)
summary(iris)

# Create factor var w/levels for species category
iris$Species <- factor(iris$Species,
                       levels = c("versicolor", "virginica", "setosa"))

# remove categorical variable
#ris_pca <- subset(iris, select = -c(Species))

summary(Iris_pca)
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

Iris_pca <- prcomp(iris[,1:4],
                   center = TRUE,   # mean of 0
                   scale. = TRUE,   # STD of 1 
                   cor=TRUE,
                   scores=TRUE)

library(rgl)


plot3d(Iris_pca$scores[,1:3])
text3d(pc$scores[,1:3],texts=rownames(iris))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}

# not great tools for 3d in R, but we can look at the 3 pcs with most variance

set.seed(42)
cl <- kmeans(iris[,1:4],3)
iris$cluster <- as.factor(cl$cluster)

plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
plot3d(pc$scores[,1:3], col=iris$Species, main="actual species")

with(iris, table(cluster, Species))





