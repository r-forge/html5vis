


# motion Chart


# Cluster Chart

library(html5Vis)
kmeansCluster <- kmeans(iris[,c("Petal.Length","Petal.Width")], centers = 3)
iris$cluster <- kmeansCluster$cluster
iris$name <- paste( iris$Species, 1:length(iris$Species), sep="_")
center <- as.data.frame(kmeansCluster$center)

m3 <- mvisClusterChart(iris, xvar = "Petal.Length", yvar = "Petal.Width", 
		clustervar = "cluster", namevar = "name", centerDf = center)
plot(m3)


# Corrplot
library(html5Vis)
data(mtcars)
cor(mtcars)
m4 <- mvisCorrplotChart(cor(mtcars))
plot(m4)





