## googleVis demo
pause <- function(){  
  invisible(readline("\nPress <return> to continue: ")) 
}

# motion Chart
M7 <- mvisMotionChart(Fruits, idvar="Fruit", timevar="Year",
                    xvar="Profit", yvar="Expenses",
                    colorvar="Location", sizevar="Sales")
plot(M7)
pause()


# Cluster Chart

library(html5Vis)
kmeansCluster <- kmeans(iris[,c("Petal.Length","Petal.Width")], centers = 3)
iris$cluster <- kmeansCluster$cluster
iris$name <- paste( iris$Species, 1:length(iris$Species), sep="_")
center <- as.data.frame(kmeansCluster$center)

m3 <- mvisClusterChart(iris, xvar = "Petal.Length", yvar = "Petal.Width", 
		clustervar = "cluster", namevar = "name", centerDf = center)
plot(m3)
pause()


# Corrplot
library(html5Vis)
data(mtcars)
cor(mtcars)
m4 <- mvisCorrplotChart(cor(mtcars))
plot(m4)
pause()





