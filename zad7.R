iris7 <- read.delim("C:\\Users\\Piotrek\\Downloads\\iris.data.txt", header = FALSE, sep = ",", dec = ".")
iris.norm = iris7
iris.norm$V1 = (iris7$V1 - min(iris7$V1))/(max(iris7$V1)-min(iris7$V1))
iris.norm$V2 = (iris7$V2 - min(iris7$V2))/(max(iris7$V2)-min(iris7$V2))
iris.norm$V3 = (iris7$V3 - min(iris7$V3))/(max(iris7$V3)-min(iris7$V3))
iris.norm$V4 = (iris7$V4 - min(iris7$V4))/(max(iris7$V4)-min(iris7$V4))

library(caret)
library(class)
library(datasets)
library (knncat)
library(gmodels)
set.seed(3033)
intrain <- createDataPartition(y = iris.norm$V5, p= 0.7, list = FALSE)
training <- iris.norm[intrain,]
testing <- iris.norm[-intrain,]
class(training$V4)
knnmodel <- knncat(train=training, test=testing, k=3, classcol = 5)
knnmodel
CrossTable(x = testing$V5, y = knnmodel$test.classes, prop.chisq = FALSE)
