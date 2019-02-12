library (tree)
library (rpart)
iris6 <- read.delim("C:\\Users\\Piotrek\\Downloads\\iris.data.txt", header = FALSE, sep = ",", dec = ".")
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(iris6), alpha * nrow(iris6))
train.set <- iris6[inTrain,]
test.set  <- iris6[-inTrain,]


tree.model <- rpart(V5 ~ V1 + V2 + V3 + V4, data = train.set)
tree.model
summary(tree.model)

plot(tree.model)
text(tree.model)

my.prediction <- predict(tree.model, test.set) 
head(my.prediction)
table(predict(tree.model,test.set,type="class"), test.set$V5)
