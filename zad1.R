library (MASS)
library (gridExtra)
data ("Cars93")
head(Cars93, 3)
Cars93$Min.Price
Cars93$MPG.city
Cars93$MPG.highway
Cars93$Weight
Cars93$Origin
Cars93$Type
Cars93$LP100KM.city <- 100/ ((Cars93$MPG.city * 1.6)/3.8)
Cars93$LP100KM.highway <- 100/ ((Cars93$MPG.highway * 1.6)/3.8)
Cars93$Weight.kg <- Cars93$Weight / 0.4536
Cars93$Min.Price.PLN <- Cars93$Min.Price * 3.35
summary (Cars93$Min.Price.PLN)
sd (Cars93$Min.Price.PLN)

quantile (Cars93$Min.Price.PLN, 0.95)
Cars93[which(Cars93$Min.Price.PLN > 115.508),31]
Cars93[which(Cars93$Min.Price.PLN > 115.508),2]
library (ggplot2)
p1 <- ggplot (Cars93, aes(x=Type)) + geom_bar()
p1
table (Cars93$Type)
pie (table (Cars93$Type))

sum(Cars93$Type == "Sporty")

p2 <- ggplot (Cars93, aes(Origin,MPG.city)) + geom_boxplot(color="blue", fill="red")
p2

usa = Cars93[which(Cars93$Origin == 'USA'),7]
summary(usa)

nonusa = Cars93[which(Cars93$Origin == 'non-USA'),7]
summary (nonusa)
test11 <- cor.test(Cars93$MPG.city, Cars93$Min.Price, method="pearson")
test12 <- cor.test(Cars93$MPG.city, Cars93$Min.Price, method="kendall")
test13 <- cor.test(Cars93$MPG.city, Cars93$Min.Price, method="spearman")

test21 <- cor.test(Cars93$MPG.highway, Cars93$MPG.city, method="pearson")
test22 <- cor.test(Cars93$MPG.highway, Cars93$MPG.city, method="kendall")
test23 <- cor.test(Cars93$MPG.highway, Cars93$MPG.city, method="spearman")

test11text <- as.character(test11$estimate)
test21text <- as.character(test21$estimate)
test11text <- paste("rxy =", test11text, sep=" ")
test21text <- paste("rxy =", test21text, sep=" ")

p3 <- ggplot (Cars93, aes(x=MPG.city, y=Min.Price)) + geom_point(color="blue") + 
  annotate("text", x = 35, y = 40, label = test11text)



p4 <- ggplot (Cars93, aes(x=MPG.highway, y=MPG.city)) + geom_point(color="blue") +
  annotate("text", x = 30, y = 40, label = test21text)
  

grid.arrange(p3,p4,nrow=1)

p5 <- ggplot(Cars93, aes(Weight)) + geom_histogram(binwidth=50)
p5
