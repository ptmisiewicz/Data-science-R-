x <- read.delim("C:\\Users\\Piotrek\\Downloads\\airpollution.txt", header = TRUE, sep = "\t", dec = ".")
summary (x$Mortality)
summary (x$Education)
summary (x$X.NonWhite)
summary (x$income)
summary (x$JanTemp)
summary (x$JulyTemp)
summary (x$NOx)

fit1 <- lm(Mortality~NOx, data=x)
summary (fit1)

p1 <- plot(x$NOx, x$Mortality); abline(fit1)
p1

fit2 <- lm(Mortality~log(NOx), data=x)
summary (fit2)

p2 <- plot(log(x$NOx), x$Mortality); abline(fit2)
p2

x$res <- studres(fit2)
x$res

x2 <- x[x$res<2,]
x2 <- x2[x2$res>-2,]

fit2 <- lm(Mortality~log(NOx), data=x2)
summary (fit2)

p2 <- plot(log(x2$NOx), x2$Mortality); abline(fit2)
p2
