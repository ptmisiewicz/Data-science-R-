gala <- read.table("C:\\Users\\Piotrek\\Downloads\\gala_data.txt", header=TRUE)
fit1 <- lm(Species~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)
shapiro.test(fit1$residuals)
gala$sqrtSpecies = sqrt(gala$Species)

fit2 <- lm(sqrtSpecies~Area+Elevation+Nearest+Scruz+Adjacent, data=gala)
summary(fit2)

plot(fit2)
shapiro.test(fit2$residuals)

fit3 <- lm(sqrtSpecies~Elevation+Area+Scruz+Adjacent, data=gala)
summary(fit3)

plot(fit3)
shapiro.test(fit3$residuals)
