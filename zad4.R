realest <- read.table("C:\\Users\\Piotrek\\Downloads\\realest.txt", header=TRUE)
fit1 <- lm(Price ~ Bedroom + Space + Room + Lot + Tax + Bathroom + Garage + Condition, data=realest)
summary (fit1)
fit2 <- lm(Price ~ Bedroom, data = realest)
summary (fit2)

price1 = coef(fit1)[1] + 3 * coef(fit1)[2] + 1500 * coef(fit1)[3] + 8 * coef(fit1)[4] + 40 * coef(fit1)[5] + 1000 * coef(fit1)[6] + 5 * coef(fit1)[7] + 1 * coef(fit1)[8] + 0 * coef(fit1)[9]
price1


