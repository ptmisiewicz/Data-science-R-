savings <- read.table("C:\\Users\\Piotrek\\Downloads\\savings.txt", header=TRUE)
summary (savings$Country)
summary (savings$Savings)
summary (savings$pop15)
summary (savings$pop75)
summary (savings$dpi)
summary (savings$ddpi)

fit1 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=savings)
summary (fit1)



savings$res = fit1$resid

savings[which(savings$res>9),]
savings[which((-7.5)>savings$res),]

savings$a=hatvalues(fit1)
savings$a

p2 <- plot(savings$a)
p2



leverageBMK = 0.2
(savings[which(savings$a>leverageBMK),])

savings$studres <- rstudent(fit1)
savings$studres


(savings[which(abs(savings$studres)>2),])

savings$DFFITS <- dffits(fit1)
savings$DFFITS

DFFITSBMK <- 2 * sqrt(5/50)
(savings[which(abs(savings$DFFITS)>DFFITSBMK),])

savings$DFBETAS <- dfbeta(fit1)
savings$DFBETAS

DFBETASBMK <- 2/(sqrt(50))
(savings[which(abs(savings$DFBETAS)>DFBETASBMK),])

COOKBMK = 4/(50-4-1)

savings$Cook <- cooks.distance(fit1)
(savings[which(savings$Cook>COOKBMK),])

savings2 <- savings[savings$Cook<0.26,]


fit2 <- lm(Savings ~ dpi + ddpi + Pop15 + Pop75, data=savings2)
summary (fit2)




savings2$DFBETAS <- dfbeta(fit2)
savings2$DFBETAS
savings2$DFBETAS[,4]
savings2$DFBETAS[,5]

p2 <- plot(savings2$DFBETAS[,4],xlab = "Numer obserwacji", ylab = "Zmiana Pop15")  
p5 <- plot(savings2$DFBETAS[,5], xlab = "Numer obserwacji", ylab = "Zmiana Pop75")
