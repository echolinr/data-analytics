

library(readr)
autos <- read_csv("~/dataset/autos.csv")
attach(autos)

lm1 <- lm(price ~ kilometer, data = autos)
summary(lm1)

lm2 <- lm(price ~ kilometer + gearbox, data = autos)
summary(lm2)

lm3 <- lm(price ~ kilometer + gearbox + yearOfRegistration, data = autos)
summary(lm3)

lm4 <- lm(price ~ kilometer + gearbox + yearOfRegistration, data = autos)
summary(lm4)

survcoefs <- coef(lm4)
survcoefs


autos$gearbox <- as.factor(autos$gearbox)
as.numeric(autos$gearbox)

plot(price ~ kilometer,col = c("red", "blue")[as.numeric(autos$gearbox)], xlab ="kilometer", ylab = "price", pch = 12)
abline(a=survcoefs[1],b=survcoefs[2],col="red",lwd=2)
abline(a=survcoefs[1]+survcoefs[3],b=survcoefs[2],col="blue",lwd=2)
legend("topleft",legend=levels(gearbox),col=c("red","blue"),pch=16)
plot(price ~ yearOfRegistration)


