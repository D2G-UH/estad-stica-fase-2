library(lmtest)

data <- read.csv("temperature.csv") 

#--------Rtd-------
rtd.anova <- aov(data$Rtd~data$Silo+data$Dia)
summary(rtd.anova)
rtd.res <- rtd.anova$residuals

#1 Normalidad
qqnorm(rtd.res)
qqline(rtd.res)
shapiro.test(rtd.res)

#2 Independencia
dwtest(rtd.anova)

#plot(rtd.anova)

#--------Mer---------
mer.anova <- aov(data$Mer~data$Silo+data$Dia)
summary(mer.anova)
mer.res <- mer.anova$residuals

#1 Normalidad
qqnorm(mer.res)
qqline(mer.res)
shapiro.test(mer.res)

#2 Independencia
dwtest(mer.anova)

#3 Varianza constante
plot(mer.anova$fitted.values,rstudent(mer.anova))
abline(h=0,lty = 2)
bartlett.test(mer.res, data$Silo)

#--------Dif--------
dif = abs(data$Mer - data$Rtd)
dif.anova <- aov(dif~data$Silo+data$Dia)
summary(dif.anova)
dif.res <- dif.anova$residuals

#1 Normalidad
qqnorm(dif.res)
qqline(dif.res)
shapiro.test(dif.res)

#2 Independencia
dwtest(dif.anova)

