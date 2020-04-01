#############
#Ejercicio 1#
#############
Gasto = c(1000, 4000, 5000, 4500, 3000, 4000, 9000, 11000, 15000, 12000, 7000, 3000)
Ventas = c(9914, 40487, 54324, 50044, 34719, 42551, 94871, 118914, 158484, 131348, 78504, 36284)

data = data.frame(Gasto, Ventas)
cor(data)
plot(Gasto, Ventas, xlab = "Gasto", ylab = "Ventas")
modelo_lineal = lm(Ventas ~ Gasto, data)
abline(modelo_lineal, col = "blue")
summary(modelo_lineal)

# Residuals analysis
resid = modelo_lineal$residuals

# 1- La media de los errores es cero y la suma de los errores es cero. 
mean(resid)
sum(resid)

# 2- Errores normalmente distribuidos 

plot(modelo_lineal)

# 3- Independencia de los residuos
# Correr este comando conectado a internet si
# no está instalado el paquete "lmtest"
# install.packages("lmtest")
library(lmtest)
dwtest(modelo_lineal)

Mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
data = data.frame(Gasto, Ventas, Mes)
print(cor(data))
modelo_multilineal = lm(Ventas ~ Gasto + Mes, data)
print(summary(modelo_multilineal))

