library(lmtest)

solveSimpleRegresion <- function()
{       
        Gasto = c(1000, 4000, 5000, 4500, 3000, 4000, 9000, 11000, 15000, 12000, 7000, 3000)
        Ventas = c(9914, 40487, 54324, 50044, 34719, 42551, 94871, 118914, 158484, 131348, 78504, 36284)
        titlex = "Gasto"
        titley = "Ventas"
        xl = c(1000, 15000)
        yl = c(9914, 131348)
        data = data.frame(Gasto, Ventas)
        
        print(cor(data, method = "pearson"))   #coeficiente de correlacion lineal
        cor.test(Gasto, Ventas, method = "pearson")
        
        plot(Gasto, Ventas, xlab = titlex, ylab = titley, xlim = xl, ylim = yl)
        modelo_lineal = lm(Ventas ~ Gasto, data)
        abline(modelo_lineal, col = "blue")
        
        plot(modelo_lineal$residuals,
             main = "Residuales", ylab = "Residuales", xlab="Indices")
        print(summary(modelo_lineal))
        
        residuos = modelo_lineal$residuals
        prediccion = modelo_lineal$fitted.values
        
        #Analisis de los supuestos del modelo lineal
        
        #1- Independencia de los residuos
        print(dwtest(modelo_lineal))   #test Durbin-Watson
        
        #2- E[residuos] = 0
        print(summary(residuos))
        plot(prediccion, residuos,
             main="Predicción", xlab="residuos", ylab="predicción")
        
        #3- Varianza constante de los residuos(Homocedasticidad)
        print(bptest(modelo_lineal))
        
        #4- Distribucion Normal con media 0 y varianza constante
        hist(residuos, main = "Histograma")
        
        qqnorm(residuos)
        qqline(residuos)
        shapiro.test(residuos)
}

solveMultipleRegresion <- function()
{
        Gasto = c(1000, 4000, 5000, 4500, 3000, 4000, 9000, 11000, 15000, 12000, 7000, 3000)
        Ventas = c(9914, 40487, 54324, 50044, 34719, 42551, 94871, 118914, 158484, 131348, 78504, 36284)
        Mes = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
        
        data = data.frame(Gasto, Ventas, Mes)
        print(cor(data))
        modelo_multilineal = lm(Ventas ~ Gasto + Mes, data)
        print(summary(modelo_multilineal))
        resid = modelo_multilineal$residuals
        
        prediccion = modelo_multilineal$fitted.values
        
        #1- Independencia de los residuos
        print(dwtest(modelo_multilineal))   #test Durbin-Watson
        
        #2- E[residuos] = 0
        print(summary(modelo_multilineal$residuals))
        
        #3- Varianza constante de los residuos(Homocedasticidad)
        print(bptest(modelo_multilineal))
        
        # layout(matrix(c(1,2,3,4),10,10, byrow = T))
        plot(modelo_multilineal$fitted.values, rstudent(modelo_multilineal),
             main="Multi Fit Studentized Residuals",
             xlab="Predictions", ylab="Studentized Resid",
             ylim=c(-2.5,2.5))
        abline(h=0, lty=2)
        
        layout(matrix(c(1,2,3,4),10,10, byrow = T))
        plot(Ventas, resid,
             main="Residuales contra Ventas",
             xlab="Ventas", ylab="Residuos")
        abline(h=0, lty=2)
        
        #4- Distribucion Normal con media 0 y varianza constante
        layout(matrix(c(1,2,3,4),10,10, byrow = T))
        hist(resid, main="Histograma de Residuos")
        
        layout(matrix(c(1,2,3,4),10,10, byrow = T))
        qqnorm(resid)
        qqline(resid)
        shapiro.test(resid)
        
        #5- Las variables independientes del modelo no están correlacionadas
        cor.test(Gasto, Mes, method = "pearson")
}

bestAdjustedModel <- function()
{
        data <- read.csv("Advertising.csv")
        attach(data)
        cor(data)
        pairs(data)
        multi.fit = lm(sales ~ newspaper + radio + TV)
        summary(multi.fit)
        
        #1- Independencia de los residuos
        dwtest(modelo_multilineal)   #test Durbin-Watson
        
        #2- E[residuos] = 0
        mean(multi.fit$residuals)
        sum(multi.fit$residuals)
        
        #3- Varianza constante de los residuos(Homocedasticidad)
        bptest(multi.fit)
        
        #4- Distribucion Normal con media 0 y varianza constante
        shapiro.test(resid)
        
        #5- Las variables independientes del modelo no están correlacionadas
        cor.test(TV, radio, method = "pearson")
        cor.test(TV, newspaper, method = "pearson")
        cor.test(radio, newspaper, method = "pearson")
        
        #Se elimna newspaper porque es la que tiene mayor p-valor
        multi.fit = lm(sales ~ radio + TV)
        summary(multi.fit)
        
        #1- Independencia de los residuos
        dwtest(modelo_multilineal)   #test Durbin-Watson
        
        #2- E[residuos] = 0
        mean(multi.fit$residuals)
        sum(multi.fit$residuals)
        
        #3- Varianza constante de los residuos(Homocedasticidad)
        bptest(multi.fit)
        
        #4- Distribucion Normal con media 0 y varianza constante
        shapiro.test(resid)
        
        #5- Las variables independientes del modelo no están correlacionadas
        cor.test(TV, radio, method = "pearson")
        
}

#############
#Ejercicio 1#
#############
solveSimpleRegresion()

#############
#Ejercicio 2#
#############
solveMultipleRegresion()

#############
#Ejercicio 3#
#############
bestAdjustedModel()
