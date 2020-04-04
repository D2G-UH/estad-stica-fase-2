
#library(lmtest)

solveRegresion <- function(x, y, titlex, titley, v, xl, yl)
{
  data = data.frame(x, y)
  
  print(cor(data, method = "pearson"))   #coeficiente de correlacion lineal
  cor.test(x, y, method = "pearson")
  
  plot(x, y, xlab = titlex, ylab = titley, xlim = xl, ylim = yl)
  modelo_lineal = lm(y ~ x, data)
  abline(modelo_lineal, col = "blue")
  print(summary(modelo_lineal))
  
  #estimando los valores del vector v
  print(predict(object = modelo_lineal, newdata = data.frame(x = v), interval = "confidence", level = 0.95))

  residuos = modelo_lineal$residuals
  prediccion = modelo_lineal$fitted.values
  
  #Analisis de los supuestos del modelo lineal
  
  #1- Independencia de los residuos
    #print(dwtest(modelo_lineal))   #test Durbin-Watson
 
  #2-E[residuos] = 0
    print(summary(residuos))
    plot(prediccion, residuos)
  
  #3- Varianza constante de los residuos(Homocedasticidad)
    #print(bptest(modelo_lineal))
    
  #4- Distribucion Normal con media 0 y varianza constante
    hist(residuos, main = "Histograma")
    
    qqnorm(residuos)
    qqline(residuos)
    
    shapiro.test(residuos)
}

#############
#Ejercicio 1#
#############
x1 = c(-1, 0, 3, 7)
fx1 = c(2, 0, 4, 7)
#solveRegresion(x1, fx1, "x", "f(x)", c(1), c(-2, 7), c(0, 7) )
  
#############
#Ejercicio 2#
#############
x2 = c(-3, -1, 1, 3, 5, 7)
fx2 = c(14, 4, 2, 8, 22, 44)
#solveRegresion(x2, fx2, "x", "f(x)", c(0, 2), c(-4, 8), c(0,44))

#############
#Ejercicio 3#
#############
cargas = c(200, 400, 500, 700, 900, 1000)
alargamientos = c(60, 120, 150, 210, 260, 290)
solveRegresion(cargas, alargamientos, "cargas(gramos)", "Alargamientos", c(0, 2), c(100, 1000), c(50, 300))







