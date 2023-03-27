# Installing the necessary packages.
install.packages("mlbench")
install.packages("ggpubr")
install.packages("PerformanceAnalytics")
install.packages("dplyr")
install.packages('rsample')
install.packages('boostrap')
install.packages('boot')
install.packages('rsq')
install.packages("nortest")
install.packages('car')
install.packages('onewaytests')
install.packages('lmtest')
install.packages('rcompanion')

analisis_GLM = function(data, dep_variable, model_selection) {
  library(corrplot)
  library(RColorBrewer)
  library(ggpubr)
  library(PerformanceAnalytics)
  library(dplyr)
  library(rsample)
  library(boot)
  library(nortest)
  library(car)
  library(onewaytests)
  library(lmtest)
  library(rcompanion)
  
  num_data = data.frame(matrix(NA, nrow(data), 0))
  for (i in 1:ncol(data)) {
    if (is.numeric(data[,i])) {
      num_data[names(data[i])] = data[,i]
    }
  } # un dataframe a partir del original quitando variables categóricas
  
  # Gráfico de dispersión genérico (si la variable es categórica, construye
  # un diagrama de barras).
  for (i in 1:ncol(data)) {
    plot(data[,dep_variable] ~ data[,i], ylab = dep_variable, 
         xlab = names(data)[i], pch = 16, cex = 0.75)
  }
  
  # Gráfico de correlación entre las variables numéricas.
  corrplot(cor(num_data))
  
  # Diagramas de caja para todas las variables.
  for (i in 1:ncol(num_data)) {
    boxplot(num_data[,i], xlab = names(num_data)[i])
  }
  
  # Un gráfico de dispersión más sofisticado.
  if (is.numeric(data[,dep_variable])) {
    for (i in 1:ncol(num_data)) {
      print(ggscatter(num_data, x = names(num_data)[i], 
                      y = dep_variable, 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      ylab = dep_variable, xlab = names(num_data)[i], size = 0.75))
    }
  }
  
  # Gráfico de correlaciones, Gráfico de dispersión y diagrama de barras.
  chart.Correlation(num_data, histogram=TRUE, pch=19)
  
  # Distinguimos si la variable dependente es numérica o categórica.
  if (is.factor(data[,dep_variable])) {
    regresion_logit = glm(data[,dep_variable] ~., 
                          data = data[,!(names(data) %in% dep_variable)],
                          family = 'binomial')
    print(summary(regresion_logit)) # regresión logit.
    
    # Error de clasificación.
    probabilities = regresion_logit %>% predict(data, type = "response")
    predicted.classes = ifelse(probabilities > 0.5, "good", "bad")
    cat('La precisión del modelo es',mean(predicted.classes == data[,dep_variable]),'\n')
    cat('El error de clasificación es',1-mean(predicted.classes == data[,dep_variable]),'\n')
    
    # Pseudos R^2.
    print(nagelkerke(regresion_logit, restrictNobs = TRUE))
    
    # Gráficos de los residuos.
    plot(regresion_logit)
    
    # Test de normalidad Shapiro-Wilk.
    print(shapiro.test(regresion_logit$residuals))
    if (shapiro.test(regresion_logit$residuals)$p.value > 0.05) {
      cat('Los residuos son normales al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son normales al 95% de confianza.','\n')
    }
    
    # Test de normalidad Lilliefors.
    print(lillie.test(regresion_logit$residuals))
    if (lillie.test(regresion_logit$residuals)$p.value > 0.05) {
      cat('Los residuos son normales al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son normales al 95% de confianza.','\n')
    }
    
    # Test de homocedasticidad Breush-Pagan.
    print(bptest(regresion_logit))
    if (bptest(regresion_logit)$p.value > 0.05) {
      cat('Los residuos son homocedásticos al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son homocedásticos al 95% de confianza.','\n')
    }
    
    # Test de homocedasticidad de Brown-Forsythe.
    bf.test(regresion_logit, data = data)
    if (bf.test(data[,dep_variable]~.)$p.value > 0.05) {
      cat('Los residuos son homocedásticos al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son homocedásticos al 95% de confianza.','\n')
    }
    
    # Test de autocorrelación Breush-Godfrey.
    print(bgtest(regresion_logit))
    if (bgtest(regresion_logit)$p.value > 0.05) {
      cat('No existe autocorrelación serial al 95% de confianza.','\n')
    } else {
      cat('Existe autocorrelación serial al 95% de confianza.','\n')
    }
    
    # Test de Durbin-Watson.
    print(dwtest(regresion_logit))
    if (dwtest(regresion_logit)$p.value > 0.05) {
      cat('No existe autocorrelación serial de primer orden al 95% de confianza.','\n')
    } else {
      cat('Existe autocorrelación serial de primer orden al 95% de confianza.','\n')
    }
    
    # Selección de modelos: Forward, Backward, Stepwise.
    if (model_selection == 'forward') {
      min_model = glm(data[,dep_variable]~ 1, 
                      data = data[,!(names(data) %in% dep_variable)], family = 'binomial')
      selected_reg <- step(min_model, scope = formula(regresion_logit), 
                           k = log(length(data[,dep_variable])),
                           direction = "forward")
      summary(selected_reg)
    } else if (model_selection == 'backward') {
      selected_reg <- step(regresion_logit, 
                           scope = list(upper = regresion_logit$formula, 
                                        lower = ~1), 
                           k = log(length(data[,dep_variable])),
                           direction = "backward")
      summary(selected_reg)
    } else if (model_selection == 'stepwise') {
      selected_reg <- step(regresion_logit, 
                           scope = list(upper = regresion_logit$formula, 
                                        lower = ~1), 
                           k = log(length(data[,dep_variable])),
                           direction = "both")
      summary(selected_reg)
    }
    
    # Cálculo de los coeficientes boostrap.
    coeficientes = function(info, index){
      coef(glm(data[,dep_variable] ~., 
               data = data[,!(names(data) %in% dep_variable)], subset = index,
               family = 'binomial'))
    }
    boostrap_coef = boot(data, coeficientes, 30)
    boostrap_coef
    boostrap_st = as.data.frame(summary(boostrap_coef))
    boostrap_st = cbind(boostrap_st,
                        t_value = abs(boostrap_st$original)/boostrap_st$bootSE,
                        p_value = pt(q = abs(boostrap_st$original)/boostrap_st$bootSE, 
                                     df=length(data[,dep_variable])-length(boostrap_st$original),
                                     lower.tail=FALSE))
    print(boostrap_st)
    
  } else {
    
    # Regresión lineal.
    regresion = glm(data[,dep_variable] ~., 
                           data = data[,!(names(data) %in% dep_variable)])
    print(summary(regresion))
    
    # Estadísticos de la bondad del modelo.
    cat('R-squared:',with(summary(regresion), 1 - deviance/null.deviance),'\n')
    cat('Adjusted R-squared:',with(summary(regresion),
                              1 - (deviance/(nobs(regresion)-
                              length(coef(regresion))))/
                              (null.deviance/(nobs(regresion)-1))),'\n')
    cat('Mean Squared Error:',mean(regresion$residuals^2),'\n')
    
    # Gráfico de los residuos.
    plot(regresion)
    
    # Test de normalidad Shapiro-Wilk.
    print(shapiro.test(regresion$residuals))
    if (shapiro.test(regresion$residuals)$p.value > 0.05) {
      cat('Los residuos son normales al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son normales al 95% de confianza.','\n')
    }
    
    # Test de normalidad Lilliefors.
    print(lillie.test(regresion$residuals))
    if (lillie.test(regresion$residuals)$p.value > 0.05) {
      cat('Los residuos son normales al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son normales al 95% de confianza.','\n')
    }
    
    # Test de homocedasticidad Breush-Pagan.
    print(bptest(regresion))
    if (bptest(regresion)$p.value > 0.05) {
      cat('Los residuos son homocedásticos al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son homocedásticos al 95% de confianza.','\n')
    }
    
    # Test de homocedasticidad de Brown-Forsythe.
    bf.test(regresion, data = data)
    if (bf.test(regresion)$p.value > 0.05) {
      cat('Los residuos son homocedásticos al 95% de confianza.','\n')
    } else {
      cat('Los residuos no son homocedásticos al 95% de confianza.','\n')
    }
    
    # Test de autocorrelación Breush-Godfrey.
    print(bgtest(regresion))
    if (bgtest(regresion)$p.value > 0.05) {
      cat('No existe autocorrelación serial al 95% de confianza.','\n')
    } else {
      cat('Existe autocorrelación serial al 95% de confianza.','\n')
    }
    
    # Test de Durbin-Watson.
    print(dwtest(regresion))
    if (dwtest(regresion)$p.value > 0.05) {
      cat('No existe autocorrelación serial de primer orden al 95% de confianza.','\n')
    } else {
      cat('Existe autocorrelación serial de primer orden al 95% de confianza.','\n')
    }
    
    # Selección de modelos: forward, backward, stepwise.
    if (model_selection == 'forward') {
      min_model = glm(data[,dep_variable]~ 1, 
                      data = data[,!(names(data) %in% dep_variable)])
      selected_reg <- step(min_model, scope = formula(regresion), 
                            k = log(length(data[,dep_variable])),
                            direction = "forward")
      summary(selected_reg)
    } else if (model_selection == 'backward') {
      selected_reg <- step(regresion, 
                              scope = list(upper = regresion$formula, 
                                           lower = ~1), 
                                           k = log(length(data[,dep_variable])),
                              direction = "backward")
      summary(selected_reg)
    } else if (model_selection == 'stepwise') {
      selected_reg <- step(regresion, 
                              scope = list(upper = regresion$formula, 
                                           lower = ~1), 
                                           k = log(length(data[,dep_variable])),
                              direction = "both")
      summary(selected_reg)
    }
    
    # Cálculo de los coeficientes boostrap.
    coeficientes = function(info, index){
      coef(glm(data[,dep_variable] ~., 
               data = data[,!(names(data) %in% dep_variable)], subset = index))
    }
    boostrap_coef = boot(data, coeficientes, 30)
    boostrap_coef
    boostrap_st = as.data.frame(summary(boostrap_coef))
    boostrap_st = cbind(boostrap_st,
                        t_value = abs(boostrap_st$original)/boostrap_st$bootSE,
                        p_value = pt(q = abs(boostrap_st$original)/boostrap_st$bootSE, 
                                     df=length(data[,dep_variable])-length(boostrap_st$original),
                                     lower.tail=FALSE))
    print(boostrap_st)
    
  }
}

library(mlbench)
data("BostonHousing")
analisis_GLM(BostonHousing, "medv", 'forward')
datos <- readRDS("Your_path/30184602-german.balanceado.RDS")
analisis_GLM(datos, 'class', 'stepwise')