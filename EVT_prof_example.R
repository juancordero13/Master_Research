#***************************************************************************
# FICHERO EJECUTABLE PARA LA CUANTIFICACI?N DE RIESGO DE MERCADO  
#        Value at Risk (VaR) and Expected Shrotfall (ES) 
# 
# METODO. TEORIA DE VALORES EXTREMOS CONDICIONAL (Cond. EVT)
# MODELO DE VOLATILIDAD: APARCH
# 
# Distributiones: Normal   : "norm" 
#                 Student-t:  "std"
#                 Student-t: "sstd"
#                 GED      :  "ged"
#                 SGED     :  "sged"
#
#
# Autor: Sonia Benito-Muela
# Fecha: Abril-2018
# *************************************************************************


#**************************************************************************
# PRIMERO: cargar datos
#**************************************************************************

x = read.table ("C:/PAPER/ENCURSO/COMPARATIVA/DATOS/IBEX35.txt")
yield = x[,1]   # vector columna

#**************************************************************************
# SEGUNDO: Datos Exogenos
#**************************************************************************

T = length(yield)

out = (1022+250)         #  number of outsample data 
insample = T-out         #  number of insample data
probability =  0.975     #  confidence level=(1-alpha)
umbral=0.90              #  The threshold

#**************************************************************************
# TERCERO: FORECAST VaR and ES -1 DAY AHEAD at x probability 
#          in the outsample period
#**************************************************************************

library(fGarch)
require(evir)

VAR = 0
ES  = 0
VAR_NORM = 0 
ES_NORM  = 0

for(j in 0:(out-1)) {return <- yield[(1+j):(insample+j)]

#**************************************************************************
# EstimaciOn de la volatilidad de los rendimientos 
# Modelo de volatilidad: GARCH : ~garch(1,1)
#                        APARCH: ~aparch(1,1)
#
# Distributiones: Normal   : "norm" 
#                 Student-t:  "std"
#                 Student-t: "sstd"
#                 GED      :  "ged"
#                 SGED     :  "sged"
#**************************************************************************
modelo <- garchFit(~aparch(1,1), data=return, cond.dist=c("norm"))

coeficientes <- coef(modelo)         # Coeficient of the APARCH model  
sigmas       <- volatility(modelo)   # Conditional standard deviation (Cond.SD)
plot(type='l',sigmas)

#**************************************************************************
#  CUARTO: Forecast volatility (APARCH) -1 day ahead
#
#**************************************************************************

mu       <- coeficientes[1:1]   # Unconditional mean of the return
residuos <- return-mu           # Innovations
residuo  <- residuos[insample]  # Last innovations data

omega    <- coeficientes[2:2]       # constant of the APARCH model
alpha    <- coeficientes[3:3]       # 
gamma    <- coeficientes[4:4]       # gamma: leverage effect
beta     <- coeficientes[5:5]       # beta: persistence in volatiliy
delta    <- coeficientes[6:6]  


sigma_t              <- sigmas[insample];   # Last canditional standard deviation
forecast_sigma_delta <- omega + alpha*(abs(residuo)-gamma*residuo)^delta +beta*sigma_t^delta; 

forecast_sigma       <- forecast_sigma_delta^(1/delta); # forecast 1 day ahead Cond.SD


#**************************************************************************
#  QUINTO: STANDARDIZE RETURN AND THRESHOLD 
#
#**************************************************************************

z <- (return-mu)/sigmas;         # Standardize return
z <- z*(-1)                      # Desplazamos al cola izquierda a la derecha

Threshold <- quantile(z, umbral) # Threshold return

#**************************************************************************
#  SEXTO: FIX A GNERALIZE PARETO DISTRIBTION (GPD) 
#
#************************************************************************

results         <- gpd(z,Threshold)   # Fix a PGD
quantil_GPD <- riskmeasures(results,probability)[,2]    # Quantil of GPD
es_GPD      <- riskmeasures(results,probability)[,3]    # Espected shorfall 


#****************************************************************************
# SEPTIMO:  QUANTIFYING RISK: VaR and ES
#
#****************************************************************************

var         <- mu + forecast_sigma*quantil_GPD*(-1)
es          <- mu + forecast_sigma*es_GPD*(-1)

VAR         <- cbind(VAR, var)
ES          <- cbind(ES, es)}

VAR_NORM = VAR[,2:(out+1)]
ES_NORM  = ES[,2:(out+1)]

save (VAR_NORM, file="c:/PAPER/ENCURSO/COMPARATIVA/R/IBEX35/VAR_ETV_NORM.txt", ascii=TRUE)
save (ES_NORM, file="c:/PAPER/ENCURSO/COMPARATIVA/R/IBEX35/ES_ETV_NORM.txt", ascii=TRUE)

