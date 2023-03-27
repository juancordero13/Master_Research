# Installing the necessary packages
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}
install_if_missing("tseries")
install_if_missing("quantmod")
install_if_missing("evmix")
install_if_missing("formattable")
install_if_missing("moments")
install_if_missing("forecast")
install_if_missing("fGarch")
install_if_missing("evir")
install_if_missing("openxlsx")

# Loading the necessary libraries
library(quantmod)
library(evmix)
library(formattable)
library(moments)
library(tseries)
library(forecast)
library(fGarch)
library(evir)
library(openxlsx)

# Getting the data from yahoo finance via API
getSymbols("^GSPC", src = "yahoo", from = "2000-01-03", to = "2021-12-31")
stock <- GSPC$GSPC.Adjusted

# Transforming the stock prices into returns by taking log differences
returns <- na.omit(diff(log(stock)) * 100)

# Length of the returns vector
T <- length(returns)

# Splitting the dataset between training and testing sets
train_set <- head(returns, round(length(returns) * 0.75))
h <- length(returns) - length(train_set)
test_set <- tail(returns, h)

# Getting the lengths of the training and testing sets
outsample <- length(test_set)
insample <- length(train_set)

# Setting the confidence level and threshold percentile
conf_level <- 0.975
per_threshold <- 0.9

# Defining empty list where we'll store the results later
VaR = c()
ES = c()

# Looping over the returns
for(j in 0:(outsample - 1)) {

    print(paste0("Iteration ", j+1, " of ", outsample, " iterations"))

    # Training set that will be used in this iteration
    return_train <- returns[(1+j):(insample+j)]

    # Fitting an APARCH(1,1) model to the training set
    # Using as a conditional distribution the normal distribution
    aparch_fit <- garchFit(~aparch(1,1), data = return_train, cond.dist = c("norm"))

    # Storing the parameters of the fitted model
    mu <- coef(aparch_fit)[1]
    omega <- coef(aparch_fit)[2]
    alpha <- coef(aparch_fit)[3]
    gamma <- coef(aparch_fit)[4]
    beta <- coef(aparch_fit)[5]
    delta <- coef(aparch_fit)[6]

    # Getting the residuals of the fitted model as value - conditional mean
    residuals <- return_train - mu
    # Residual value for the last observation
    residual_t <- residuals[insample]

    # Storing the conditional sd of the fitted model
    sigmas <- volatility(aparch_fit)
    # Conditional sd for the last observation
    sigma_t <- sigmas[insample]

    # Plotting the conditional volatility of the model
    plot(sigmas, type = "l", main = "Conditional volatility of the APARCH(1,1) model",
         xlab = "Time", ylab = "Conditional volatility")

    # Forecasting the conditional volatility of the model one day ahead
    frcst_sigma <- (omega + 
                    alpha * (abs(residual_t) - gamma * residual_t) ^ delta +
                    beta * sigma_t ^ delta) ^ (1 / delta)

    # Standarizing the returns
    z <- (return_train - mu) / sigmas
    # Multiplying the returns by (-1)
    # We are interested in the extreme losses, not in the extreme gains
    z <- z * (-1)
    # Getting the threshold value
    threshold <- quantile(z, per_threshold)

    # Fitting a Generalized Pareto Distribution to the exceedances
    gpd_fit <- gpd(z, threshold)

    # Getting the GPD quantile
    gpd_quantile <- riskmeasures(gpd_fit, conf_level)[,2]
    # And the Expected Shortfall
    gpd_es <- riskmeasures(gpd_fit, conf_level)[,3]

    # Quantifying market risk: forecasting the VaR and ES
    frcst_var <- mu + frcst_sigma * gpd_quantile * (-1)
    frcst_es <- mu + frcst_sigma * gpd_es * (-1)

    # Storing the results of each iteration in a list
    VaR <- c(VaR, frcst_var)
    ES <- c(ES, frcst_es)
    }

mrm_df <- data.frame(cbind(VaR, ES))

# Exporting the results to an excel file
write.xlsx(mrm_df, "VaR_sp500.xlsx")

# Plotting the results
plot(VaR, type = "l", main = "VaR and ES of the S&P 500 index",
     xlab = "Time", ylab = "VaR and ES")
lines(ES, col = "red")
