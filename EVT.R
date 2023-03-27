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
install_if_missing("purrr")
install_if_missing("ggplot2")
install_if_missing("ggthemes")

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
library(purrr)
library(ggplot2)
library(ggthemes)

# Defining a function for plotting the time series of the stocks and returns
plot_series <- function(list_of_stocks) {

    list_of_returns <- lapply(list_of_stocks, function(x) diff(log(x)) * 100)
    # Plotting both the stock prices and the returns over time
    lapply(list_of_stocks, function(x) print(plot(x, main = paste0("Stock of ", names(x), " over time"),
                                            xlab = "Time", ylab = "Yields")))
    lapply(list_of_returns, function(x) print(plot(x, main = paste0("Returns of ", names(x), " over time"),
                                        xlab = "Time", ylab = "Yields")))
    hist_plot <- function(series_df) {
        # Basic histogram
        ggplot(series_df, aes(x = series_df[, ncol(series_df)])) + 
        geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
        geom_density(alpha = .2, fill = "#FF6666") +
        ggtitle(paste0("Histogram of ", names(series_df))) +
        xlab("Returns") +
        ylab("Density")
        }

    lapply(list_of_returns, function(x) hist_plot(x))
    }

# Defining a function to obtain some descriptive statistics of the returns
desc_stats <- function(returns) {

    # Obtaining some descriptive statistics
    returns <- na.omit(returns)
    sample_size <- length(returns)
    returns_mean <- mean(returns)
    returns_median <- median(returns)
    returns_max <- max(returns)
    returns_min <- min(returns)
    returns_sd <- sd(returns)
    returns_kurtosis <- (1 / length(returns)) *
                        sum((returns - mean(returns))^4) / sd(returns)^4 - 3
    jb_test <- jarque.test(as.vector(returns))
    jb_p_value <- jb_test$p.value
    returns_skewness <- skewness(returns)
    adf_returns <- adf.test(returns)
    adf_p_value = adf_returns$p.value[1]
    st_list <- list(sample_size, returns_mean, returns_median, returns_max, returns_min,
                    returns_sd, returns_kurtosis, returns_skewness, jb_p_value,
                    adf_p_value)
    desc_stats <- data.frame(do.call(cbind, st_list))
    colnames(desc_stats) <- c("Sample size", "Mean", "Median", "Max", "Min", "Sd", "Kurtosis",
                             "Skewness", "Jarque-Bera", "ADF")
    rownames(desc_stats) <- c("returns")

    return(desc_stats)
    }

# Defining a function for calculating VaR and ES
market_risk_measure <- function(returns, conf_level, size_train, per_threshold) {

    # Calculating the VaR and the ES for the returns
    # Length of the returns vector
    T <- length(returns)

    # Splitting the dataset between training and testing sets
    train_set <- head(returns, round(length(returns) * size_train))
    h <- length(returns) - length(train_set)
    test_set <- tail(returns, h)

    # Getting the lengths of the training and testing sets
    outsample <- length(test_set)
    insample <- length(train_set)

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
    return(mrm_df)
    }

# Getting the time series data for the stocks of interest
codes <- c("sp500" = "^GSPC", "btc" = "BTC-USD", "gbp_usd" = "GBPUSD=X", 
           "gold" = "GC=F", "brent" = "BZ=F")
stocks_df <- data.frame()
for (i in 1:length(codes)) {
    stock_code <- as.character(codes[i])
    stock <- get_data(stock_code, start_date = "2000-01-01",
                      end_date = "2023-02-28")
    key <- names(which(codes == stock_code))
    stocks_df[[key]] <- stock
}

# Getting the series of stocks
for (code in c("BTC-USD", "GBPUSD=X", "GC=F", "BZ=F", "^GSPC")) {
    getSymbols(code, from = "2000-01-01", to = "2023-02-28")
}
sp500 <- `GSPC`$`GSPC.Adjusted`
btc_usd <- `BTC-USD`$`BTC-USD.Adjusted`
gbp_usd <- `GBPUSD=X`$`GBPUSD=X.Adjusted`
gold_usd <- `GC=F`$`GC=F.Adjusted`
brent_usd <- `BZ=F`$`BZ=F.Adjusted`

# Building a list with all the series of stocks to iterate over
all_stocks <- list(sp500, btc_usd, gbp_usd, gold_usd, brent_usd)

# Also building a list with all the returns series (log differences)
all_returns <- lapply(all_stocks, function(x) na.omit(diff(log(x)) * 100))

# Also, since we are interested in the extreme losses, we multiply the returns by (-1)
inv_returns <- lapply(all_returns, function(x) x * (-1))

# Calculate main descriptive statistics for the returns series
descriptive_stats <- data.frame()
for (returns in all_returns) {
    descriptive_stats <- rbind(descriptive_stats, desc_stats(returns))
}
rownames(descriptive_stats) <- c("S&P500", "BTC/USD", "GBP/USD", "GOLD/USD", "Crude Oil Brent")
print(formattable(descriptive_stats))

# Plotting stocks over time, returns over time and histogram of returns for all series
plot_series(list_of_stocks = all_stocks)

# Calculating the optimal threshold
# Upper 10% rule of DuMounchel
u_dumounchel <- lapply(inv_returns, function(x) quantile(x, 0.90))

# Ferreira et al. (2003) rule: Extreme values as those above sqrt(n)
u_ferreira <- lapply(inv_returns, function(x) quantile(x, 1 - sqrt(length(x)) / length(x)))

# Rule of Loretan and Philips (1994) for calculating the optimal threshold
u_loretan <- lapply(inv_returns, function(x) quantile(x,
                                 1 - (length(x)ˆ(2/3) / log(log(length(x)))) / length(x)))

# Mean Excess Plot or Mean Residual Life Plot
mrlplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)), nt = 20)

# Parameter Stability Plot
tcplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)), nt = 20)

# Hill Plot
hillplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)))