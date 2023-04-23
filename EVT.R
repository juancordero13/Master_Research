####################################################################################################
#######################-- BLOCK 0: SETTING UP THE ENVIRONMENT --####################################
####################################################################################################

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
install_if_missing("eva")

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
library(Metrics)
library(eva)


####################################################################################################
###########################-- BLOCK 1: DEFINING FUNCTIONS --########################################
####################################################################################################

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
# The function also calculate the GPD quantiles obtained for each optimal threshold value
market_risk_measure <- function(returns, conf_level, size_train, threshold_value) {

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
    VaR <- c()
    ES <- c()
    gpd_quantiles <- c()
    
    # Looping over the returns
    for(j in 0:(outsample - 1)) {

        print(paste0("Iteration ", j+1, " of ", outsample, " iterations"))

        # Training set that will be used in this iteration
        return_train <- returns[(1+j):(insample+j)]

        # Fitting an APARCH(1,1) model to the training set
        # Using as a conditional distribution the normal distribution
        aparch_fit <- garchFit(~aparch(1,1), data = return_train, cond.dist = c("norm"),
            trace = FALSE)

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
        # plot(sigmas, type = "l", main = "Conditional volatility of the APARCH(1,1) model",
        #     xlab = "Time", ylab = "Conditional volatility")

        # Forecasting the conditional volatility of the model one day ahead
        frcst_sigma <- (omega + 
            alpha * (abs(residual_t) - gamma * residual_t) ^ delta +
            beta * sigma_t ^ delta) ^ (1 / delta)

        # Standarizing the returns
        z <- (return_train - mu) / sigmas
        # Multiplying the returns by (-1)
        # We are interested in the extreme losses, not in the extreme gains
        z <- z * (-1)

        # Fitting a Generalized Pareto Distribution to the exceedances
        gpd_fit <- gpd(z, threshold_value)

        # Getting the GPD quantile
        gpd_quantile <- riskmeasures(gpd_fit, conf_level)[,2]
        # And the Expected Shortfall
        gpd_es <- riskmeasures(gpd_fit, conf_level)[,3]

        # Quantifying market risk: forecasting the VaR and ES
        frcst_var <- mu + frcst_sigma * gpd_quantile * (-1)
        frcst_es <- mu + frcst_sigma * gpd_es * (-1)

        # Storing the results of each iteration in a list
        gpd_quantiles <- c(gpd_quantiles, gpd_quantile)
        VaR <- c(VaR, frcst_var)
        ES <- c(ES, frcst_es)
        }

    # Storing the results in a dataframe
    mrm_df <- data.frame(cbind(VaR, ES))

    # Getting the main descriptive statistics of the results
    stats_gpd <- c(mean(gpd_quantiles), sd(gpd_quantiles), min(gpd_quantiles), max(gpd_quantiles))
    stats_var <- c(mean(mrm_df$VaR), sd(mrm_df$VaR), min(mrm_df$VaR), max(mrm_df$VaR))
    stats_es <- c(mean(mrm_df$ES), sd(mrm_df$ES), min(mrm_df$ES), max(mrm_df$ES))

    stats_mrm <- data.frame(cbind(stats_gpd, stats_var, stats_es))

    return(list(gpd_quantiles, mrm_df, stats_mrm))
    }

# Defining a function for application Li et al (2014) RMSE based method to select threshold
Li_method <- function(returns, min_threshold, range_length, max_threshold) {

    # Making a list of thresholds of length 100 from the initial threshold to the 99.9th percentile
    u_seq <- seq(quantile(returns, min_threshold), quantile(returns, max_threshold), 
        length.out = range_length)

    # Creating an empty list to store later the RMSE values
    rmse_values <- rep(NA, length(u_seq))

    # Iterating over the thresholds
    for (i in seq_along(u_seq)) {
        # cat("ITERATION NUMBER: ", i,"\n")

        # For each threshold, calculate exceedances,
        u <- u_seq[i]
        exceedances <- returns[returns > u]

        # fitting the gpd to these exceedances
        # print("Fitting the GPD... \n")
        # cat("Number of exceedances: ", length(exceedances),"\n")
        # cat("Threshold: ", u,"\n")
        gpd_instance <- evir::gpd(data = exceedances,
                            threshold = u)

        # calculate the fitted values
        # print("Getting the fitted values...\n")
        fitted_values <- evir::pgpd(exceedances,
                              xi = gpd_instance$par.ests["xi"],
                              beta = gpd_instance$par.ests["beta"])

        # print(qqplot(exceedances, fitted_values))
        # abline(0,1)

        # calculate the Root Mean Squared Error (RMSE)
        # print("Calculating the RMSE...\n")
        rmse_values[i] <- rmse(ecdf(exceedances)(exceedances), fitted_values)

        print("--------------------------------------------------------------------------------")
        cat("Threshold value: ", u_seq[i],"\n")
        cat("RMSE: ", rmse_values[i],"\n")
        cat("Number of exceedances: ", length(exceedances),"\n")
        # cat(plot(fitted_values, ecdf(exceedances)(exceedances)), "\n")
    }

    # Retrieving the thresholds that minimizes the RMSE
    best_u <- u_seq[which.min(rmse_values)]

    print(ggplot(data.frame(u_seq, rmse_values), aes(x = u_seq, y = rmse_values)) +
    geom_line() +
    geom_vline(xintercept = best_u, linetype = "dashed", color = "red") +
    xlab("Threshold") +
    ylab("RMSE"))


    return(best_u)
    }

# Defining a function for application of V. Choulakian and M. A. Stephens (2001) method based
# on the Cramer Von Mises and Anderson Darling goodness of fit tests
Choukalian_method <- function(returns, min_threshold, range_length, max_threshold) {

    p_values <- c()
    # Making a list of thresholds of length 100 from the initial threshold to the 99.9th percentile
    u_seq <- seq(quantile(returns, min_threshold), quantile(returns, max_threshold), 
        length.out = range_length)

    print(u_seq)
    for (i in seq_along(u_seq)) {
        
        cat("Iteration number: ", i,"\n")

        # For each threshold, calculate exceedances,
        u <- u_seq[i]
        exceedances <- returns[returns > u]

        # Applying the Cramer Von Mises test for the exceedances assuming they follow a GPD
        cvm_test <- gpdCvm(exceedances, bootstrap = TRUE,
            bootnum = 5, allowParallel = TRUE, numCores = 8)
        
        cat("Threshold value: ", u,"\n")
        cat("p-value: ", cvm_test$p.value,"\n")
        print(cvm_test)

        # This does not make sense at the moment as p-values are highly unpredictable
        # The moment the p-value is greater than 0.1, we stop the search for the threshold
        # if (cvm_test$p.value >= 0.1) {
        #     print("We found the optimal threshold before the loop was ended.")
        #     break
        # }

    # Getting the p-values for each threshold
    p_values <- c(p_values, cvm_test$p.value)
    
    }

    # Plotting the p-values against the range of threholds
    print(ggplot(data.frame(u_seq, p_values), aes(x = u_seq, y = p_values)) +
        geom_line() + xlab("Thresholds") + ylab("P-values"))
}

####################################################################################################
################-- BLOCK 2: RETRIEVING, PROCCESSING AND PLOTTING THE DATA --########################
####################################################################################################

# Getting the series of stocks vía Yahoo Finance API
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

####################################################################################################
#########-- BLOCK 3: CALCULATING OPTIMAL THRESHOLDS ACCORDING TO METHODOLOGIES --###################
####################################################################################################

# We standarize the returns and multiply by (-1) since we are interested in the extreme losses.
# Here, we should standarize the returns with conditional sd instead of unconditional sd.
transformed_returns <- lapply(all_returns, function(x) (x - mean(x)) / sd(x) * (-1))

# Upper 10% rule of DuMounchel
u_dumounchel <- lapply(transformed_returns, function(x) quantile(x, 0.90))

# Rule of Loretan and Philips (1994) for calculating the optimal threshold
u_loretan <- lapply(transformed_returns, function(x) quantile(x,
                                 1 - (length(x)**(2/3) / log(log(length(x))) / length(x))))

# Ferreira et al. (2003) rule: Extreme values as those above sqrt(n)
u_ferreira <- lapply(transformed_returns, function(x) quantile(x, 1 - sqrt(length(x)) / length(x)))

# Mean Excess Plot or Mean Residual Life Plot
par(mfrow = c(1, 1))
lapply(transformed_returns, function(x) print(mrlplot(as.vector(x),
    tlim = c(quantile(x, 0.8), quantile(x, 0.99)),
    nt = 20,legend.loc = NULL)))

# Storing the optimal values of the Mean Residual Life Plot manually
u_mrlplot <- c(1.5, 1.1, 1.3, 1.1, 1.55)

# Parameter Stability Plot
par(mfrow = c(2, 1))
lapply(transformed_returns, function(x) print(tcplot(as.vector(x),
    tlim = c(quantile(x, 0.8), quantile(x, 0.99)),
    nt = 20, legend.loc = NULL)))
# Storing the optimal values of the Parameter Stability Plot manually
u_psplot <- c(1.5, 1.2, 1.3, 1.1, 1.55)

# Hill Plot
par(mfrow = c(1, 1))
lapply(transformed_returns, function(x) print(hillplot(as.vector(x),
    tlim = c(quantile(x, 0.8), quantile(x, 0.99)),
    legend.loc = NULL)))
# Storing the optimal values of the Hill Plot manually
u_hillplot <- c(1.5, 1.17, 1.29, 1.45, 1.2)

# Calling the function Li_rmse above to calculate optimal thresholds for each stock
u_li_rmse <- lapply(inv_returns, function (x) Li_method(as.vector(x),
    min_threshold = 0.30, range_length = 100))

# Calling the function Choukalian_method above to calculate optimal thresholds for each stock
u_choukalian <- Choukalian_method(river_data, min_threshold = 0.00,
    range_length = 20, max_threshold = 0.99)

# Storing all the optimal thresholds in a dataframe (rows = stocks, columns = methodologies)
opt_thresholds <- data.frame(cbind(u_ferreira, u_loretan, u_dumounchel,
    u_li_rmse, u_mrlplot, u_psplot, u_hillplot, u_choukalian))
rownames(opt_thresholds) <- c("S&P500", "BTC/USD", "GBP/USD", "GOLD/USD", "Crude Oil Brent")

####################################################################################################
############-- BLOCK 4: ESTIMATING MARKET RISK MEASURES FOR EACH THRESHOLD --#######################
####################################################################################################

# Getting the mean, sd, max and min of the VaR and ES for each stock
# Using the Upper 10% rule of DuMounchel
start_time <- Sys.time()

# Calculating VaR and ES for each stock and methodology

# Creating an excel file (workbook) where we will store the results
# There will be a sheet for each methodology, where rows = stats and columns = stocks
xlsx_workbook <- createWorkbook()

# Looping through the optimal thresholds (one for each methodology)
# and through the returns series and estimating VaR and ES
for (j in 1:ncol(opt_thresholds)) {

    cat("Applying methodology ",j,"of ",ncol(opt_thresholds),". \n")

    mrm_data <- list()
    mrm_stats <- list()
    pareto_quantiles <- list()
    for (i in 1:length(all_returns)) {
        mrm <- market_risk_measure(returns = all_returns[[i]],
                                   threshold_value = as.numeric(opt_thresholds[i, j]),
                                   conf_level = 0.99, 
                                   size_train = 0.75)
        
        pareto_quantiles <- c(pareto_quantiles, mrm[[1]])
        mrm_data <- c(mrm_data, mrm[[2]])
        mrm_stats <- c(mrm_stats, mrm[[3]])
    }

    mrm_stats <- data.frame(mrm_stats)
    colnames(mrm_stats) <- c("Quantiles S&P500", "VaR S&P500", "ES S&P500", "Quantiles BTC/USD", "VaR BTC/USD", "ES BTC/USD",
        "Quantiles GBP/USD", "VaR GBP/USD", "ES GBP/USD", "Quantiles GOLD/USD", "VaR GOLD/USD", "ES GOLD/USD",
        "Quantiles Crude Oil Brent", "VaR Crude Oil Brent", "ES Crude Oil Brent")
    rownames(mrm_stats) <- c("Mean", "SD", "Min", "Max")

    addWorksheet(xlsx_workbook, sheetName = colnames(opt_thresholds)[j])
    writeData(xlsx_workbook, sheet = colnames(opt_thresholds)[j], x = mrm_stats,
              row.names = TRUE)

    saveWorkbook(xlsx_workbook,
                 file = "/Users/juancordero/Desktop/My_GitHub/Master_Research/Output/results.xlsx",
                 overwrite = TRUE)
}

end_time <- Sys.time()
cat("It took ", (end_time - start_time) / 60, " minutes to run the code. \n")
