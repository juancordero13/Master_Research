# STEP 1: INSTALLING THE NECESSARY PACKAGES AND LIBRARIES

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
install_if_missing("evir")

# Loading 1the necessary libraries
library(quantmod)
library(evmix)
library(formattable)
library(moments)
library(tseries)
library(evir)

# STEP 2: GETTING AND TRANSFORMING THE DATA FROM S&P 500 INDEX
# Getting the data from yahoo finance via API
getSymbols("^GSPC", src = "yahoo", from = "2000-01-03", to = "2021-12-31")
stock <- GSPC$GSPC.Adjusted

# Transforming the stock prices into returns by taking log differences
returns <- diff(log(stock)) * 100

# Plotting both the stock prices and the returns over time
plot(stock, type = "l", main = "S&P 500 Index", xlab = "Time",
     ylab = "Stock Price")
plot(returns, type = "l", main = "S&P 500 Index", xlab = "Time",
     ylab = "Yields(%)")

# Obtaining some descriptive statistics (same as in Sonia et al.(2022))
returns <- na.omit(returns)
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
st_list <- list(returns_mean, returns_median, returns_max, returns_min,
                returns_sd, returns_kurtosis, returns_skewness, jb_p_value,
                adf_p_value)
desc_stats <- data.frame(do.call(cbind, st_list))
colnames(desc_stats) <- c("Mean", "Median", "Max", "Min", "Sd", "Kurtosis",
                          "Skewness", "Jarque-Bera", "ADF")
rownames(desc_stats) <- c("returns")
print(formattable(desc_stats))

# Plotting the histogram of the returns along with the normal density
hist(returns, breaks = 100, main = "Histogram of the Returns vs Normal Density",
     xlab = "Returns", ylab = "Frequency", prob = TRUE)
curve(dnorm(x, mean = mean(returns), sd = sd(returns)), add = TRUE,
      col = "blue", lwd = 2)

# STEP 3: FITTING THE GPD TO THE RETURNS. REPLICATING SONIA ET AL.(2022) TABLE 3
# Since we are interested in the extreme losses, we multiply the returns by -1
x <- returns * (-1)
# Create empty lists to store the values afterwards
percentile_values <- list()
threshold_returns <- list()
exceedances_values <- list()
shape_values <- list()
scale_values <- list()
ks_test_values <- list()
se_shape_values <- list()
se_scale_values <- list()

for (quantile in c(95:99)) {
  list_name <- paste("q", quantile, sep = "")
  assign(list_name, list())
}

# For each percentile, we fit a GPD and obtain the quantiles with evmix package fgpd function
for (percentile in c(80:99) / 100) {
  gpd_fit <- fgpd(x = x, u = quantile(x, percentile))
  for (alpha in c(95:99) / 100) {
    gpd_quantile <- evmix::qgpd(p = alpha, sigmau = gpd_fit$sigmau,
                                xi = gpd_fit$xi)
    list_name <- paste0("q", alpha * 100)
    list_element <- get(list_name)
    list_element <- append(list_element, gpd_quantile)
    assign(list_name, list_element)
  }
  # Kolmogorov-Smirnov test to compare the empirical CDF with the fitted GPD
  ks_test <- ks.test(x = as.numeric(as.vector(x[x > quantile(x, percentile)] - quantile(x, percentile))),
                     y = "pgpd",
                     sigma = as.numeric(gpd_fit$sigmau), 
                     xi = as.numeric(gpd_fit$xi))

  # Creating a dataframe with all the values
  ks_test_values <- append(ks_test_values, ks_test$p.value)
  percentile_values <- append(percentile_values, percentile)
  threshold_returns <- append(threshold_returns, quantile(x, percentile))
  exceedances_values <- append(exceedances_values,
                               length(x[x > quantile(x, percentile)]))
  shape_values <- append(shape_values, as.numeric(gpd_fit$xi))
  scale_values <- append(scale_values, as.numeric(gpd_fit$sigmau))
  se_shape_values <- append(se_shape_values, as.numeric(gpd_fit$se[2]))
  se_scale_values <- append(se_scale_values, as.numeric(gpd_fit$se[1]))
}
data_list <- list(percentile_values, threshold_returns, exceedances_values,
                  shape_values, se_shape_values, scale_values, se_scale_values,
                  q95, q96, q97, q98, q99, ks_test_values)
for (column in data_list) {
  column <- as.numeric(column)
}
data_df <- data.frame(do.call(cbind, data_list))
colnames(data_df) <- c("Percentile", "Threshold_Returns", "Exceedances",
                       "Shape", "se_shape", "Scale", "se_scale", "q95",
                       "q96", "q97", "q98", "q99", "ks_pvalue")
print(formattable(data_df))

# STEP 4: REPLICATING PLOTS FROM SONIA ET AL.(2022) FIGURE 2
gpd_function <- function(y, sigma, xi) {
  1 - (1 + xi / sigma * y) ^ (-1 / xi)
}

# Plotting the empirical CDF of the exceedances against the fitted GPD
# for each percentile from 95th to 99th -- Sonia et al. (2022) Figure 2
plot_function <- function(percentile) {
  sigma <- as.numeric(data_df$Scale[percentile == data_df$Percentile])
  xi <- as.numeric(data_df$Shape[percentile == data_df$Percentile])
  y <- as.numeric(x[x > quantile(x, percentile)] - quantile(x, percentile))

  emp_cdf <- list()
  y_sorted <- sort(y)
  for (i in 1:length(y_sorted)) {
    emp_cdf <- append(emp_cdf, length(y_sorted[y_sorted <= y_sorted[i]]) / length(y_sorted))
  }

  plotting_function <- function (y) {
    return(gpd_function(y, xi = xi, sigma = sigma))
  }

  plot(plotting_function, from = 0, to = 4.2,
       main = paste0("Empirical CDF vs. GPD (u = ",percentile,")"),
       xlab = "Excess yield", ylab = "Fn(x-u)", lwd = 2)
  points(y_sorted, emp_cdf, col = "red", pch = 20)
}

for (i in c(95:99)) {
  plot_function(i / 100)
}

# Plotting GPD quantiles against the threshold for each percentile 
# from 95th to 99th -- Sonia et al. (2022) Figure 4
q_matrix <- as.matrix(data_df[, c("q95", "q96", "q97", "q98", "q99")])
plot(data_df$Threshold_Returns, q_matrix[,1], main = "Quantiles vs. Thresholds",
     xlab = "Thresholds", ylab = "Quantiles", col = 2, lwd = 2, type = "o",
     ylim = c(min(as.numeric(unlist(q_matrix))), max(as.numeric(unlist(q_matrix)))))
matlines(data_df$Threshold_Returns, q_matrix[, -1], lwd = 2, col = 3:6,
         type = "o", pch = 1:5)
legend("topleft", legend = c(95:99), lwd = 2, col = 2:6, title = "Percentiles")

# Plotting the MLE estimations of the shape and scale parameters
# along with confidence intervals -- Sonia et al. (2022) Figure 3
# Calculate the t-value for 95% confidence interval
t_value = as.numeric(qt(p = 0.975, df = 2))
# Create IC_low and IC_high for shape
data_df$IC_low_shape <- as.numeric(data_df$Shape) - t_value * as.numeric(data_df$se_shape)
data_df$IC_high_shape <- as.numeric(data_df$Shape) + t_value * as.numeric(data_df$se_shape)
# Create IC_low and IC_high for scale
data_df$IC_low_scale <- as.numeric(data_df$Scale) - t_value * as.numeric(data_df$se_scale)
data_df$IC_high_scale <- as.numeric(data_df$Scale) + t_value * as.numeric(data_df$se_scale)

# Plotting the shape and scale parameters along with confidence intervals
plot(data_df$Percentile, data_df$Shape, main = "Shape parameter vs. Percentile",
     xlab = "Percentile", ylab = "Shape", lwd = 2, type = "o",
     ylim = c(min(data_df$IC_low_shape), max(data_df$IC_high_shape)))
lines(data_df$Percentile, data_df$IC_high_shape, col = "red", lwd = 2)
lines(data_df$Percentile, data_df$IC_low_shape, col = "red", lwd = 2)
plot(data_df$Percentile, data_df$Scale, main = "Scale parameter vs. Percentile",
     xlab = "Percentile", ylab = "Scale", lwd = 2, type = "o",
     ylim = c(min(data_df$IC_low_scale), max(data_df$IC_high_scale)))
lines(data_df$Percentile, data_df$IC_high_scale, col = "red", lwd = 2)
lines(data_df$Percentile, data_df$IC_low_scale, col = "red", lwd = 2)


# From here on, the code needs to be revised, just trying some things out

# Mean Excess Plot or Mean Residual Life Plot
mrlplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)), nt = 20)

# Parameter Stability Plot
tcplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)), nt = 20)

# Hill Plot
hillplot(as.vector(x), tlim = c(quantile(x, 0.8), quantile(x, 0.99)))