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

rendimientos <- as.vector(all_returns[[1]])
rendimientos <- as.numeric(rendimientos)
rendimientos <- rendimientos * (-1)

umbral <- quantile(rendimientos, 0.95)
extremos <- rendimientos[rendimientos > umbral]
gpd_modelo <- gpd(rendimientos, umbral)

help(mrlplot)