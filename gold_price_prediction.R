library(readr)
library(dplyr)
library(janitor)
library(tidyverse)

data = read_csv("gold_prices.csv",
                na = c("", NA))

# specify column types
data = data |>
  clean_names() |>
  mutate_at(vars(7, 13, 19, 25, 35, 46, 68, 75, 81), as.integer) |>
  mutate(across(c(30, 36, 41, 47, 52, 57, 62, 69), as.factor)) |>
  select(-c(2:5, 7, 18, 80)) |> # since we're not evaluating any of the other characteristics of gold prices. as well, USO adj close = USO close, and DJI adj close = DJI close. so get rid of those
  rename("gold_adj_close" = "adj_close") |>
  mutate_at(vars(2:7, 9:12, 14:18, 20:23, 25:28, 31:34, 36:39, 42:45, 47:50, 52:55, 57:61, 64:68, 70:73), ~c(0, diff(log(.)))) |> # get log returns of gold. since we want to evaluate impact on movements of gold rather than the absolute price
  slice(-1) # remove the first row since we don't have the log returns


index = createDataPartition(data$gold_adj_close, p = 0.8, list = FALSE) # 80/20 split
train_data = data[index,]
test_data = data[-index,]

x_train = as.matrix(select(train_data, !gold_adj_close))
y_train = train_data$gold_adj_close
x_test = as.matrix(select(test_data, !gold_adj_close))
y_test = test_data$gold_adj_close

# any NA values
colSums(is.na(data))[colSums(is.na(data)) > 0] # none to worry about


## EXPLORATORY ANALYSIS
summary(data)



ggplot(og_data, aes(x = date, y = gold_adj_close)) +
  geom_line() +
  labs(x = "Date", y = "Gold Price", title = "Gold Price Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5))

# distribution of other variables
ggplot(data = data, mapping = aes(x = date, y = sp_ajclose)) + 
  geom_line() + 
  labs(x = "Date", y = "S&P 500 Index Price", title = "S&P 500 Index Time Series Plot")
# seeing an overall upward trend (appears to be pretty linear), except for a bit of flattening
# out at around the 2015 - 2016 period. Significant drop near the end, representing covid

ggplot(data = data, mapping = aes(x = date, y = dj_close)) + 
  geom_line() + 
  labs(x = "Date", y = "Dow Jones Index Price", title = "Dow Jones Index Time Series Plot")
# same overall pattern as s&p, but with a steeper climb past 2017. the overall trend appears to 
# rise more exponentially

ggplot(data = data, mapping = aes(x = date, y = eg_ajclose)) + 
  geom_line() + 
  labs(x = "Date", y = "NYSE:EGO Stock Price", title = "NYSE:EGO Stock Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5))
# moves very similarly to our response variable, overall decreasing trend
# also appears to move inversely to the indices (S&P and DJI)

ggplot(data = data, mapping = aes(x = date, y = eu_price)) + 
  geom_line() + 
  labs(x = "Date", y = "EURUSD Exchange Rate", title = "EURUSD Exchange Rate Time Series Plot")
# same period of sideways trading as we saw in S&P and DJI indices
# steep dropoff in exchange rate (EUR lost value versus USD)

ggplot(data = data, mapping = aes(x = date, y = of_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Brent Oil Futures Price", title = "Brent Oil Futures Time Series Plot")

ggplot(data = data, mapping = aes(x = date, y = os_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Crude Oil WTI / USD Price", title = "Crude Oil WTI / USD Time Series Plot")

ggplot(data = data, mapping = aes(x = date, y = sf_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Silver Futures Price", title = "Silver Futures Time Series Plot")
# moves in somewhat the same direction as gold, but with slightly more volatility. these prices are thought to typically move together

ggplot(data = data, mapping = aes(x = date, y = usb_price)) + 
  geom_line() + 
  labs(x = "Date", y = "US 10-Year Treasury Note", title = "US 10-Year Treasury Time Series Plot")

ggplot(data = data, mapping = aes(x = date, y = plt_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Platinum Futures Price", title = "Platinum Futures Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5))
# similar to gold and silver

ggplot(data = data, mapping = aes(x = date, y = pld_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Palladium Futures Price", title = "Palladium Futures Time Series Plot")

ggplot(data = data, mapping = aes(x = date, y = rho_price)) + 
  geom_line() + 
  labs(x = "Date", y = "Rhodium Spot Price", title = "Rhodium Spot Price Time Series Plot")
# looks weird so we might need to do investigation.

sum(data$rho_price == 0)

for (i in 1:3){
  data$rho_price = ifelse(data$rho_price == 0, lag(data$rho_price), data$rho_price)
}
# now we have our rhodium spot price fixed on the assumption that days with zero prices would equate to their last known traded price

ggplot(data = data, mapping = aes(x = date, y = usdi_price)) + 
  geom_line() + 
  labs(x = "Date", y = "US Dollar Index", title = "US Dollar Index Time Series Plot")

ggplot(data = data, mapping = aes(x = date, y = gdx_adj_close)) + 
  geom_line() + 
  labs(x = "Date", y = "NYSEARCA:GDX ETF Price", title = "NYSEARCA:GDX ETF Time Series Plot")
# moves similarly, but is expected since it's based on the underlying asset gold. therefore it's expected to
# be highly correlated with the response.

ggplot(data = data, mapping = aes(x = date, y = uso_close)) + 
  geom_line() + 
  labs(x = "Date", y = "NYSEARCA:USO ETF Price", title = "NYSEARCA:USO ETF Time Series Plot")



# log transform of variables + differencing to assess log returns. just want to make sure everything looks normally distributed
data = data |>
  mutate_at(vars(2:7, 9:12, 14:18, 20:23, 25:28, 31:34, 36:39, 42:45, 47:50, 52:55, 57:61, 64:68, 70:73), ~c(0, diff(log(.)))) |> # get log returns of gold. since we want to evaluate impact on movements of gold rather than the absolute price
  slice(-1) # remove the first row since we don't have the log returns
  

ggplot(data = data, mapping = aes(x = gold_adj_close)) + 
  geom_histogram() + 
  labs(x = "SPDR Gold ETF Log Returns", y = "Frequency", title = "Histogram of Gold Log Returns") + # very steep center and narrow tails.
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(-0.1, 0.1)


library(e1071)
kurtosis(data$gold_adj_close) # larger than normal distribution, much heavier tails compared to normal
skewness(data$gold_adj_close) # negative (longer left tail), distribution of return is somewhat asymmetric

spectrum(data$gold_adj_close, spans = c(25, 5, 25), main = "Smoothed Periodogram of Gold Log Returns") # somewhat cyclical


# log returns of all other variables
ggplot(data = data, mapping = aes(x = sp_ajclose)) + 
  geom_histogram() + 
  labs(x = "S&P 500 Log Returns", y = "Frequency", title = "Histogram of S&P 500 Log Returns") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(-0.1, 0.1)

ggplot(data = data, mapping = aes(x = dj_close)) + 
  geom_histogram() + 
  labs(x = "Dow Jones Index Log Returns", y = "Frequency", title = "Histogram of Dow Jones Log Returns") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(-0.1, 0.1)

ggplot(data = data, mapping = aes(x = eg_ajclose)) + 
  geom_histogram() + 
  labs(x = "Date", y = "NYSE:EGO Stock Price", title = "Histogram of NYSE:EGO Log Returns") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, mapping = aes(x = eu_price)) + 
  geom_histogram() + 
  labs(x = "Date", y = "EURUSD Exchange Rate", title = "Histogram of EURUSD exchange rate Log Returns") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(-0.1, 0.1)
  

ggplot(data = data, mapping = aes(x = of_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = os_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = sf_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = usb_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = plt_price)) + 
  geom_histogram() + 
  labs(x = "Date", y = "Platinum Futures Price", title = "Histogram of Platinum Futures Log Returns") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data, mapping = aes(x = pld_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = rho_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = usdi_price)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = gdx_adj_close)) + 
  geom_histogram()

ggplot(data = data, mapping = aes(x = uso_close)) + 
  geom_histogram()

# we can see that all predictor variables follow a type of lognormal distribution.


## TRAIN/TEST SPLIT: 80/20
library(caret)

set.seed(2024)
index = createDataPartition(data$gold_adj_close, p = 0.8, list = FALSE) # 80/20 split
train_data = data[index,]
test_data = data[-index,]

x_train = as.matrix(select(train_data, !gold_adj_close))
y_train = train_data$gold_adj_close
x_test = as.matrix(select(test_data, !gold_adj_close))
y_test = test_data$gold_adj_close


ts_train = data |>
  slice(1: round(0.8 * nrow(data)))

ts_test = data |>
  slice((round(0.8 * nrow(data))+1):nrow(data))

ggplot() +
  geom_line(data = ts_train, aes(x = date, y = gold_adj_close, color = "Training Data")) +
  geom_line(data = ts_test, aes(x = date, y = gold_adj_close, color = "Test Data")) +
  scale_color_manual(values = c("Training Data" = "blue", "Test Data" = "red")) +
  labs(title = "Training and Test Sets", x = "Date", y = "Gold Adjusted Close Price")



## FITTING MODELS 

# LINEAR REGRESSION
data = data |>
  select(-c("date"))

lmod = lm(gold_adj_close ~ . , data = train_data)
summary(lmod)

lmod_preds = predict(lmod, test_data, type = "response")
mse_lmod = mean((y_test - lmod_preds)^2)
mse_lmod


# LASSO REGRESSION + CV
library(glmnet)
lambda_value = cv.glmnet(x = x_train,
                         y = y_train,
                         alpha = 1, # LASSO regression
                         nfolds = 10, # 10 fold CV, performs a grid search to find best lambda value
                         family = "gaussian")

plot(lambda_value, main = "Log Lambda Value")
lambda = lambda_value$lambda.min


lasso_mod = glmnet(x = x_train,
                   y = y_train,
                   alpha = 1,
                   lambda = lambda,
                   family = "gaussian")

lasso_coefs = coef.glmnet(lasso_mod)

lasso_preds = predict(lasso_mod, newx = x_test, s = lambda, type = "response")
mse_lasso = mean((y_test - lasso_preds)^2)
mse_lasso # slightly less than lmod



## GAM
library(mgcv)

gam_mod = gam(gold_adj_close ~ s(sp_ajclose) + s(dj_close) + s(eg_ajclose) + s(eu_price) +
                s(of_price) + s(os_price) + s(sf_price) + s(usb_price) + s(plt_price) + 
                s(pld_price) + s(rho_price) + s(usdi_price) + s(gdx_adj_close) + s(uso_close), data = train_data)

summary(gam_mod)

library(ggeffects)
gam_plots = plot(ggpredict(gam_mod))

gam_preds = predict(gam_mod, newdata = test_data, type = "response")
mse_gam = mean((y_test - gam_preds)^2)
mse_gam



## RANDOM FOREST MODEL
library(randomForest)
rf_mod = randomForest(gold_adj_close ~ ., data = train_data, importance = TRUE, ntree = 100)
plot(rf_mod, main = "Random Forest Trees vs. Error")
varImpPlot(rf_mod, type = 1, n.var = 15, main = "% Increase in MSE")
varImpPlot(rf_mod, type = 2, n.var = 15, main = "Increase in Node Purity")

rf_preds = predict(rf_mod, newdata = test_data, type = "response")
mse_rf = mean((y_test - rf_preds)^2)
mse_rf



# ARMA model 
library(xts)

gold_ts = as.ts(xts(data$gold_adj_close, order.by = as.Date(data$date)))

aic_table = function(dataset, p, q){
  table = matrix(NA, (p + 1), (q + 1))
  for (i in 0:p){
    for (j in 0:q){
      table[i + 1, j + 1] = arima(dataset, order = c(i, 0, j))$aic
    }
  }
  dimnames(table) = list(paste("AR",0:p,sep=""),paste("MA",0:q,sep=""))
  table
}

p_select = aic_table(gold_ts,4,4)
min(p_select) # lowest aic at ARMA(1, 2)

arma_ts = arima(gold_ts, order = c(1, 0, 2)) # arima model
acf(resid(arma_ts), main = "ACF Plot of Logarithmic Returns of Gold") # acf plot

Box.test(resid(arma_ts), lag=20, type="Ljung-Box") # ljung box test


library(forecast)
arma_preds = forecast(arma_ts, h = nrow(ts_test))
mean((arma_preds - ts_test$gold_adj_close)^2)



## BAYESIAN GLM (since we know that all predictors are lognormally distributed)
bayes_mod = stan_glm(gold_adj_close ~ ., data = train_data, family = "gaussian")

bayes_preds = predict(bayes_mod, newdata = test_data, type = "response")
mse_bayes = mean((y_test - bayes_preds)^2)
mse_bayes



# bayesian time series models
library(bsts)

bayes_ts = AddLocalLinearTrend(list(), data$gold_adj_close)
bayes_ts = AddSeasonal(bayes_ts, data$gold_adj_close, nseasons = 52)
bs_ts = bsts(data$gold_adj_close,
               state.specification = bayes_ts,
               niter = 1000)
bsts_preds = predict(bs_ts, horizon = 360)



VECM_tsDyn = VECM(ts_train, lag=1, r=2,
                  estim = "ML",
                   LRinclude = "none",
                   exogen = dum_season)








