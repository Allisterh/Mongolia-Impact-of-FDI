library(tseries)
library(readxl)
library(vars)
library(tidyverse)
library(stargazer)
library(forecast)

data = read_excel("C:\\Users\\jants\\Downloads\\mgl_fdi_research.xlsx", sheet = "model1 data") 

rgdp_yoy = ts(data$`real gdp yoy`, start=c(2009, 1), frequency = 4)
net_fdi = ts(data$`net fdi %gdp per quarter`, start=c(2009, 1), frequency = 4)
debt_gdp = ts(data$`debt %gdp`, start=c(2009, 1), frequency = 4)
unemp = ts(data$unemp, start=c(2009, 1), frequency = 4) 
m2 = ts(data$`m2 %gdp`, start=c(2009, 1), frequency = 4)
trade = ts(data$`trade turnover`, start=c(2009, 1), frequency = 4)
ind_prod_index = ts(data$`industrial prod index`, start=c(2009, 1), frequency = 4)
net_fdi_level = ts(data$`fdi inflow`, start = c(2009, 1), frequency = 4)
rwage = ts(data$lwage, start = c(2009, 1), frequency = 4)

# some plots

ggplot(data = data) + geom_point(mapping = aes(x = net_fdi, y = rgdp_yoy)) # remove outlier netfdi data at 33
                                                                           # also could remote outlier gdp data at 46

variables = list(rgdp_yoy, net_fdi, debt_gdp, unemp, m2, trade, ind_prod_index, net_fdi_level, rwage)

df = data.frame("rgdp" = rgdp_yoy, "net_fdi" = net_fdi, "debt_gdp" = debt_gdp, 
                "unemp" = unemp, "m2" = m2, "trade" = trade, "ind_prod" = ind_prod_index,
                "net_fdi_level" = net_fdi_level, "rwage" = rwage)

rownames(df) = index(rgdp_yoy)

# ACF and PACF

for (i in 1:length(df)) {
  print(i)
  acf(df[, i], main = colnames(df)[i])
  pacf(df[, i], main = colnames(df)[i])
} 

# Phillips Pearon, ADF Test and Unit Root Test

pptab = NULL
for (i in 1:ncol(df)) {
  pp = PP.test(df[, i])
  adf = adf.test(df[, i])
  kpss = kpss.test(df[, i])
  result = cbind(pp$p.value, adf$p.value, kpss$p.value)
  pptab = rbind(pptab, result)
}
rownames(pptab) = colnames(df)
colnames(pptab) = c("Phillips Pearon", "ADF", "KPSS")
pptab

# Data Transformations, Stationary Tests again

lrgdp_diff = diff(log(rgdp_yoy + 1))
lnet_fdi_diff = (diff(log(net_fdi + 2)))
net_fdi_level_diff2 = diff(diff(net_fdi_level))
ldebt_gdp_diff2 = diff(diff(log(debt_gdp)))
lunemp_diff = (diff(log(unemp)))
lm2_diff2 = diff(diff(log(m2)))
ltrade_diff = diff(log(trade + 1))
lind_prod_index_diff = diff(log(ind_prod_index + 1))
rwage_diff = diff(rwage)

#df.transformed = cbind(lrgdp_diff, lnet_fdi_diff, net_fdi_level_diff2, 
#                         ldebt_gdp_diff2, lunemp_diff, lm2_diff2,
#                         ltrade_diff, lind_prod_index_diff, rwage_diff)

df.transformed = cbind(lrgdp_diff, lnet_fdi_diff, 
                       ldebt_gdp_diff2, lunemp_diff, lm2_diff2,
                       ltrade_diff, lind_prod_index_diff, rwage_diff)

df.transformed = na.omit(df.transformed)

pptab1 = NULL
for (i in 1:ncol(df.transformed)) {
  pp = PP.test(df.transformed[, i])
  adf = adf.test(df.transformed[, i])
  kpss = kpss.test(df.transformed[, i])
  result = cbind(pp$p.value, adf$p.value, kpss$p.value)
  pptab1 = rbind(pptab1, result)
}
rownames(pptab1) = colnames(df.transformed)
colnames(pptab1) = c("Phillips Pearon", "ADF", "KPSS")
pptab1

# Optimal lag

lagselect1 = VARselect(df.transformed, lag.max = 10)
lagselect1$selection

# Estimate VAR

df.var = df.transformed
var1 = VAR(df.var, p = 4, type = c("const"))
summary(var1)

stargazer(var1[["varresult"]], type="text")

for (i in 2:9) {
  irf1 = irf(var1, impulse = colnames(var1$datamat[i]), response = colnames(var1$datamat[1]), 
             n.ahead = 4, ortho = TRUE, ci = 0.95, boot = TRUE, runs = 100, cumulative = FALSE)
  plot(irf1)
}


# Model stability

roots(var1, )

# Granger casuality

granger1 = causality(va1, cause = "lgdp_diff")
granger2 = causality(var1, cause = "net_fdi_level_diff")


fit = lm(lrgdp_diff ~ ., data = df.transformed)
summary(fit)
step.model = stepAIC(fit, direction = "both", trace = FALSE)
summary(step.model)