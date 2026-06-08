
library(fitdistrplus)
library(ggplot2)
df <- read.csv("C:\\Users\\HP\\Downloads\\Barasat Medical College Queue Arrival Data-4.csv")
inter_arrival <- df$Inter.Arrival.Time.in.minutes.[!is.na(df$Inter.Arrival.Time.in.minutes.)]
inter_arrival <- inter_arrival[inter_arrival > 0]

library(ggplot2)
plot_df <- data.frame(time = inter_arrival)
ggplot(plot_df, aes(x = time)) +
  geom_histogram(
    binwidth = 0.02,           
    fill = "darkcyan", 
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(0, 2.4, by = 0.2), 
    limits = c(0, 2.4)
  ) +
  labs(
    title = "Distribution of Inter-arrival Times",
    subtitle = "X-axis intervals set to 0.2 minutes",
    x = "Inter-arrival Time (Minutes)",
    y = "Frequency"
  ) +  theme_minimal()

#Fit various distributions
fit_exp     <- fitdist(inter_arrival, "exp")
fit_gamma   <- fitdist(inter_arrival, "gamma")
fit_weibull <- fitdist(inter_arrival, "weibull")
fit_lnorm   <- fitdist(inter_arrival, "lnorm")

#Compare Goodness-of-Fit Statistics
#This will show AIC, BIC, and Log-likelihood
summary_stats <- gofstat(list(fit_exp, fit_gamma, fit_weibull, fit_lnorm),
                         fitnames = c("Exponential", "Gamma", "Weibull", "Log-normal"))
print(summary_stats)

library(ggplot2)

plot_df <- data.frame(time = inter_arrival)

ggplot(plot_df, aes(x = time)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "yellow", color = "black") +
  stat_function(fun = dexp, args = list(rate = fit_exp$estimate), aes(color = "Exponential"), size = 1) +
  stat_function(fun = dlnorm, args = list(meanlog = fit_lnorm$estimate[1], sdlog = fit_lnorm$estimate[2]), aes(color = "Log-normal"), size = 1) +
  stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate[1], rate = fit_gamma$estimate[2]), aes(color = "Gamma"), size = 1) +
  scale_x_continuous(breaks = seq(0, 2.4, by = 0.2), limits = c(0, 2.4)) +
  labs(title = "Comparison of Distribution Fits", x = "Inter-arrival Time (min)", y = "Density", color = "Distribution") +
  theme_minimal()



summary_stats <- gofstat(list(fit_exp, fit_gamma, fit_weibull, fit_lnorm),
                         fitnames = c("Exponential", "Gamma", "Weibull", "Log-normal"))

print(data.frame(
  AIC = summary_stats$aic,
  BIC = summary_stats$bic,
  row.names = c("Exponential", "Gamma", "Weibull", "Log-normal")
))




# 3. Perform Kolmogorov-Smirnov Test for each
# For Exponential
ks_exp <- ks.test(inter_arrival, "pexp", rate = fit_exp$estimate["rate"])

# For Log-normal
ks_lnorm <- ks.test(inter_arrival, "plnorm", 
                    meanlog = fit_lnorm$estimate["meanlog"], 
                    sdlog = fit_lnorm$estimate["sdlog"])

# For Gamma
ks_gamma <- ks.test(inter_arrival, "pgamma", 
                     shape = fit_gamma$estimate["shape"], 
                     rate = fit_gamma$estimate["rate"])
ks_gamma
ks_lnorm
ks_exp