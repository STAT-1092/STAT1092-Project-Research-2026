
library(fitdistrplus)
library(ggplot2)


df_service <- read.csv("C:\\Users\\HP\\Downloads\\Barasat_Medical_College_Service_Dataset (1).csv")

inter_service <- df_service$Inter.Service.Time..In.Minutes.[!is.na(df_service$Inter.Service.Time..In.Minutes.)]
inter_service <- inter_service[inter_service > 0]


plot_df <- data.frame(time = inter_service)
ggplot(plot_df, aes(x = time)) +
  geom_histogram(
    binwidth = 0.2,           
    fill = "darkcyan", 
    color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(0, 3.5, by = 0.5), 
    limits = c(0, 3.5)
  ) +
  labs(
    title = "Distribution of Inter-arrival Times",
    subtitle = "X-axis intervals set to 0.5 minutes",
    x = "Inter-service Time (Minutes)",
    y = "Frequency"
  ) +  theme_minimal()

fit_exp      <- fitdist(inter_service, "exp")
fit_gamma    <- fitdist(inter_service, "gamma")
fit_weibull  <- fitdist(inter_service, "weibull")
fit_lnorm    <- fitdist(inter_service, "lnorm")
fit_erlang6  <- fitdist(inter_service, "gamma", fix.arg = list(shape = 6))



fits <- list(fit_exp, fit_gamma, fit_weibull, fit_lnorm, fit_erlang6)
fit_names <- c("Exponential", "Gamma", "Weibull", "Log-normal", "Erlang-6")


results <- data.frame(
  Distribution = fit_names,
  AIC = sapply(fits, function(x) x$aic),
  BIC = sapply(fits, function(x) x$bic)
)


results$KS_Stat <- sapply(1:length(fits), function(i) {
  f <- fits[[i]]
  dist_p_func <- paste0("p", f$distname) # e.g., "pexp", "pgamma"
  
  
  params <- as.list(c(f$estimate, f$fix.arg))
  
  
  ks_result <- suppressWarnings(do.call(ks.test, c(list(x = inter_service, y = dist_p_func), params)))
  return(ks_result$statistic)
})


results <- results[order(results$AIC), ]

cat("\n--- Goodness of Fit Comparison Table ---\n")
print(results)


denscomp(fits, 
         legendtext = fit_names,
         main = "Inter-Service Time: Distribution Comparison",
         xlab = "Time (Minutes)",
         datacol = "lightpink", 
         fitlty = 1,
         fitcol = c("red", "blue", "green", "purple", "orange"))


cat("\n--- Best Overall Fit: Log-normal Parameters ---\n")
print(fit_lnorm$estimate)

cat("\n--- Comparison Fit: Erlang-6 Parameters ---\n")
print(fit_erlang6$estimate)
