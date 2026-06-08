
library(fitdistrplus)
library(ggplot2)
service_df <- read.csv("C:\\Users\\HP\\Downloads\\Barasat_Medical_College_Service_Dataset (1).csv")

service_summary <- service_df[!is.na(service_df$Time.Period) & service_df$Time.Period != "", ]
yt_data <- as.numeric(service_summary$No..of.Patients)


barplot(yt_data, 
        names.arg = service_summary$Time.Period, 
        col = "lightgreen", 
        border = "black",
        main = "Distribution of Patients Served Y(t)",
        xlab = "Time Period (10-Minute Intervals)",
        ylab = "Number of Patients Served",
        las = 2,           
        cex.names = 0.6)


fit_p_y <- fitdist(yt_data, "pois")


fit_nb_y <- fitdist(yt_data, "nbinom")
fit_g <- fitdist(yt_data, "geom")
# A common starting point for size is max(data) or Mean^2/(Mean-Var)
n_est <- 38 # Estimated from MLE calculation
fit_b <- fitdist(yt_data, "binom", fix.arg = list(size = n_est), start = list(prob = mean(yt_data)/n_est))





gof_results_y <- gofstat(list(fit_p_y, fit_nb_y, fit_g, fit_b), 
                         fitnames = c("Poisson", "Neg-Binomial", "Geometric", "Binomial"))

print(gof_results_y)

