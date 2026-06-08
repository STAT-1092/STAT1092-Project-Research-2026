# 1. Define the data
intervals <- c("0.5-3.5", "3.5-6.5", "6.5-9.5", "9.5-12")
frequencies <- c(1, 12, 21, 3)

# 2. Create the plot using barplot for exact interval labels
# 'names.arg' puts your custom text labels on the x-axis
bp <- barplot(frequencies, 
        names.arg = intervals, 
        col = "skyblue", 
        border = "black",
        main = "Patient Distribution by Interval",
        xlab = "Number of Patients (Intervals)",
        ylab = "Frequency",
        ylim = c(0, 25)) # Adjust y-axis limit to fit the peak

# 3. Optional: Add the frequency numbers on top of each bar
text(x = bp, y = frequencies + 1, labels = frequencies, cex = 0.9)

# 4. Add a grid for better readability
grid(nx = NA, ny = NULL)

# 1. Define the data








# 1. Setup Data
midpoints <- c(2, 5, 8, 11)
frequencies <- c(1, 12, 21, 3)
breaks_vec <- c(0.5, 3.5, 6.5, 9.5, 12.5)

# 2. Create the Histogram
# We save the histogram to an object to use its coordinates
h <- hist(rep(midpoints, frequencies), 
          breaks = breaks_vec, 
          col = "skyblue", 
          border = "white",
          main = "Frequency Polygon of Patient Data (Service)",
          xlab = "Patients (Class Intervals)", 
          ylab = "Frequency",
          xaxt = "n")

# 3. Add the Frequency Polygon (Joining the midpoints)
# We add (0,0) at the start and end to 'anchor' the polygon to the x-axis
x_poly <- c(0.5, midpoints, 12.5)
y_poly <- c(0, frequencies, 0)

lines(x_poly, y_poly, type = "b", col = "red", lwd = 2, pch = 16)

# 4. Add Custom X-axis labels
axis(1, at = midpoints, 
     labels = c("0.5-3.5", "3.5-6.5", "6.5-9.5", "9.5-12.5"), 
     cex.axis = 0.8)




# 1. Input the raw data from your image
patients_data <- c(4, 10, 8, 8, 7, 9, 7, 8, 5, 5, 7, 7, 6, 7, 7, 8, 11, 6, 8, 12, 2, 7, 8, 9, 7, 9, 6, 6, 7, 5, 8, 6, 6, 6, 6, 8, 7)

# 2. Generate the frequency table
freq_table <- table(patients_data)
print(freq_table)

# 3. Create a Bar Plot to visualize the counts
barplot(freq_table, 
        main="Frequency of Patients", 
        xlab="Number of Patients", 
        ylab="Frequency", 
        col="lightgreen", 
        border="black")



