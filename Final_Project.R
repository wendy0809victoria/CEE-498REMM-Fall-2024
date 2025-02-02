library(readr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(VIM)
library(ggridges)
library(reshape2)
library(gridExtra)
library(grid)
library(bbmle)

my_array <- array(data = 1:24, dim = c(2, 3, 4)) # 2 rows, 3 columns
# print(my_array) # [[1, 3, 5], [2, 4, 6]]

dimnames(my_array) <- list(
  rows = c("Row1", "Row2"),               
  cols = c("Col1", "Col2", "Col3"),       
  layers = c("Layer1", "Layer2", "Layer3", "Layer4")  
)

# print(my_array)

failure_array <- array(NA, dim = c(11, 16), dimnames = list(
  Number = c(7907, 7908, 7909, 7910, 7911, 7912, 7913, 7914, 7915, 7916, 7917),
  Failure_Times = paste0("T", 1:16)
))

failure_array[1, 1:6] <- c(194, 209, 250, 279, 312, 493)
failure_array[2, 1:8] <- c(413, 427, 485, 522, 622, 687, 696, 865)
failure_array[3, 1:16] <- c(90, 100, 160, 346, 407, 456, 470, 494, 550, 570, 649,
                            733, 777, 836, 865, 983)
failure_array[4, 1:9] <- c(74, 131, 179, 208, 710, 722, 792, 813, 842)
failure_array[5, 1:6] <- c(55, 375, 431, 535, 755, 994)
failure_array[6, 1:10] <- c(23, 284, 371, 378, 498, 512, 574, 621, 846, 917)
failure_array[7, 1:13] <- c(97, 148, 159, 163, 304, 322, 464, 532, 609, 689, 690,
                            706, 812)
failure_array[8, 1:16] <- c(50, 94, 196, 268, 290, 329, 332, 347, 544, 732, 811, 
                            899, 945, 950, 955, 991)
failure_array[9, 1:4] <- c(359, 368, 380, 650)
failure_array[10, 1:6] <- c(50, 304, 309, 592, 627, 639)
failure_array[11, 1:2] <- c(130, 623)

# print(failure_array)

inter_failure_df <- melt(inter_failure_array, varnames = c("Number", "Inter_Failure_Times"), value.name = "Time_Diff")

inter_failure_df <- na.omit(inter_failure_df)

# print(inter_failure_df)

ift <- ggplot(inter_failure_df, aes(x = as.factor(Number), y = Time_Diff)) +
  geom_violin(trim = TRUE, drop = FALSE, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Inter Failure Times Distribution",
    x = "Airplane Number",
    y = "Inter-Failure Times"
  )
ggsave("Inter-Failure_Times_Ridge_Plot.png", plot=ift)


inter_failure_array <- array(NA, dim = c(11, 16), dimnames = list(
  Number = c(7907, 7908, 7909, 7910, 7911, 7912, 7913, 7914, 7915, 7916, 7917),
  Inter_Failure_Times = paste0("T", 1:16)
))

inter_failure_array[1, 2:6] <- failure_array[1, 1:6][-1] - failure_array[1, 1:6][-length(failure_array[1, 1:6])] 
inter_failure_array[2, 2:8] <- failure_array[2, 1:8][-1] - failure_array[2, 1:8][-length(failure_array[2, 1:8])] 
inter_failure_array[3, 2:16] <- failure_array[3, 1:16][-1] - failure_array[3, 1:16][-length(failure_array[3, 1:16])] 
inter_failure_array[4, 2:9] <- failure_array[4, 1:9][-1] - failure_array[4, 1:9][-length(failure_array[4, 1:9])] 
inter_failure_array[5, 2:6] <- failure_array[5, 1:6][-1] - failure_array[5, 1:6][-length(failure_array[5, 1:6])] 
inter_failure_array[6, 2:10] <- failure_array[6, 1:10][-1] - failure_array[6, 1:10][-length(failure_array[6, 1:10])] 
inter_failure_array[7, 2:13] <- failure_array[7, 1:13][-1] - failure_array[7, 1:13][-length(failure_array[7, 1:13])] 
inter_failure_array[8, 2:16] <- failure_array[8, 1:16][-1] - failure_array[8, 1:16][-length(failure_array[8, 1:16])] 
inter_failure_array[9, 2:4] <- failure_array[9, 1:4][-1] - failure_array[9, 1:4][-length(failure_array[9, 1:4])] 
inter_failure_array[10, 2:6] <- failure_array[10, 1:6][-1] - failure_array[10, 1:6][-length(failure_array[10, 1:6])]
inter_failure_array[11, 2:2] <- failure_array[11, 1:2][-1] - failure_array[11, 1:2][-length(failure_array[11, 1:2])]

# print(inter_failure_array)
new_array <- failure_array[!is.na(failure_array)]

non_na_array <- array(new_array, dim = c(1, length(new_array)))

# print(non_na_array)  
# print(new_array)      

# print(length(sort(new_array))) # 96
# print(length(unique(sort(new_array)))) # 93
sort_array <- sort(new_array)
# print(sort_array)
unique_array <- unique(sort_array)
# print(length(diff(sort_array))) # 95
diff_array <- diff(sort_array)
multiple <- which(diff_array == 0)
# print(length(multiple))
# print(multiple) # 2 26 87

num_of_failures <- array(1, dim = length(unique_array), dimnames = list(unique_array))
for (i in 1:length(multiple)) {
  num_of_failures[multiple[i]-i+1] <- 2 
}
# print(num_of_failures)

num_of_failures_1 <- array(1, dim = length(sort_array), dimnames = list(sort_array))

failure_df <- data.frame(
  Time = as.numeric(names(num_of_failures)),
  Failures = as.numeric(num_of_failures)
)

failure_df_1 <- data.frame(
  Time = as.numeric(names(num_of_failures_1)),
  Failures = as.numeric(num_of_failures_1)
)

ift <- ggplot(failure_df_1, aes(x = Time)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Histogram of Failure Number in Each Time Interval", x = "Time", y = "Failure Count") +
  scale_x_continuous(
    breaks = seq(0, 1000, by = 100),    
    minor_breaks = seq(0, 1000, by = 50) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 1)
  ) +
  theme_minimal()
ggsave("Histogram_of_Failures.png", plot=ift)

num_of_airplanes <- array(11, dim = length(unique_array), dimnames = list(unique_array))

# MCRF(k) = MCRF(k-1)+i(k)/r(k)
MCRF_array <- array(0, dim = length(unique_array), dimnames = list(unique_array))
for (i in 1:length(MCRF_array)) {
  if (i == 1) {
    MCRF_array[i] <- round(num_of_failures[i]/num_of_airplanes[i], digits = 3)
  } else {
    MCRF_array[i] <- round(MCRF_array[i-1] + num_of_failures[i]/num_of_airplanes[i], digits = 3)
  }
}
print(MCRF_array)

inter_failure_df <- as.data.frame(as.table(inter_failure_array))
inter_failure_df <- inter_failure_df[!is.na(inter_failure_df$Freq), ]
inter_new_array <- inter_failure_array[!is.na(inter_failure_array)]
inter_non_na_array <- array(inter_new_array, dim = c(1, length(inter_new_array)))
# print(inter_non_na_array)
# print(mean(inter_non_na_array)) # 85.57647

colnames(inter_failure_df) <- c("Airplane", "Failure_Time", "Interval")

ift <- ggplot(inter_failure_df, aes(x = factor(Airplane), y = Interval)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Inter-Failure Time for Each Airplane's Air Conditioning Unit",
    x = "Airplane Number",
    y = "Inter-Failure Time"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Inter-Failure_Times_Violin_Plot.png", plot=ift)

ift_boxplot <- ggplot(inter_failure_df, aes(x = factor(Airplane), y = Interval)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +  
  labs(
    title = "Inter-Failure Time for Each Airplane's Air Conditioning Unit",
    x = "Airplane Number",
    y = "Inter-Failure Time"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Inter-Failure_Times_Box_Plot.png", plot = ift_boxplot)

table_data <- data.frame(
  Number_of_Failure = num_of_failures,
  Number_of_Airplane = num_of_airplanes,
  MCRF = MCRF_array
)

data <- data.frame(
  Time = unique_array,
  MCRF = MCRF_array
)

MCRF <- ggplot(data, aes(x = Time, y = MCRF)) +
  geom_step(color = "blue", size = 1) +    
  labs(title = "Mean Cumulative Repair Function (Step Plot)", 
       x = "Time (t)", 
       y = "MCRF") +
  scale_x_continuous(
    breaks = seq(0, 1000, by = 50),    
    minor_breaks = seq(0, 1000, by = 25) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 1),       
    labels = function(x) format(x, nsmall = 3)  
  ) +
  theme_minimal() +                         
  theme(plot.title = element_text(hjust = 0.5))  
ggsave("Mean_Cumulative_Repair_Function.png", plot=MCRF)

horizontal_table <- t(table_data)
print(horizontal_table)

table_grob <- tableGrob(horizontal_table)

png("table_output.png", width = 4000, height = 600)
grid.draw(table_grob)  
dev.off()

print(sort_array)

T_max <- max(sort_array)
log_likelihood <- function(alpha, beta) {
  Lambda_T <- alpha * T_max^beta
  log_lambda <- log(alpha) + log(beta) + (beta - 1) * log(sort_array)
  -sum(log_lambda) + Lambda_T
}

start_vals <- list(alpha = 0.02, beta = 0.05)
fit <- mle2(log_likelihood, start = start_vals)
print(fit)

sort_array_0 <- c(0, 100, 200, 300, 400, 500, 600, 700, 800,
                  900, 1000)
params <- coef(fit)
alpha_hat <- params["alpha"]
beta_hat <- params["beta"]
computed_values <- numeric(length(sort_array_0))
computed_values[1] <- 0
for (t in 2:length(sort_array_0)) {
  computed_values[t] <- (alpha_hat) * (sort_array_0[t]^(beta_hat) - sort_array_0[t - 1]^(beta_hat))
}
NHPP <- computed_values
NHPP_array <- data.frame(Index = c(0, 100, 200, 300, 400, 500, 600, 700, 800,
                                   900, 1000), 
                         Value = NHPP)
nhpp <- ggplot(NHPP_array, aes(x = Index, y = Value)) +
  geom_line(color = "blue") +        
  geom_point(color = "red") +  
  scale_x_continuous(
    breaks = seq(0, 1000, by = 100),    
    minor_breaks = seq(0, 1000, by = 50) 
  ) +
  scale_y_continuous(
    breaks = seq(0, 15, by = 1)
  ) +      
  labs(
    title = "Estimated Failure Number in Each Time Interval",    
    x = "Time",                     
    y = "Failure Count"                      
  ) +
  theme_minimal() 
ggsave("NHPP.png", plot=nhpp)

ct <- c(3, 9, 9, 13, 10, 13, 11, 9, 9, 6, 4)
prob <- numeric(length(ct))
for (t in 1:length(ct)) {
  prob[t] <- exp(-computed_values[t]) * computed_values[t]^ct[t] / factorial(ct[t])
}
lambda_h <- mean(ct)
print(lambda_h)
prob_h <- numeric(length(ct))
for (t in 1:length(ct)) {
  prob_h[t] <- exp(-lambda_h) * lambda_h^ct[t] / factorial(ct[t])
}
# print(mean(prob)) # 0.08586366
# print(sd(prob)) # 0.04800104
# print(mean(prob_h)) # 0.08868015
# print(sd(prob_h)) # 0.04404692
print("Probability from our NHPP model: ")
print(prob)
print("Probability from the homogenous Poisson Process model: ")
print(prob_h)

lambda_hat <- function(t) alpha_hat * t^beta_hat
time_seq <- seq(0, T_max, length.out = 100)
lambda_values <- sapply(time_seq, lambda_hat)

plot(sort_array, rep(0, length(sort_array)), pch = 16, xlab = "Time", ylab = "Intensity",
     main = "NHPP Fit (Power Law)")
lines(time_seq, lambda_values, col = "blue", lwd = 2)
legend("topright", legend = "Fitted λ(t)", col = "blue", lwd = 2)

simulate_nhpp <- function(alpha, beta, T_max, n_events) {
  Lambda_inv <- function(u) ((u * (beta + 1)) / alpha)^(1 / (beta + 1))
  u <- runif(n_events)
  sapply(u, Lambda_inv)
}

simulated_times <- simulate_nhpp(alpha_hat, beta_hat, T_max, 10)
print(simulated_times)
