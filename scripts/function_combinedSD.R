#Function to determine the standard deviation of multiple groups of data combined given only mean, sd and n of each group
combined_sd <- function(mean_col, sd_col, n_col) {
  qs <- c()
  for (i in 1:length(mean_col)) {
    qs <- c(qs, (n_col[i]-1)*sd_col[i]^2 + n_col[i]*mean_col[i]^2)
  }
  
  qc <- sum(qs)
  
  sc = sqrt( (qc - (sum(n_col))*weighted.mean(mean_col, n_col)^2)/(sum(n_col)-1) )
  
  return(sc)
}

#Example
data_1 <- c(3,4,3,6,2,7,1,9,3,6)

data_2 <- c(9,8,6,9,7,9)

data_comb <- c(data_1, data_2)

sd(data_comb)

combined_sd(mean_col = c(mean(data_1), mean(data_2)),
            sd_col = c(sd(data_1), sd(data_2)),
            n_col = c(length(data_1), length(data_2)))



data_example <- data.frame(mean = c(mean(data_1), mean(data_2)),
                           sd = c(sd(data_1), sd(data_2)),
                           n = c(length(data_1), length(data_2)))

data_example %>% summarise(sd = combined_sd(mean, sd, n))