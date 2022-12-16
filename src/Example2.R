library(MASS)
library(tidyverse)

mvn_samp <- function(theta_2_init, n, n0){
  
  theta_2 <- numeric()
  theta_1 <- numeric()
  
  ##Initialize simulation values
  theta_2[1] <- theta_2_init
  theta_1[1] <- 0
  
  for(i in 2:(n + n0)){
    
    ##Update simulation values
    theta_1[i] <- rnorm(1, mean = 2 + (1/5)*(theta_2[i-1] - 3), sd = sqrt(33/5))
    theta_2[i] <- rnorm(1, mean = 3 + (1/5)*(theta_1[i] - 2), sd = sqrt(66/7))
    
  }
  
  ##Return dataframe with all non-removed simulated values
  cha <- data.frame(
    theta1 = theta_1[(n0+1):(n+n0)],
    theta2 = theta_2[(n0+1):(n+n0)]
  )
  
  return(cha)
}


##Graph simulated values
grap <- mvn_samp(3, 20000, 2000)

grap %>%
  ggplot(aes(x=theta1, y=theta2) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10, 15)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10, 15)) +
  theme(
    legend.position='bottomleft'
  ) +
  ylab("Theta_2") +
  xlab("Theta_1")


##Graph actual density
grap_real <- as.data.frame(mvrnorm(n=1e6, c(2,3), matrix(c(7,2,2,10), nrow = 2)))

grap_real %>%
  ggplot(aes(x=V1, y=V2) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10, 15)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-10, 15)) +
  theme(
    legend.position='bottomleft'
  ) +
  ylab("Theta_2") +
  xlab("Theta_1")






