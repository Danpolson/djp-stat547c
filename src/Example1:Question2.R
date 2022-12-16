library(tidyverse)
met_hast <- function(mu_0, n_0, Y, a, b, n){
  
  y <- sum(Y)
  m <- length(Y)
  
  ##initial guess
  mu <- mu_0
  ret_vec <-c(mu_0)
  
  for(i in 2:(n_0 + n)){
    
    ##uniform rv for sampling
    u <- runif(1, min = 0, max = 1)
    
    ##potential new value to move to
    mu_new <- rnorm(1, mean = mu, sd = 1)
    
    ##pivotal quantities
    fxmu <- exp(-mu*(m + b))*(mu^(y + a - 1))*dnorm(Y[1], mean = mu)*
      dnorm(Y[2], mean = mu)*dnorm(Y[3], mean = mu)
    
    fymu <- exp(-mu_new*(m + b))*(mu_new^(y + a - 1))*dnorm(Y[1], mean = mu)*
      dnorm(Y[2], mean = mu)*dnorm(Y[3], mean = mu)
    
    ##probability to change value
    p <- min(c(1,(fymu/fxmu)))
    
    if(p >= u){
      
        ret_vec <- append(ret_vec, mu_new)
        
        mu <- mu_new
      
    }
    
    else{
      
      ret_vec <- append(ret_vec, mu)
      
    }
  }
  
  ##return 
  return(ret_vec[1001:11000])
  
}

m <- 3
Y <- c(10,10,13)
a <- 2
b <- 2
mu_0 <- 15.3
n_0 <- 1000
n <- 10000


mu_sample <- tibble ( mu = met_hast(mu_0, n_0, Y, a, b, n))
ggplot (mu_sample , aes ( x = mu )) +
  geom_density ( color = " red " ) +
  stat_function ( fun = dgamma , args = list (sum(Y) + a , rate = m + b) , color = " blue " ) +
  theme_minimal() +
  ylab("Density") +
  xlab("Theta")
