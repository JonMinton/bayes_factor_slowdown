## Maximum likelihood 

#The alternative model with the maximised Bayes Factor is not the maximum likelihood model of the data post 2012, due to the $\sigma^2$ parameter being fixed, and the $\mu$ parameter space being constrained to fixed values. The maximum likelihood model will now be calculated (again, for completeness). 

source("scripts/load_packages_and_functions.R")

ll_wrapper <- function(par, x){
  mu <- par[[1]]
  sig_sq <- exp(par[[2]])
  
  
  sig <- sqrt(sig_sq)
  n <- length(x)
  
  - n * log(sig)  - (n/2) * log(2 * pi) - (1 / 2 * sig_sq) * sum((x - mu)^2)
}

tmp <- 
  e0_uk %>% 
  group_by(sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  filter(between(year, 2012, 2017)) %>% 
  select(-e0) %>% 
  spread(sex, ch_e0)

x_female <- pull(tmp, female)
x_male   <- pull(tmp, male  )
x_total  <- pull(tmp, total )

rm(tmp)

opt_par_female <- optim(c(0,0), ll_wrapper, x = x_female, hessian = TRUE, method= "BFGS", control = list(fnscale = -1, trace = TRUE, maxit = 1000))
opt_par_male <- optim(c(0,0), ll_wrapper, x = x_male, hessian = TRUE, method= "BFGS", control = list(fnscale = -1, trace = TRUE, maxit = 1000))
opt_par_total <- optim(c(0,0), ll_wrapper, x = x_total, hessian = TRUE, method= "BFGS", control = list(fnscale = -1, trace = TRUE, maxit = 1000))


#Do the Hessians make sense?
  
solve(-opt_par_female$hessian)
solve(-opt_par_male$hessian)
solve(-opt_par_total$hessian)


#No, they have not been computered for any of the above. (Perhaps as there are just a few observations)



