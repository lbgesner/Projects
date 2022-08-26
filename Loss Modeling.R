#install packages
install.packages("tidyverse")
library(tidyverse)
library(MASS)
#get claim data set
claim_data <- read.table('../data/claimdata.txt',
                        header = TRUE)
claim_data <- as_tibble(claim_data)
#rename columns
claimdata <- claim_data %>%
  rename_all(function(.name) {
    .name %>% 
      tolower %>%
    str_replace(" ", "-")
  })
claimdata <- rename(claimdata, expo = exp)
#explore relation
empirical <- claimdata$nclaims %>% 
  table %>% prop.table %>% as.numeric
empirical
k <- 1:(length(empirical) - 1)
ab0_relation <- empirical[k+1] / empirical[k] * k  
ab0_data <- tibble(k = k, ab0_rel = ab0_relation)
#plot relation
ggplot(data.frame(k = k, relation = ab0_relation), 
       aes(x = k, y = relation)) +
  theme_bw() + ylab('frac') +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
#empirical mean and variance
m <- sum(claimdata$nclaims)/sum(claimdata$expo)
var <- sum((claimdata$nclaims - m * claimdata$expo)^2)/
  sum(claimdata$expo)
#fit neg binom distribution
freq_glm_nb <- glm.nb(nclaims ~ 1 + offset(log(expo)), 
                      link = log,
                      data = claimdata)
freq_glm_nb %>% broom::tidy()
summary(freq_glm_nb)
#log likelihood neg binom
neg_log_nb <- function(par, freq, expo){
  mu <- expo*exp(par[1])
  r <- exp(par[2])
  -sum(dnbinom(freq, size = r, mu = mu, log = TRUE))
}
nb <- nlm(neg_log_nb, c(1, 1), hessian = TRUE, 
              freq = claimdata$nclaims, expo = claimdata$expo)
#fit poisson distribution
freq_glm_poi <- glm(nclaims ~ 1, offset = log(expo), 
                    family = poisson(link = "log"), 
                    data = claimdata)
freq_glm_poi %>% broom::tidy()
summary(freq_glm_poi)
#log likelihood poisson
neg_log_poi <- function(par, freq, expo){
  lambda <- expo*exp(par)
  -sum(dpois(freq, lambda, log = T))
}
poi <- nlm(neg_log_poi, 1, hessian = TRUE, 
               freq = claimdata$nclaims, expo = claimdata$expo)
#use AIC to find best distribution
AIC_poi <- 2*length(poi$estimate) + 2*poi$minimum
AIC_nb <- 2*length(nb$estimate) + 2*nb$minimum
c(AIC_nb = AIC_nb, AIC_poi = AIC_poi)

