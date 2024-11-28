## code to prepare `test_data` dataset goes here
# n = 1000
# status = rbinom(n, 1, 0.2)
# 
# test_data = data.frame(phs  = rnorm(n) + (1 * status),
#                        status = status,
#                        age = sample(40:100, n, replace = TRUE))

set.seed(4649580)
n <- 1000
age = runif(n, 40, 80) # generate random age at recruitment
cens <- 15*runif(n)    # randomize years until censoring
phs = rnorm(n)         # generate random PHS score
h <- .02*exp(.04*(age-50)+0.8*(phs)) 
# calculate risk with age and PHS score
t <- -log(runif(n))/h  # calculate time of diagnosis
status <- ifelse(t<=cens,1,0) 
# if time of diagnosis is less than time of censoring
# then "case", otherwise "control"
t <- pmin(t, cens)     # choose the lesser of diagnosis time or censoring time
age = age + t          # Add age of recruitment to age of diagnosis/censoring

test_data = data.frame(phs, age, status)

usethis::use_data(test_data, overwrite = TRUE)
