library(ggplot2)
n = 1000
status = rbinom(n, 1 ,0.4)
testdata = tibble(status = status,
                      phs  = rnorm(n) + (1 * status),
                      ref = phs,
                      Age = sample(60:100, n, replace = TRUE))

qplot(x = lp, fill = as.factor(status), data = testdata)

hr_names <- c('HR98_50', 'HR20_50', 'HR80_20', 'HR95_50')

swc_caco = status

if (is.null(ref)) {
    num_critvals <- matrix(c(10.097845, Inf, -Inf, 9.004659, 9.639069, Inf, 9.946332, Inf), ncol = 2, byrow = TRUE)
    den_critvals <- matrix(c(9.123500, 9.519703, 9.123500, 9.519703, -Inf, 9.004659, 9.123500, 9.519703), ncol = 2, byrow = TRUE)
} else {
    num_critvals <- matrix(c(quantile(ref, 0.98), Inf, -Inf, quantile(ref, 0.2), quantile(ref, 0.8), Inf, quantile(ref, 0.95), Inf), ncol = 2, byrow = TRUE)
    den_critvals <- matrix(c(quantile(ref, 0.3), quantile(ref, 0.7), quantile(ref, 0.3), quantile(ref, 0.7), -Inf, quantile(ref, 0.2), quantile(ref, 0.3), quantile(ref, 0.7)), ncol = 2, byrow = TRUE)
}
num_critvals
den_critvals

require(survival)

swc_numcases <- sum(swc_caco == 1)
swc_numcontrols <- sum(swc_caco == 0)

swc_wvec <- swc_caco * (swc_popnumcases / swc_numcases) + (!swc_caco) * (swc_popnumcontrols / swc_numcontrols)
swc_switch = FALSE
swc_popnumcases = 1
swc_popnumcontrols = 1


#######################################################
#

prctile_list = c(0, 0.2, 0.7, 0.8, 0.98, 1)
prctile_labels = c('0-20th', '20-70th', '70-80th', '80-98th', '98-100th')


testdata$percentile = 
    with(testdata,
         cut(lp, breaks = quantile(lp, probs = prctile_list),
             labels = prctile_labels,
             include.lowest = TRUE))

