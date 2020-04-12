#####################################
#  Code for simplex regression in   #
#  Stan, where the frequencies must #
#  add to 1.                        #
#####################################

library(rstan)
options(mc.cores = parallel::detectCores())
library(dplyr)
library(ggridges)


#----------------------------#
#   Data on M&M Counts       #
#----------------------------#
mms = c("Brown", "Blue", "Orange", "Yellow", "Red", "Green")
day1 = c(1, 4, 7, 6, 3, 3)
day2 = c(0, 7, 7, 5, 3, 3)

data = as.matrix(cbind(day1, day2))
sums = rowSums(data)

K = length(mms)
N = sum(sums)

#----------------------------------#
# Convert counts of each colour    #
# to a vector: one for each choice #
#----------------------------------#
counts = c(rep(1, times = sums[1]), rep(2, times = sums[2]), rep(3, times = sums[3]), rep(4, times = sums[4]), rep(5, times = sums[5]), rep(6, times = sums[6]))


#------------------------------#
# Save data as a list for STAN #
#------------------------------#
dataList = list (
  N = N,
  K = K,
  counts = counts
)


#------------------------------#
#       Define the Model       #
#    see p. 288 of manual      #
#------------------------------#
modelString = "
  data {
    int<lower=1> N;
    int<lower=2> K;
    int<lower=1,upper=K> counts[N];
  }

  parameters {
    simplex[K] theta;
  }

  model {
    theta ~ dirichlet(rep_vector(1.0, K));
    for (n in 1:N) {
      counts[n] ~ categorical(theta);
    }
  }
"
writeLines(modelString, con="model.stan")


#--------------------------#
#     run STAN             #
#--------------------------#
stanFit <- stan(file = "model.stan", 
                data = dataList, 
                pars = "theta",
                warmup = 2000,
                iter = 7000, 
                chains = 3)


#-------------------------#
# Check MCMC Performance  #
#-------------------------#
print(stanFit)
stan_trace(stanFit, pars = c("theta"), inc_warmup = TRUE)


#------------------------#
#    Plot Parameters     #
#------------------------#
stan_plot(stanFit, par = "theta")
plot(stanFit, par = "theta", show_density = TRUE, ci_level = 0.95, fill_color = "skyblue")


#-----------------------#
#    Extract Data       #
#-----------------------#
mcmcChains = as.data.frame(stanFit)
chainLength = length(mcmcChains[, 1])


#-----------------------#
#  Plot posteriors      #
#-----------------------#
posteriors = c(mcmcChains[, 1], mcmcChains[, 2], mcmcChains[, 3], mcmcChains[, 4], mcmcChains[, 5], mcmcChains[, 6])
labels = c(rep("Brown", times = chainLength), rep("Blue", times = chainLength), rep("Orange", times = chainLength), rep("Yellow", times = chainLength), rep("Red", times = chainLength), rep("Green", times = chainLength))
postData = data.frame(posteriors, labels)
ggplot(postData, aes(x = posteriors, y = labels)) +
  theme_bw() +
  geom_density_ridges(fill = "dodgerblue", alpha = 0.5)



