# 0. Setting and loading packages  ####
#_______________________________

# loading required libraries
library(bayesm)
options(digits=2)

# loading required data
data(cheese)
str(cheese)

retailer <- levels(cheese$RETAILER)
nreg <- length(retailer)

# looping through the accounts an create a list of data items consisting
# of the X and y components of the linear regression model in each account
#

regdata <-  NULL

for (i in 1:nreg){
  filter <- cheese$RETAILER == retailer[i]
  y <- log(cheese$VOLUME[filter])
  X <- cbind(1, cheese$DISP[filter], log(cheese$PRICE[filter]))
  regdata[[i]] <- list(y=y, X=X)
  
}

#i <- 2

# invoking a hierarchical linear model ()
Data <- list(regdata=regdata)
Mcmc <- list(R=2000)

set.seed(7831)
system.time(out <- bayesm::rhierLinearModel(Data = Data, Mcmc = Mcmc))


# 1. dynamic pricing with hierarchical regression  ####
#________________________________________________

# loading required libraries
library(bayesm)
library(MCMCpack)

# loading required data
data(cheese)
str(cheese)

retailer <- levels(cheese$RETAILER)
nreg <- length(retailer)
beta.ls <- matrix(NA, nrow = nreg, ncol = 3)
colnames(beta.ls) <-  c("Intercept", "Disp", "log(Price)")

# Find OLS estimator
regdata <-  NULL

for (i in 1:nreg){
  filter <- cheese$RETAILER == retailer[i]
  y <- log(cheese$VOLUME[filter])
  X <- cbind(1, cheese$DISP[filter], log(cheese$PRICE[filter]))
  beta.ls[i,] <- lm(y ~ X - 1)$coefficients
  regdata[[i]] <- list(y=y, X=X)
  
}

# Find Hierarchical models estimators
Data <- list(regdata=regdata)
sim.size <- 2000
burn.in <- 1000
Mcmc <- list(R = sim.size)

set.seed(2008)
system.time(out <- bayesm::rhierLinearModel(Data = Data, Mcmc = Mcmc))

beta_draw <- out$betadraw[, 1, burn.in:sim.size]
int <- round(apply(beta_draw, 1, mean), 2)

beta_draw <- out$betadraw[, 2, burn.in:sim.size]
b_dip <- round(apply(beta_draw, 1, mean), 2)

beta_draw <- out$betadraw[, 3, burn.in:sim.size]
b_price <- round(apply(beta_draw, 1, mean), 2)

beta_retailer <- cbind.data.frame(retailer, int, b_dip, b_price)
