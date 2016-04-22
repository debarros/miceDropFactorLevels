#miceDropFactorLevels

# When pool.compare is called with method = likelihood,
# if levels were dropped from factor variables in one of the models
# (because those levels were not present in the data),
# an error will be generated:

########################################################  
##  > pool.compare(fit1,fit2)                         ##
##  Error in model.matrix(formula, data) %*% coefs :  ##
##  non-conformable arguments                         ##
##  >                                                 ##
########################################################  

# This is due to model.matrix creating columns for every level of a factor variable,
# regardless of whether those levels occur in the data.

library(mice)
a = rnorm(1000)  #generate random number set to use
d1 = data.frame(  #create some data
  x = a + sample(c(NA,0),1000,T,c(.4,.6)),
  y = a + rnorm(1000,1,2) + sample(c(NA,0),1000,T,c(.3,.7)),
  z = sort(factor(sample(c(1:3,NA),1000,T),1:4))[match(a, sort(a))]
)
d1.mids = mice(d1) #create multiple omputations
d1.mira1 = with(d1.mids,lm(x~z+y+I(y^2))) # repeated analysis on one model
d1.mira2 = with(d1.mids,lm(x~z+y)) # repeated analysis on subset model
d1.compare = pool.compare(d1.mira1, d1.mira2,d1.mids, "likelihood") # produces an error



d1$z = factor(d1$z) # set levels of z based on observations of z
d2.mids = mice(d1) #multiple imputations
d2.mira1 = with(d2.mids,lm(x~z+y+I(y^2))) # repeated analysis on one model
d2.mira2 = with(d2.mids,lm(x~z+y)) # repeated analysis on subset model
d2.compare = pool.compare(d2.mira1, d2.mira2,d2.mids, "likelihood") # no error

d2.mipo1 = pool(d2.mira1) # pool analysis on the first model
d2.mipo2 = pool(d2.mira2) # pool analysis on the first model
summary(d2.mipo1) 
summary(d2.mipo2)
d2.compare


