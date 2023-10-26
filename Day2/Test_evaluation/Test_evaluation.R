### Test evaluation without a gold standard

# 26 October 2023 - Eleftherios Meletis
library("runjags")


### Recap from Day 1

# - Sensitivity, Specificity - definitions

# - Bayes theorem - Markov Chain Monte Carlo

# - Apparent & true prevalence estimation

# Familiar with this JAGS model?
  
ap_model <- 
  'model {
  
  # Define likelihood distribution of the data
  # JAGS Binomial distribution Arguments: ap, n 
  
  y ~ dbin(ap,n)
  
  # Specify prior distribution for parameters of interest 
  # Uniform (non-informative) prior distribution 
  ap ~ dbeta(1,1)
  
  #data# n, y
  #monitor# ap
  #inits# ap
}
'

# What about this one? 
  
  
tp_model <- 
'model {
  
  # Define likelihood distribution of the data
  # JAGS Binomial distribution Arguments: ap, n 
  
  y ~ dbin(ap,n)
  
  ap <- tp * Se + (1-tp)*(1-Sp)
  # Specify prior distribution for parameters of interest 
  # Uniform (non-informative) prior distribution 
  tp ~ dbeta(1,1)
  
  # Prior distributions for Se, Sp
  Se ~ dbeta(100,9)
  Sp ~ dbeta(100,9)
  
  #data# n, y
  #monitor# tp, Se, Sp
  #inits# tp, Se, Sp
}
'

#  What's the problem in the tp model (degrees of freedom vs parameters of interest)

# Sensitivity - Specificity estimation with and without a gold standard

## Hui-Walter paradigm/model (1980)

#  - A particular model formulation that was originally designed for evaluating diagnostic tests in the absence of a gold standard

#  - Not originally/necessarily Bayesian - implemented using Maximum Likelihood 
  

# * If we don't know the true disease status, how can we estimate sensitivity or specificity for either test?

## Hui-Walter paradigm (1980)
  
#  * But we will use the data/observations from the manuscript published back in 1980.

# Encode the Table_1 data in RStudio
  
pop_1 = matrix(nrow=3,ncol=3)
rownames(pop_1) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_1) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_1[1,1] = 14
pop_1[1,2] = 4
pop_1[2,1] = 9
pop_1[2,2] = 528
#Total rows and columns
pop_1[1,3] = pop_1[1,1] + pop_1[1,2]
pop_1[2,3] = pop_1[2,1] + pop_1[2,2]
pop_1[3,1] = pop_1[1,1] + pop_1[2,1]
pop_1[3,2] = pop_1[1,2] + pop_1[2,2]
N_1 = sum(pop_1[1,1] + pop_1[1,2] + pop_1[2,1] + pop_1[2,2])
pop_1[3,3] = N_1
pop_1

## Now let's do pop_2
pop_2 = matrix(nrow=3,ncol=3)
rownames(pop_2) = c("Mantoux_Test_Pos", "Mantoux_Test_Neg", "Total")
colnames(pop_2) = c("Tine_Test_Pos", "Tine_Test_Neg", "Total")

pop_2[1,1] = 887
pop_2[1,2] = 31
pop_2[2,1] = 37
pop_2[2,2] = 367
#Total rows and columns
pop_2[1,3] = pop_2[1,1] + pop_2[1,2]
pop_2[2,3] = pop_2[2,1] + pop_2[2,2]
pop_2[3,1] = pop_2[1,1] + pop_2[2,1]
pop_2[3,2] = pop_2[1,2] + pop_2[2,2]
N_2 = sum(pop_2[1,1] + pop_2[1,2] + pop_2[2,1] + pop_2[2,2])
pop_2[3,3] = N_2
pop_2



## Hui-Walter model
  
#  - A particular model formulation that was originally designed for evaluating diagnostic tests in the absence of a gold standard

# - Also known as the two_test - two_population setting/paradigm

  ## Model Specification ('hw_definition')
  
hw_definition <- c("model{
  Population_1 ~ dmulti(prob_1, N_1)
  Population_2 ~ dmulti(prob_2, N_2)
  
  #Population_1
  
  # Test1+ Test2+
	prob_1[1] <- (prev[1] * ((se[1])*(se[2]))) + ((1-prev[1]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_1[2] <- (prev[1] * ((se[1])*(1-se[2]))) + ((1-prev[1]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_1[3] <- (prev[1] * ((1-se[1])*(se[2]))) + ((1-prev[1]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_1[4] <- (prev[1] * ((1-se[1])*(1-se[2]))) + ((1-prev[1]) * ((sp[1])*(sp[2])))
	
	#Population_2
  
  # Test1+ Test2+
	prob_2[1] <- (prev[2] * ((se[1])*(se[2]))) + ((1-prev[2]) * ((1-sp[1])*(1-sp[2])))
  
  # Test1+ Test2-
	prob_2[2] <- (prev[2] * ((se[1])*(1-se[2]))) + ((1-prev[2]) * ((1-sp[1])*(sp[2])))

  # Test1- Test2+
	prob_2[3] <- (prev[2] * ((1-se[1])*(se[2]))) + ((1-prev[2]) * ((sp[1])*(1-sp[2])))

  # Test1- Test2-
	prob_2[4] <- (prev[2] * ((1-se[1])*(1-se[2]))) + ((1-prev[2]) * ((sp[1])*(sp[2])))

  prev[1] ~ dbeta(1, 1)
  prev[2] ~ dbeta(1, 1)
  
  se[1] ~ dbeta(1, 1)T(1-sp[1], )
  sp[1] ~ dbeta(1, 1)
  se[2] ~ dbeta(1, 1)T(1-sp[2], )
  sp[2] ~ dbeta(1, 1)

  #data# Population_1, Population_2, N_1, N_2
  #monitor# prev, prob_1, prob_2, se, sp
  #inits# prev, se, sp
}
")

library('runjags')

Population_1 <- as.numeric(pop_1[1:2,1:2])
Population_2 <- as.numeric(pop_2[1:2,1:2])


prev <- list(chain1=c(0.05,0.99), chain2=c(0.95,0.05))
se <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))
sp <- list(chain1=c(0.5,0.99), chain2=c(0.99,0.5))

results <- run.jags(hw_definition, n.chains=2)


# Remember to check convergence and effective sample size!
  
plot(results)
pt <- plot(results)
print(pt[["prev[1].plot1"]])
print(pt[["se[1].plot1"]])
print(pt[["sp[1].plot1"]])
print(pt[["sp[1].plot3"]])

summary(results)

## Exercise
  
#  Run the model and compare the results we the ones obtained from the original Hui-Walter model.

## Points to discuss


# 1. What is the latent variable here - also discuss about it tomorrow.

# 2. Can this type of models support more tests and more populations?
  
# 3. What is conditional (in)depedence between diagnostic tests and can we adjust for that?
  
## Any questions?
  
