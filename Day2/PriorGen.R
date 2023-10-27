# Day 2.1 Material
#################################

# How to choose and generate prior distributions?

require(PriorGen)

# Exercise 1: Check if the codes below create the prior parameters for slides 1 to 3.

?findbeta
# Example 1
findbeta(themean = .9, percentile = .95, lower.v = F, percentile.value = .80)
# Example 2
findbeta(themean = .99, percentile = .95, lower.v = F, percentile.value = .90)
# Example 3
findbeta(themean = .01, percentile = .70, lower.v = T, percentile.value = .05)
# Example 4
findbeta_abstract(themean.cat = "Average",thevariance.cat = "High")
# Example 5
# ?

?findbeta_abstract
?findbeta_panel
?findbeta_raw


# Exercise 2: What if I wanted to create a beta prior for the specificity and I knew that 
# 1a. the mean specificity lies between (0.4-0.6) and 
# 1b. we are 95% certain that it is lower than 0.8
out1<-findbeta(themean=0.5, percentile = 0.95, lower.v = T, percentile.value = 0.8)
# 1b. we are 99% certain that it is lower than 0.6
out2<-findbeta(themean=0.5, percentile = 0.99, lower.v = T, percentile.value = 0.88)


# Exercise 3: Plotting of priors based on hyperparameters of Beta distribution.
x=seq(0,1,0.001)
plot(out1,type="l",lwd=3,ylab="Density (Specificity - 1 & 2)",ylim=c(0,5))
lines(out2,type="l",lwd=3, col="red")
legend("topright",legend = c("Prior1","Prior2"),fill=c("black","red"))

# Plot the density for Example 3!
x=seq(0,1,0.001)
plot(x,dbeta(x,shape1 = 1.57, shape2 = 155.53),type="l",lwd=3,ylab="Density (Specificity)")

# Plot more examples
# ?
