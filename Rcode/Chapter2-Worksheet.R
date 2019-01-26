## Chapter 2, Worksheet for examples

sapply( "pause.R", source)
sapply( "Chapter2.R", source)

# Section 2.1
# Reproduce Figure 2.1 of BDA

split.screen(c(2,2))
screen(1)
n = 5; y = 3; inc = 0.001
post = theta_posterior_uniform( y, n, inc )
plot( post$theta, post$posterior , type = "l", yaxt = "n", ann = FALSE )
title( main = "n = 5, y = 3" )

screen(2)
n = 20; y = 12
post = theta_posterior_uniform( y, n, inc )
plot( post$theta, post$posterior , type = "l", yaxt = "n", ann = FALSE )
title( main = "n = 20, y = 12" )

screen (3)
n = 100; y = 60
post = theta_posterior_uniform( y, n, inc )
plot( post$theta, post$posterior , type = "l", yaxt = "n", ann = FALSE )
title( main = "n = 100, y = 60" )

screen(4)
n = 1000; y = 600
post = theta_posterior_uniform( y, n, inc )
plot( post$theta, post$posterior , type = "l", yaxt = "n", ann = FALSE )
title( main = "n = 1000, y = 600" )

#Sys.sleep(10)

pause("Proceed to Laplace's analysis")

dev.off()

# Laplace's analysis of female births

cat(paste("Laplace's analysis of female births:", 
"241,945 girls",
"251,527 boys",
"born in Paris between 1745 and 1770.",
"",
"Compute Pr( theta >= 0.50 | data )",
sep = "\n"))

y = 241945
n = 241945 + 251527

cat(paste("With uniform prior:", "Posterior distribution is Beta( y+1, n-y+1 )",
paste("Pr( theta >= 0.5 | y = 241,945, n = 251,527+241,945 ) = ", toString(pbeta(0.5, y+1, n-y+1, lower.tail = FALSE)), sep = ""), "" , sep = "\n"))

pause("Continue ... ")

cat(paste("Posterior predictive distribution describes behavior of new observation given the data",
"",
"Pr( y* | y ) = int_{theta} Pr( y | theta, y ) p( theta | y ) dtheta", 
"",
sep = "\n"))

pause("")

cat(paste("For the binomial model with uniform prior:",
"Pr( y* = 1 | y ) = int_{0}^1 theta * p( theta | y ) dtheta",
" = E( theta | y )", 
" = ( y+1 ) / ( n+2 )",
"",
"This is known as Laplace's law of succession.",
"","",
sep = "\n"))

pause("Applied to Laplace's problem ... ")

cat(paste("For Laplace's problem of female births, what is the probability that the next birth is a girl, given the past observation?", "", 
paste("Pr( y* = 1 | past data ) = ", toString( (y+1) / (n+2) ), sep = ""), 
"",
"Note: This calculation is obtained by averaging over the entire posterior distribution of theta given the data.",
"Uncertainty in the data and parameter is 'automatically' accounted for in this calculation.",
"",
sep = "\n"))

pause("")

cat("Section 2.2: Posterior as a compromise between data and prior information")
cat("BLACKBOARD")

pause("")

cat("Section 2.4: Informative prior distributions")

pause("")

cat(paste("We have already seen that the uniform prior on theta yields a Beta( y+1, n-y+1 ) posterior.",
"",
"In general, the Beta(a,b) prior yields a Beta( a+y-1, n+b-y+1 ) posterior for theta.",
"",
sep = "\n"))

pause("")

cat(paste("This yields the following calculations for the posterior:",
"",
"E( theta | y ) = ( a+y ) / ( a+b+n )",
"",
"Var( theta | y ) = ( E( theta | y )(1 - E( theta | y )) ) / ( a + b + n + 1 )",
"","",
sep = "\n"))

pause("Discussion of conjugate prior families ...")

cat(paste("Example (p. 37) of placenta previa", "",
"980 births (n = 980)",
"437 female (y = 437)","",
"What evidence for claim that female births in population of placenta previa births is less than 0.485?","",
sep = "\n"))

y = 437
n = 980

pause("First Analysis: With uniform prior")

cat(paste("Assuming Uniform prior on theta","",
"Given the data, the posterior distribution is Beta( 438, 544 )","",
"Compute the posterior probability of 'theta <= 0.485' given data:","","",
paste("Pr( theta <= 0.485 | data ) = ", toString( pbeta( 0.485 , y+1, n-y+1 ) ), sep = ""),"",
sep = "\n"))

pause("")

cat(paste("For a more precise assessment of the value of theta, we compute a posterior interval.","",
"First approach using the quantiles of the Beta( 438, 544 ) distribution, we obtain 95% posterior interval with","",
paste("Lower limit: ", toString(round(qbeta(0.025 , y+1, n-y+1 ),3)), sep = ""),
paste("Upper limit: ", toString(round(qbeta(0.975 , y+1, n-y+1 ),3)), sep = ""),"",
sep = "\n"))

pause("Alternatively, the interval limits can be approximated by simulation from the posterior.")

cat(paste("Simulate 1000 draws from the Beta( 438, 544 ) distribution.","",
"Use the empirical 2.5% and 97.5% quantiles as the lower and upper limits of the posterior interval.","",
sep = "\n"))

sims = rbeta(1000, y+1, n-y+1)
sims_order = sort(sims)

lower = sims_order[25]
upper = sims_order[976]

pause("Results of simulation ... ")

cat(paste("Simulation gives a posterior interval with","",
paste("Lower limit: ", toString(round(lower,3)), sep = ""),
paste("Upper limit: ", toString(round(upper,3)), sep = ""),"",
sep = "\n"))


pause("Second Analysis: With Beta prior")

cat(paste("In light of prior information, the uniform prior may not be reasonable.","",
"We expect the proportion of females to be somewhere between 40% and 50%, but the uniform prior places equal mass everywhere in the interval.","",
"With this in mind, we might specify a Beta( a , b ) prior with a = 0.485 and b = 0.515.","",
"Let's see how this affects the analysis.","",
sep = "\n"))

a = 0.485
b = 0.515

cat("First, we can plot the prior to see how it differs from the uniform prior.")

plot_beta( a , b , 0.001 )
pause("")
dev.off()

cat(paste("In this case, the posterior distribution is Beta( 437.485, 543.515 ),",
"which gives a 95% posterior interval with:", "",
paste("Lower limit: ", toString(round(qbeta(0.025 , a+y, n+b-y ),3)), sep = ""),
paste("Upper limit: ", toString(round(qbeta(0.975 , a+y, n+b-y ),3)), sep = ""),"",
sep = "\n"))

pause("What happens with a more informative prior?")

cat("Consider the Beta( 4.85, 5.15 ) prior")

a = 4.85
b = 5.15

plot_beta( a , b, 0.001 )

cat(paste("In this case, the posterior distribution is Beta( 441.85, 548.15 ),",
"which gives a 95% posterior interval with:","",
paste("Lower limit: ", toString(round(qbeta(0.025 , a+y, n+b-y ),3)), sep = ""),
paste("Upper limit: ", toString(round(qbeta(0.975 , a+y, n+b-y ),3)), sep = ""),"",
sep = "\n"))

cat("In this case, the data is sufficiently strong to override the prior distribution.")

cat("Section 2.5: Normal distribution with known variance")

pause("")

cat(paste("We assume a single observation y is drawn from the Normal( theta , sigma2 ) distribution, with sigma2 known:","",
"p( y | theta ) = (sqrt(2*pi*sigma2)) * exp( -( y-theta )^2 / ( 2*sigma2 ) ).","",
"A conjugate prior for theta is given by the Normal( mu0, tau02 ) distribution.", "", 
"In this case, the posterior distribution of theta given y can be computed in closed form as Normal( mu1, tau12 ), where","",
"mu1 = ( (mu0 / tau02) + (y / sigma2) ) / ( (1/tau02) + (1/sigma2) )",
"1/tau12 = (1 / tau02) + (1 / sigma2)","",
sep = "\n"))

pause("")

cat(paste("Posterior predictive distribution for the normal distribution.","",
"Y* | y ~ N( mu1 , sigma2 + tau12 )","",
sep = "\n"))

pause("Next: Normal model with multiple observations")

cat(paste(" ..... ", sep = "\n" ))

pause("")

cat("Section 2.6: Other standard single-parameter models")



cat("Example: Estimating the rate from count data")

cat(paste("Assume asthma deaths occur at a rate of theta per 100,000 people in a population","",
"In a population of 200,000 people, it is assumed that the number of deaths follows a Poisson distribution with intensity 2.0*theta, where the 2.0 = 200,000 / 100,000 is the exposure.","",
"We wish to do inference with unknown parameter theta.","",
sep = "\n"))

pause("First step: Set up the prior for theta")

cat(paste("Typical asthma mortality rates in Western countries are about 0.60 per 100,000, with rates above 1.5 very rare.","",
"Explore Gamma distribution for different parameters:","",
sep = "\n"))

a = 1; b = 1; mn = 0.001; mx = 2; inc = 0.0001;
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")
 
a = 5; b = 1
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")

a = .5; b = 1
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")

a = 1; b = .5
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")

a = .25; b = .5
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")

a = 3; b = 5
cat(paste("a = ", toString(a), "; b = ", toString(b), sep = ""))
plot_gamma(a, b, mn, mx, inc)
pause("")

cat(paste("Data: ", "",
"3 Deaths out of 200,000","",
"y = 3",
"x = 2.0","",
"Posterior distribution of theta given (y , x):","",
"theta | y , x ~ Gamma( 3 + y , 5 + x) ~ Gamma( 6 , 7 )",
sep = "\n"))

a = 3; b = 5
post = theta_posterior_gamma(3 , 2, a , b , mn , mx , inc )
plot( post$theta , post$post , type = "l", ylab = "density", xlab = "theta")
title( main = "Gamma(6,7) posterior" )

pause("")

y = 3; x = 2.0

cat(paste("95% posterior interval for theta given data:","",
paste("Lower limit: ", toString(0.0), sep = ""),
paste("Upper limit: ", toString(qgamma(0.95, a + y, b + x)), sep = ""),"",
sep = "\n"))

pause("")

cat(paste("Posterior probability that the asthma death rate is greater than 1.0 per 100,000:","",
paste("Pr( theta > 1.0 | past data ) = ", toString(pgamma(1.0 , a+y, b+x, lower.tail = FALSE)), sep = ""),"",
sep = "\n"))

pause("")

cat(paste("If the same rate of 3 per year is observed for a 10 year period, the posterior becomes:","",
"Gamma( a + y , b + x ) ~ Gamma( 33.0 , 25.0)","",
sep = "\n"))

y = 30; x = 20
plot_gamma( a + y, b + x, mn, mx, inc )

pause("")
dev.off()

cat(paste("The above 95% predictive interval with lower limit 0 has upper limit","",
paste("Upper limit: ", toString(qgamma(0.95 , a + y, b + x )), sep = ""),"",
"and the posterior probability that the death rate is greater than 1.0 is:","",
paste("Pr( theta  > 1.0 | y = 30, x = 20.0 ) = ", toString(pgamma( 1.0, a+y, b+x, lower.tail = FALSE)), sep = ""),"",
sep = "\n"))

pause("")

cat("Example: Cancer rates")

pause("")

cat(paste("Observe the rate of kidney cancer death county by county across the country.","",
"A large number of counties with the highest rate are in the midwest.","",
"But a large number of counties in the lowest rate are also in the midwest.","",
sep = "\n"))

pause("Bayesian model for cancer incidence.")

cat(paste("Assume number of deaths in county j follows","",
"y_j ~ Poisson( 10*n_j*theta_j ),",
"where","",
"y_j is the number of deaths",
"theta_j is the death rate per person per year",
"n_j is the population of county j.","",
"Since we have a different parameter for each county, we assign a prior","",
"theta_j ~ Gamma( a , b ) with a = 20, beta = 430,000, from which the posterior of county j is",
"theta_j | y_j ~ Gamma( 20 + y_j , 430,000 + 10*n_j ).","",
sep = "\n"))

pause("Stop here")
