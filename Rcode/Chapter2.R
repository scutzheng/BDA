## Chapter 2 (Single parameter models)

# Section 2.1 - Estimating a probability from binomial data
# Binomial distribution for number of successes 'y' in 'n' exchangeable trials
# density:
# p( y | theta ) = choose( n , y ) * theta^y * ( 1-theta )^(n-y) 

# Example: estimate the sex ratio within a population of human births
# theta = proportion of female births in the population
# phi = ( 1-theta ) / theta = ratio of male to female births (alternative parameterization)
# y = number of girls in n recorded births

# Assuming Uniform prior for theta

theta_posterior_uniform <- function( y , n , inc = 0.01 ){
	ths = seq( inc, 1 - inc, inc ) 	# create sequence of candidate thetas 
	post = ths^y * (1-ths)^(n-y) / beta( y+1, n-y+1 )
	list( theta = ths, posterior = post )
}

# Assuming Beta(a,b) prior for theta

theta_posterior_beta <- function( y , n, a = 1, b = 1, inc = 0.01 ){
	ths = seq( inc, 1-inc, inc )	# create sequence of candidate thetas
	post = ths^( a + y - 1 ) * (1-ths)^( n + b - y - 1 ) / beta( a+y-1, n+b-y-1 )
	list( theta = ths, posterior = post )
}

# Plot the Beta( a, b) distribution

plot_beta <- function( a , b , inc = 0.01 ){
	ths = seq(inc, 1-inc, inc )	# create sequence of candidate values
	probs = ths^( a - 1 ) * ( 1-ths )^( b - 1 ) / beta( a , b )
	plot( ths, probs , type = "l", ylab = "density", xlab = "theta" )
}

# Plot the Gamma( a , b ) distribution

plot_gamma <- function( a , b, mn = 0.1, mx = 50, inc = 0.1 ){
	ths = seq(mn, mx, inc)	# create sequence of candidate values
	probs = dgamma( ths, a , b)
	plot( ths, probs, type = "l", ylab = "density", xlab = "theta")
}

theta_posterior_gamma <- function( y , x , a = 1 , b = 1 , mn = 0.1 , mx = 10 , inc = 0.01 ){
	ths = seq( mn , mx , inc )
	post = dgamma( ths, a + y, b + x )
	list( theta = ths, posterior = post )
}
