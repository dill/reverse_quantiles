# go from quantiles to mean/sd for log-Normal distribution
# based on working at https://www.johndcook.com/quantiles_parameters.pdf

# takes as input all of your samples, computes the quantiles
# and then returns the mean/sd of the variable, assuming it's
# log-normally distributed
ln_get_mean_var <- function(samples, q1=0.025, q2=0.975){

  # first get where the quantiles lie, x1, x2
  x1 <- quantile(samples, p=q1)
  x2 <- quantile(samples, p=q2)

  # now the inverse CDF ("quantile function", Phi^-1) for
  # Normal(0,1) at the two points defined above
  iCDF1 <- qnorm(q1)
  iCDF2 <- qnorm(q2)

  # now calculate the mean and sd
  mu <- (iCDF2 * log(x1) - iCDF1 * log(x2))/(iCDF2 - iCDF1)
  sigma <- (log(x1)-log(x2))/(iCDF1-iCDF2)

  # use unname() to remove names from the results
  return(list(mu=unname(mu), sigma=unname(sigma)))
}


# test
rvs <- rlnorm(10000, 100, 10)

ln_get_mean_var(rvs)

