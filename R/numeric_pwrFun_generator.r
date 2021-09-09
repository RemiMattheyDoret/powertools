

numeric_pwrFun_generator = function(testFun, nbIterations)
{
	if (missing(nbIterations)) nbIterations = 1e3
	if (missing(testFun)) stop("Parameter testFun is missing.")
	if (class(testFun) != "function") stop("Parameter testFun must be function (a function that samples data and return a p.value).")

	return(
		function(params,alpha)
		{
			return(mean(replicate(nbIterations, testFun(params) < alpha)))
		}
	) # returns a function
}

