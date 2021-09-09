
pwr = function(params, alpha, pwrFun, printProgress=TRUE)
{
	### Alpha
	if (missing(alpha)) alpha = 0.05
	if (class(alpha) != "numeric") stop("Parameter alpha must be numeric.")
	if (alpha < 0 | alpha > 1) stop(paste("Received parameter alpha = ", alpha, ". Alpha must be bounded between 0 and 1 (included)"))
	
	### params
	if (missing(params)) stop(paste("Parameter sampleSizes must be present"))
	if (class(params) != "data.frame") stop(paste("Parameter params must be a data.frame."))
	if (any(names(params) == "pwr")) stop("Parameter params already has a column named 'pwr'. The name 'pwr' must be reserved to indicate the estimated power.")

	### pwrFun
	if (class(pwrFun) != "function") stop("Parameter alpha must be a function")

	### Number of params valules to explore
	nbParamValues = nrow(params)
	if (nbParamValues == 0) stop("Parameter params is empty")

	### Initialize stuff to print progress
	if (printProgress)
	{
		nbSteps = min(nbParamValues,100)
		progress_step = round(nbParamValues / nbSteps)
		if (progress_step == 0) progress_step = 1
		cat(paste0(
			"[",
			paste(rep("_", nbSteps), collapse=""),
			"]"
		))
	}


	### Compute powers
	pwr = numeric(nbParamValues)
	for (index in 1:nbParamValues)
	{
		### Print progress
		if (printProgress)
		{
			if (index%%progress_step == 0) 
			{
				nbDone = ceiling(index/progress_step)
				nbLeft = nbSteps-nbDone
				cat(paste0(
					"\r",
					"[",
					paste0(rep("#", nbDone), collapse=""),
					paste0(rep("_", nbLeft), collapse=""),
					"]"
				))
			}
		}

		### Compute power
		pwr[index] = pwrFun(params=params[index,, drop=FALSE], alpha=alpha)
		if (is.na(pwr[index]) | !(pwr[index] >= 0 & pwr[index] <= 1))
		{
			stop(paste0("Parameter pwrFun returned a non-valid power estimate (returned ", pwr[index],") for sample parameters ", paste(c(params[index,]), collapse=" | "), "."))
		}
	}

	if (printProgress) cat("\n")
	return(pwr)
}
