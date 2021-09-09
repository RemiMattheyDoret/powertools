
require(ggplot2)
library(rgl)

sapply(list.files("/Users/remi/Documents/programming/R.Stats/powertools/R/", full.names=TRUE), source, .GlobalEnv)


###### With only one parameter. The sample size of a one sample t-test
myTest = function(params)
{
	t.test(rnorm(params$n,0.5,1))$p.value
}

pwrFun = numeric_pwrFun_generator(myTest, nbIterations=5e3)

params = data.frame(n=seq(2,80,by=4))
params$pwr = pwr(params, alpha=0.05, pwrFun)
get1DParamForPwr(target_pwr=0.9, isPlot=TRUE)



### 2D
myTest = function(params)
{
	return(
		anova(aov(
			c(rnorm(params$n1, 10, 1), rnorm(params$n2, 10.5, 1))
			~
			c(rep('A',params$n1), rep('B',params$n2))
		))$Pr[1]
	)
}

pwrFun = numeric_pwrFun_generator(myTest, nbIterations=2e2)

params = expand.grid(n1=seq(2,40,by=2), n2=seq(2,40,by=2))
params$pwr = pwr(params, 0.05, pwrFun)

#persp(
#	unique(params$n1),
#	unique(params$n2), 
#	matrix(params$pwr, ncol=length(unique(params$n1)), nrow=length(unique(params$n2)))
#)

image(
	unique(params$n1),
	unique(params$n2), 
	matrix(params$pwr, ncol=length(unique(params$n1)), nrow=length(unique(params$n2)))
)

