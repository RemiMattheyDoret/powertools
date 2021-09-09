
get1DParamForPwr = function(target_pwr, isPlot=TRUE)
{
	target_pwr = 0.9
	model = loess(n ~ pwr, data=params)
	target_x = predict(model, newdata = data.frame(pwr = target_pwr), se=TRUE)$fit

	if (isPlot)
		print(ggplot(params, aes(x=n, y=pwr)) + geom_point() + geom_smooth(method="loess") + theme_classic(16) + geom_hline(yintercept=target_pwr) + geom_vline(xintercept=target_x) + geom_point(data=data.frame(pwr=target_pwr, n=target_x), color="red", size=2.5))

	return (target_x)
}
