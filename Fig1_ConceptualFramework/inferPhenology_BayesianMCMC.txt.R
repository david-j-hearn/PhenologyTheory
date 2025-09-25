#source("../SharedCode/theoryOfPhenology.txt.R")
source("theoryOfPhenology.txt.R")

if(!require(fitdistrplus)) { install.packages("fitdistrplus") }

library(fitdistrplus)

betaMean = function(shape1, shape2)
{
	shape1=as.numeric(shape1)
	shape2=as.numeric(shape2)
	return(shape1 / (shape1+shape2))
}

betaVariance = function(shape1, shape2)
{
	shape1=as.numeric(shape1)
	shape2=as.numeric(shape2)
	return( (shape1*shape2) / ( (shape1+shape2)*(shape1+shape2)*(shape1+shape2+1) ) )
}

covariance = function(onset="beta", duration="beta", oParList, dParList, min=0, max=365, N=20000)
{
	onset_f = get(paste("r", onset, sep=""))
	duration_f = get(paste("r", duration, sep=""))

	o = min + (max-min) * do.call(onset_f, c(list(n=N), oParList))
	d = (max-o) * do.call(duration_f, c(list(n=N), dParList))

	eO = mean(o[1:N/2])
	eD = mean(d[1:N/2])
	eOD = mean(o[(N/2+1):N] * d[(N/2+1):N])
	return(list(oMean = eO, oDuration = eD, covOE = eOD-eO*eD))
}


#obtain initial parameter values for MCMC run based on provided random sampler function rfunc and its associated parameters initParList. The oParList and dParList must be initialized to the length of the onset distribution parameters and duration distribution parameters
initializeParameters = function( rfunc, initParList, oParList, dParList)
{
	for(i in 1:length(oParList)) 
	{
		oParList[[i]] = do.call(rfunc, c(list(n=1),initParList))
	}
	for(i in 1:length(dParList)) 
	{
		dParList[[i]] = do.call(rfunc, c(list(n=1),initParList))
	}
	return(list(oParList=oParList,dParList=dParList))
}

#obtain the a and b beta shape parameters from the mean and variance parameters
getShapeParameters = function(mean, variance,m=0,M=365)
{
	mean = (mean-m)/(M-m)
	variance = variance / ((M-m)^2)
	a = mean * ( -(mean*mean) + mean - variance) / variance
	b = (mean*mean*mean -2*mean*mean + mean*variance + mean - variance) / variance
	return(list(shape1=a, shape2=b))
}

#Search heuristically for maximum likelihood parameter estimates based on provided vector of observed times (data), starting ML search parameters, and maximum range of parameter values.
#beta onset and duration only
getShapeParams.ML.estimate = function(data, min=0, max=365, minML=0.1, maxML=8000, resample=FALSE, oParList=NULL, dParList=NULL)
{
	if(is.null(oParList) || is.null(dParList))
	{	
		print("Getting initial estimates of parameters")
		pars = getShapeParams.estimate(data, min=min, max=max)
		oParList = pars$oParList
		dParList = pars$dParList
	}
	print("Fitting parameters")

	fdb = tryCatch(
		       {

			       message("Trying to fit the distribution...")
			       fitdist(data, "X_beta_beta", start=list(oShape1=oParList[[1]], oShape2=oParList[[2]], dShape1=dParList[[1]], dShape2=dParList[[2]]), lower=minML, upper=maxML)

		       },
		       error=function(cond) 
		       {
			       message("Failed initial attempt to apply ML. Resampling data and trying again.")
			       data = sample(data, size=length(data), replace=TRUE)
			       fdb = fitdist(data, "X_beta_beta", start=list(oShape1=oParList[[1]], oShape2=oParList[[2]], dShape1=dParList[[1]], dShape2=dParList[[2]]), lower=minML, upper=maxML)
			       return(fdb)
		       },
		       #warning=function(cond) 
		       #{
		       #message(cond)
		       #},
		       finally={
			       message("Something unknown happened during the ML optimization")
		       }
	)    
	oParList = list(shape1=fdb$estimate[[1]], shape2=fdb$estimate[[2]])
	dParList = list(shape1=fdb$estimate[[3]], shape2=fdb$estimate[[4]])
	return( list( oParList = oParList, dParList = dParList ) )
}

#VERY rudimentary function to get initial values for ML estimation - not recommended!
#mo = estimate of onset from prior knowledge (default is 100 days)
#sdo = estimate of average deviation from onset based on prior knowledge (default is 1 week)
#md = estimate of duration of phenophase from prior knowledge (default is 35 days)
#sdd = estimate of average deviation of duration based on prior knowledge (default is 5 days)
#defaults are based on spring ephemeral phenologies
getShapeParams.estimate = function(data, min=0, max=365, mo=100, sdo=7, md=35, sdd=5)
{
	md = md*(max-min)/(max-mo)+min
	sdd = sdd*(max-min)/(max-mo)


	mo = (mo-min)/(max-min)
	md = (md-min)/(max-min)

	sdd = (sdd-min)/(max-min)
	sdo = (sdo-min)/(max-min)

	vo = sdo*sdo
	vd = sdd*sdd

	ao = mo * ( -(mo*mo) + mo - vo) / vo
	bo = (mo*mo*mo -2*mo*mo + mo*vo + mo - vo) / vo

	ad = md * ( -(md*md) + md - vd) / vd
	bd = (md*md*md -2*md*md + md*vd + md - vd) / vd


	return( list( oParList = list(shape1=ao, shape2=bo), dParList = list(shape1=ad, shape2=bd) ) )
}


#Depricated - NOT RECOMMENDED
getShapeParams.Onset.estimate = function(data, min=0, max=365, m_mh=1, m_sh=-2.5, m_c=0, v_mh=0, v_sh=2, v_c=0)
{
	data = (data - min) / (max - min)
	m_hat = mean(data)
	s_hat = sd(data)

	m = m_mh*m_hat + m_sh*s_hat + m_c
	v = v_mh*m_hat + v_sh * (s_hat^2) + v_c

	a = m * ( -(m*m) + m - v) / v
	b = (m*m*m -2*m*m + m*v + m - v) / v

	return(list(shape1 = a, shape2 = b))

}

#Depricated - NOT RECOMMENDED
getShapeParams.Duration.estimate = function(data, min=0, max=1, mf=0.85, vf=0.5, s1=1.666667,s2=2)
{
	data = (data - min) / (max - min)
	m_hat = mean(data)
	s_hat = var(data)

	m = s1*s2*(1-mf)*m_hat
	v = vf * s_hat

	a = m * ( -(m*m) + m - v) / v
	b = (m*m*m -2*m*m + m*v + m - v) / v

	return(list(shape1 = a, shape2 = b))

}



#Returns log value of normally distributed prior, truncated, but not normalized; assumes parameter independence; onset and duration must be beta distributed
logDPrior.normalForBetaBeta = function(oParList, dParList, priorParList = list(vmo = 25, mo = 100, vvo = 25, vo = 50, vmd = 25, md = 25, vvd = 25, vd = 50, min=0, max=365))
{
	oMean = priorParList$min + (priorParList$max-priorParList$min)*betaMean(oParList[[1]], oParList[[2]])
	oVar = ((priorParList$max-priorParList$min)^2) * betaVariance(oParList[[1]], oParList[[2]])
	dMean = (priorParList$max-oMean)*betaMean(dParList[[1]], dParList[[2]])
	dVar = ((priorParList$max-oMean)^2) * betaVariance(dParList[[1]], dParList[[2]])

	if(oMean<min || oVar<0 || dMean<0 || dVar<0)
		{
		return(-Inf)
		}
	if(oMean>max || oVar>(max-min)*(max-min) || dMean>max-oMean || dVar>(max-oMean)*(max-oMean))
		{
		return(-Inf)
		}

	#prior for mean of onset
	pMO = dnorm(x=oMean, mean=priorParList$mo, sd=sqrt(priorParList$vmo))
	#prior vor variance of onset
	pVO = dnorm(x=oVar, mean=priorParList$vo, sd=sqrt(priorParList$vvo))
	#prior for mean of duration
	pMD = dnorm(x=dMean, mean=priorParList$md, sd=sqrt(priorParList$vmd))
	#prior for variance of duration
	pMV = dnorm(x=oVar, mean=priorParList$vd, sd=sqrt(priorParList$vvd))

	return(log(pMO) + log(pVO) + log(pMD) + log(pMV))
}

#Returns log value of uniform prior, but with exponential increase between 0 and the minimum of the uniform distribution to accommodate smaller-than-expected values; assumes parameter independence; onset and duration must be beta distributed
logDPrior.uniformExpForBetaBeta = function(oParList, dParList, priorParList=list(mo=50,Mo=365,md=7,Md=365,mvo=1,Mvo=365*365,mvd=1,Mvd=365*365,lamb=1,min=0,max=365))
{
	min = priorParList$min
	max = priorParList$max
	mo = priorParList$mo
	Mo = priorParList$Mo
	md = priorParList$md
	Md = priorParList$Md
	mvo = priorParList$mvo
	Mvo = priorParList$Mvo
	mvd = priorParList$mvd
	Mvd = priorParList$Mvd
	lamb = priorParList$lamb

	mO = min + (max-min)*betaMean(oParList$shape1,oParList$shape2)
	vO = betaVariance(oParList$shape1, oParList$shape2)*(max-min)^2
	mD = (max-mO)*betaMean(dParList$shape1,dParList$shape2)
	vD = betaVariance(dParList$shape1, dParList$shape2)*(max-mO)^2

	if(mO>Mo || vO > Mvo || mD > Md || vD > Mvd) { return(-Inf) } 
	if(mO<=0 || vO <= 0 || mD<=0 || vD<=0) { return(-Inf) } 
	pmo = uefbbHelper(p=mO, lamb=lamb, m=mo, M=Mo)
	pmvo = uefbbHelper(p=vO, lamb=lamb, m=mvo, M=Mvo)
	pmd = uefbbHelper(p=mD, lamb=lamb, m=md, M=Md)
	pmvd = uefbbHelper(p=vD, lamb=lamb, m=mvd, M=Mvd)

	return(pmo+pmd+pmvo+pmvd)
}

#helper function for logDPrior.uniformExpForBetaBeta
uefbbHelper = function(p,lamb,m,M)
	{
		top1 = exp(lamb*p)
		top2 = exp(lamb*m)
		den = (exp(m*lamb)-1)/lamb + exp(lamb*m)*(M-m)
	if(top1==Inf) { top1 = 0 }
		#print(c(top1,top2,den))
	#return(log((exp(lamb*p)*(p<=m) + exp(lamb*m)*(p>m && p<=M))/((exp(m*lamb)-1)/lamb + exp(lamb*m)*(M-m))))
	val = log((top1*(p<=m) + top2*(p>m && p<=M))/den)
	if(is.nan(val)) { stop("Produced a NaN value during priors calculations. Try setting lamb to a smaller value in your list of prior hyperparameter values") }
	return(val)
	}

#Returns log value of uniform prior; does not assume beta onset beta duration; assumes parameter independence
logDPrior.uniformExp = function(oParList, dParList, priorParList=list(min=1,max=2000,lamb=1))
{
	params = unlist(list(oParList,dParList))
	len = length(params)
	pB_U = 0 + (params <= priorParList$max)
	if(sum(pB_U) < len) {
		return(-Inf)
	}

	lamb = priorParList$lamb
	min = priorParList$min
	max = priorParList$max

	tot = 0
	for(p in names(oParList))
		{
		par = oParList[[p]]
		tot = tot+ log((exp(lamb*par)*(par<=min) + exp(lamb*priorParList$min)*(par>min && par<=max))/((exp(min*lamb)-1)/lamb + exp(lamb*min)*(max-min)))
		}

	for(p in names(dParList))
		{
		par = dParList[[p]]
		tot = tot+ log((exp(lamb*par)*(par<=min) + exp(lamb*priorParList$min)*(par>min && par<=max))/((exp(min*lamb)-1)/lamb + exp(lamb*min)*(max-min)))
		}
	return( tot )

}



#Returns log value of uniform prior
logDPrior.uniform = function(oParList, dParList, priorParList= list(min=1,max=1000)) 
{
	params = unlist(list(oParList,dParList))
	len = length(params)
	pB_L = 0 + (params >= priorParList$min)
	pB_U = 0 + (params <= priorParList$max)
	if(sum(pB_L, pB_U) < 2*len) {
		return(-Inf)
	}
	val = len * log( 1 / (priorParList$max - priorParList$min) )
	return( val )
}


#Depricated - DO NOT USE
logDPrior.narrowBetaOnset = function(oParList, dParList, priorParList= list(min=1,max=1000,rate=1000)) 
{
	params = unlist(list(oParList,dParList))
	len = length(params)
	pB_L = params >= priorParList$min
	pB_U = params <= priorParList$max
	if(sum(pB_L, pB_U) < 2*len) {
		return(-Inf)
	}
	#val = len * log( 1 / (priorParList$max - priorParList$min) )
	val = dexp( betaVariance( oParList$shape1, oParList$shape2 ), rate = priorParList$rate )
	return( log( val ) )
}

#Wrapper for prior distribution function
calculateLogPrior = function(oParList=NULL, dParList=NULL, logDPrior=logDPrior.uniform, priorParList = list(min=1,max=1000), verbose=FALSE)
{
	if(is.null(oParList) || is.null(dParList))
	{
		message("Paramter values must be set to calculate the priors")

	}
	if(verbose) 
	{
		print("Calculating log prior")
	}

	val = do.call(logDPrior, list("oParList" = oParList, "dParList" = dParList, "priorParList"=priorParList))

	if(verbose) 
	{
		print("Done Calculating log prior")
		print(val)
	}

	return(val)
}


#Create log likelihood value for a set of input data and a provided density function 
#f_X is the sampling density function of phenology times (should be generated by function get.f_X for phenology data, but can be any pdf function with parameters built in) 
#	It must be vectorized (get.f_X, by construction, it is vectorized)
calculateLogLikelihood = function(data, f_X)
{
	vals = log( f_X(data) )	
	return( sum(vals) )
}

#Provide numerator of Bayes rule
calculateLogPosterior.unnormalized = function( logPrior, logLikelihood )
{
	return( logPrior + logLikelihood )
}

#Generates candidate parameters using a Brownian random walk. 
#Users provide the walk rate for onset parameters (oRate) and duration parameters (dRate)
#The slower the rates, the faster the mixing rate, in general
#bias should be kept at 0
#known bug: Mixing rate is not correctly calculated
generateCandidateParameterValues = function(bias=0, oRate=0.1, dRate = 0.1, oParList, dParList, BB=F)
{
	BBi=BB
	onsetChanged = TRUE

	if(BBi) {

		item = sample(c(1,2,3,4), 1) 

		oMean = betaMean(oParList[[1]], oParList[[2]])
		oVar = betaVariance(oParList[[1]], oParList[[2]])
		dMean = betaMean(dParList[[1]], dParList[[2]])
		dVar = betaVariance(dParList[[1]], dParList[[2]])

		dVarScale = dMean/dVar
		oVarScale = oMean/oVar

		if(item==1) oMean = abs(oMean + rnorm(1, bias, oRate))
		if(item==2) oVar = abs(oVar + rnorm(1, bias, oRate) / oVarScale)

		if(item==3) dMean = abs(dMean + rnorm(1, bias, dRate))
		if(item==4) dVar = abs(dVar + rnorm(1, bias, dRate) / dVarScale)

		if( oMean<0 || oMean > 1 || dMean<0 || dMean > 1 || oVar<0 || dVar<0 ) {
			BBi=FALSE
			oRate = 100*oRate
			dRate = 100*dRate
		}

		else {
			if(item==3 || item==4) onsetChanged = FALSE
			dParList[[1]] = dMean * ( -(dMean*dMean) + dMean - dVar) / dVar
			dParList[[2]] = ((dMean^3) - 2 * (dMean^2) + dMean*dVar + dMean - dVar) / dVar

			oParList[[1]] = oMean * ( -(oMean*oMean) + oMean - oVar) / oVar
			oParList[[2]] = ((oMean^3) - 2 * (oMean^2) + oMean*oVar + oMean - oVar) / oVar
		}

	} 
	if(!BBi)
	{
		for(i in 1:length(oParList)) 
		{
			oParList[[i]] = abs(oParList[[i]] + rnorm(1, bias, oRate))
		}
		for(i in 1:length(dParList)) 
		{
			dParList[[i]] = abs(dParList[[i]] + rnorm(1, bias, dRate))
		}
	}
	return(list(oCandParList=oParList,dCandParList=dParList, onsetChanged=onsetChanged))
}

#return a list with two functions, the f_X density function and the f_C density function based on provided input parameters and onset and duration density types. Smaller n is faster, but less precise numerical estimates. M and m are the time period start and end times 
get.f_X.f_C = function(m, M, n, onset, duration, oParList, dParList)
{
	fCs = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
	CDF = F_C.vector(x=fCs$x, y=fCs$y)
	F_C = F_C.function(x=fCs$x, y=CDF)
	Pts = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
	fXs = f_X.vector(x=fCs$x, Pts)
	f_X = f_X.function(x=fCs$x, y=fXs)
	return( list(f_X=Vectorize(f_X, vectorize.args=c("x")),fCs=fCs) )
}

#return the f_X density function based on provided input parameters and onset and duration density types. Smaller n is faster, but less precise numerical estimates. M and m are the time period start and end times 
get.f_X = function(m, M, n, onset, duration, oParList, dParList)
{
	#print(dParList)
	fCs = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)

	#print("Getting F_C.vector")
	CDF = F_C.vector(x=fCs$x, y=fCs$y)

	#print("Getting f_C.function")
	F_C = F_C.function(x=fCs$x, y=CDF)

	#print("Getting P_t.vector")
	Pts = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
	if(is.null(Pts)) { return(NULL) }

	#print("Getting f_X.vector")
	fXs = f_X.vector(x=fCs$x, Pts)

	#print("Getting f_X.function")
	f_X = f_X.function(x=fCs$x, y=fXs)

	return( Vectorize(f_X, vectorize.args=c("x")) )
}

#Print the ongoing trace during an MCMC run
printDiagnostics = function(iter, totAcceptedO, totAcceptedD, oParList, dParList, pr, lk, post, onset="beta", duration="beta", oRate, dRate, s=1, min=0, max=365)
{
	oPars = paste(unlist(oParList), collapse=' ')
	dPars = paste(unlist(dParList), collapse=' ')

	mvText = ""
	if(onset == "beta" && duration == "beta") 
	{
		meanO = min + (max-min)*betaMean( oParList[[1]], oParList[[2]] ) 	
		varO = ((max-min)^2 ) * betaVariance( oParList[[1]], oParList[[2]] ) 	
		meanD = (max-meanO)*betaMean( dParList[[1]], dParList[[2]] ) 	
		varD = ((max-meanO)^2 ) * betaVariance( dParList[[1]], dParList[[2]] )
		mvText = paste( "o mean", meanO, "o var", varO, "d mean", meanD, "d var", varD)
	}

	print( paste( "i, (onset), (duration), accept, pr, lk, post, oRate, dRate ", iter, "(", oPars, ") (", dPars , ")", totAcceptedO, totAcceptedO / (iter/s), totAcceptedD, totAcceptedD / (iter/s), pr, lk, post, oRate, dRate, mvText ) ) 
}

#As written, this MCMC requires a symmetric proposal. Brownian random walk is the default
mcmc = function(
		data,
		n = 1000, 	#number of increments to use in numerical integration routines
		rInitFunc = rnorm, #random sampling function to sample initial parameter values - not used if useMLToInit or usePriorToInit are set to TRUE
		initParList = list(mean=50,sd=0.1),	#parameters for the rInitFunc - not used if useMLToInit or usePriorToInit are set to TRUE
		onset = "beta", 	#the distribution of the onset. This should be a density sampling function in R without the r in front, for example "beta" 
		oParList = list(shape1=50, shape2=50),	#Initial parameter values for the onset distribution - not used if useMLToInit or usePriorToInit are set to TRUE
		duration = "beta", #the distribution of the duration. This should be a density sampling function in R without the r in front, for example "beta"
		dParList = list(shape1=50, shape2=50),	#Initial parameter values for the duration distribution - not used if useMLToInit or usePriorToInit are set to TRUE
		useInputParams = FALSE, #If set to TRUE, uses the provided input parameters; overridden by usePriorToInit and by useMLToInit
		min = 0,	#the start time of the phenological time period; for an annual time period, the start is 0 days
		max = 365,	#the end time of the phenological time period; for an annual time period, the end time is 365 days
		logDPrior = logDPrior.normalForBetaBeta, #sets the prior distribution function. Can be logDPrior.normalForBetaBeta, logDPrior.uniform, or logDPrior.uniformExpForBetaBeta
		#April 10 mean onset, one week standard deviation on onset
		#Mean duration is 25 days (3-4 weeks) with 1 week variation
		priorParList = list(vmo = 25, mo = 100, vvo = 25, vo = 50, vmd = 25, md = 25, vvd = 25, vd = 50, min=0, max=365),	#set the hyperparameters for the prior distribution funciton
		usePriorToInit = FALSE,	#use the prior hyperparameters to specify the initial parameter values
		useMLToInit=FALSE,	#use ML to get the initial parameter values. If usePriorToInit is set to TRUE, the initial parameters for the ML search are based on the prior hyperparameters
		proposalBias = 0,	#Depricated
		proposalRate = 0.8,	#Set the rate of the Brownian walk to generate candidate parameter values. Both onset and duration random walks start with the same rate
		proposalAdapt = 0.0,	#Adapt the candidate random walk rate. Not currently implemented correctly, so use with extreme caution! Ideally, will adapt rates to optimize mixing rate
		useBetaProposal = F,	#Counterintuitively, if set to TRUE, the beta distributions, if used, will be parametrized by mean and variance rather than by shape parameters
		nIter = 10000,	#The number of iterations to run the MCMC
		sampleInterval = 100,	#not currently implemented, currently saves state of every iteration
		printInterval = 10,	#print the MCMC state to the screen every printInterval iterations
		graphResults = FALSE,	#If a real-time graphical output of the MCMC progress is desired, set to TRUE; must have access to graphics drivers when run
		graphInterval = 10,	#Generates graphics every graphInterval iterations of the MCMC
		graphPopulationSize = 10000,	#Provides estimate of population size from which sample is taken - only needed to display distributions of the phenological extremes
		trueOParList = NULL,	#If the true parameters are known, provide these here for reporting and display purposes
		trueDParList = NULL,	#If the true parameters are known, provide these here for reporting and display purposes
		trueParams = list(k1=0,o=0,x=0,c=0,kn=0,xvar=0,ovar=0,cvar=0)	#If the values of simulated data are known, provides these here for reporting and display purposes
)
{
	#initialize diagnostics
	BB=FALSE
	dRate = proposalRate
	oRate = proposalRate
	s = 2
	if(onset == "beta" && duration == "beta" && useBetaProposal) { BB=TRUE }
	else { s = 1 }

	sampledOParams = list()
	sampledDParams = list()
	lk = rep(1,nIter)
	pr = rep(1,nIter)
	post = rep(1,nIter)
	totAcceptedO=0
	totAcceptedD=0

	#set the initial parameters so that their prior probability is positive
	print("Initializing parameters")
	pr[1] = -Inf
	if( useMLToInit && onset == "beta" && duration == "beta")
	{
		oParList = NULL;
		dParList = NULL;
		print("Initializing parameters to maximum likelihood estimates")
		if(usePriorToInit)
		{
			print("Setting initial ML estimates to means of prior distribution following up with ML estimation, and using priorParList:")
			oParList = getShapeParameters(priorParList$mo, priorParList$vo,m=min,M=max)
			dParList = getShapeParameters(priorParList$md, priorParList$vd,m=0, M=max-priorParList$mo)
		}
		MLParams = getShapeParams.ML.estimate(data, oParList=oParList, dParList=dParList, min=min, max=max)
		oParList = MLParams$oParList
		dParList = MLParams$dParList
		pr[1] = calculateLogPrior(oParList=oParList, dParList=dParList, logDPrior = logDPrior, priorParList = priorParList, verbose=TRUE)
	}
	else if( usePriorToInit && onset == "beta" && duration == "beta")
	{
		print("Initializing parameters to means of prior distributions:")
		oParList = getShapeParameters(priorParList$mo, priorParList$vo,m=min,M=max)
		dParList = getShapeParameters(priorParList$md, priorParList$vd,m=0, M=max-priorParList$mo)
		print(oParList)
		print(dParList)
		pr[1] = calculateLogPrior(oParList=oParList, dParList=dParList, logDPrior = logDPrior, priorParList = priorParList)
	}
	else if( useInputParams )
	{
		print("Using provided parameters as initial parameters")
		pr[1] = calculateLogPrior(oParList=oParList, dParList=dParList, logDPrior = logDPrior, priorParList = priorParList)
		print(pr[1])
	}

	#in case any of the above initialization procedures failed, attempt to generate feasible initial parameter values
	while(pr[1] == -Inf)
	{
		#sample the initial parameters
		print("Initializing parameters to random values")
		initParams = initializeParameters( rfunc = rInitFunc, initParList = initParList, oParList =oParList, dParList=dParList)
		oParList = initParams$oParList
		dParList = initParams$dParList

		#calculate the prior value for the initial parameter values
		pr[1] = calculateLogPrior(oParList=oParList, dParList=dParList, logDPrior = logDPrior, priorParList = priorParList)
	}

	sampledOParams[[1]] = oParList
	sampledDParams[[1]] = dParList

	print("Finished initializing parameters to the following values:")
	print(oParList)
	print(dParList)

	#set up the sampling distribution to calculate the likelihood with the initial parameter values
	f_X = get.f_X(m=min, M=max, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)

	if(is.null(f_X)) { stop("Couldn't generate f_X function. Quitting MCMC") }

	#determine the log likelihood of the data given the initial parameters
	lk[1] = calculateLogLikelihood(data=data, f_X=f_X)

	#determine the log posterior (unnormalized) of the initial parameters 
	post[1] = calculateLogPosterior.unnormalized( logPrior=pr[1], logLikelihood=lk[1] )

	print(paste("Length of oParList: ", length(oParList)))
	print(paste("Length of dParList: ", length(dParList)))

	printDiagnostics(iter=1, totAcceptedO=0, totAcceptedD=0, oParList=oParList, dParList=dParList, pr=pr[1], lk=lk[1], post=post[1], onset=onset, duration=duration, oRate=oRate, dRate=dRate, s=s)

	#perform the MCMC
	for( i in 2:(nIter+1)) 
	{
		#sample candidate parameter values based on random walk from the current parameters
		candParams = generateCandidateParameterValues(bias=proposalBias, oRate=oRate, dRate=dRate, oParList=oParList, dParList=dParList, BB=BB)
		oCandParList = candParams$oCandParList
		dCandParList = candParams$dCandParList
		onsetChanged = candParams$onsetChanged

		#determine the log prior probability of the candidate
		prCand = calculateLogPrior(oParList=oCandParList, dParList=dCandParList, logDPrior = logDPrior, priorParList = priorParList)

		if(prCand == -Inf)
		{
			#no sense calculating the likelihood if the prior is 0
			#no change in parameters, update current iteration to previous values
			pr[i] = pr[i-1]
			lk[i] = lk[i-1]
			post[i] = post[i-1]
			next
		}

		#set up the sampling distribution to calculate the likelihood with the candidate parameter values
		f_X = get.f_X(m=min, M=max, n=n, onset=onset, duration=duration, oParList=oCandParList, dParList=dCandParList)

		keep = FALSE
		if(is.null(f_X)) { keep = TRUE }

		if(!keep)
		{
			#determine the log likelihood of the data given the candidate parameters
			lkCand = calculateLogLikelihood(data=data, f_X=f_X)

			#determine the log posterior (unnormalized) of the candidates as log prior + log likelihood
			postCand = calculateLogPosterior.unnormalized( logPrior=prCand, logLikelihood=lkCand)
		}
		else
		{
			postCand = post[i-1]
		}


		#calculate acceptance ratio and accept or reject candidate
		A = postCand - post[i-1]
		A = exp(A)
		u = runif(1,0,1)
		if(u <= A && !keep) 
		{
			#track mixing rate - currently buggy!
			if(onsetChanged) totAcceptedO=totAcceptedO+1
			if(!onsetChanged || !BB) totAcceptedD=totAcceptedD+1

			#assign the candidate to the next iteration of current parameters
			oParList = oCandParList
			dParList = dCandParList

			pr[i] = prCand
			lk[i] = lkCand
			post[i] = postCand
		}
		if(u > A || keep)
		{
			#no change in parameters, update current iteration to prior values
			pr[i] = pr[i-1]
			lk[i] = lk[i-1]
			post[i] = post[i-1]
		}

		sampledOParams[[i]] = oParList
		sampledDParams[[i]] = dParList

		#print diagnostics to the screen if at the print interval
		if( i %% printInterval == 0)
		{
			printDiagnostics(iter=i, totAcceptedO=totAcceptedO, totAcceptedD=totAcceptedD, oParList=oParList, dParList=dParList, pr = pr[i], lk = lk[i], post = post[i], onset=onset, duration=duration, oRate=oRate, dRate=dRate, s=s)
			#estimate mixing rate, currently buggy!
			if(totAcceptedO/(i/s) < 0.3) { oRate = oRate*(1-proposalAdapt) }
			else { oRate = oRate*(1+proposalAdapt) }
			if(totAcceptedD/(i/s) < 0.3) { dRate = dRate*(1-proposalAdapt) }
			else { dRate = dRate*(1+proposalAdapt) }
		}
		#provide a real-time visual display of the MCMC status
		if( graphResults ) 
		{
			if( i %% graphInterval == 0 || i ==2)
			{

				m=min
				M=max
				N=graphPopulationSize
				n=2000

				xl = min(data) - 50
				xh = max(data) + 50

				yl = 0
				yh = 0.1
				if(i==2) 
				{ 
					hist(data,probability=T,col=rgb(1,0,1,0.5), breaks=seq(xl,xh,5), ylim=c(yl,yh), main=NULL, xlab="Day of Year", ylab="Frequency") 
					legend("topleft", c("Xk=1", "Xo", "X", "Xc", "Xk=N", "True"), 
					       bty = "n", 
					       angle = c(0, 0, 0, 0, 0, 0), density = c(100, 100, 100, 100, 100, 100),
					       fill=c(rgb(1,204/255,0,1),rgb(1,0,0,1),rgb(1,0,1,1),rgb(0,0,1,1), rgb(0,1,1,1), "black"))
				}

				rect(xh-15,yh-0.05,xh,yh,col="white",border="white")
				text(xh-15,yh-0.025,labels=paste("i",i),pos=4)
				#make curves of inferred distributions
				fCs = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
				f_C = f_C.function(x=fCs$x, y=fCs$y)
				CDF = F_C.vector(x=fCs$x, y=fCs$y)
				F_C = F_C.function(x=fCs$x, y=CDF)
				Pts = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
				if(!is.null(Pts))
				{
					fXs = f_X.vector(x=fCs$x, Pts)

					#f_x
					points(fCs$x, fXs, type="l",col="purple")
					abline(v=sum(fCs$x*fXs/sum(fXs)),col="purple")
					print(paste("Estimate of X:", sum(fCs$x*fXs/sum(fXs)))) 


					#f_o_scaled
					xs = seq(0,1,0.001)
					ys = dbeta(xs, shape1=oParList[[1]], shape2=oParList[[2]]) / (M - m)
					xs = m + (M-m)*xs
					points(xs, ys , type="l", col="red") #col=rgb(1,0,0,1))
					abline(v=sum(xs*ys/sum(ys)),col="red")
					#polygon(xs, ys, col=rgb(1,0,0,0.5) , border=NULL)

					print(paste("Estimate of O:", sum(xs*ys/sum(ys)))) 

					#f_c_scaled
					points(fCs$x, fCs$y, type="l", col="blue") #col=rgb(0,0,1,1))
					abline(v=sum(fCs$x*fCs$y/sum(fCs$y)),col="blue")
					#polygon(fCs$x, fCs$y, col=rgb(0,0,1,0.5) , border=NULL)

					print(paste("Estimate of C:", sum(fCs$x*fCs$y/sum(fCs$y)))) 

					#fxk=1 
					k=1
					fXk1_N = N*dbeta((fCs$x-m)/(M-m),oParList[[1]],oParList[[2]])*choose(N-1,k-1)*(pbeta((fCs$x-m)/(M-m),oParList[[1]],oParList[[2]])^(k-1))*((1-pbeta((fCs$x-m)/(M-m),oParList[[1]],oParList[[2]]))^(N-k))
					abline(v=sum(fCs$x*fXk1_N/sum(fXk1_N)), col="orange")
					print(paste("Estimate of Xk1:", sum(fCs$x*fXk1_N/sum(fXk1_N)))) 
					fXk1_N = yl + (yh-yl)*fXk1_N/max(fXk1_N)
					points(fCs$x, fXk1_N, type="l", col="orange") #col=rgb(1,204/255,0,1))
					#polygon(fCs$x, fXk1_N, col=rgb(1,204/255,0,0.5) , border=NULL)


					#fxk=N
					k=N
					fXkN_N = f_X_k(fCs$x, N=N, k=k, f_X.func=f_C, F_X.func=F_C)
					abline(v=sum(fCs$x*fXkN_N/sum(fXkN_N)), col="cyan")
					print(paste("Estimate of XkN:", sum(fCs$x*fXkN_N/sum(fXkN_N)))) 
					fXkN_N = yl + (yh-yl)*fXkN_N/max(fXkN_N)
					points(fCs$x, fXkN_N, type="l", col="cyan") #col=rgb(0,1,1,1))
					#polygon(fCs$x, fXkN_N, col=rgb(0,1,1,0.5) , border=NULL)
				}


				if(!is.null(trueOParList) && !is.null(trueDParList) )
				{

					fCs = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList=trueOParList, dParList=trueDParList)
					f_C = f_C.function(x=fCs$x, y=fCs$y)
					CDF = F_C.vector(x=fCs$x, y=fCs$y)
					F_C = F_C.function(x=fCs$x, y=CDF)
					Pts = P_t.vector(m=m, M=M, n=n, onset=onset, oParList=trueOParList, F_C.func = F_C)
					if(!is.null(Pts))
					{
						fXs = f_X.vector(x=fCs$x, Pts)

						#f_x
						points(fCs$x, fXs, type="l",col="black")

						mx = sum(fCs$x*fXs/sum(fXs))
						abline(v=mx,col="gray")

						#f_o_scaled
						xs = seq(0,1,0.001)
						ys = dbeta(xs, shape1=trueOParList[[1]], shape2=trueOParList[[2]]) / (M - m)
						xs = m + (M-m)*xs
						points(xs, ys , type="l", col="black")
						mo = sum(xs*ys/sum(ys))
						abline(v=mo,col="gray")
						if(i==2) { polygon(xs, ys, col=rgb(1,0.1,0.1,0.1) , border=NULL) }

						#f_c_scaled
						points(fCs$x, fCs$y, type="l", col="black")
						mc = sum(fCs$x*fCs$y/sum(fCs$y))
						abline(v=mc,col="gray")
						if(i==2) { polygon(fCs$x, fCs$y, col=rgb(0.1,0.1,1,0.1) , border=NULL) }

						#fxk=1 
						k=1
						fXk1_N = N*dbeta((fCs$x-m)/(M-m),trueOParList[[1]],trueOParList[[2]])*choose(N-1,k-1)*(pbeta((fCs$x-m)/(M-m),trueOParList[[1]],trueOParList[[2]])^(k-1))*((1-pbeta((fCs$x-m)/(M-m),trueOParList[[1]],trueOParList[[2]]))^(N-k))
						mxk1= sum(fCs$x*fXk1_N/sum(fXk1_N))
						abline(v=mxk1, col="gray")
						fXk1_N = yl + (yh-yl)*fXk1_N/max(fXk1_N)
						points(fCs$x, fXk1_N, type="l", col="black")
						if(i==2) { polygon(fCs$x, fXk1_N, col=rgb(1,204/255,0.1,0.1) , border=NULL) }

						#fxk=N
						k=N
						fXkN_N = f_X_k(fCs$x, N=N, k=k, f_X.func=f_C, F_X.func=F_C)
						mxkN= sum(fCs$x*fXkN_N/sum(fXkN_N))
						abline(v=mxkN, col="gray")
						fXkN_N = yl + (yh-yl)*fXkN_N/max(fXkN_N)
						points(fCs$x, fXkN_N, type="l", col="black")
						if(i==2) { polygon(fCs$x, fXkN_N, col=rgb(0.1,1,1,0.1) , border=NULL) }
					}
					abline(v=trueParams$k1,col="black")
					abline(v=trueParams$o,col="black")
					abline(v=trueParams$x,col="black")
					abline(v=trueParams$c,col="black")
					abline(v=trueParams$kn,col="black")
					print(paste("True params in the population are (k1, o, x, c, kn):", trueParams$k1, trueParams$o, trueParams$x, trueParams$c, trueParams$kn))
					print(paste("True params in the population are (ovar, xvar, cvar):", trueParams$ovar, trueParams$xvar, trueParams$cvar))
					print(paste("True params based on simulation params are (k1, o, x, c, kn):", mxk1, mo, mx, mc, mxkN))
				}
			}
		}
	}

	#set up and return the output
	outputList = list("oParams" = sampledOParams, "dParams" = sampledDParams, "logLikelihood"=lk,"logPrior"=pr,"logPosterior"=post)

	return(outputList)
}


#Private function - Print the MCMC status
printMCMCDataToFile = function(file, mcmcOutput, nIter, thin=10, type="betabeta",min=0, max=365, trueOParams=NULL, trueDParams=NULL)
{

	print(paste("Printing MCMC output to", file))
	oNames = paste("onset.",names(mcmcOutput$oParams[[1]]),sep="")
	dNames = paste("duration.",names(mcmcOutput$dParams[[1]]),sep="")

	addendum1=""
	addendum2=""
	oNamesTrue=""
	dNamesTrue=""
	go=FALSE
	if(!is.null(trueOParams) && !is.null(trueOParams) )
	{
		oNamesTrue = paste("true.onset.", names(trueOParams), sep="")
		dNamesTrue = paste("true.duration.", names(trueDParams), sep="")
		addendum1 = paste(c(unlist(trueOParams),unlist(trueDParams)),collapse='\t')

		if(type=="betabeta") 
		{
			go=TRUE
			addendum2 = paste(c("true.mean.onset","true.variance.onset","true.mean.duration","true.variance.duration"))
		}
	}


	fileConn = file(file)

	inds = seq(1,nIter,by=thin)

	if(type=="betabeta") 
	{
		lines = c(paste(c("gen",oNames,dNames,"lk","prior","post","mean.onset","variance.onset","mean.duration","variance.duration",oNamesTrue,dNamesTrue,addendum2),collapse='\t'))
	}
	else 
	{
		lines = c(paste(c("gen",oNames,dNames,"lk","prior","post",oNamesTrue,dNamesTrue,addendum2),collapse='\t'))
	}
	tLine = lines[1]

	lines[1] = trimws(lines[1])

	if(go) 
	{
		t.mean.onset=min+(max-min)*betaMean(trueOParams$shape1,trueOParams$shape2)
		t.variance.onset=betaVariance(trueOParams$shape2,trueOParams$shape2)*(max-min)^2
		t.mean.duration=(max-t.mean.onset)*betaMean(trueDParams$shape1,trueDParams$shape2)
		t.variance.duration=betaVariance(trueDParams$shape1,trueDParams$shape2)*(max-t.mean.onset)^2
		addendum2 = paste(c(t.mean.onset,t.variance.onset,t.mean.duration,t.variance.duration),collapse='\t')
	}

	for(i in 1:length(inds))
	{
		ind = inds[i]
		op = unlist(mcmcOutput$oParams[[ind]])
		dp = unlist(mcmcOutput$dParams[[ind]])
		if(length(mcmcOutput$oParams[[ind]])<=0 || length(mcmcOutput$dParams[[ind]])<=0)
		{
			if(i==1) { stop("Could not access MCMC output") }
			lines[i+1] = paste(c(ind,tLine),collapse='\t')
		}
		else
		{
			additional=""
			if(type=="betabeta") 
			{
				mean.onset=min+(max-min)*betaMean(mcmcOutput$oParams[[ind]][1],mcmcOutput$oParams[[ind]][2])
				variance.onset=betaVariance(mcmcOutput$oParams[[ind]][1],mcmcOutput$oParams[[ind]][2])*(max-min)^2
				mean.duration=(max-mean.onset)*betaMean(mcmcOutput$dParams[[ind]][1],mcmcOutput$dParams[[ind]][2])
				variance.duration=betaVariance(mcmcOutput$dParams[[ind]][1],mcmcOutput$dParams[[ind]][2])*(max-mean.onset)^2
				additional = paste(c(mean.onset,variance.onset,mean.duration,variance.duration),collapse='\t')
			}

			tLine = paste(c(unlist(mcmcOutput$oParams[[ind]]),unlist(mcmcOutput$dParams[[ind]]),mcmcOutput$logLikelihood[ind],mcmcOutput$logPrior[ind],mcmcOutput$logPosterior[ind],additional,addendum1,addendum2),collapse='\t')
			lines[i+1] = paste(c(ind,unlist(mcmcOutput$oParams[[ind]]),unlist(mcmcOutput$dParams[[ind]]),mcmcOutput$logLikelihood[ind],mcmcOutput$logPrior[ind],mcmcOutput$logPosterior[ind],additional,addendum1,addendum2),collapse='\t')

		}
	lines[i+1] = trimws(lines[i+1])
	tLine = trimws(tLine)
	}
	writeLines(lines, fileConn)
	close(fileConn)
}



#Find the maximum a posterior parameter values based on the MCMC output
findMAP = function(MCMCOutput) {
	iteration = match(max(MCMCOutput$logPosterior), MCMCOutput$logPosterior)
	return(list("iteration" = iteration, "oParamList"=MCMCOutput$oParams[[iteration]],
		    "dParamList"=MCMCOutput$dParams[[iteration]],
		    "log.likelihood"= MCMCOutput$logLikelihood[[iteration]], 
		    "log.posterior"= MCMCOutput$logPosterior[[iteration]]))
}

#Find the maximum likelihood parameter values based on the MCMC output
findMLE = function(MCMCOutput) {
	iteration = match(max(MCMCOutput$logLikelihood), MCMCOutput$logLikelihood)
	return(list("iteration" = iteration, "oParamList"=MCMCOutput$oParams[[iteration]],
		    "dParamList"=MCMCOutput$dParams[[iteration]],
		    "log.likelihood"= MCMCOutput$logLikelihood[[iteration]], 
		    "log.posterior"= MCMCOutput$logPosterior[[iteration]]))
}



