

# Input:
#	N: population size
#	SS: sample size
#	nReps: number of simulation replicates
#	m: lower bound on phenophase onset (0 for the start of a year)
#	M: upper bound on phenophase cessation (365 for the end of a year)
#	root name of distribution that defines the time of the onset, O, of phenophase, e.g., "beta" or "norm"
#		o1: the value of the first parameter of the onset distribution, e.g., shape1
#		o2: the value of the second parameter of the onset distribution, e.g., shape2
#	root name of distribution that defines the duration, D|O, of the phenophase, e.g., "beta" or "norm"
#		d1: the value of the first parameter of the duration distribution, e.g., shape1
#		d2: the value of the second parameter of the duration distribution, e.g., shape2
#	dt: the distance between times when the time axis is (uniformly) discretized
# Internal
#	n : the number of discretized values used in numerical routines
#	len = (M-m) / dt : the number of discretized time points considered
# Output:
#	onset[nRep,N]: matrix of the times to the phenophase onset for replicate i, individual j
#	duration[nRep,N]: matrix of the phenophase durations for replicate i, individual j 
#	f_C[ len ]: vector of the density values of the time at cessation, which is O+D|O
#	F_C[ len ]: the CDF corresponding to the density f_C
#	P_t[ len ]: probabilities that an individual is in phenophase at each time point
#	N_t[ len, N ]: probabilities of the # of individuals in phenophase for each time point
#	f_X[ len ]: normalized version of P_t
#	F_X[ len ]: the CDF corresponding to the density f_X
#	f_X_k[ len ]: vector of densities of the kth smallest individual in a sample of size SS
#	F_X_k[ len ]: the CDF corresponding to the density f_X_k
#	F_inv_X_k[ len ] : the inverse of the CDF corresponding to the density f_X_k


#onset and duration are densities defined on the interval [0,1]
#	both the p and d versions of the onset and duration density functions must be available
#	except for "dirac", distributions must be continuous (i.e., no atoms, no discontinuities)
#		no singularities
#		make a wrapper if original function has singularities in order to remove singularities
#	distribution functions need to be defined on [0,1] 
#		provide the actual min and max; will scale values appropriately
#	oParList and dParList are the parameter lists for the onset and duration functions, resp.
#		the parameter(s) of the distribution must be listed first before the other input items are listed


#f_D marginalized across onset
f_D = Vectorize(function(x, m=0, M=365, onset="beta", duration="beta", oParList, dParList, n=1000)
	{
	
	onset_f = get(paste("d", onset, sep=""))
	duration_f = get(paste("d", duration, sep=""))

	dx = (M-m)/n
	xv = seq(m,M, by=dx)

	tot = 0
	for(o in xv)
		{
		if(x+o<=M && o<M)
			{
			multiplier = 1
			if(o==m || o==M) multiplier = 0.5
			fo = do.call(onset_f, c(list(x=(o - m) / ( M - m )), oParList)) / (M-m)
			fd = do.call(duration_f, c(list(x= x  / (M - o)), dParList)) / (M-o)
			tot = tot + multiplier * fo * fd * dx
			}
		}
	return(tot)
	}, vectorize.args=c("x"))


#' Composite trapezoidal rule numerical integration
#' 
#' Numerically integrate a function using the composite trapezoid rule. 
#'
#' The composite trapezoid rule is useful to integrate functions that act linear 
#' when zoomed in appropriately.
 
#' @param f A vectorized function
#' @param lower The lower bound of integration
#' @param upper The upper bound of integration
#' @param n The number of subdivisions used to discretize the interval between \code{lower} and \code{upper}
#' ... The remaining arguments to pass to the function \code{f}
#' @NoRd
trapIntegrate.vector = function(f=NULL, lower, upper, n, xv=NULL, yv=NULL, ...)
	{
	if( !is.null(xv) && !is.null(yv) )
		{
		incr = xv[2]-xv[1]
		n = length(xv)-1

		}
	else if(is.null(f))
		{
		stop("The input function is NULL, and the x and y vectors are not provided")
		}
	else
		{
		incr = (upper-lower)/n
		xv = seq(lower,upper,incr)
		yv = f(xv, ...)
		}
	intv = double(n+1)
	intv[1]=0
	for(i in 2:(n+1)) 
		{
		intv[i] = intv[i-1] + 0.5*incr * (yv[i-1] + yv[i])  
		}
	return(intv)
	}

#Private function
spline.wrapper = Vectorize(function(input, min=0,max=365, ...)
	{
	if(is.null(input)) { return(0) }
	if(sum(!is.na(input)) < length(input)) { return(0) }

	if(input< min || input>max) { return(0) }
	
	return( spline( ..., xout=input)$y )

	}, vectorize.args=c("input"))

#dirac sampler
rdirac = function(n=1, offset=0)
	{
	return( rep(offset,n) )
	}

#dirac distribution
ddirac = function(x=0,offset=0)
	{
	if(x==offset) { return(1) }
	return(0)
	}

#dirac CDF
pdirac = function(q=0,offset=0,lower.tail=T)
	{
	if(q < offset) { return( 1*(!lower.tail - 0) ) }
	return( abs( 1*(!lower.tail - 1) ) )
	}


#Produce vector of values of the cessation; larger n, more precise, but slower
f_C.vector = function(m=0, M=365, n=1000, onset="beta", duration="beta", oParList=list(shape1=50,shape2=50), dParList=list(shape1=50,shape2=50))
	{
	onset_f = get(paste("d", onset, sep=""))
	duration_f = get(paste("d", duration, sep=""))
	
	xv = seq(m,M,(M-m)/n)
	
	if(onset == "dirac") 
		{
		oParList[[1]] = m + oParList[[1]] * (M - m)
		if( duration == "dirac" )
			{
			dParList[[1]] = m + dParList[[1]] * (M - m)
			if( oParList[[1]] < m ) { 
				warning("Dirac offset less than minimum bound") 
				return( list(x=xv,y=rep(0, n+1)) )
				}
			if( oParList[[1]] + dParList[[1]] > M ) { 
				warning("Dirac end time point greater than maximum bound") 
				return( list(x=xv,y=rep(0, n+1)) )
				}
			# both Dirac delta yields uniform 
			else { return ( list(x=xv,y=rep(1 / dParList[[1]], n+1))  ) }
			}
		else
			{
			#dirac onset yields distribution of the duration shifted by o1
			return( list(x=xv,y=do.call( Vectorize(duration_f, vectorize.args=c("x")), c(list(x = ( xv - oParList[[1]]) / ( M - oParList[[1]] )), dParList)) / ( M - oParList[[1]])) ) 
			}
		}
	else if(duration == "dirac")
		{
		#already handled case that onset is Dirac, so onset is not be dirac
		#simply the probability of the onset shifted by the dirac offset parameter

		dParList[[1]] = m + dParList[[1]] * (M - m)
		return( list(x=xv,y=do.call( Vectorize(onset_f, vectorize.args=c("x")), c( list(x = (xv - dParList[[1]]) / (M - dParList[[1]])), oParList)) / ( M - dParList[[1]]) ))
		}

	t_func = Vectorize(function(x, m, M, onset_f, duration_f, oParList, dParList, z)  
		{
		o = do.call(onset_f, c(list(x = (x - m) / ( M - m )), oParList)) / (M-m)
		d = do.call(duration_f, c(list(x=( z - x ) / (M - x)), dParList)) / (M-x)
		return(o*d)
		} , vectorize.args = c("x") ) 


	fCs = double(length(xv))
	i = 1
	for( x in xv )
		{
		
		tryCatch(
		expr = {
			fCs[i] = integrate(f=t_func, lower = m, upper = x, m = m, M = M, onset_f = onset_f, duration_f = duration_f, oParList=oParList, dParList=dParList, z = x)$value
		},
		error = function(e) 
			{
			if(i==1) { fCs[i] = 0 }
			if(i>1) { fCs[i] = fCs[i-1] }
			}
		)
		i = i + 1
		}
	return( list(x=xv, y=fCs ) )
	}

#returns an R function that provides the values of the cessation pdf given input times
f_C.function = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", duration="beta", oParList=list(shape1=50,shape2=50), dParList=list(shape1=50,shape2=50) )
	{
	if(is.null(x) || is.null(y))
		{
		vals = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList = oParList, dParList = dParList)
		xv = vals$x
		yv = vals$y
		}
	else
		{
		xv = x
		yv = y
		}

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(0) }
		return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv) ) )
    		}
    	return( f )
	}

#returns a vector of the values of the cessation CDF given a set of input times
F_C.vector = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", duration="beta", oParList=list(shape1=50,shape2=50), dParList=list(shape1=50,shape2=50) )
	{
	if(is.null(x) || is.null(y))
		{
		vals = f_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList = oParList, dParList = dParList)
		xv = vals$x
		yv = vals$y
		}
	else
		{
		xv = x
		yv = y
		}

	return( trapIntegrate.vector(f=NULL, xv=xv, yv=yv) )
	}

#returns an R function that gives the value of the cessation CDF for an input value
F_C.function = function( x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", duration="beta", oParList=list(shape1=50,shape2=50), dParList=list(shape1=50,shape2=50)  )
	{
	if(is.null(x) || is.null(y))
		{
		vals = F_C.vector(m=m, M=M, n=n, onset=onset, duration=duration, oParList = oParList, dParList = dParList)
		xv = vals$x
		yv = vals$y
		}
	else
		{
		xv = x
		yv = y
		}

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv)) )
    		}
    	return( f )

	}

#returns a vector of values of the pdf of P_t
P_t.vector = function(m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), F_C.func=NULL) 
	{
	onset_CDF = Vectorize(get(paste("p", onset, sep="")), vectorize.args=c("q"))
	
	t = seq(m,M,(M-m)/n)
	O = do.call(onset_CDF, c(list(q = (t - m) / ( M - m )), oParList)) 
	C = F_C.func(t)
	return( O * ( 1 - C ) )
	}

#returns an R function of P_t
P_t.function = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), F_C.func=NULL)
	{

if(is.null(x) || is.null(y))
		{
		vals = P_t.vector(m=m, M=M, n=n, onset=onset, oParList = oParList, F_C.func=F_C.func)
		xv = seq(m,M,(M-m)/n)
		yv = vals
		if(is.null(vals)) { return(NULL) }
		}
	else
		{
		xv = x
		yv = y
		}

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv)) )
    		}
    	return( f )


	}

#returns a vector of values of the pdf of X, the observed times
#y is the set of P_t's from P_t.vector
f_X.vector = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), P_t.func=NULL)
	{
	if( !is.null(x) && !is.null(y) )
		{
		xv = x
		yv = y
		incr = xv[2]-xv[1]
		n = length(xv)-1
		}
	else if(is.null(P_t.func))
		{
		message("The P_t function is required when x and y are not input. If you are running an MCMC, consider tightening the priors to fix this issue")
		return(NULL)
		}
	else
		{
		incr = (M-m)/n
		xv = seq(m,M,incr)
		yv = P_t.func(xv)
		}
	yv.orig = yv
	yv[1] = yv[1]/2
	yv[n+1] = yv[n+1]/2
	scale = 1 / (incr * sum(yv) )
	return( scale * yv.orig )
	}


#returns an R function of the pdf of X
#y is the set of f_X values
f_X.function = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), P_t.func=NULL)
	{
	if(is.null(x) || is.null(y))
		{
		if(is.null(P_t.func)) 
			{
			return(NULL)
			}
		vals = f_X.vector(m=m, M=M, n=n, onset=onset, oParList = oParList, P_t.func=P_t.func)
		if(is.null(vals)) { return(NULL) }
		xv = seq(m,M,(M-m)/n)
		yv = vals
		}
	else
		{
		xv = x
		yv = y
		}

	f <- Vectorize(function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv)) )
    		}, vectorize.args = c("x"))
    	return( f )

	}

#returns a vector of values of the CDF of X
#y is the set of f_X values
F_X.vector = function( x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), P_t.func=NULL ) 
	{
	if(is.null(x) || is.null(y))
		{
		vals = f_X.vector(m=m, M=M, n=n, onset=onset, oParList = oParList, P_t.func=P_t.func)
		if(is.null(vals)) { return(NULL) }
		xv = seq(m,M,(M-m)/n)
		yv = vals
		}
	else
		{
		xv = x
		yv = y
		}
	return( trapIntegrate.vector(f=NULL, xv=xv, yv=yv) )
	}

#returns an R function of the CDF of X
#y values are the F_X values
F_X.function = function(x=NULL, y=NULL, m=0, M=365, n=1000, onset="beta", oParList=list(shape1=50,shape2=50), P_t.func=NULL)
	{
	if(is.null(x) || is.null(y))
		{
		vals = F_X.vector(m=m, M=M, n=n, onset=onset, oParList = oParList, P_t.func=P_t.func)
		xv = seq(m,M,(M-m)/n)
		yv = vals
		}
	else
		{
		xv = x
		yv = y
		}

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv)) )
    		}
    	return( f )

	}

#returns a vector of the values of the pdf of f_X_k
#y values are the F_X's
f_X_k = Vectorize(function(x, N=100, k=1, f_X.func=NULL, F_X.func=NULL)
	{
	
	val = N * f_X.func(x) * choose(N-1, k-1) * ( F_X.func(x)^(k-1) ) * ( (1 - F_X.func(x))^(N-k) )
	return(val)

	}, vectorize.args=c("x"))


#returns a general-purpose spline function based on a set of x-ordered, input points
function_Factory = function(x=NULL, y=NULL, useAbs=TRUE)
	{
	if(is.null(x) || is.null(y))
		{
		stop("Vectors x and y vectors need to be provided to function_Factory")
		}
	if(sum(is.na(y))>1) { stop("NA values present in input y's in function_Factory") }

	xv=x
	yv=y

	f <- function(x) 
		{
		if(useAbs) { return( abs( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv)) ) }
		return( spline.wrapper(input=x, min=xv[1], max=xv[length(xv)], x= xv, y=yv) )
    		}
    	return( f )
	}

#calculate the PDF of the amount of time that a population is in a phenophase based on a difference between the population's last phenophase time and its earliest phenophase time
populationPhenologyDuration.vector = function(func.O1, func.CN, min=0, max=365, N=1000, n=1000, renormalize=FALSE)
	{
	message("Calculating the density of ranges of phenologies in the population")

	tRs = seq(min,max,(max-min)/(n-1))

	ys = double(n)
	for( i in 1:(n-1) )
		{

		if(i %% 50 ==1) { print(i) }

		x.O1 = seq(0,max-tRs[i],(max-min)/(n-1))
		x.CN = x.O1+tRs[i]
		
		y.O1 = func.O1(x.O1)
		y.CN = func.CN(x.CN)


		yv = y.O1*y.CN

		intc = trapIntegrate.vector(xv=x.O1, yv=yv)

		ys[i] = max(intc)

		}

	#normalize
	if(renormalize) { ys = ys/sum(ys) }

	return(list(tRs=tRs,density=ys))
	}

#returns a vector of the values of the CDF of X_k
#y values are the f_X_k values
F_X_k.vector = function(x = NULL, y=NULL, min = 0, max = 365, n=1000, N=100, k=1, f_X.func=NULL, F_X.func=NULL)
	{
	if(is.null(x) || is.null(y))
		{
		xv = seq(min,max,(max-min)/n)
		yv = f_X_k(xv, N=N, k=k, f_X.func=f_X.func, F_X.func=F_X.func)
		}
	else
		{
		xv = x
		yv = y
		}
	return( trapIntegrate.vector(f=NULL, xv=xv, yv=yv) )
	}

#returns an R function of the CDF of X_k
F_X_k.function = function(x = NULL, y=NULL, min = 0, max = 365, n=1000, N=100, k=1, f_X.func=NULL, F_X.func=NULL)
	{
	if(is.null(x) || is.null(y))
		{
		xv = seq(min,max,(max-min)/n)
		yv = F_X_k.vector(min=min,max=max,n=n,N=N,k=k,f_X.func=f_X.func,F_X.func=F_X.func)
		}
	else
		{
		xv = x
		yv = y
		}

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( abs( spline.wrapper(input=x, min=min, max=max, x= xv, y=yv)) )
    		}
    	return( f )
	}

#returns an R function of the inverse of a CDF based on the input X and input Y values from a CDF. The y values can be generated by any of the F_*.vector functions above
CDFinv.function = function(originalX, originalY_fromCDF, min=0, max=365)
	{
	xv = originalY_fromCDF
	yv = originalX

	n = length(originalX)

	print(n)


	new.xv = seq(0,1,1/(n-1))
	dx = new.xv[2] - new.xv[1]
	new.yv = rep(-1,n)

	new.yv[1] = min
	new.yv[n] = max

	#very inefficient, but my brain can't handle otherwise today; at least it works
	for(i in 2:n) {
		found = F
		for(j in 1:(n-1)) {
			if(new.xv[i] == xv[j]) {
				new.yv[i] = vy[j]
				found=T
				break
			} else if(new.xv[i] >= xv[j] && new.xv[i] <= xv[j+1] ) {
				prop = (new.xv[i] - xv[j]) / ( xv[j+1] - xv[j]) 
				new.yv[i] = yv[j] + prop*(yv[j+1]-yv[j]) 
				found = T
				break
			}
		}
		if(!found) {
			if(new.xv[i] < xv[1]) {
				prop = (xv[1] - new.xv[i]) / (xv[1] - 0) 
				new.yv[i] = prop * (yv[1] - min) + min
			} else if( new.xv[i] > xv[n] ) {
				prop = (new.xv[i] - xv[n]) / (1 - xv[n])
				new.yv[i] = prop * (max - yv[n]) + yv[n]
			} else {
				stop("Something weird happened.") 
			}
		}
	}

	f = Vectorize(function(x)
		{
		tryCatch(
			expr = {
				suppressWarnings( {
				if(x <= 0) { return( min ) }
				if(x >= 1) { return( max ) }


				pure = x/dx
				index = floor(x/dx)

				if(pure==index || index == n) { val = new.yv[index] }
				else { val = new.yv[index] + (new.yv[index+1] - new.yv[index])/2  }

				if(val < min) { return(min) }
				if(val > max) { return(max) }
				return(val)
				} )
			},
			error = function(e) 
				{
				return(NULL)
				}
			#,
			#warning=function(w) 
			#	{
			#	return(va)
			#	}
			)
		}, vectorize.args=c("x"))
	return(f)
	}

#general function that makes an R function of the inverse of a PDF. Y values can be generated by any of the f_*.vector functions
f_inv.function = function(originalX, originalY, min = 0, max = 1)
	{
	xv = originalY
	yv = originalX
	#m = min(xv)
	#M = max(xv)
	

	f <- function(x) 
		{
		if(sum(is.na(yv))>1) { return(rep(0,length(xv))) }
		return( 	spline.wrapper(input=x, min=min, max=max, x= xv, y=yv) )
    		}
    	return( f )

	
	}


#returns an R function of the pdf of N_t
f_N_t = function(t=200, N=1000, P_t.func = NULL)
	{
	x=0:N
	if(length(t)>1) { stop("f_N_t requires a scalar t as input") }
	if(is.null(P_t.func)) { return(NULL) }
	Pt = P_t.func(t)
	return( dbinom(x=x, size=N, prob=Pt) )
	}

#the x's are the numbers 0 through N, and the y's are the f_N_t values at the x's
F_N_t.function = function(x=NULL, y=NULL, t=200, N=1000, P_t.func = NULL)
	{
	if(length(t)>1) { stop("F_N_t requires a scalar t as input") }

	if(is.null(x) || is.null(y))
		{
		xv = 0:N
		yv = f_N_t(t=t, N=N, P_t.func=P_t.func) 
		}
	else
		{
		xv = x
		yv = y
		}

	f <- Vectorize(function(x) 
		{
		return(sum(yv[1:(x+1)]))
    		}, vectorize.args=c("x"))
    	return( f )
	}


#Depricated
dX_beta_beta = function(x, oShape1, oShape2, dShape1, dShape2, min=0, max=365, n=500)
	{
		for(xt in x)
		{
		if(xt < min || xt > max) { stop(paste("Input values need to be between the minimum, ", min, "and maximum", max, "values" )) }
		}

	onset = "beta"
	duration = "beta"
	oParList = list(shape1=oShape1, shape2=oShape2)
	dParList = list(shape1=dShape1, shape2=dShape2)

        fCs = f_C.vector(m=min, M=max, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
        CDF = F_C.vector(x=fCs$x, y=fCs$y)
        F_C = F_C.function(x=fCs$x, y=CDF)
        Pts = P_t.vector(m=min, M=max, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
        fXs = f_X.vector(x=fCs$x, Pts)
        f_X = f_X.function(x=fCs$x, y=fXs)

	return( f_X(x) )
	}

#Depricated
pX_beta_beta = function(q, oShape1, oShape2, dShape1, dShape2, min=0, max=365, n=500)
	{

	onset = "beta"
	duration = "beta"
	oParList = list(shape1=oShape1, shape2=oShape2)
	dParList = list(shape1=dShape1, shape2=dShape2)

        fCs = f_C.vector(m=min, M=max, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
        CDF = F_C.vector(x=fCs$x, y=fCs$y)
        F_C = F_C.function(x=fCs$x, y=CDF)
        Pts = P_t.vector(m=min, M=max, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
        fXs = f_X.vector(x=fCs$x, Pts)
        f_X = f_X.function(x=fCs$x, y=fXs)
	FXs = F_X.vector(x=fCs$x, y=fXs, n=n)
	F_X = F_X.function(x=fCs$x, y=FXs)

	return( F_X(q) )
	}

#Depricated
qX_beta_beta = Vectorize(function(p,oShape1, oShape2, dShape1, dShape2, min=0, max=365, n=500)
	{
	if(p <= 0) { return(min) }
	if(p >= 1) { return(max) }

	onset = "beta"
	duration = "beta"
	oParList = list(shape1=oShape1, shape2=oShape2)
	dParList = list(shape1=dShape1, shape2=dShape2)

        fCs = f_C.vector(m=min, M=max, n=n, onset=onset, duration=duration, oParList=oParList, dParList=dParList)
        CDF = F_C.vector(x=fCs$x, y=fCs$y)
        F_C = F_C.function(x=fCs$x, y=CDF)
        Pts = P_t.vector(m=min, M=max, n=n, onset=onset, oParList=oParList, F_C.func = F_C)
        fXs = f_X.vector(x=fCs$x, Pts)
        f_X = f_X.function(x=fCs$x, y=fXs)
	FXs = F_X.vector(x=fCs$x, y=fXs, n=n)
	FinvX = CDFinv.function(originalX=fCs$x, originalY_fromCDF=FXs, min=min, max=max)

	}, vectorize.args = c("p") )


#Depricated
rX_beta_beta = function(n,oShape1, oShape2, dShape1, dShape2, min=0, max=365)
	{
	data = rbeta(n, shape1=oShape1, shape2=oShape2) * (max - min) + min
	for(i in 1:length(data))
		{
		duration = (max-data[i]) * rbeta(1, shape1=dShape1, shape2=dShape2)
		samp = runif(1,0,duration)
		data[i] = data[i] + samp
		}
	return(data)
	}

#Depricated
mX_beta_beta = function(o,oShape1, oShape2, dShape1, dShape2, min=0, max=365, dx=0.1)
	{
		xv = seq(min,max, by=dx)
		len= length(xv)
		fX = dX_beta_beta(xv, oShape1, oShape2, dShape1, dShape2, min=0, max=365)
		return(  ( xv^o ) %*% ( fX*dx  ) - (xv[1]^0)*fX[1]*dx/2 - (xv[len]^0)*fX[len]*dx/2 )
	}		
