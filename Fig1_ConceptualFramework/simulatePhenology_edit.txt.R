#Functions to simulate phenology based on an input onset distribution and an input duration distribution


#Simulate the phenological events of a population of size N, and return randomly observed times of individuals in phenophase
simulatePhenology = function(N=1000, subSS = 10, min=0, max=365, onset = "beta", duration="beta", oParList=list(shape1=50, shape2=50), dParList=list(shape1=50, shape2=50))
	{
	onset_f = get(paste("r", onset, sep=""))
	duration_f = get(paste("r", duration, sep=""))

#initialize observation times of the individuals
	individual.observations = double()
	individual.observations.ind = double()

#set up onset, duration, and cessation times of each individual's phenophase
	o = min + (max-min) * do.call(onset_f, c(list(n=N), oParList))
	d = (max-o) * do.call(duration_f, c(list(n=N), dParList))
	c = o + d;

#get the phenological extremes
	true.maxTime = max(c)
	true.minTime = min(o)


#Randomly sample the times when individuals are in the phenophase
	totTime = sum(d)
	times = sort(runif(subSS, 0, totTime))

#Figure out the observed times within individual phenophases based on the sampled times
	ctr = 1
	prevMin = 0
	curMin = d[1]
	ctrS = 1
	for(time in times)
	{
		while(time >= curMin)
		{
			ctr = ctr + 1
			prevMin = curMin
			curMin = curMin + d[ctr]
		}

		if(time >= prevMin && time < curMin)
		{
			individual.observations[ctrS] = o[ctr] + (time - prevMin)
			individual.observations.ind[ctrS] = ctr
			ctrS = ctrS + 1
		}
	}

	return(list(true.maxTime=true.maxTime, true.minTime=true.minTime, observations=individual.observations, observations.inds = individual.observations.ind, onsets=o, durations=d))
	}

#simulate multiple populations, and for each population find the phenological extremes and range in extremes
simulateRanges = function(reps = 1000, N=1000, subSS = 10, min=0, max=365, onset = "beta", duration="beta", oParList=list(shape1=50, shape2=50), dParList=list(shape1=50, shape2=50))
	{
	ranges = double(reps)
	Omins = double(reps)
	Cmaxs = double(reps)
	
	for(i in 1:reps)
		{
		if(i %% 100==1) { print(i) }
		data = simulatePhenology(N=N, subSS=subSS, min=min, max=max, onset = onset, duration=duration, oParList=oParList, dParList=dParList)
		ranges[i] = data$true.maxTime - data$true.minTime
		Omins[i] = data$true.minTime
		Cmaxs[i] = data$true.maxTime
		}
	return(list(ranges=ranges, OMin = Omins, CMax = Cmaxs))
	}









