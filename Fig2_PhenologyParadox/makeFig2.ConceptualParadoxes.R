makePlot = function(seed=4) {
	library(latex2exp)
	source("phenologySimulation.R")
	#source("phenologyInference.R")

	labels = LETTERS[1:6]

	layout(matrix(1:6, nrow = 2, byrow = TRUE))


	par(mar = c(3, 3, 1, 1), oma = c(2, 2, 2, 2))  # oma = outer margins (bottom, left, top, right)

	#set the range of responses
	minResponse = 0
	maxResponse = 365
	rangeResponse = maxResponse-minResponse
	rangeMid = rangeResponse/2

	#set the max and min mean duration
	maxDuration = 90
	minDuration = 30
	rangeDuration = maxDuration-minDuration 

	#set the min and max mean onset (from Callinger et al. 2013)
	minOnset = 75
	maxOnset = 225
	rangeOnset = maxOnset-minOnset
	meanO = rangeResponse/2

	#set the range in temperature (from Callinger et al. 2013)
	minC = -2
	maxC = 24
	rangeC = maxC-minC
	meanCovariate = minC + rangeC / 2   #based on mean of uniform random variable between minC and maxC (uniform sampling)

	#set the min and max duration slopes
	maxDurationSlope = rangeDuration / rangeC
	minDurationSlope = maxDurationSlope / 5

	#set the min and max onset slopes
	maxOnsetSlope = rangeOnset / rangeC
	minOnsetSlope = 0.75

	#set the cushion, which defines the separation between the paradox thresholds
	cushion = minDurationSlope / 2

	#set the SD on onset and cessation
	sigma = 7
	SS = 30

	OK = T
	while(OK) {

		#+++
		for(scenarioNumber in 1:6) {

			if(scenarioNumber == 1) {
				slopeD = runif(1, minDurationSlope, maxDurationSlope)
				slopeO = runif(1, minOnsetSlope, maxOnsetSlope)

				slopeD = 1
				slopeO = 3

				durationStart = minDuration + 20
				tLines = c("1: +++", "$\\beta_O > 0$, $\\beta_D > 0$")

					#expression("1: +++   " * (beta[plain(O)] > 0) * ",   " * (beta[plain(D)] > 0))
			}

			#+-+
			if(scenarioNumber == 2) {
				slopeD = -runif(1, minDurationSlope, maxDurationSlope)
				minSO = -slopeD/2 + cushion
				if(minSO > maxOnsetSlope || minSO < 0) { 
					warning("The minimum allowable slope for scenario 2 was greater than the maximum allowable slope.") 
					next
				}
				slopeO = runif(1, minSO, maxOnsetSlope)

				slopeD = -1.5
				slopeO = 2

				durationStart = maxDuration
				#label_text="2: +-+"
				#label_text = TeX("2: +-+\r$0<2\\beta_O<-\\beta_D") 
				tLines = c("2: +-+", "$0 < -\\beta_D < 2\\beta_O$")
			}

			#+--	Paradox

			if(scenarioNumber == 3) {
				sOK = F
				while(!sOK) {
					#slopeD = -runif(1, minDurationSlope, maxDurationSlope)
					slopeD = -runif(1, 2*minOnsetSlope, maxDurationSlope)
					maxSO = -slopeD/2 - cushion
					#cat(paste("minDurationSlope: ", minDurationSlope, " maxDurationSlope: ", maxDurationSlope, " minOnsetSlope: ", minOnsetSlope, " maxSO: ", maxSO, " maxOnsetSlope: ", maxOnsetSlope, " Picked slopeD: ", slopeD, "\n"))
					if(maxSO < minOnsetSlope ) { 
						warning(paste("For scenario 3, max onset slope, ", maxSO, " is less than min onset slope, ", minOnsetSlope, ".")) 
						sOK=F
						#stop("EEK")
						next
					}
					if(maxSO < 0 ) { 
						warning("For scenario 3, max onset slope is negative when it should be positive.") 
						sOK=F
						next
					}
					slopeO = runif(1, minOnsetSlope, maxSO)

					durationStart = maxDuration + 20
					sOK = T
				}
				slopeD = -3
				slopeO = 1
				#label_text="3: +--"
				tLines = c("3: +--", "$0 < 2\\beta_O < -\\beta_D$")
			}

			#---
			if(scenarioNumber == 4) {
				slopeD = -runif(1, minDurationSlope, maxDurationSlope)
				slopeO = -runif(1, minOnsetSlope, maxOnsetSlope)

				durationStart = maxDuration

				slopeD = -2
				slopeO = -2
				#label_text="4: ---"
				tLines = c("4: ---", "$\\beta_O<0, \\beta_D<0$")
			}

			#-+-
			if(scenarioNumber == 5) {
				slopeD = runif(1, minDurationSlope, maxDurationSlope)
				maxSO = -slopeD/2 - cushion
				if(maxSO < -maxOnsetSlope) {
					warning(paste("For scenario 5, the maximum onset slope ", maxSO, " is less than the minimum onset slope, ", (-maxOnsetSlope), "."))
					next
				}
				slopeO = runif(1, -maxOnsetSlope, maxSO)

				durationStart = minDuration + 20

				slopeD = 1
				slopeO = -3
				#label_text="5: -+-"
				tLines = c("5: -+-", "$0 < \\beta_D < -2\\beta_O$")
			}

			#-++
			if(scenarioNumber == 6) {
				sOK=F
				while(!sOK) {
					slopeD = runif(1, minDurationSlope, maxDurationSlope)
					minSO = -slopeD/2 + cushion
					if(minSO > -minOnsetSlope) {
						warning(paste("For scenario 6, the minimum onset slope ", minSO, " is greater than the maximum onset slope, ", (-minOnsetSlope), "."))
						sOK=F
						next
					}
					slopeO = runif(1, minSO, -minOnsetSlope)

					durationStart = minDuration + 20
					sOK=T
				}

				slopeD = 3
				slopeO = -1
				#label_text="6: -++"
				tLines = c("6: -++", "$0 < -2\\beta_O < \\beta_D$")
			}

			cat(paste("__________________________________________________________________________________________\nScenario: ", scenarioNumber, "\nOnset slope: ", slopeO, "\nDuration slope: ", slopeD, "\n_____________________________________________________________________________\n\n"))

			sl = 0
			su = sl + durationStart
			el = sl + slopeO * rangeC
			eu = el + durationStart + slopeD * rangeC

			minR = min(sl,su,el,eu)
			maxR = max(sl,su,el,eu)

			#print(c(sl,su,el,eu))

			meanD = ((su - sl) + (eu - el) ) / 2

			interceptO = meanO - meanCovariate * slopeO
			interceptD = meanD - meanCovariate * slopeD
			interceptC = interceptO+interceptD
			interceptT = interceptO + interceptD / 2

			slopeT = slopeO + slopeD/2
			slopeC = slopeO + slopeD


			sim = simulateCovariate(n=SS, slopeO=slopeO, interceptO=interceptO, sigma=sigma, slopeD=slopeD, interceptD=interceptD, minCovariate=minC, maxCovariate=maxC)

			#cat(paste("Generating plots"))
			fitT <- lm(sim$Ts ~ sim$X)
			fitO <- lm(sim$O ~ sim$X)
			fitC <- lm(sim$C ~ sim$X)

			plot(sim$X, sim$Ts, ylim=c(minResponse, maxResponse),col="purple", pch=4,cex=1.25)	#Observed times

			points(sim$X, sim$O, col="red", pch=16, cex=0.75)							#onset times
			points(sim$X, sim$C, col="blue", pch=16, cex=0.75)							#cessation times

			segments(sim$X, sim$O, sim$X, sim$C, col=rgb(0,0,0,0.3))	#durations

			abline(a=interceptO, b=slopeO, col="red", lwd=1)
			abline(a=interceptC, b=slopeC, col="blue", lwd=1)
			abline(a=interceptT, b=slopeT, col="purple", lwd=2)

			#abline(fitT, col = "black", lwd = 2)
			#abline(fitO, col = "black", lwd = 1)
			#abline(fitC, col = "black", lwd = 1)

			col="black"
			if(scenarioNumber == 3 || scenarioNumber == 6) { col = "purple" }

			# Get plot region boundaries
			usr <- par("usr")  # usr = c(xmin, xmax, ymin, ymax)

			# Label content
			cex_text <- 1.5  # Larger text
			strh <- strheight("A", cex=cex_text) + 0.2 * strheight("A", cex=cex_text)

			# Measure dimensions of the label
			#strw <- strwidth(l_text, cex = cex_text)
			#strh <- strheight("A", cex = cex_text)

			# Position text a little down from the top edge and slightly inset from left
			x_text <- usr[1] + 0.02 * (usr[2] - usr[1])  # small left offset
			y_text <- usr[4] - strh / 2  # center vertically in the box

			# Draw rectangle:
			# top edge = usr[4] (plot top)
			# bottom = top - strh
			#rect(xleft = x_text - 0.01 * (usr[2] - usr[1]),  # small left padding
			#ybottom = usr[4] - strh,
			#xright = x_text + strw,
			#ytop = usr[4],
			#border = "black", col = NA, lwd = 1)

			# Add text inside rectangle


# Plot each line
for (i in seq_along(tLines)) {
  text(x = x_text,
       y = y_text - (i - 1) * strh,
       labels = TeX(tLines[i]),
	   cex = cex_text,
       adj = c(0, 1),
	   col=col
	   )
}



			#text(x = x_text,
				 #y = y_text,
				 #labels = label_text,
				 #adj = c(0, 2),  # left-aligned, vertically centered
				 #cex = cex_text, 
				 #col=col)


			#text(x,y, text, adj=c(0,0), cex = 1.2, font = 2, col = col)

			mtext(labels[scenarioNumber], side=1, line=2.5, adj=0, cex=0.9, font=2)

			#points(mean(sim$X), meanO+meanD/2, col="purple", pch = 16, cex = 2)

		}
		#char = readline("Done?")
		#OK = T
		#if(char == "y") { OK = F }
		OK=F
	}
}




#	record scenario type
#        Onset   Dur     Obs
#1       +       +       +       STANDARD
#2       +       -       +
#3       +       -       -       PARADOX
#4       -       -       -       STANDARD
#5       -       +       -
#6       -       +       +       PARADOX
#7       -       -       +       IMPOSSIBLE with sufficiently large sample size
#8       +       +       -       IMPOSSIBLE with sufficiently large sample size
