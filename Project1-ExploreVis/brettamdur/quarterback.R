
rankdf = ranksCombinedClean
limitno = 100 # analysis only includes matches where both players were ranked higher than limitno
minfreq = 50  # only include points where two ranks opposed each other at least minfreq times
plotStartRank = 1 # start grids at this rank number 
plotStopRank = 100 # end grids at this rank number
# start and stop allow a view of a subset of the limitno x limitno grid.  limitno creates the universe, start and stop give a 
# view into that universe.

# This is the order in which to run the functions.  See README file, and the functions
# themselves, for descriptions of the functions' input/output and what they do.
actuals <- limitRanks(rankdf = rankdf,
		      limitno = limitno, 
		      minfreq = minfreq, 
		      plotStartRank = plotStartRank, 
		      plotStopRank = plotStopRank)
hold <- perfectValues(limitno)
hold2 <- perfectReg(hold)
hold3 <- makePredictions(hold2, actuals, plotStartRank = plotStartRank, plotStopRank = plotStopRank)
# makePredictions return five things: 4 graphs (see the comments in the function) and the
# score (the MSE) for the collection of win/loss data passed to the function.

