
# creates and returns a data frame of the variables we will use in 
# plotting starting plots
buildTemplates <- function(){
	r1 <- c(10,8.04,9.14,7.46, 8, 6.58)
	r2 <- c(8, 6.95, 8.14, 6.77, 8, 5.76)
	r3 <- c(13, 7.58, 8.74, 12.74, 8, 7.71)
	r4 <- c (9, 8.81, 8.77, 7.11, 8, 8.84)
	r5 <- c (11, 8.33, 9.26, 7.81, 8, 8.47)
	r6 <- c (14, 9.96, 8.10, 8.84, 8, 7.04)
	r7 <- c (6, 7.24, 6.13, 6.08, 8, 5.25)
	r8 <- c (4, 4.26, 3.10, 5.39, 19, 12.50)
	r9 <- c (12, 10.84, 9.13, 8.15, 8, 5.56)
	r10 <-c (7, 4.82, 7.26, 6.42, 8, 7.91)
	r11 <-c (5, 5.68, 4.74, 5.73, 8, 6.89)
	
	df <- as.data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11))
	names(df) <- c('Xabc', 'Ya', 'Yb', 'Yc', 'Xd', 'Yd')
	
	return(df)
}

get_startData <- function(input, templateValues){
	
	# select the x and y vectors to plot
	xy <- switch(input$startType,
				 "goodFit" = list(templateValues$Xabc, templateValues$Ya),
				 "nonLinear" = list(templateValues$Xabc, templateValues$Yb),
				 "outlier1" = list(templateValues$Xabc, templateValues$Yc),
				 "outlier2" = list(templateValues$Xd, templateValues$Yd)
				 #"colinearity" = list(templateValues$Xabc, templateValues$Yc)
	)

	x <- as.numeric(xy[[1]])
	y <- as.numeric(xy[[2]])
	# plotData = rbind(data.frame(x=x,y=y), as.numeric(c(input$xvalue,input$yvalue))) #add the user point, if any
	plotData = data.frame(x=x,y=y)
	if((!input$xvalue == "") & (!input$yvalue == "")){
		plotData = rbind(plotData, as.numeric(c(input$xvalue,input$yvalue)))
	}

		
# 	x <- xy[[1]]
# 	y <- xy[[2]]
	
	return(plotData)
}


