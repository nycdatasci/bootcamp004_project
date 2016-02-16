# server.R
# 
#source('./helperfunctions.R')
source('./helpers2.R')
library("shiny")
library("ggplot2")
library("stargazer")
library(car)

templateValues <- buildTemplates() # Build data templates

shinyServer(
	function(input, output) {
		output$startChart <- renderPlot({
			
			plotVectors <- get_startData(input, templateValues)
 
			g <- ggplot(plotVectors, aes(x,y)) + 
				geom_smooth(method = lm, se = FALSE) + 
				geom_point(col="blue", size=4) +
				xlab("Predictor") +
				ylab("Response") +
				theme_minimal() +
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA)) 
			print(g)
		})
		
		output$startChart_copy <- renderPlot({
			
			plotVectors <- get_startData(input, templateValues)
			fit <- lm(y~x, plotVectors)
			
			g <- ggplot(plotVectors, aes(x,y)) + 			
				geom_smooth(method = lm, se = FALSE) + 
				geom_point(col="blue", size=4) +
				xlab("Predictor") +
				ylab("Response") +
				geom_text(label = rownames(fortify(fit)), nudge_x = 0.8, nudge_y = -0.2) +
				geom_segment(aes(x=x, xend=x, y=y, yend=fit$fitted.values)) +
				theme_minimal()+
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA))				
			print(g)
		})		
		
# 		output$variancePlot <- renderPlot({
# 			
# 			plotVectors <- get_startData(input, templateValues)
# 			x <- plotVectors[[1]]
# 			y <- plotVectors[[2]]
# 			modelData <- data.frame(x,y)
# 			fit <- lm(y ~ x, modelData)
# 			
# 			spreadLevelPlot(fit)
# 			
# 		})
		
		output$observedVfitPlot <- renderPlot({
			
			plotVectors <- get_startData(input, templateValues)
			fit <- lm(y ~ x, plotVectors)
			
			g <- ggplot(fit, 
						aes(fit$fitted.values, y)) + 
				geom_point(col="blue", size=4) +
				geom_smooth(method = loess, se=FALSE) +
				ylab("Observed") +
				xlab("Fitted Values") +
				ggtitle("Observed vs. Fitted Values") +
				geom_text(label = rownames(fortify(fit)), nudge_x = 0.1, nudge_y = -0.1) +
				theme_classic() +
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA))
			print(g)
		})

		output$residVfitPlot <- renderPlot({
			
			plotVectors <- get_startData(input, templateValues)
			fit <- lm(y ~ x, plotVectors)
			
			g <- ggplot(fit, 
						aes(fit$fitted.values, fit$residuals)) + 
				geom_point(col="blue", size=4) +
				geom_smooth(method = loess, se=FALSE) +
				ylab("Residuals") +
				xlab("Fitted Values") +
				ggtitle("Residuals vs. Fitted Values") +
				geom_text(label = rownames(fortify(fit)), nudge_x = 0.1, nudge_y = -0.1) +
				theme_classic() +
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA))
			print(g)
		})
		
		output$qqPlot <- renderPlot({
			
			plotVectors <- get_startData(input, templateValues)
			fit <- lm(y ~ x, plotVectors)
			
			g <- ggplot(fit, aes(sample = fit$residuals)) + 
				stat_qq() + 
				ggtitle("Normal Q-Q Plot") +
				theme_classic() +
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA))
			
			df.new <- ggplot_build(g)$data[[1]] 	# see http://stackoverflow.com/questions/14958814/
													# how-can-i-label-the-points-of-a-quantile-quantile-
													# plot-composed-with-ggplot2
			df.new$name<-rownames(plotVectors)[order(fit$residuals)]
			
			g2 <- ggplot(df.new,aes(theoretical,sample)) +
				geom_point(col="blue", size=4) +
				geom_abline(linetype = "dotted") + 
				ggtitle("Normal Q-Q Plot") +
				geom_text(label = df.new$name, nudge_x = 0.1, nudge_y = -0.1) +
				theme_classic() +
				theme(panel.background = element_rect(fill = 'lightyellow'),
					  plot.background = element_rect(fill="grey", color = NA))
			
			print(g2)
		})		
		
		
	  	output$regSum <- renderPrint({ # outputs regression summary
	  		plotVectors <- get_startData(input, templateValues)
# 	  		x <- plotVectors[[1]]
# 	  		y <- plotVectors[[2]]
# 	  		
# 	  		modelData <- data.frame(x,y)
	  		fit <- lm(y ~ x, plotVectors)
	  		stargazer(fit, type = "html", report = "vcp*",
	  				  covariate.labels = c("Coefficient: x", "Intercept") # will need to change this for 
	  				  													  # multivariate regression	
			)
	  		
	  	})
	  	
	  	output$cooks <- renderPlot({ # outputs cooks distance plot
	  		plotVectors <- get_startData(input, templateValues)
## 	  		x <- plotVectors[[1]]
## 	  		y <- plotVectors[[2]]
## 	  		
## 	  		modelData <- data.frame(x,y)
	  		fit <- lm(y ~ x, plotVectors)
	  		
	  		p5<-ggplot(fit, aes(.hat, .stdresid)) + 
	  			geom_point(aes(size=.cooksd), na.rm=TRUE, col="blue") +
	  			stat_smooth(method="loess", na.rm=TRUE, se = FALSE) +
	  			geom_text(label = rownames(fortify(fit)), nudge_x = 0.004, nudge_y = -0.08) +
	  			xlab("Leverage")+ylab("Standardized Residuals") +
	  			ggtitle("Residual vs Leverage and Cook's Distance") +
	  			scale_size_continuous("Cook's Distance", range=c(1,5)) +
	  			theme_classic() +
	  			theme(legend.position="bottom") + 
	  			theme(panel.background = element_rect(fill = 'lightyellow'),
	  				  plot.background = element_rect(fill="grey", color = NA),
	  				  legend.background = element_rect(fill = 'lightyellow'))
	  		
	  		print(p5)
	  		
	  		# influencePlot(fit)
	  	})
	  	
	  	output$anscombeText <- renderUI({ # outputs anscombe text when necessary
	  		if(input$startType == "goodFit" | 
	  		   input$startType == "nonLinear" |
	  		   input$startType == "outlier1" | 
	  		   input$startType == "outlier2" 
	  		   ){
	  			tags$p("Source of Data Sets:", 
	  				   tags$a(href="https://en.wikipedia.org/wiki/Anscombe%27s_quartet", 
	  				   	   "Anscombe's Quartet")
	  			)
	  		}
	  		else{
	  			""
	  		}
	  		
	  	})	  	
	  	
})

# x <- seq(1,20,1)
# # # y <- (2 * x) ^ 2 + rnorm(length(x),mean = 0, sd = 1) ^ 2
# y <- (2 * x) + rnorm(length(x),mean = 0, sd = 4)