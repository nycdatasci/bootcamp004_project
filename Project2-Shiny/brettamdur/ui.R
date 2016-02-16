shinyUI(fluidPage(
	theme = "bootstrap.css",
	titlePanel("Regression Diagnostics: An Introduction"),
	hr(color="white", height=4),
	
	#### INSTRUCTIONS ROW ####
	fluidRow(
		column(3,
			h3("Instructions")
		),	
		column(8, offset = 1,
			"
			This tool provides an introduction to the diagnostics used to assess the validity of a 
			linear regression analysis.  It is intended to help those who are relatively new to regression
			learn about the diagnostic techniques and the plots that support these techniques.  
			",
			p(),
			"
			Choose a data set in the \"Data Selection\" drop down below.  You can see your data and the 
			regression model it generates in the \"What We're Diagnosing\" section.  You can also add an 
			additional point to the data sets, to see the impact of additional data.  The \"Diagnostics\" section
			includes tabs that provide plots and explanations that show how the diagnostics apply to the data 
			you've chosen.  Click here for some suggested approaches to using this tool to learn about the 
			diagnostics.
			"
		)
	),
	
	fluidRow(
		column(12,
			hr(color="white", height=4)
		)
	),
	
	#### WHAT WE'RE DIAGNOSING ROW ####
	fluidRow(
		column(3,
			#### DATA SELECTION COLUMN:	####
			h3("What We're Diagnosing")	
		),
		column(8, offset = 1,
			fluidRow(
				column(6,
					plotOutput("startChart", width = "100%", height = "238px")
				),
				column(6,
					htmlOutput("regSum", height = "150px")
				)
			),
			
			fluidRow(
				column(12,
					uiOutput("anscombeText"),
					hr(color="white", height=4)
				)
			),			
			
			fluidRow(
				column(6,
				   h4("Data Selection"),
				   selectInput("startType", label = (""), 
			   			choices = list("Good Fit" = "goodFit", 
			   						   "Non Linear" = "nonLinear",
			   						   "Outlier 1" = "outlier1",
			   						   "Outlier 2" = "outlier2",
			   						   "Heteroskedascticity" = "heterosk",
			   						   selected = "nonLinear" 
			   			)
				   ),
				   sliderInput("slider1", label = "Error",
				   			min = -5, max = 5, value = 0)
				),
				column(6,
					   br(),
					fluidRow(
						column(4, 
							   br(),br(),
							   "New Point:"
						),
						column(4,
							br(),
							textInput("xvalue", "X Value", width = "100%")
						),
						column(4,
						   br(),
						   textInput("yvalue", "Y Value", width = "100%")	   
						)
					),
					br(),
					# submitButton(text = "Submit", icon = NULL, width = "25%")
					#uiOutput("alignSubmit")
					""
				)
			)
			
			
		)
		

	), # end of what we're testing row
	
	fluidRow(
		column(12,
			   hr(color="white", height=4)
		)
	),	
	
	# DIAGNOSTICS ROW
	fluidRow(
		column(3,
			h3("The Diagnostics"),
			plotOutput("startChart_copy", width = "100%", height = "150px")
		),
		column(8, offset = 1,
		   tabsetPanel(position = "above",
		   			tabPanel(h4("Linearity"), 
		   					 fluidRow(
		   					 	column(12, align = "center",
		   					 		br(), br(),
		   					 		plotOutput("observedVfitPlot", width = "80%", height = "200px")
		   					 	)
		   					 ),
		   					 
		   					 br(),
		   					 
		   					 fluidRow(
		   					 	column(12,
	   					 		   wellPanel(id = "tPanel",style = "overflow-y:scroll; 
	   					 		   		  max-height: 150px; 
	   					 		   		  background-color: black",
	   					 		   		  h3("Linear Relationship"),
	   					 		   		  p(),
	   					 		   		  
	   					 		   		  
	   					 		   		  "
	   					 		   		  	As its name implies, linear regression analysis presumes a linear
											relationship between predictor and response variables.  Fitting a 
											linear model to non-linear data can lead to misleading results 
											and/or inaccurate predictions.
	   					 		   		  ",
	   					 		   		  
	   					 		   		  p(),
	   					 		   		  
	   					 		   		  "
	   					 		   		  	For simple linear regression involving only one predictor, we
											can assess the linearity of the predictor-response relationship
											through a number of visualizations.  The plot shown above, depicting
											observed versus predicted values, is one of them.  A linear 
											predictor-response relationship will show the points in a 
											diagonal line.  
	   					 		   		  ",
	   					 		   		  
	   					 		   		  p(),
	   					 		   		  
	   					 		   		  "
											Note that for more than two predictors, we cannot visualize the
											relationship between the predictors and response, since the plot
											would require more than three dimensions.  We can use other techniques 
											for assessing linearity in these situations.  These techniques
											will be included in future version of this tool.
	   					 		   		  "
	   					 		   )		   				
		   					 	)
		   					 )
		   			), 
		   			tabPanel(h4("Variance"), 
		   					 fluidRow(
		   					 	column(12, align = "center",
		   					 		   br(), br(),
		   					 		   plotOutput("residVfitPlot", width = "80%", height = "200px")
		   					 	)
		   					 ),
		   					 br(),
		   					 wellPanel(id = "tPanel",style = "overflow-y:scroll; 
		   					 		   		  max-height: 150px; 
		   					 		  background-color: black",
		   					 		  h3("Constant Variance of the Errors"),
		   					 		  p(),
		   					 		  
		   					 		  "
		   					 		  	For regression to be valid, the error terms must have \"constant
										variance\".  One reason for this requirement is the fact that our
										ordinary least squares calculation gives all observations the same
										weight.  This is true even though observations with bigger errors
										provide less help in explaining the variability in the 
										response variable.  So if there is a pattern to the error terms (meaning 
										that they're not constant), our regression is failing to account for
										some relationship between the predictor and response variables.
		   					 		  ",
		   					 		  p(),
		   					 		  
		   					 		  "
										We check for constant variance by examining a plot of the residuals vs.
										the fitted values.  Observing any pattern in the data is cause for concern.
										One typical pattern is a \"fan\" shape, where the points start in a 
										cluster and the expand further from the x axis (for example, 
										take a look at the Residuals vs. Fitted Values plot for the Outlier 1 data
										set used in this tool).  Another is a football or
										egg shape, with tighter values at the ends and broader distribution in the
										middle.
		   					 		  "
		   					 )
		   			), 
		   			tabPanel(h4("Normality"),
		   					 fluidRow(
		   					 	column(12, align = "center",
		   					 		   br(), br(),
		   					 		   plotOutput("qqPlot", width = "80%", height = "200px")
		   					 	)
		   					 ),	
		   					 br(),
		   					 wellPanel(id = "tPanel",style = "overflow-y:scroll; 
	   					 		   		  max-height: 150px; 
		   					 		  background-color: black",
		   					 		  h3("Normality of Errors"),
		   					 		  p(),
		   					 		  
		   					 		  
		   					 		  "
										It is often said that the error terms must be
										normally distributed for a regression analysis to be valid.  Technically, 
										non-normality has no impact on the coefficients that regression generates,
										so normality is not formally a requirement for regression validity. On the
										other hand, the confidence intervals and p-values associated with 
										regression are affected by normality.  Because we frequently rely 
										heavily on confidence-related measurements in regression, normality of 
										errors is often thought of as a requirement.
									  ",
		   					 		  
		   					 		  p(),

		   					 		  "
										We can use a q-q plot test for normality of errors.  Here, the q's 
										stand for quantile.  When we use quantiles, we divide a distribution up
										evenly into a specified number of divisions.  The quantiles represent the
										values at each division line.  For example, if we divide a normal 
										distribution into four quantiles, the first quantile ends at -0.674, 
										the second quantile ends at 0, and the third quantile ends at 0.674.
									  ",
		   					 		  
		   					 		  p(),
		   					 		  
		   					 		  "
										In the regression context, a q-q plot shows how the error terms from our 
										regression map to the values we would expect to see if the errors came 
										from a normal 
										distribution.  Suppose we have 11 observations.  On the y axis, the 
										q-q plot shows the 11 residuals, ordered from most negative at the bottom 
										to most
										positive at the top.  The x axis shows the values we would expect 
										at each quantile
										if we divided a normal distribution into 11 quantiles.  If our errors 
										are normally distributed, we will see a straight line positive 
										relationship between our x and y values.  If we don't, our errors are not
										normally distributed.  Note that the line shown in a q-q plot is not a
										regression line. By convention, it's a line that goes through the first
										and third quantile of the theoretical distribution.  This makes sense,
										of course, since we want to know how close our errors are to the theoretical
										normal distribution.  A regression line would just show us the trend in 
										the errors we generated, not their distance from where we'd like them to be.
									  ",
		   					 		  
		   					 		  p(),
		   					 		  
		   					 		  "
										To use this tool to demonstrate normality of errors, observe the 
										differences in the q-q plot that each data set generates, and add some
										points with large and small residuals to see their impact on the q-q 
										plot.
	
		   					 		  "		  
		   					 )		   					 
		   			),
		   			tabPanel(h4("Lev/Inf"),
		   					 fluidRow(
		   					 	column(12, align = "center",
		   					 		   br(), br(),
		   					 		   plotOutput("cooks", width = "80%", height = "200px")
		   					 	)
		   					 ),	
		   					 br(),
		   					 wellPanel(id = "tPanel",style = "overflow-y:scroll; 
		   					 		  max-height: 150px; 
		   					 		  background-color: black",
		   					 		  h3("Leverage, Influence, and Cook's Distance"),
		   					 		  p(),
		   					 		  
		   					 		  "
										Points that are unusual in their x value, y value, or both are called 
										outliers.  What we do with outliers depends on how they impact
										our regression analysis.  We can assess the impact of outliers 
										by looking at leverage, influence, and a measurement called 
		   					 		  	Cook's Distance.", 
		   					 		  
		   					 		  p(),
										
		   					 		  	"One way to understand leverage, influence, and Cook's distance is to think
										of a seesaw. The impact of adding weight to a seesaw depends on how much
										weight you're adding, and how far the weight is from the fulcrum.  It's the 
										same with regression.  Think of our regression line as the seesaw. In 
										regression, a point's horizontal distance from the fulcrum is called its leverage.  
										In other 
										words, leverage measures how far the x value of a point is from the mean
										of x.  So adding a point with high leverage to our regression analysis is 
										like adding weight to a seesaw far away 
										from its center.  To assess a point's impact on our regression analsysis, we also need 
										to know its \"weight\".  You can think of a point's weight as being equal to 
										how far the point's y value is from the 
										regression line.  This vertical distance is, of course,  the point's residual.  
										Even a heavy weight
										(residual) won't impact the seesaw (regression line) much if its 
										distance (leverage) from the fulcrum (mean of x) is small.
		   					 		  ",
		   					 		  
		   					 		  	p(),

										"
										Influence measures the actual impact of a point on the regression line.  So points 
										that are both far from the mean of x and far from the regression line have 
										high influence.  A point that is far from the mean of x but exactly on our 
										regression line has no influence, because it didn't move our line at all.  
										Similarly, a point that is vertically far from our regression line but 
										exactly at the center of the x values will have no impact on the slope of our
										line (althoug it will affect the intercept).",
	
										p(),
										
										"Cook's Distance measures a point's impact on the regression analysis
										by comparing the regression with the point to what the regression would
										look like without the point. Generally, a Cook's distance 
										measurement greater than 1.0 should trigger some investigation into a particular
										data point.
		   					 		  ",
										
										p(),
										"
										To use this tool to experiment with leverage, influence, and Cook's 
										distance, add some points that are both close to and far from both the
										mean of x and the regression line.  Notice the impact on
										the slope of the regression line, and observe how different values 
										affect the point's leverage, influence, and Cook's distance.
										"
		   					 )
		   			),
		   			tabPanel(h4("MC"),
		   				h4("Mulicolinearity: Coming Soon")
		   			),
		   			tabPanel(h4("Fit"),
		   				h4("Goodness of Fit: Coming Soon")			 
		   			)
		   		)# end tabsetpanel
		   )	
	
	), # end of Diagnostics row
	
	fluidRow(align = "center",
		h1("")
		#h5("Copyright 2016, Brett M. Amdur.  For suggestions or questions, contact me at brett.amdur@gmail.com")
	)
	
	
	) # end of fluidPage
) # end of shinyUI
