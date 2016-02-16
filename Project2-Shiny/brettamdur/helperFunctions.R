# helper functions for regression diagnostics
# shiny project.

sourceChart <- function(){
	x <- seq(1,20,1)
	y <- (2 * x) + rnorm(length(x),mean = 0, sd = 2)
	ggplot(data.frame(x=x,y=y), aes(x,y)) + geom_smooth() + geom_point()
}