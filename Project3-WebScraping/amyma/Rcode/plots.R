library(scatterplot3d)

plots<-function(df){
  par(mfrow=c(1,1))
col <- rgb(df$r/255.0, df$g/255.0, df$b/255.0)
scatterplot3d(df[,5:7],main="3-D Scatterplot iris_scaled",color = col,pch=19)

par(mfrow=c(1,3))
col_r <- rgb(df$r/255.0,0,0)
plot(df$g,df$b, col=col_r, pch=19, cex=1)

col_g <- rgb(0,df$g/255.0,0)
plot(df$r,df$b, col=col_g, pch=19, cex=1)

col_b<- rgb(0,0,df$b/255.0)
plot(df$r,df$g, col=col_b, pch=19, cex=1)

}
# comedy
 plots(df_comedy)
# action
 plots(df_action)
# horror
 plots(df_horror)
# animation
 plots(df_animation)
 
 library(plotly)
 third_plot<-function(df){
 plot_ly(df, x = r, y = g, z =b,marker = list(color =as.vector(as.character(df$html_color))),type = "scatter3d", mode = "markers")
   }
 
 df=df_animation
 col_r <- rgb(df$r/255.0,0,0)
 col_g <- rgb(0,df$g/255.0,0)
 col_b<- rgb(0,0,df$b/255.0)
 

 plot_ly(df, x = r, y = b,mode="markers",
         marker = list(color =as.vector(as.character(col_g))))
 

 plot_ly(df,x=g,y=b, mode="markers",
         marker = list(color =as.vector(as.character(col_r))))
 
 
 plot_ly(df$r,df$g, mode="markers",
         marker = list(color =as.vector(as.character(col_b))))