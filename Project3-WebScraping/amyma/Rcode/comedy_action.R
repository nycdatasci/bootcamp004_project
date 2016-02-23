setwd("~/Documents/python/a/result3 10-1--using")
library(dplyr)
library(ggplot2)
library(ggthemes)
#---------------------------comedy-----------------------------
# which color shows the most?
df_comedy<-read.csv("comedy.csv")
df2<-df_comedy[,8:25]
df3<-as.data.frame(apply( df2, 1, which.min))
cnames = as.data.frame(colnames(df2)[df3[,1]])
colnames(cnames)<-"color"
col_comedy<-cnames%>%
  group_by(color)%>%
  summarise(count=n())

ggplot(col_comedy, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color),stat="identity",position='dodge')+
  theme_economist()+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+
  labs(y="Basic Color",x="Count")

## color combination
com_comedy<-cbind(movie=df_comedy$movie,cnames)
out <- split( com_comedy, f = com_comedy$movie)
a<-intersect(out$comedy_6.[2],out$comedy_7.[2])
merge_com<- data.frame() 
for (i in 1:28){
    for (j in 1:28){
        if (i!=j){
            a<-intersect(out[[i]][2],out[[j]][2])
            merge_com<-rbind(merge_com,a)
}}}

merge_comdy<-merge_com%>%
  group_by(color)%>%
  summarise(count=n())%>%
  arrange(desc(count))
#---------------------------action-----------------------------
### which color shows the most?
df_action<-read.csv("action.csv")
df2<-df_action[,8:25]
df3<-as.data.frame(apply( df2, 1, which.min))
cnames = as.data.frame(colnames(df2)[df3[,1]])
colnames(cnames)<-"color"
col_action<-cnames%>%
  group_by(color)%>%
  summarise(count=n())

ggplot(col_action, aes(x=reorder(color,-count),y=count)) + 
  geom_bar(aes(fill = color),stat="identity",position='dodge')+
  theme_economist()+
  scale_fill_manual(values = c("black"= "#000000",
                               "gray" ="#808080",
                               "white" ="#FFFFFF",
                               "silver"= "#C0C0C0",
                               "maroon" ="#800000",
                               "red" ="#FF0000",
                               "olive"= "#808000",
                               "yellow" ="#FFFF00",
                               "green" ="#008000",
                               "teal"= "#008080",
                               "aqua" ="#00FFFF",
                               "navy" ="#000080",
                               "blue"= "#0000FF",
                               "purple" ="#800080",
                               "fuchsia" ="#FF00FF",
                               "orange"="#FFA500",
                               "brown" ="#8B4513"),guide=FALSE)+
  labs(y="Basic Color",x="Count")

## color combination
com_action<-cbind(movie=df_action$movie,cnames)
out <- split( com_action, f = com_action$movie)
a<-intersect(out$action_6.[2],out$action_7.[2])
merge_act<- data.frame() 
for (i in 1:28){
  for (j in 1:28){
    if (i!=j){
      a<-intersect(out[[i]][2],out[[j]][2])
      merge_act<-rbind(merge_act,a)
    }}}

merge_action<-merge_act%>%
  group_by(color)%>%
  summarise(count=n())%>%
  arrange(desc(count))
