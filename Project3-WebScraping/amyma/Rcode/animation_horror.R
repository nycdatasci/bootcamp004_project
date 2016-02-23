#---------------------------horror-----------------------------
### which color shows the most?
df_horror<-read.csv("Horror.csv")
df2<-df_horror[,8:25]
df3<-as.data.frame(apply( df2, 1, which.min))
cnames = as.data.frame(colnames(df2)[df3[,1]])
colnames(cnames)<-"color"
col_horror<-cnames%>%
  group_by(color)%>%
  summarise(count=n())

ggplot(col_horror, aes(x=reorder(color,-count),y=count)) + 
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
com_horror<-cbind(movie=df_horror$movie,cnames)
out <- split( com_horror, f = com_horror$movie)
a<-intersect(out$horror_6.[2],out$horror_7.[2])
merge_hor<- data.frame() 
for (i in 1:28){
  for (j in 1:28){
    if (i!=j){
      a<-intersect(out[[i]][2],out[[j]][2])
      merge_hor<-rbind(merge_hor,a)
    }}}

merge_horror<-merge_hor%>%
  group_by(color)%>%
  summarise(count=n())%>%
  arrange(desc(count))
#---------------------------animation-----------------------------
# which color shows the most?
df_animation<-read.csv("animation.csv")
df2<-df_animation[,8:25]
df3<-as.data.frame(apply( df2, 1, which.min))
cnames = as.data.frame(colnames(df2)[df3[,1]])
colnames(cnames)<-"color"
col_animation<-cnames%>%
  group_by(color)%>%
  summarise(count=n())

ggplot(col_animation, aes(x=reorder(color,-count),y=count)) + 
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
com_anim<-cbind(movie=df_animation$movie,cnames)
out <- split( com_anim, f = com_anim$movie)
a<-intersect(out$anim_6.[2],out$anim_7.[2])
merge_anm<- data.frame() 
for (i in 1:28){
  for (j in 1:28){
    if (i!=j){
      a<-intersect(out[[i]][2],out[[j]][2])
      merge_anm<-rbind(merge_anm,a)
    }}}

merge_anim<-merge_anm%>%
  group_by(color)%>%
  summarise(count=n())%>%
  arrange(desc(count))

