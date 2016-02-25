col_action$movie<-rep("Action",dim(col_action)[1])
col_animation$movie<-rep("animation",dim(col_animation)[1])
col_comedy$movie<-rep("comedy",dim(col_comedy)[1])
col_horror$movie<-rep("Horror",dim(col_horror)[1])
data<-rbind(col_action,col_animation,col_comedy,col_horror)

ggplot(data,aes(movie,count))+
  geom_bar(aes(fill=color),position = "dodge",stat = "identity")+
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
                               "brown" ="#8B4513"))
