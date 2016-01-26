#Project for visualizing movie production/box office data from boxofficemojo.com
#Want to look at networks of film makers, grouped by different criteria
#this code generates all the plots from the presentation only by modifying the filters near line 100 and the plotting parameters near line 200
library(igraph)#for network plots
library(dplyr)#for reshaping
library(reshape2)#also for reshaping
library(RColorBrewer)#for coloring my plots

movies=read.csv("movie_data.csv",header=TRUE,stringsAsFactors = FALSE)#load the data, leaving the missing data as "N/A" for now, will actually make it easier to remove later
#some rows have errors from the scrapping (columns shifted), but I think most will be removed later anyways, because they tend to be very incomplete records if they have this error

movie_makers=select(movies,
                    actor1,actor2,actor3,actor4,actor5,actor6)#simplify things by only considering the people that will be put in the network
#movie_makers=movie_makers[1:5,]#short version for testing, can comment out later
n=length(movie_makers)#saving this will make it easier to change line 11 for future runs with a different subset
possible_connections=n*(n-1)/2#number of possible connections for one movie, based on the number of people considered for each movie, will be useful later
movie_makers$row_number=1:length(movie_makers[,1])#add a row number row, i'll need this

#Prmiary task is to reshape the movie_makers data so it can be used to make a network plot, this requires a data frame of "nodes":showing the film makers and their properties
                                                                                      # and also a data frame of "links":showing which film makers are connected to which (by collaborations)


#generate the node list first, for now its just a list of names, properties for grouping will be added later (will have a dummy group 1)###################################
movie_nodes=melt(movie_makers, id.vars=c('row_number'))#this gets all actors from the various slots into one column
movie_nodes=select(movie_nodes,name=value)#remove columns we dont care about
movie_nodes[movie_nodes == "N/A"] <- NA 
movie_nodes=movie_nodes[complete.cases(movie_nodes),]#remove those incomplete records
movie_nodes=unique(movie_nodes)#remove duplicate records
movie_nodes=as.data.frame(sapply(movie_nodes,sub,pattern='\\*',replacement=""))#remove all astrixes which sometimes appear after actors names (not even sure what this indicates in BOM), this screws up a column name
movie_nodes=as.data.frame(sapply(movie_nodes,sub,pattern='\\,',replacement=""))#remove commas from names, will make things a little easier
colnames(movie_nodes) = "name"#fix the names
movie_nodes=as.data.frame(arrange(movie_nodes,name))#finally, reorder the nodes by name
movie_nodes=unique(movie_nodes)#remove duplicate records #this must be done AFTER the astrix removal, or there will be duplicates
rownames(movie_nodes) <- 1:nrow(movie_nodes)#fix row names
length(movie_nodes[,1])#about 739 actors in the set
movie_nodes$group=1#adds the dummy group, will give better grouping later


#now want to add some attributes for grouping to the node list,this will take a few steps
#start with a frame listing all info that contributes to the attributes I'm interested in
node_attributes1=select(movies,name=actor1,box_office=domestic,distributor,genre,budget)#select actors and attributes for each movie, doing it one actor slot at a time
node_attributes2=select(movies,name=actor2,box_office=domestic,distributor,genre,budget)
node_attributes3=select(movies,name=actor3,box_office=domestic,distributor,genre,budget)
node_attributes4=select(movies,name=actor4,box_office=domestic,distributor,genre,budget)
node_attributes5=select(movies,name=actor5,box_office=domestic,distributor,genre,budget)
node_attributes6=select(movies,name=actor6,box_office=domestic,distributor,genre,budget)

node_attributes=rbind(node_attributes1,node_attributes2,node_attributes3,node_attributes4,node_attributes5,node_attributes6)#put results all in one frame
node_attributes=as.data.frame(sapply(node_attributes,sub,pattern='\\*',replacement=""))#remove astrix from names
node_attributes=as.data.frame(sapply(node_attributes,sub,pattern='\\$',replacement=""))#remove dollar signs from budget and box office
node_attributes=as.data.frame(sapply(node_attributes,sub,pattern='\\,',replacement=""))#remove commas from budget and box office
node_attributes=as.data.frame(sapply(node_attributes,sub,pattern='\\,',replacement=""))#remove commas from budget and box office, not sure why it was needed twice
node_attributes$budget=as.character(node_attributes$budget)

fix_list=grepl("million",node_attributes[,5])#these rows need to be fixed because they contain a string instead of a dollar amount
node_attributes=as.data.frame(sapply(node_attributes,sub,pattern=' million',replacement=""))#drop the million string, will multiple by 10^6 in a moment
node_attributes$box_office = as.numeric(as.character(node_attributes$box_office))
node_attributes$budget = as.numeric(as.character(node_attributes$budget))
node_attributes$budget[fix_list]=node_attributes$budget[fix_list]*10^6#rescale those values that I dropped the "million" string for

#now use this frame to get some attributes for grouping, start with distributor (which is related to studio) 
node_studio=summarise(group_by(node_attributes,name,studio=distributor),studio_count=n())#displays how many times each actor has worked with each studio
max_studio=summarise(node_studio,max_studio_count=max(studio_count))#get the most frequent studio for each actor(just the number)
node_studio=left_join(max_studio,node_studio,by="name")
node_studio=filter(node_studio,studio_count==max_studio_count)#now has list of actors and their most frequent studio(s)
#need to decide what to do with ties,since data set is large anyways (for network plotting), simple solution is to just throw them out
#slighty better solution is to replace their studio with a "no preference" place holder, after they are thrown out the "none" studio can be put in later
node_studio2=node_studio[duplicated(node_studio[,1]) | duplicated(node_studio[,1], fromLast=TRUE),]#records with no dominte studio preference
node_studio=anti_join(node_studio,node_studio2,by="name")#remove those duplicate records
node_studio=select(node_studio,name,studio)#drop info I don't want/need
movie_nodes=left_join(movie_nodes,node_studio,by="name")
movie_nodes[] = lapply(movie_nodes, as.character)
movie_nodes$studio[is.na(movie_nodes$studio)] = "None"#replace NA's with a studio preference of "None"

#next attribute I'll add is the prefered genre, will mostly be the same as the procedure for preferred studio
node_genre=summarise(group_by(node_attributes,name,genre),genre_count=n())#displays how many times each actor has worked with each studio
node_genre=filter(node_genre,genre!="Unknown")#don't consider movies with genre as unknown, basicaly im just ignoring these incomplete records
#would be better if over lapping genres (e.g. romantic comedy)were counted twice, one comedy, one romance, maybe do this later? sounds difficult
max_genre=summarise(node_genre,max_genre_count=max(genre_count))#get the most frequent studio for each actor(just the number)
node_genre=left_join(max_genre,node_genre,by="name")
node_genre=filter(node_genre,genre_count==max_genre_count)#now has list of actors and their most frequent studio(s)
#need to decide what to do with ties,since data set is large anyways (for network plotting), simple solution is to just throw them out
#slighty better solution is to replace their genre with a "no preference" place holder, after they are thrown out the "none" studio can be put in later
node_genre2=node_genre[duplicated(node_genre[,1]) | duplicated(node_genre[,1], fromLast=TRUE),]#records with no dominate genre preference
node_genre=anti_join(node_genre,node_genre2,by="name")#remove those duplicate records
node_genre=select(node_genre,name,genre)#drop info I don't want/need
movie_nodes=left_join(movie_nodes,node_genre,by="name")
movie_nodes[] = lapply(movie_nodes, as.character)
movie_nodes$genre[is.na(movie_nodes$genre)] = "None"#replace NA's with a genre preference of "None"

#next attribute I'll add is the average budget of a person's films, 
node_budget=summarise(group_by(node_attributes,name),budget_avg=mean(budget,na.rm=TRUE))#get the average budget of films for each person
movie_nodes=left_join(movie_nodes,node_budget,by="name")

#final attribute I'll add is the average box office of a person's films, 
node_box_office=summarise(group_by(node_attributes,name),box_office_avg=mean(box_office,na.rm=TRUE))#get the averge box office for films for each person
movie_nodes=left_join(movie_nodes,node_box_office,by="name")

#############before making link frame, can reduce the set even more by applying any filters#######################################
#############variations of these filters, and the options for the actual plot command will create all the plots shown in the presentation#########

#movie_nodes=filter(movie_nodes,genre!="None")#remove people with no dominate genre preference
#movie_nodes=filter(movie_nodes,studio!="None")#remove people with no dominate studio preference
#movie_nodes=filter(movie_nodes,genre=="Comedy"|genre=="Drama"|genre=="Action"|genre=="Fantasy"|genre=="Horror"|genre=="Romance")#look at a subset of genres
movie_nodes=filter(movie_nodes,studio=="Fox"|studio=="Universal"|studio=="Buena Vista"|studio== "Sony / Columbia"|studio=="Paramount"|studio=="Warner Bros.")
#movie_nodes=filter(movie_nodes,complete.cases(movie_nodes))#remove incomplete records
#movie_nodes=filter(movie_nodes,studio=="Warner Bros.")
#movie_nodes=filter(movie_nodes,studio=="Fox")#
#movie_nodes=filter(movie_nodes,studio== "Sony / Columbia")
movie_nodes=filter(movie_nodes,genre== "Drama")

#movie_nodes2<<-arrange(movie_nodes,genre)#need to sort by genre before assigning the number so that circular network plots are more clear
movie_nodes2<<-arrange(movie_nodes,studio)#need to sort by genre before assigning the number so that circular network plots are more clear
movie_nodes=movie_nodes2
movie_nodes$id=0:(length(movie_nodes[,1])-1)#will need these ID numbers to substitute numerical values for the names in the link list, must be done AFTER filter
movie_nodes=select(movie_nodes,id,name,group,studio,studio,genre,budget_avg,box_office_avg)#re-order the rows so the ID is first
#one network plotting package requires numerical names, and the other requires identical labels between nodes and links, so ID # first works for both



##########generate the link frame next###############################
max_size=length(movie_makers[,1])*(possible_connections)#largest possible length for the link list given the number of movie records and number of possible connections per movie
#but its expected to reduce alot
  
src=rep("N/A",max_size)#call missing values "N/A", will make it easier to remove later
trgt=rep("N/A",max_size)

movie_links=data.frame(src,trgt,stringsAsFactors=FALSE)#links frame, will be needed to make network plots
#links frame starts at maximum possible size, will aggregate and then throw away the na's also, so it will shrink later

#this function generate a chunk of the links data frame, takes a row (one movie) of the movie_makers frame as input
link_gen=function(movie_row){
  i=movie_row[[n+1]]#row number of current row of the movie_makers frame, should be last column of input
  r=(i-1)*possible_connections+1#intialize  row counter for the link frame based on the current row number in the movie_makers frame
  for(j in 1:(n-1)){#loop over columns as sources(except last column, because that provides no new information)
   for(k in (j+1):(n)){#loop over over columns as targets, size of loop depends on current source (must be to the right of current source)
      movie_links[r,1]<<-movie_row[[j]]#update source, must be carreful that I update the global movie_links frame, and not just the copy that is in the scope of the function
      movie_links[r,2]<<-movie_row[[k]]#update target
      r=r+1#increment link frames row number index
   }
 }
}


link_gen(movie_makers)#apply the function to the whole movie makers frame at once
movie_links[movie_links == "N/A"] <- NA 
movie_links=movie_links[complete.cases(movie_links),]#remove those incomplete records
movie_links=filter(movie_links,src!=trgt)#remove links to oneself(possible if same person acted/directed/produced)
movie_links=as.data.frame(sapply(movie_links,sub,pattern='\\*',replacement=""))#remove all astrixes which sometimes appear after actors names (not even sure what this indicates in BOM)
movie_links=as.data.frame(t(apply(movie_links, 1, sort)))#fix the order of links to be alphabetical (otherwise it wont aggregate correctly), this line screws up my column names
movie_links=as.data.frame(select(movie_links,src=V1,trgt=V2))#fix the names
length(movie_links[,1])#have about 70K links before aggregation
#do the aggregation
links_by_unique=group_by(movie_links,src,trgt)#group by src
movie_links=summarise(links_by_unique,weight=n())#this will eliminate repeats, and instead replace it with the "weight" of the link,which is a count of how many times the link has occured
movie_links=as.data.frame(arrange(movie_links,src,trgt))#finally, reorder the links by the source name, then target name
length(movie_links[,1])#have about 15K links after aggregation

#last step before network plot is to replace the character names in the link frame with the numerical values it is expecting, we'll do this by merging with the ID number in our node list
temp_df=select(movie_nodes,src=name,id)#temporary frame to make the id join easier
movie_links=inner_join(movie_links, temp_df, by = "src")#do the id join
movie_links=select(movie_links,src=id,trgt,weight)#rename row and throw out old one


temp_df=select(movie_nodes,trgt=name,id)#temporary frame to make the id join easier
movie_links=inner_join(movie_links, temp_df, by = "trgt")#do the id join
movie_links=select(movie_links,src,trgt=id,weight)#rename row and throw out old 
movie_links$src=as.numeric(movie_links$src)#force type
movie_links$trgt=as.numeric(movie_links$trgt)#force type, if its not numerical the network map will fail

#####all thats left is setting final options to display the plot
movie_network= graph.data.frame(movie_links, movie_nodes, directed=FALSE)#defines the network object to be plotted

l = layout.circle(movie_network)  # define circle layout for when/if its used


col_genre =rep("grey40", vcount(movie_network))#intializes color vector
col_studio=rep("grey40", vcount(movie_network))#intializes color vector

#assign colors based on genre
col_genre[V(movie_network)$genre=="Comedy"] = "orange"
col_genre[V(movie_network)$genre=="Drama"] = "blue"
col_genre[V(movie_network)$genre=="Horror"] = "black"
col_genre[V(movie_network)$genre=="Action"] = "green"
col_genre[V(movie_network)$genre=="Romance"] = "red"
col_genre[V(movie_network)$genre=="Fantasy"] = "purple"

col_studio[V(movie_network)$studio=="Warner Bros."] = "orange"
col_studio[V(movie_network)$studio=="Fox"] = "blue"
col_studio[V(movie_network)$studio=="Paramount"] = "black"
col_studio[V(movie_network)$studio== "Sony / Columbia"] = "green"
col_studio[V(movie_network)$studio=="Buena Vista"] = "red"
col_studio[V(movie_network)$studio=="Universal"] = "purple"







#actual plot done here
plot(movie_network,layout=l,vertex.label=NA,vertex.size=3,
     vertex.color=col_studio)#,  #set the colors, the way I've defined the 

   


