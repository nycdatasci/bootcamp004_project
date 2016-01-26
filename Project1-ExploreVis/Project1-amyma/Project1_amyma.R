setwd("~/Documents/R /Data Bootcamp/Project 1")
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(googleVis)
salary=read.xlsx("salary.xlsx",sheet = 1)
salary$PAID_WAGE_PER_YEAR<-as.numeric(salary$PAID_WAGE_PER_YEAR)
salary$CASE_RECEIVED_DATE<-as.Date(salary$CASE_RECEIVED_DATE,'%m/%d/%Y')
salary$CASE_RECEIVED_Year<-format(salary$CASE_RECEIVED_DATE,"%Y")
salary=tbl_df(salary)

#------------------------
# Compare with Other Jobs
#------------------------

subdata3<-group_by(salary,JOB_TITLE_SUBGROUP) %>%
  summarise(med.salary = median(as.numeric(PAID_WAGE_PER_YEAR),na.rm=TRUE))%>%
  arrange(med.salary)


ggplot(subdata3,aes(reorder(JOB_TITLE_SUBGROUP,-med.salary),med.salary,fill=JOB_TITLE_SUBGROUP=="data scientist"))+
  theme_hc()+ 
  scale_fill_manual(values = c("grey","blue"),guide=FALSE)+
  ylim(0,150000)+
  geom_bar(stat="identity",position='dodge',width=0.6)+
  labs(x="Job Title Subgroup", y="Median Salary")

#--------------------------
#   By visia type
#------------------------
s_ds<-salary[salary$JOB_TITLE_SUBGROUP=="data scientist",]
visa<-group_by(s_ds,VISA_CLASS) %>%
summarise(med.salary = median(as.numeric(PAID_WAGE_PER_YEAR),na.rm=TRUE),count=n())%>%
arrange(med.salary)
ggplot(visa,aes(reorder(VISA_CLASS,-med.salary),med.salary,fill=VISA_CLASS %in% c("H-1B","greencard")))+
theme_hc()+
scale_fill_manual(values = c("grey","blue"),guide=FALSE)+
ylim(0,155000)+
geom_bar(stat="identity",position='dodge',width=0.6)+
labs(x="VISA_CLASS", y="Median Salary")



#------------------------
# Job requirments
#------------------------


s_ds<-salary[salary$JOB_TITLE_SUBGROUP=="data scientist",]
exp_ds<-s_ds %>% 
  group_by(EXPERIENCE_REQUIRED_Y_N) %>%
  summarise(Count=n(),Med_Sa=median(as.numeric(PAID_WAGE_PER_YEAR)))
exp_ds[,1]<-c("No","Yes","Missing")

ggplot(exp_ds,aes(reorder(EXPERIENCE_REQUIRED_Y_N,-Med_Sa),Med_Sa,fill=EXPERIENCE_REQUIRED_Y_N))+
  theme_hc()+ 
  scale_fill_economist(guide=FALSE)+
  geom_bar(stat="identity",position='dodge',width=0.5)+
  labs(x="Experience Required?", y="Median Salary")

####

edu_ds<-s_ds %>% 
  group_by(EDUCATION_LEVEL_REQUIRED) %>%
  summarise(Count=n(),Med_Sa=median(as.numeric(PAID_WAGE_PER_YEAR)))
edu_ds[4,1]<-c("Missing")

ggplot(edu_ds,aes(reorder(EDUCATION_LEVEL_REQUIRED,-Med_Sa),Med_Sa,fill=EDUCATION_LEVEL_REQUIRED))+
  theme_hc()+ 
  scale_fill_economist(guide=FALSE)+
  geom_bar(stat="identity",position='dodge',width=0.6)+
  labs(x="Education Requirement", y="Median Salary")
#------------------------
# Compare between states
#------------------------

s_ds<-salary[salary$JOB_TITLE_SUBGROUP=="data scientist",]

state_ds<-s_ds %>% 
  group_by(WORK_STATE) %>%
  summarise(Count=n(),Med_Sa=median(as.numeric(PAID_WAGE_PER_YEAR))) %>%
  arrange(desc(Count))

head(state_ds, n = 5)
negeo<- gvisGeoChart(state_ds[,1:2], "WORK_STATE", "Count", 
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colorAxis="{colors: ['white', 'blue']}"))
nebar<-gvisBarChart(state_ds[c(1:5),1:2], "WORK_STATE", "Count",                 
                    options=list(legend='none'))

state_nm<-gvisMerge(negeo,nebar, horizontal=TRUE,tableOptions="cellspacing=5")
plot(state_nm)

# 2.adjusted median salary using price parity
pp<-read.xlsx("price_parity.xlsx",sheet=1)
pp<-arrange(pp,State)
#------------------------
# introduce a new dataset
#------------------------
ggplot(pp, aes(x=reorder(State,-Price.Parity), y=Price.Parity,fill=(State %in% c("California","New York","Washington","Massachusetts","Texas")))) +
geom_bar(stat='identity') +
geom_hline(yintercept = 100,color="grey",size=0.5)+
coord_flip(ylim=c(85,120))+
scale_fill_economist(guide=FALSE)+
labs(x="", y="",title="2013 Regional Price Parities by States")

###
adj_state_ds<-left_join(state_ds,pp,by=c("WORK_STATE"="State"))
adj_state_ds<-mutate(adj_state_ds,adj.Med_Sa=round(Med_Sa/Price.Parity*100)) %>%
  arrange(desc(adj.Med_Sa))
adj_sgeo<- gvisGeoChart(adj_state_ds[,c(1,5)], "WORK_STATE", "adj.Med_Sa", 
                        options=list(region="US", 
                                     displayMode="regions", 
                                     resolution="provinces",
                                     colorAxis="{colors: ['white', 'green']}"))

adj_sbar<-gvisBarChart(adj_state_ds[c(1:5),c(1,5)], "WORK_STATE", "adj.Med_Sa",                  
                       options=list(legend='none'))

state_ms<-gvisMerge(adj_sgeo,adj_sbar, horizontal=TRUE,tableOptions="cellspacing=5")
plot(state_ms)

#------------------------
# Compare between companies
#------------------------
# 1. based on num of jobs
comp_ds<-salary[salary$JOB_TITLE_SUBGROUP=="data scientist",]
comp_ds<-comp_ds%>%
  select(EMPLOYER_NAME,as.numeric(PAID_WAGE_PER_YEAR))%>%
  group_by(EMPLOYER_NAME) %>%
  mutate(Count=n(),Med_Sa=median(as.numeric(PAID_WAGE_PER_YEAR)))%>%
  select(EMPLOYER_NAME,Count,Med_Sa)%>%
  unique()%>%
  ungroup()%>%
  arrange(desc(Count))%>%
  top_n(10,Count)

comp_ds<-rbind(c("Company",4,110000),comp_ds)
comp_ds<-cbind(root=c(NA,rep("Company",10)),comp_ds)

Tree <- gvisTreeMap(comp_ds, idvar = "EMPLOYER_NAME", 
                    parentvar = "root" , 
                    sizevar = "Count", 
                    colorvar = "Med_Sa", 
                    options = list(
                      minColor='white',
                      maxColor='blue',
                      headerHeight=20,
                      fontColor='black',
                      showScale=TRUE))
plot(Tree)

# 2. based on average salary

tm_ds<-salary[salary$JOB_TITLE_SUBGROUP=="data scientist",]
tm_ds$EMPLOYER_NAME<-gsub("INTENT MEDIA, INC.","INTENT MEDIA",tm_ds$EMPLOYER_NAME)
tm_ds<-tm_ds%>%
  select(EMPLOYER_NAME,WORK_STATE,as.numeric(PAID_WAGE_PER_YEAR))%>%
  inner_join(.,state_ds[1:5,1],by="WORK_STATE")%>%
  group_by(EMPLOYER_NAME) %>%
  mutate(Count=n(),Med_Sa=median(as.numeric(PAID_WAGE_PER_YEAR)))%>%
  select(EMPLOYER_NAME,WORK_STATE,Count,Med_Sa) %>%
  group_by(WORK_STATE)%>%
  unique()%>%
  top_n(5,Med_Sa)%>%
  arrange(WORK_STATE,desc(Count))

# create a parent list 
parent_list<-state_ds[1:5,]
parent_list<-cbind(parent_list[,1,drop=F], data.frame(parent=rep("States",5)), parent_list[,2:3,drop=F])
parent_list<-rbind(c("States",NA,sum(salary$JOB_TITLE_SUBGROUP=="data scientist"),median(as.numeric(salary$PAID_WAGE_PER_YEAR))),parent_list)
colnames(parent_list)<-colnames(tm_ds)

# combine ts_ds with parent list
tm<-rbind(parent_list,tm_ds)


Tree2 <- gvisTreeMap(tm, idvar = "EMPLOYER_NAME", 
                    parentvar = "WORK_STATE" , 
                    sizevar = "Count", 
                    colorvar = "Med_Sa", 
                    options = list(
                      minColor='white',
                      maxColor='green',
                      headerHeight=20,
                      fontColor='black',
                      showScale=TRUE))
plot(Tree2)

#------------------------
# Back up plots
#------------------------
# 1.which is the most popular jobs? 

subdata<-group_by(salary,JOB_TITLE_SUBGROUP) %>%
  summarise(count = n())


ggplot(subdata,aes(reorder(JOB_TITLE_SUBGROUP,-count),count,fill=JOB_TITLE_SUBGROUP=="data scientist"))+
  geom_bar(stat="identity",position='dodge',width=0.6)+
  theme_hc()+ 
  scale_fill_manual(values = c("grey","blue"),guide=FALSE)+
  labs(x="Job title Subgroup", y="Count")

# 2.Histogram of average salary of Jobs (Back up)
subdata2<-group_by(salary,JOB_TITLE_SUBGROUP)%>%
  summarise(avg.salary = mean(as.numeric(PAID_WAGE_PER_YEAR),na.rm=TRUE))%>%
  arrange(avg.salary)


ggplot(subdata2,aes(reorder(JOB_TITLE_SUBGROUP,-avg.salary),avg.salary,fill=JOB_TITLE_SUBGROUP=="data scientist"))+
  theme_hc()+ 
  scale_fill_manual(values = c( "grey", "blue"))+
  geom_bar(stat="identity",position='dodge',width=0.6)+
  labs(x="Job Title Subgroup", y="Average Salary")

# 3. boxplot
# we need get rid of the outliers in lawyer part
qplot(reorder(JOB_TITLE_SUBGROUP,-as.numeric(PAID_WAGE_PER_YEAR),FUN=median),
      as.numeric(PAID_WAGE_PER_YEAR),
      data=group_by(salary,JOB_TITLE_SUBGROUP),
      geom="boxplot",
      color=JOB_TITLE_SUBGROUP)

# 4.Histogram of adjusted median salary of Jobs (Back up)

pp<-read.xlsx("price_parity.xlsx",sheet=1)
pp<-arrange(pp,State)

adj_job_ds<-left_join(salary,pp,by=c("WORK_STATE"="State")) %>%
  mutate(.,adj.salary=as.numeric(PAID_WAGE_PER_YEAR)/Price.Parity*100)%>%
  group_by(.,JOB_TITLE_SUBGROUP)%>%
  summarise(., adj.med.salary = median(as.numeric(adj.salary),na.rm=TRUE))%>%
  arrange(.,adj.med.salary)


ggplot(adj_job_ds,aes(reorder(JOB_TITLE_SUBGROUP,-adj.med.salary),adj.med.salary,fill=JOB_TITLE_SUBGROUP=="data scientist"))+
  theme_hc()+ 
  scale_fill_economist()+
  geom_bar(stat="identity",position='dodge')+
  labs(x="Job title", y="median of adjusted salary")

# 5. states' median salary without adjusted

sgeo <- gvisGeoChart(state_ds[,c(1,3)], "WORK_STATE", "Med_Sa", 
                     options=list(region="US", 
                                  displayMode="regions", 
                                  resolution="provinces",
                                  colorAxis="{colors: ['white', 'green']}"))
plot(sgeo)

