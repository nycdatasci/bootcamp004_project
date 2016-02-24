
#The below document contains the process for calling the UN ComTrade API, specifically for all import and export values of beef, 
#pork, coconut oil, and palm oil. The code calls the country and area codes of trading partners and adds them to a data frame. 

install.packages("rjson")
library(rjson)
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

#Extracting datasets for four products -- coconut oil, palm oil, beef, pork
#Compare coconut oil to palm oil
#Compare beef to pork

############################## API to UNCOMTRADE ################################

#function

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="S2"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}



###################### Exporting Specific Data ######################

#The indices below (841/842, 4243, 4242, 0113, 0111) refer to USA, Coconut Oil, Palm Oil, Swine (Pork), and Beef. 
#Unfortunately, the API only allows me to call five years maximum. In addition, the USA uses a different index for all years prior to 1981. 
#The indices refer to revision to of the Standard International Trade Classification. 

#For the majority of import values
qtesta <- get.Comtrade(r="841", p="all", ps="1975,1976,1976,1978,1979", fmt="csv", cc="4243,4242,0113,0111")
qtestb <- get.Comtrade(r="841", p="all", ps="1980", fmt="csv", cc="4243,4242,0113,0111")
qtestc <- get.Comtrade(r="842", p="all", ps="1981,1982,1983,1984", fmt="csv", cc="4243,4242,0113,0111")
qtestd <- get.Comtrade(r="842", p="all", ps="1985,1986,1987,1988,1989", fmt="csv", cc="4243,4242,0113,0111")
qteste <- get.Comtrade(r="842", p="all", ps="1990,1991,1992,1993,1994", fmt="csv", cc="4243,4242,0113,0111")
qtestf <- get.Comtrade(r="842", p="all", ps="1995,1996,1997,1998,1999", fmt="csv", cc="4243,4242,0113,0111")
qtestg <- get.Comtrade(r="842", p="all", ps="2000,2001,2002,2003,2004", fmt="csv", cc="4243,4242,0113,0111")
qtesth <- get.Comtrade(r="842", p="all", ps="2005,2006,2007,2008,2009", fmt="csv", cc="4243,4242,0113,0111")
qtesti <- get.Comtrade(r="842", p="all", ps="2010,2011,2012,2013,2014", fmt="csv", cc="4243,4242,0113,0111")
qtestj <- get.Comtrade(r="842", p="all", ps="2015", fmt="csv", cc="4243,4242,0113,0111")

#For the majority of export values
qtestk <- get.Comtrade(r="all", p="841", ps="1975,1976,1976,1978,1979", fmt="csv", cc="4243,4242,0113,0111")
qtestl <- get.Comtrade(r="all", p="841", ps="1980", fmt="csv", cc="4243,4242,0113,0111")
qtestm <- get.Comtrade(r="all", p="842", ps="1981,1982,1983,1984", fmt="csv", cc="4243,4242,0113,0111")
qtestn <- get.Comtrade(r="all", p="842", ps="1985,1986,1987,1988,1989", fmt="csv", cc="4243,4242,0113,0111")
qtesto <- get.Comtrade(r="all", p="842", ps="1990,1991,1992,1993,1994", fmt="csv", cc="4243,4242,0113,0111")
qtestp <- get.Comtrade(r="all", p="842", ps="1995,1996,1997,1998,1999", fmt="csv", cc="4243,4242,0113,0111")
qtestq <- get.Comtrade(r="all", p="842", ps="2000,2001,2002,2003,2004", fmt="csv", cc="4243,4242,0113,0111")
qtestr <- get.Comtrade(r="all", p="842", ps="2005,2006,2007,2008,2009", fmt="csv", cc="4243,4242,0113,0111")
qtests <- get.Comtrade(r="all", p="842", ps="2010,2011,2012,2013,2014", fmt="csv", cc="4243,4242,0113,0111")
qtestt <- get.Comtrade(r="all", p="842", ps="2015", fmt="csv", cc="4243,4242,0113,0111")

#create data frame for each of the calls

#Import
dqtesta <- as.data.frame(do.call(rbind, qtesta))
dqtestb <- as.data.frame(do.call(rbind, qtestb))
dqtestc <- as.data.frame(do.call(rbind, qtestc))
dqtestd <- as.data.frame(do.call(rbind, qtestd))
dqteste <- as.data.frame(do.call(rbind, qteste))
dqtestf <- as.data.frame(do.call(rbind, qtestf))
dqtestg <- as.data.frame(do.call(rbind, qtestg))
dqtesth <- as.data.frame(do.call(rbind, qtesth))
dqtesti <- as.data.frame(do.call(rbind, qtesti))
dqtestj <- as.data.frame(do.call(rbind, qtestj))

#Export
dqtestk <- as.data.frame(do.call(rbind, qtestk))
dqtestl <- as.data.frame(do.call(rbind, qtestl))
dqtestm <- as.data.frame(do.call(rbind, qtestm))
dqtestn <- as.data.frame(do.call(rbind, qtestn))
dqtesto <- as.data.frame(do.call(rbind, qtesto))
dqtestp <- as.data.frame(do.call(rbind, qtestp))
dqtestq <- as.data.frame(do.call(rbind, qtestq))
dqtestr <- as.data.frame(do.call(rbind, qtestr))
dqtests <- as.data.frame(do.call(rbind, qtests))
dqtestt <- as.data.frame(do.call(rbind, qtestt))


#append all data frames

append=do.call(rbind, list(dqtesta, dqtestb, dqtestc, dqtestd, dqteste, dqtestf, dqtestg, dqtesth, dqtesti, dqtestj, dqtestk, dqtestl,
                           dqtestm, dqtestn, dqtesto, dqtestp, dqtestq, dqtestr, dqtests, dqtestt))

#export files to csv 
write.table(append, file = "data_all.csv", sep=",",row.names=F)
UNdf = append

