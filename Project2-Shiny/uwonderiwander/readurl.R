library(curl)
library(RCurl)
library(RJSONIO)

GetMakeDetails = function(make_code) {
 #r1 = paste0("http://vpic.nhtsa.dot.gov/api/vehicles/DecodeWMI/",make_code,"?format=json")
 #r1
 
  
  if ((substr(make_code, 3, 3) == ' ') | (substr(make_code, 1, 1) == '#') | (substr(make_code, 1, 1) == '?') |
      (substr(make_code, 1, 1) == '\'') | (substr(make_code, 1, 1) == '-') | (substr(make_code, 1, 1) == ':') | 
      (substr(make_code, 1, 1) == '?') | (substr(make_code, 1, 1) == '/') | (substr(make_code, 1, 1) == '*') |
      (substr(make_code, 1, 1) == '\t') | (substr(make_code, 1, 1) == '\\') | (substr(make_code, 1, 1) == '.') | 
      (substr(make_code, 2, 2) == ' ') | (substr(make_code, 2, 2) == '`')) {
    return(c("Unknown", "Unknown", "Unknown", "", "", "Unknown"))
  }
 r3 =  url(paste0("http://vpic.nhtsa.dot.gov/api/vehicles/DecodeWMI/",make_code,"?format=json"))

 #r3
 l = readLines(r3, n = 1)
 #l
 l2 = fromJSON(l)
 close(r3)
 if ( length(l2$Results) > 0)
 {
   return(c(ifelse(is.null(l2$Results[[1]][["CommonName"]]),"None", l2$Results[[1]]["CommonName"]),
            ifelse(is.null(l2$Results[[1]][["Make"]]),"None", l2$Results[[1]]["Make"]),
            ifelse(is.null(l2$Results[[1]][["ManufacturerName"]]),"None", l2$Results[[1]]["ManufacturerName"]),
            ifelse(is.null(l2$Results[[1]][["ParentCompanyName"]]),"None", l2$Results[[1]]["ParentCompanyName"]),
            ifelse(is.null(l2$Results[[1]][["URL"]]),"None", l2$Results[[1]]["URL"]),
            ifelse(is.null(l2$Results[[1]][["VehicleType"]]),"None", l2$Results[[1]]["VehicleType"])))
 }
 else {
   return(c("Unknown", "Unknown", "Unknown", "", "", "Unknown"))
 }
}

a = make_codes_found[1226]
a
substr(a, 3,3)
results = GetMakeDetails("#1G")
results[[1]]
results[[2]]
results[[3]]
results[[4]]
results[[5]]

is.null(results[[5]])

results[[6]]

is.null(results[[6]])

MakeDetails_tbl[nrow(MakeDetails_tbl) + 1, ] = c("1FU", results[1], results[2], results[3], results[4], results[5], results[6])

is.null(results[6])
  
class(results)
results
showConnections()
warnings()
