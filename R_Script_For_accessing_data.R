#My API Key: deleted 


# Import libraries
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# https://www.eia.gov/opendata/qb.php?sdid=TOTAL.RUUCUUS.M
#Unleaded Regular Gasoline, U.S. City Average Retail Price, Monthly
#TOTAL.RUUCUUS.M

#Imported Crude Oil Real Price, Monthly
#STEO.RAIMUUS_RP.M

#Real Disposable Personal Income, Monthly
#STEO.YD87OUS.M

#Civilian Unemployment Rate, Monthly
#STEO.XRUNR.M 

#Producer Price Index: Petroleum, Monthly
#STEO.WP57IUS.M


#Consumer Price Index (all urban consumers), Monthly
#STEO.CICPIUS.M








# API Key from EIA
key <- # Paste your Series IDs in the list, separated by commas
padd_key <- list('TOTAL.RUUCUUS.M ','STEO.RAIMUUS_RP.M',
                 'STEO.YD87OUS.M','STEO.XRUNR.M ',
                 'STEO.WP57IUS.M', 'STEO.CICPIUS.M')# Choose the start and end dates
startdate <- "2010-01-01" #YYYY-MM-DD
enddate <- "2021-04-01" #YYYY-MM-DD


j = 0
for (i in padd_key) {url <- paste('http://api.eia.gov/series/?api_key=',key,'&series_id=',i,sep="")  # Make the call to the EIA's API
res <- GET(url)
json_data <- fromJSON(rawToChar(res$content))
data <- data.frame(json_data$series$data)
data$Year <- substr(data$X1,1,4)
data$Month <- substr(data$X1,5,6)
data$Day <- 1 # Create date format
data$Date <- as.Date(paste(data$Year, data$Month, data$Day,    sep=''), 
                     format = "%Y%m%d") # Rename the column to its given name from the EIA
colnames(data)[2]  <- json_data$series$name # Drop the unnecessary date columns
data <- data[-c(1,3,4,5)]

if (j == 0){
  data_final <- data
}
else{
  data_final <- merge(data_final,data,by="Date")
}

j = j + 1
}# Splice the data between the start and end dates
data_final <- subset(data_final, Date >= startdate & Date <= enddate)



head(data_final)



class(data_final)




write_csv(data_final, file = "eia_months.csv")
