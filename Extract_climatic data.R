################### Daily ###########################
################### Daily ###########################
################### Daily ###########################



my_function <- function(lon, lat, site) {    
  # Mean daily temperature
  ncin <- nc_open("TG_mean.nc")
  print(ncin)
  t <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(t)
  obsoutput <- ncvar_get(ncin, # look for closest lon and lat
                         start = c(which.min(abs(ncin$dim$longitude$vals - lon)),
                                   which.min(abs(ncin$dim$latitude$vals - lat)),
                                   1),
                         count = c(1,1,-1))
  DataMeanT <- data.frame(DateN = t, MeanDailyT = obsoutput)
  nc_close(ncin) 
  Data=DataMeanT
  Data$Date=as.Date(Data$DateN,origin="2010-01-01")
  Data$Year=format(Data$Date,"%Y")
  Data$Month=format(Data$Date,"%m")
  Data$Day=format(Data$Date,"%d")
  Data$YearMonthDay=format(Data$Date, format="%Y-%m-%d")
  # Data <- within(DataMeanT, {
  #  Date <- as.Date(DateN, origin="2000-01-01")
  #   Year <- format(Data$Date,"%Y")
  #  Month <- format(Data$Date,"%m")
  #  YearMonth <- format(Data$Date, format="%Y-%b")
  # })
  Data_annual <- with(Data, aggregate(list("AirT" = MeanDailyT), list(Year=YearMonthDay),#Year or Month
                                      FUN = mean, na.action = na.pass))    
  Data_annual$Site=site   #This line adds a column with the Site name 
  
  write.table(Data_annual, file = "ClimaticData.csv", row.names=FALSE, 
              append = TRUE, col.names = F, sep = ", ", quote = TRUE)          # Export table, with append=TRUE it adds coulmns to an existing table 
  return(Data_annual) # SAVE AGGREGATED DATA FRAME 
}


# ITERATE THROUGH EACH LON/LAT PAIR ELEMENTWISE 
write.table(data.frame("Year", "Prec", "Site"), file = "PrecData.csv", row.names=FALSE, 
            append = F, col.names = F, sep = ", ", quote = TRUE)  # create a blank table template, values will be added to this template during the iteration 

# ITERATE THROUGH EACH LON/LAT PAIR ELEMENTWISE
df_list <- Map(my_function, df$E, df$N, df$Site)

df_list <- mapply(my_function, df$E, df$N, df$Site, SIMPLIFY=FALSE)    # EQUIVALENT C


### Format the data

Dv_temp <- read.csv("ClimaticData.csv", sep=",", stringsAsFactors = F)
head(Dv_temp)
Dv_temp2 <- do.call(rbind, str_split(Dv_temp$Year,"-"))
head(Dv_temp2)
Dv_temp2 <- as.data.frame(Dv_temp2)
colnames(Dv_temp2)[1] <- "Year"
colnames(Dv_temp2)[2]<- "Month"
head(Dv_temp2)
Dv_temp$Year <- NULL
new_temp<- cbind(Dv_temp2,Dv_temp)
head(new_temp)

colnames(new_temp)[3] <- "AirT"
colnames(new_temp)[4]<- "Site"

new_temp$AirT <- as.numeric(new_temp$AirT)
new_temp$Year <- as.numeric(new_temp$Year)

new_temp2 <- new_temp %>% group_by(Year, Site) %>% summarise(Mean_temp=mean(AirT))
head(new_temp2)

new_temp3<- transform(new_temp2, LINK=paste(Site,Year,sep = "_"))
head(new_temp3)
#############################    MONTHLY    ########################
#############################    MONTHLY    ########################
#############################    MONTHLY    ########################

my_function <- function(lon, lat, site) {    
  ncin <- nc_open("TG_mean.nc")
  print(ncin)
  t <- ncvar_get(ncin,"time")
  tunits <- ncatt_get(ncin,"time","units")
  nt <- dim(t)
  obsoutput <- ncvar_get(ncin, # look for closest lon and lat
                         start = c(which.min(abs(ncin$dim$longitude$vals - lon)),
                                   which.min(abs(ncin$dim$latitude$vals - lat)),
                                   1),
                         count = c(1,1,-1))
  DataMeanT <- data.frame(DateN = t, MeanDailyT = obsoutput)
  nc_close(ncin) 
  Data=DataMeanT
  Data$Date=as.Date(Data$DateN, origin = "1968-01-01")
  Data$Year=format(Data$Date,"%Y")
  Data$Month=format(Data$Date,"%m")
  Data$YearMonth=format(Data$Date, format="%Y-%b")
  # Data <- within(DataMeanT, {
  #  Date <- as.Date(DateN, origin="2000-01-01")
  #   Year <- format(Data$Date,"%Y")
  #  Month <- format(Data$Date,"%m")
  #  YearMonth <- format(Data$Date, format="%Y-%b")
  # })
  Data_annual <- with(Data, aggregate(list("AirT" = MeanDailyT), list(Year=YearMonth),#Year or Month
                                      FUN = mean, na.action = na.pass))    
  Data_annual$Site=site   #This line adds a column with the Site name 
  
  write.table(Data_annual, file = "ClimaticData.csv", row.names=FALSE, 
              append = TRUE, col.names = F, sep = ", ", quote = TRUE)          # Export table, with append=TRUE it adds coulmns to an existing table 
  return(Data_annual) # SAVE AGGREGATED DATA FRAME 
}