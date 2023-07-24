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


###################################### AVERAGE YEAR  Daily ##############################################
###################################### AVERAGE YEAR  Daily ##############################################
###################################### AVERAGE YEAR  Daily ##############################################






slope=raster('slope_1KMmd_GMTEDmd.tif')
elevation=raster('elevation_1KMmd_GMTEDmd.tif')

slope_info=terra::extract(slope, data[,c('lon','lat')]) %>% as.data.frame() %>% mutate(site_id= 1:nrow(data))
colnames(slope_info)[1]= 'slope'
elevation_info=terra::extract(elevation, data[,c('lon','lat')])%>% as.data.frame()%>% mutate(site_id= 1:nrow(data))
colnames(elevation_info)[1]= 'elevation'



info <- data1 %>% distinct(Latitude, Longitude, .keep_all = TRUE) %>% drop_na()

data$site_id = 1:nrow(data)

unique(data$site_id)
table(info$Mapping_Ye)
range(info$Mapping_Ye)

colnames(data)[16] = 'lat'
colnames(data)[17] = 'lon'


data$Year <- format(data$Pozorovani, "%Y")
data$Month <- format(data$Pozorovani, "%m")
data$Day <- format(data$Pozorovani, "%d")

data=data %>% filter(!site_id %in% c('907','908'))
extract_temp <- function(data, ncfile) {
  
  # Inicializa el dataframe de resultados
  result <- data.frame(
    Taxón = character(),
    Pozorovani = as.Date(character()),
    Mapping_Ye = numeric(),
    lat = numeric(),
    lon = numeric(),
    site_id = integer(),
    temperature = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Recorre cada fila en los datos
  for (i in 1:nrow(data)) {
    
    # Obtiene las coordenadas y año del punto de muestreo
    lon <- data$lon[i]
    lat <- data$lat[i]
    year <- data$Mapping_Ye[i]
    
    # Encuentra los índices correspondientes en el archivo netCDF
    lon_index <- which.min(abs(ncfile$dim$longitude$vals - lon))
    lat_index <- which.min(abs(ncfile$dim$latitude$vals - lat))
    
    # Convierte los tiempos del archivo netCDF a fechas y encuentra los índices del inicio y final del año
    time_vals <- as.Date(ncfile$dim$time$vals, origin = "1950-01-01")
    start_index <- which(time_vals == as.Date(paste(year, "01", "01", sep = "-")))
    end_index <- which(time_vals == as.Date(paste(year, "12", "31", sep = "-")))
    
    # Si start_index o end_index son NULL (es decir, no se encontraron coincidencias), pasa a la siguiente iteración del bucle
    if (is.null(start_index) | is.null(end_index)) {
      next
    }
    
    # Obtiene la temperatura media del año
    temp <- mean(ncvar_get(ncfile, "rr", start = c(lon_index, lat_index, start_index), count = c(1, 1, end_index - start_index + 1)))
    
    # Agrega los resultados al dataframe
    result <- rbind(result, data.frame(
      Taxón = data$Taxón[i],
      Pozorovani = as.Date(data$Pozorovani[i]),
      Mapping_Ye = data$Mapping_Ye[i],
      lat = data$lat[i],
      lon = data$lon[i],
      site_id = data$site_id[i],
      temperature = temp
    ))
    cat('*')
  }
  
  return(result)
}




# Abre el archivo netCDF
ncfile <- nc_open("TG_mean.nc")
ncfile <- nc_open("tx_ens_mean_0.25deg_reg_v27.0e.nc")
ncfile <- nc_open("tn_ens_mean_0.25deg_reg_v27.0e.nc")
ncfile <- nc_open("rr_ens_mean_0.25deg_reg_v27.0e.nc")

# Carga tus datos de muestreo
# data <- read_csv("tu_archivo.csv") # remplaza esto con tu archivo de datos

# Aplica la función para extraer la temperatura
result <- extract_temp(data, ncfile)
result1 <- extract_temp(data, ncfile)
result2 <- extract_temp(data, ncfile)
result3 <- extract_temp(data, ncfile)

# Cierra el archivo netCDF cuando hayas terminado
nc_close(ncfile)

str(info)
write_csv2(result, 'Temp_Kouba.csv')


colnames(result)[7] = 'Temp_avg'
colnames(result1)[7] = 'Temp_max'
colnames(result2)[7] = 'Temp_min'
colnames(result3)[7] = 'Prec'

result1 = result1[, c(6,7)]
result2 = result2[, c(6,7)]
result3 = result3[, c(6,7)]

slope_info = slope_info %>% drop_na()
elevation_info = elevation_info %>% drop_na()

z= cbind(result,result1,result2,result3, slope_info,elevation_info)
z = z[,-c(8,10,12,15,17)]
write.csv2(z, 'Climatic_data.csv')



