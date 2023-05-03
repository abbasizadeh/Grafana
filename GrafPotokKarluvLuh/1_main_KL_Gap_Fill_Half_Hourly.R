# Missing data imputation using imputeTS package
library(forecastML)
library(imputeTS)
library(rstudioapi)

dirname(getActiveDocumentContext()$path)
getwd()

source(paste0(dirname(getActiveDocumentContext()$path), '/DataPrepration.r'))


################################################################################
# Hladina Karlův Luh

FileNames <- list.files(paste0('./GrafPotokKarluvLuh/data/', Folders[1])) 
# Creating Half-Hourly sequence using forecastML  
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Karluv_Luh[[i]]$DateTime <- 
    as.POSIXct(paste(Hladina_Karluv_Luh[[i]]$date, 
                     Hladina_Karluv_Luh[[i]]$Time), 
               format = "%Y-%m-%d %H:%M", 
               tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Karluv_Luh[[i]][5:length(Hladina_Karluv_Luh[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Karluv_Luh[[i]] <- rbind(Hladina_Karluv_Luh[[i]][1:4], BP) 
}

# which(is.na(Hladina_Karluv_Luh[[3]]$date))
# Hladina_Karluv_Luh[[2]][1:30]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Hladina_Karluv_Luh)){
  Hladina_Karluv_Luh[[i]]$Complete_value <- na_kalman(Hladina_Karluv_Luh[[i]]$value)
}

ggplot_na_imputations(Hladina_Karluv_Luh[[1]]$value, Hladina_Karluv_Luh[[1]]$Complete_value)
# plot(m4[1:100], type = "l") 


# Createing data table for all data
Hladina_Karluv_Luh_dt <- Hladina_Karluv_Luh[[1]]
for (i in 2:length(Hladina_Karluv_Luh)){
  Hladina_Karluv_Luh_dt <- rbind(Hladina_Karluv_Luh_dt, Hladina_Karluv_Luh[[i]])
}



# ggplot_na_distribution(Hladina_Karluv_Luh[[1]]$value)
# ggplot_na_gapsize(Hladina_Karluv_Luh[[1]]$value)

# saveRDS(Hladina_Karluv_Luh_dt, file = './output/Hladina_Karluv_Luh.rds')


################################################################################
# Hladina_Karluv_Luh_ofest 

FileNames <- list.files(paste0('./GrafPotokKarluvLuh/data/', Folders[2])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Karluv_Luh_ofest[[i]]$DateTime <- as.POSIXct(paste(Hladina_Karluv_Luh_ofest[[i]]$date, Hladina_Karluv_Luh_ofest[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Karluv_Luh_ofest[[i]][5:length(Hladina_Karluv_Luh_ofest[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Karluv_Luh_ofest[[i]] <- rbind(Hladina_Karluv_Luh_ofest[[i]][1:4], BP) 
}

# which(is.na(Hladina_Karluv_Luh_ofest[[3]]$date))
# Hladina_Karluv_Luh_ofest[[3]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Hladina_Karluv_Luh_ofest)){
  Hladina_Karluv_Luh_ofest[[i]]$Complete_value <- na_kalman(Hladina_Karluv_Luh_ofest[[i]]$value)
}

ggplot_na_imputations(Hladina_Karluv_Luh_ofest[[1]]$value[1:150], 
                      Hladina_Karluv_Luh_ofest[[1]]$Complete_value[1:150])

# Createing data table for all data
Hladina_Karluv_Luh_ofest_dt <- Hladina_Karluv_Luh_ofest[[1]]
for (i in 2:length(Hladina_Karluv_Luh_ofest)){
  Hladina_Karluv_Luh_ofest_dt <- rbind(Hladina_Karluv_Luh_ofest_dt, 
                                       Hladina_Karluv_Luh_ofest[[i]])
}

# saveRDS(Hladina_Karluv_Luh_ofest_dt, file = './output/Hladina_Karluv_Luh_ofest.rds')


################################################################################
# Prutok_Karluv_Luh

FileNames <- list.files(paste0('./GrafPotokKarluvLuh/data/', Folders[3])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Prutok_Karluv_Luh[[i]]$DateTime <- as.POSIXct(paste(Prutok_Karluv_Luh[[i]]$date, Prutok_Karluv_Luh[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Prutok_Karluv_Luh[[i]][5:length(Prutok_Karluv_Luh[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Prutok_Karluv_Luh[[i]] <- rbind(Prutok_Karluv_Luh[[i]][1:4], BP) 
}

# which(is.na(Prutok_Karluv_Luh[[3]]$date))
# Prutok_Karluv_Luh[[2]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Prutok_Karluv_Luh)){
  Prutok_Karluv_Luh[[i]]$Complete_value <- na_kalman(Prutok_Karluv_Luh[[i]]$value)
}

ggplot_na_imputations(Prutok_Karluv_Luh[[1]]$value[1:150], Prutok_Karluv_Luh[[1]]$Complete_value[1:150])

# Createing data table for all data
Prutok_Karluv_Luh_dt <- Prutok_Karluv_Luh[[1]]
for (i in 2:length(Prutok_Karluv_Luh)){
  Prutok_Karluv_Luh_dt <- rbind(Prutok_Karluv_Luh_dt, Prutok_Karluv_Luh[[i]])
}

# saveRDS(Prutok_Karluv_Luh_dt, file = './output/Prutok_Karluv_Luh.rds')


################################################################################
# Temperature

FileNames <- list.files(paste0('./GrafPotokKarluvLuh/data/', Folders[4])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Temperature[[i]]$DateTime <- as.POSIXct(paste(Temperature[[i]]$date, Temperature[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Temperature[[i]][5:length(Temperature[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Temperature[[i]] <- rbind(Temperature[[i]][1:4], BP) 
}

# which(is.na(Temperature[[1]]$date))
# Temperature[[3]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Temperature)){
  Temperature[[i]]$Complete_value <- na_kalman(Temperature[[i]]$value)
}

ggplot_na_imputations(Temperature[[1]]$value[1:150], Temperature[[1]]$Complete_value[1:150])

# Createing data table for all data
Temperature_dt <- Temperature[[1]]
for (i in 2:length(Temperature)){
  Temperature_dt <- rbind(Temperature_dt, Temperature[[i]])
}

# saveRDS(Temperature_dt, file = './output/KL_Temperature.rds')


################################################################################
# Water_Temperature

FileNames <- list.files(paste0('./GrafPotokKarluvLuh/data/', Folders[5])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Water_Temperature[[i]]$DateTime <- as.POSIXct(paste(Water_Temperature[[i]]$date, Water_Temperature[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Water_Temperature[[i]][5:length(Water_Temperature[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Water_Temperature[[i]] <- rbind(Water_Temperature[[i]][1:4], BP) 
}

# which(is.na(Water_Temperature[[1]]$date))
# Temperature[[3]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Water_Temperature)){
  Water_Temperature[[i]]$Complete_value <- na_kalman(Water_Temperature[[i]]$value)
}

ggplot_na_imputations(Water_Temperature[[1]]$value[1:150], Water_Temperature[[1]]$Complete_value[1:150])

# Createing data table for all data
Water_Temperature_dt <- Water_Temperature[[1]]
for (i in 2:length(Water_Temperature)){
  Water_Temperature_dt <- rbind(Water_Temperature_dt, Water_Temperature[[i]])
}

# saveRDS(Water_Temperature_dt, file = './output/KL_Water_Temperature.rds')



Hladina_Karluv_Luh <- readRDS('./GrafPotokKarluvLuh/output/Hladina_Karluv_Luh.rds')
Hladina_Karluv_Luh_ofest <- readRDS('./GrafPotokKarluvLuh/output/Hladina_Karluv_Luh_ofest.rds')
Prutok_Karluv_Luh <- readRDS('./GrafPotokKarluvLuh/output/Prutok_Karluv_Luh.rds')
KL_Temperature <- readRDS('./GrafPotokKarluvLuh/output/KL_Temperature.rds')
KL_Water_Temperature <- readRDS('./GrafPotokKarluvLuh/output/KL_Water_Temperature.rds')

# # Imputing missing values using mean value
# m1 <- na_mean(TIMESERIES)
# ggplot_na_distribution(m1)
# plot(m1[1:100], type = "l")
# 
# # Imputing missing values using median value
# m2 <- na_mean(TIMESERIES, option = 'median')
# ggplot_na_distribution(m2)
# plot(m2[1:100], type = "l")
# 
# # Imputing missing values using linear interpolation
# m3 <- na_interpolation(TIMESERIES)
# ggplot_na_distribution(m3)
# plot(m3[1:100], type = "l")


# Imputing missing values using Kalman
# for longer and more complex time series (with trend and seasonality)
# it is good idea to try na.kalman and na.seadec.