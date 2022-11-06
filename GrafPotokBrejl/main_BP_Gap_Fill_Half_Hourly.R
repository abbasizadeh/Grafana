# Missing data imputation using imputeTS package
library(forecastML)
library(imputeTS)


setwd(dirname(getActiveDocumentContext()$path))
getwd()

source(paste0(dirname(getActiveDocumentContext()$path), '/BP_DataPrepration.r'))


################################################################################
# Hladina Brejl

# Creating Half-Hourly sequence using forecastML  
FileNames <- list.files(paste0('./data/', Folders[1])) 
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Brejl[[i]]$DateTime <- as.POSIXct(paste(Hladina_Brejl[[i]]$date, Hladina_Brejl[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Brejl[[i]][5:length(Hladina_Brejl[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Brejl[[i]] <- rbind(Hladina_Brejl[[i]][1:4], BP) 
}

# which(is.na(Hladina_Brejl[[3]]$date))
# Hladina_Brejl[[2]][1:30]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Hladina_Brejl)){
  Hladina_Brejl[[i]]$Complete_value <- na_kalman(Hladina_Brejl[[i]]$value)
}

ggplot_na_imputations(Hladina_Brejl[[1]]$value, Hladina_Brejl[[1]]$Complete_value)
# plot(m4[1:100], type = "l") 


# Createing data table for all data
Hladina_Brejl_dt <- Hladina_Brejl[[1]]
for (i in 2:length(Hladina_Brejl)){
  Hladina_Brejl_dt <- rbind(Hladina_Brejl_dt, Hladina_Brejl[[i]])
}


# ggplot_na_distribution(Hladina_Brejl[[1]]$value)
# ggplot_na_gapsize(Hladina_Brejl[[1]]$value)

# saveRDS(Hladina_Brejl_dt, file = './output/Hladina_Brejl.rds')


################################################################################
# Hladina_Brejl_Ofest

FileNames <- list.files(paste0('./data/', Folders[2])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Brejl_Ofest[[i]]$DateTime <- as.POSIXct(paste(Hladina_Brejl_Ofest[[i]]$date, Hladina_Brejl_Ofest[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Brejl_Ofest[[i]][5:length(Hladina_Brejl_Ofest[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Brejl_Ofest[[i]] <- rbind(Hladina_Brejl_Ofest[[i]][1:4], BP) 
}

# which(is.na(Hladina_Brejl_Ofest[[3]]$date))
# Hladina_Brejl_Ofest[[3]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Hladina_Brejl_Ofest)){
  Hladina_Brejl_Ofest[[i]]$Complete_value <- na_kalman(Hladina_Brejl_Ofest[[i]]$value)
}

ggplot_na_imputations(Hladina_Brejl_Ofest[[1]]$value, Hladina_Brejl_Ofest[[1]]$Complete_value)

# Createing data table for all data
Hladina_Brejl_Ofest_dt <- Hladina_Brejl_Ofest[[1]]
for (i in 2:length(Hladina_Brejl_Ofest)){
  Hladina_Brejl_Ofest_dt <- rbind(Hladina_Brejl_Ofest_dt, Hladina_Brejl_Ofest[[i]])
}

# saveRDS(Hladina_Brejl_Ofest_dt, file = './output/Hladina_Brejl_Ofest.rds')


################################################################################
# Prutok_Brejl

FileNames <- list.files(paste0('./data/', Folders[3])) 
# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Prutok_Brejl[[i]]$DateTime <- as.POSIXct(paste(Prutok_Brejl[[i]]$date, Prutok_Brejl[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Prutok_Brejl[[i]][5:length(Prutok_Brejl[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Prutok_Brejl[[i]] <- rbind(Prutok_Brejl[[i]][1:4], BP) 
}

# which(is.na(Prutok_Brejl[[3]]$date))
# Prutok_Brejl[[2]][1:20]

# Missing data imputation using imputeTS package and kalman method
for(i in 1:length(Prutok_Brejl)){
  Prutok_Brejl[[i]]$Complete_value <- na_kalman(Prutok_Brejl[[i]]$value)
}

ggplot_na_imputations(Prutok_Brejl[[1]]$value, Prutok_Brejl[[1]]$Complete_value)

# Createing data table for all data
Prutok_Brejl_dt <- Prutok_Brejl[[1]]
for (i in 2:length(Prutok_Brejl)){
  Prutok_Brejl_dt <- rbind(Prutok_Brejl_dt, Prutok_Brejl[[i]])
}

# saveRDS(Prutok_Brejl_dt, file = './output/Prutok_Brejl.rds')



################################################################################
# Temperature

FileNames <- list.files(paste0('./data/', Folders[4])) 
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

# Createing data table for all data
Temperature_dt <- Temperature[[1]]
for (i in 2:length(Temperature)){
  Temperature_dt <- rbind(Temperature_dt, Temperature[[i]])
}

# saveRDS(Temperature_dt, file = './output/BP_Temperature.rds')


################################################################################

# Water_Temperature
FileNames <- list.files(paste0('./data/', Folders[5])) 
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

ggplot_na_imputations(Water_Temperature[[1]]$value, Water_Temperature[[1]]$Complete_value)

# Createing data table for all data
Water_Temperature_dt <- Water_Temperature[[1]]
for (i in 2:length(Water_Temperature)){
  Water_Temperature_dt <- rbind(Water_Temperature_dt, Water_Temperature[[i]])
}

# saveRDS(Water_Temperature_dt, file = './output/BP_Water_Temperature.rds')



Hladina_Brejl <- readRDS('./output/Hladina_Brejl.rds')
Hladina_Brejl_Ofest <- readRDS('./output/Hladina_Brejl_Ofest.rds')
Prutok_Brejl <- readRDS('./output/Prutok_Brejl.rds')
BP_Temperature <- readRDS('./output/BP_Temperature.rds')
BP_Water_Temperature <- readRDS('./output/BP_Water_Temperature.rds')


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