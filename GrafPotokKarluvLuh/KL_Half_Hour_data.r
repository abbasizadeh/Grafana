library(data.table)
library(ggplot2)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
getwd()


Folders <- list.files('./data') 

read_csv_dir <- function(File, Folder){
  FilePath <- paste0(dirname(getActiveDocumentContext()$path), '/data')
  FileDir <- glue::glue('{FilePath}/{Folder}/{File}')
  return(FileDir)
}

# function for decomposing Time column into date and time columns
# Time column format: 2022-05-10 12:51:22 and 10/05/2022
date_time_decom = function(x){
  if(grepl("/", x$Time[1])){
    x$date <- as.Date(x$Time, format = "%m/%d/%y")
    x$Time <- format(as.POSIXct(x$Time, tryFormats = "%m/%d/%Y %H:%M"), 
                     format = '%H:%M')
  }else{
    x$date <- as.Date(x$Time)
    x$Time <- format(as.POSIXct(x$Time), format = '%H:%M')
  }
  
  return(x)}

################################################################################
# Hladina Karlův Luh

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[1])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[1])
}

# reading csv files and storing them into a list (GW_TSs) 
Hladina_Karluv_Luh <- lapply(FilePath, read.csv)
Hladina_Karluv_Luh <- lapply(Hladina_Karluv_Luh, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 25)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Hladina_Karluv_Luh[[i]] <- date_time_decom(Hladina_Karluv_Luh[[i]])
  Hladina_Karluv_Luh[[i]]$ID <- rep(Name[i], length(Hladina_Karluv_Luh[[i]]$Time))
  # Hladina_Karluv_Luh[[i]]$day <- lubridate::day(Hladina_Karluv_Luh[[i]]$date)
  # Hladina_Karluv_Luh[[i]]$month <- lubridate::month(Hladina_Karluv_Luh[[i]]$date)
  # Hladina_Karluv_Luh[[i]]$year <- lubridate::year(Hladina_Karluv_Luh[[i]]$date)
  
  names(Hladina_Karluv_Luh[[i]]) <- c('Time','value', 'date', 'ID')
  Hladina_Karluv_Luh[[i]] <- Hladina_Karluv_Luh[[i]][,c('date','Time', 'ID','value')]
    }

# checking the columns' configuration
Hladina_Karluv_Luh[[1]]
Hladina_Karluv_Luh[[3]]

# Createing data table for all data
Hladina_Karluv_Luh_dt <- Hladina_Karluv_Luh[[1]]
for (i in 2:length(Hladina_Karluv_Luh)){
  Hladina_Karluv_Luh_dt <- rbind(Hladina_Karluv_Luh_dt, Hladina_Karluv_Luh[[i]])
}

saveRDS(Hladina_Karluv_Luh_dt, file = './output/Hladina_Karluv_Luh.rds')
################################################################################
# Hladina_Karluv_Luh_ofest 

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[2])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[2])
}

# reading csv files and storing them into a list (GW_TSs) 
Hladina_Karluv_Luh_ofest <- lapply(FilePath, read.csv)
Hladina_Karluv_Luh_ofest <- lapply(Hladina_Karluv_Luh_ofest, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 31)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Hladina_Karluv_Luh_ofest[[i]] <- date_time_decom(Hladina_Karluv_Luh_ofest[[i]])
  Hladina_Karluv_Luh_ofest[[i]]$ID <- rep(Name[i], length(Hladina_Karluv_Luh_ofest[[i]]$Time))
  names(Hladina_Karluv_Luh_ofest[[i]]) <- c('Time','value', 'date', 'ID')
  Hladina_Karluv_Luh_ofest[[i]] <- Hladina_Karluv_Luh_ofest[[i]][,c('date','Time', 'ID','value')]
}

# checking the columns' configuration
Hladina_Karluv_Luh_ofest[[1]]
Hladina_Karluv_Luh_ofest[[3]]

# Createing data table for all data
Hladina_Karluv_Luh_ofest_dt <- Hladina_Karluv_Luh_ofest[[1]]
for (i in 2:length(Hladina_Karluv_Luh_ofest)){
  Hladina_Karluv_Luh_ofest_dt <- rbind(Hladina_Karluv_Luh_ofest_dt, Hladina_Karluv_Luh_ofest[[i]])
}

saveRDS(Hladina_Karluv_Luh_ofest_dt, file = './output/Hladina_Karluv_Luh_ofest.rds')

################################################################################
# Prutok_Karluv_Luh
# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[3])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[3])
}

# reading csv files and storing them into a list (GW_TSs) 
Prutok_Karluv_Luh <- lapply(FilePath, read.csv)
Prutok_Karluv_Luh <- lapply(Prutok_Karluv_Luh, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 24)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Prutok_Karluv_Luh[[i]] <- date_time_decom(Prutok_Karluv_Luh[[i]])
  Prutok_Karluv_Luh[[i]]$ID <- rep(Name[i], length(Prutok_Karluv_Luh[[i]]$Time))
  
  names(Prutok_Karluv_Luh[[i]]) <- c('Time','value', 'date', 'ID')
  Prutok_Karluv_Luh[[i]] <- Prutok_Karluv_Luh[[i]][,c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Prutok_Karluv_Luh[[1]]
Prutok_Karluv_Luh[[3]]

# Createing data table for all data
Prutok_Karluv_Luh_dt <- Prutok_Karluv_Luh[[1]]
for (i in 2:length(Prutok_Karluv_Luh)){
  Prutok_Karluv_Luh_dt <- rbind(Prutok_Karluv_Luh_dt, Prutok_Karluv_Luh[[i]])
}

saveRDS(Prutok_Karluv_Luh_dt, file = './output/Prutok_Karluv_Luh.rds')

################################################################################
# Temperature

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[4])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[4])
}

# reading csv files and storing them into a list (GW_TSs) 
Temperature <- lapply(FilePath, read.csv)
Temperature <- lapply(Temperature, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 18)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Temperature[[i]] <- date_time_decom(Temperature[[i]])
  Temperature[[i]]$ID <- rep(Name[i], length(Temperature[[i]]$Time))
  
  names(Temperature[[i]]) <- c('Time','value', 'date', 'ID')
  Temperature[[i]] <- Temperature[[i]][,c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Temperature[[1]]
Temperature[[3]]

# Createing data table for all data
Temperature_dt <- Temperature[[1]]
for (i in 2:length(Temperature)){
  Temperature_dt <- rbind(Temperature_dt, Temperature[[i]])
}

saveRDS(Temperature_dt, file = './output/KL_Temperature.rds')
################################################################################

# Water_Temperature

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[5])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[5])
}

# reading csv files and storing them into a list (GW_TSs) 
Water_Temperature <- lapply(FilePath, read.csv)
Water_Temperature <- lapply(Water_Temperature, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 24)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Water_Temperature[[i]] <- date_time_decom(Water_Temperature[[i]])
  Water_Temperature[[i]]$ID <- rep(Name[i], length(Water_Temperature[[i]]$Time))
  names(Water_Temperature[[i]]) <- c('Time','value', 'date', 'ID')
  Water_Temperature[[i]] <-  Water_Temperature[[i]][,c('date','Time', 'ID','value')]
}

# checking the columns' configuration
Water_Temperature[[1]]
Water_Temperature[[3]]

# Createing data table for all data
Water_Temperature_dt <- Water_Temperature[[1]]
for (i in 2:length(Water_Temperature)){
  Water_Temperature_dt <- rbind(Water_Temperature_dt, Water_Temperature[[i]])
}


saveRDS(Water_Temperature_dt, file = './output/KL_Water_Temperature.rds')

################################################################################




# calculating mean value of sub-daily time scale
# GW_TSs_daily <- list()
# for (i in 1:length(GW_TSs)) {
#   GW_TSs_daily[[i]] <- GW_TSs[[i]][, mean(value), by = .(day, month, year, ID)]
#   GW_TSs_daily[[i]]$date <- as.Date(with(GW_TSs_daily[[i]], paste(year, month, day, sep="-")), "%Y-%m-%d")
#   GW_TSs_daily[[i]]$day <- NULL
#   GW_TSs_daily[[i]]$month <- NULL
#   GW_TSs_daily[[i]]$year <- NULL
#   names(GW_TSs_daily[[i]]) <- c('ID', 'value', 'date')
#  }


# plotting 
dta <- data.frame(Hladina_Brejl[[1]])
for (i in 2:5){
  dta <- rbind(dta ,data.frame(Hladina_Brejl[[i]])) 
}

ggplot(data = dta) + geom_line(aes(x = date, y = value)) + facet_wrap(~ID, ncol = 5) 

ggplot(data = dta) + geom_line(aes(x = date, y = value, color = ID)) + 
  theme(legend.position="none")


GW_TS <- data.table(Hladina_Brejl[[5]])
p <- ggplot() + geom_line(data = GW_TS, aes(x = date, y = value)) +
  ggtitle(paste0('Sensor ', as.character(5)))
# ggsave(filename = paste0(j, '.png'), plot = p, path = "C:/Users/Hossein/OneDrive/Desktop/R/Sensors/New sensors/",
#        width = 30, height = 15, units = 'cm')


# Files <- gsub('-2022-08-28 ', '-', Files)