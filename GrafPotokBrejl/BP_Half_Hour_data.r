library(data.table)
library(ggplot2)
library(rstudioapi)
library(forecastML)

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
# Hladina Brejl

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[1])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[1])
}

# reading csv files and storing them into a list (GW_TSs) 
Hladina_Brejl <- lapply(FilePath, read.csv)
Hladina_Brejl <- lapply(Hladina_Brejl, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 20)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Hladina_Brejl[[i]] <- date_time_decom(Hladina_Brejl[[i]])
  Hladina_Brejl[[i]]$ID <- rep(Name[i], length(Hladina_Brejl[[i]]$Time))
 
  names(Hladina_Brejl[[i]]) <- c('Time','value', 'date', 'ID')
  Hladina_Brejl[[i]] <- Hladina_Brejl[[i]][,c('date','Time', 'ID','value')]
    }

# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Brejl[[i]]$DateTime <- as.POSIXct(paste(Hladina_Brejl[[i]]$date, Hladina_Brejl[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Brejl[[i]][5:length(Hladina_Brejl[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Brejl[[i]] <- rbind(Hladina_Brejl[[i]][1:4], BP) 
  }

which(is.na(Hladina_Brejl[[4]]$date))
Hladina_Brejl[[4]][1:20]
head(Hladina_Brejl[[5]])

# Createing data table for all data
Hladina_Brejl_dt <- Hladina_Brejl[[1]]
for (i in 2:length(Hladina_Brejl)){
  Hladina_Brejl_dt <- rbind(Hladina_Brejl_dt, Hladina_Brejl[[i]])
}

saveRDS(Hladina_Brejl_dt, file = './output/Hladina_Brejl.rds')


################################################################################
# Hladina_Brejl_Ofest

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[2])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[2])
}

# reading csv files and storing them into a list (GW_TSs) 
Hladina_Brejl_Ofest <- lapply(FilePath, read.csv)
Hladina_Brejl_Ofest <- lapply(Hladina_Brejl_Ofest, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 26)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Hladina_Brejl_Ofest[[i]] <- date_time_decom(Hladina_Brejl_Ofest[[i]])
  Hladina_Brejl_Ofest[[i]]$ID <- rep(Name[i], length(Hladina_Brejl_Ofest[[i]]$Time))
  # Hladina_Brejl_Ofest[[i]]$day <- lubridate::day(Hladina_Brejl_Ofest[[i]]$date)
  # Hladina_Brejl_Ofest[[i]]$month <- lubridate::month(Hladina_Brejl_Ofest[[i]]$date)
  # Hladina_Brejl_Ofest[[i]]$year <- lubridate::year(Hladina_Brejl_Ofest[[i]]$date)
  names(Hladina_Brejl_Ofest[[i]]) <- c('Time','value', 'date', 'ID')
  Hladina_Brejl_Ofest[[i]] <- Hladina_Brejl_Ofest[[i]][,c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Hladina_Brejl_Ofest[[2]][1:20]

# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Hladina_Brejl_Ofest[[i]]$DateTime <- as.POSIXct(paste(Hladina_Brejl_Ofest[[i]]$date, Hladina_Brejl_Ofest[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Hladina_Brejl_Ofest[[i]][7:length(Hladina_Brejl_Ofest[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Hladina_Brejl_Ofest[[i]] <- rbind(Hladina_Brejl_Ofest[[i]][1:6], BP) 
}

which(is.na(Hladina_Brejl_Ofest[[4]]$date))
Hladina_Brejl_Ofest[[2]][1:20]

# Createing data table for all data
Hladina_Brejl_Ofest_dt <- Hladina_Brejl_Ofest[[1]]
for (i in 2:length(Hladina_Brejl_Ofest)){
  Hladina_Brejl_Ofest_dt <- rbind(Hladina_Brejl_Ofest_dt, Hladina_Brejl_Ofest[[i]])
}

saveRDS(Hladina_Brejl_Ofest_dt, file = './output/Hladina_Brejl_Ofest.rds')

################################################################################

# Prutok_Brejl

# The directory of the downloaded csv file
FileNames <- list.files(paste0('./data/', Folders[3])) 

FilePath <- list()
for (i in 1:length(FileNames)){
  FilePath[i] <- read_csv_dir(FileNames[i], Folders[3])
}

# reading csv files and storing them into a list (GW_TSs) 
Prutok_Brejl <- lapply(FilePath, read.csv)
Prutok_Brejl <- lapply(Prutok_Brejl, as.data.table)

# extracting the sensors' IDs
Name <- substring(FileNames, 19)
Name <- substr(Name, start =  1, stop =  nchar(Name)-4)

for(i in 1:length(FileNames)){
  Prutok_Brejl[[i]] <- date_time_decom(Prutok_Brejl[[i]])
  Prutok_Brejl[[i]]$ID <- rep(Name[i], length(Prutok_Brejl[[i]]$Time))
  
  names(Prutok_Brejl[[i]]) <- c('Time','value', 'date', 'ID')
  Prutok_Brejl[[i]] <- Prutok_Brejl[[i]][, c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Prutok_Brejl[[1]]
Prutok_Brejl[[4]]

# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Prutok_Brejl[[i]]$DateTime <- as.POSIXct(paste(Prutok_Brejl[[i]]$date, Prutok_Brejl[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Prutok_Brejl[[i]][7:length(Prutok_Brejl[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Prutok_Brejl[[i]] <- rbind(Prutok_Brejl[[i]][1:6], BP) 
}

which(is.na(Prutok_Brejl[[5]]$date))
Prutok_Brejl[[5]][1:20]

# Createing data table for all data
Prutok_Brejl_dt <- Prutok_Brejl[[1]]
for (i in 2:length(Prutok_Brejl)){
  Prutok_Brejl_dt <- rbind(Prutok_Brejl_dt, Prutok_Brejl[[i]])
}

saveRDS(Prutok_Brejl_dt, file = './output/Prutok_Brejl.rds')


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
  Water_Temperature[[i]] <- Water_Temperature[[i]][, c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Water_Temperature[[1]]
Water_Temperature[[4]]


# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Water_Temperature[[i]]$DateTime <- as.POSIXct(paste(Water_Temperature[[i]]$date, Water_Temperature[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Water_Temperature[[i]][7:length(Water_Temperature[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Water_Temperature[[i]] <- rbind(Water_Temperature[[i]][1:6], BP) 
}

which(is.na(Water_Temperature[[4]]$date))
Water_Temperature[[4]][1:20]

# Createing data table for all data
Water_Temperature_dt <- Water_Temperature[[1]]
for (i in 2:length(Water_Temperature)){
  Water_Temperature_dt <- rbind(Water_Temperature_dt, Water_Temperature[[i]])
}

saveRDS(Water_Temperature_dt, file = './output/Water_Temperature.rds')

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
  Temperature[[i]] <- Temperature[[i]][, c('date','Time', 'ID','value')]
}


# checking the columns' configuration
Temperature[[1]]
Temperature[[4]]

# Creating Half-Hour data
for(i in 1:length(FileNames)){
  # Creating Date + Time column
  Temperature[[i]]$DateTime <- as.POSIXct(paste(Temperature[[i]]$date, Temperature[[i]]$Time), format = "%Y-%m-%d %H:%M", tz = "GMT")
  # selecting rounded times
  BP <- Temperature[[i]][7:length(Temperature[[i]]$date)]
  # Fill the gaps with NA values
  BP <- fill_gaps(BP, date_col = 5, frequency = "30 min")
  Water_Temperature[[i]] <- rbind(Temperature[[i]][1:6], BP) 
}

which(is.na(Temperature[[4]]$date))
Temperature[[4]][1:20]

# Createing data table for all data
Temperature_dt <- Temperature[[1]]
for (i in 2:length(Temperature)){
  Temperature_dt <- rbind(Temperature_dt, Temperature[[i]])
}

saveRDS(Temperature_dt, file = './output/Temperature.rds')











