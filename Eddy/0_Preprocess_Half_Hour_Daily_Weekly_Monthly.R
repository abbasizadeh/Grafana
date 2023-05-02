library(rstudioapi)
library(data.table)
library(ggplot2)
library(chron)
library(forecastML)
library(imputeTS)
library(stringr)
library(zoo)

# setwd(dirname(getActiveDocumentContext()$path))
getwd()

file_name <- list.files('./Eddy/data')

# EC data has two lines of headers
# name <- scan("./data/summary_report_2022-01-01_2022-10-04.txt", nlines = 1, what = character())
# name <- scan("./data/summary_report_2020-01-01_2022-11-06.txt", nlines = 1, what = character())
name <- scan(paste0("./Eddy/data/", file_name[1]), nlines = 1, what = character())
tt <- fread(paste0("./Eddy/data/", file_name[1]))
# summary_data <- read.delim("./data/summary_report_2022-01-01_2022-10-04.txt")
# dta1 <- read.delim("./data/summary_report_2020-01-01_2022-11-14.txt")
# head(dta1$P_RAIN_1_1_1)
dta <- read.table(paste0("./Eddy/data/", file_name[1]), skip = 2, header = FALSE) %>%
  as.data.table()

names(dta) <- name

dta$DateTime <- 
  as.POSIXct(paste(dta$date, dta$time), format="%Y-%m-%d %H:%M:%S", tz = "GMT")

dta <- as.data.table(dta)

which(is.na(dta$DateTime))
names(dta)
which(is.na(dta$ET))

dta[,225]
class(dta$DateTime)

dta <- fill_gaps(dta, date_col = 225, frequency = "30 min")

which(is.na(dta$ET))
which(is.na(dta$DateTime))

# filling the gaps in the date column
dta <- as.data.table(dta)
dta[, date := lubridate::date(dta$DateTime)]
which(is.na(dta$date))

# ggplot_na_distribution(dta$ET)

# remove the first part of data where there are lots of spikes
subset_dta <- dta[min(which(dta$date == as.Date("2020-07-01"))):length(dta$date)]
# ggplot_na_distribution(subset_dta$ET)


# saveRDS(subset_dta, file = './output/Eddy_30_min.rds')
Eddy_data <- readRDS('./Eddy/output/Eddy_30_min.rds') |> as.data.table()
ggplot(Eddy_data, aes(x = DateTime, y = ET)) + geom_line()
###############################################################################

# Cheking the missing values
# ggplot_na_distribution(Eddy_data$h2o_flux)
ggplot_na_distribution(Eddy_data$ET)


# ET data 
# 30 minute data
# Replacing the negative values with NA
ET_30min <- Eddy_data[ , .(date, DateTime, ET)][, ET := ET * 0.5][, ET:= ifelse(ET <0, NA, ET)]
# ggplot_na_distribution(ET_30min$ET)
ggplot(ET_30min, aes(x = DateTime, y = ET)) + geom_line()



# Daily data
# Pre_Daily <- ET_30min[, ET:= ifelse(is.na(ET), 0, ET)]
ET_Daily <- ET_30min[, sum(ET, na.rm = TRUE), by = date]
names(ET_Daily) <- c("date", "ET")
# ggplot_na_distribution(ET_Daily$ET)
ggplot(ET_Daily, aes(x = date, y = ET)) + geom_line() +
  labs(y = 'ET [mm]')

# Weekly data
Pre_weelly <- ET_Daily[, Week := week(date)]
ET_Weekly <- Pre_weelly[, sum(ET, na.rm = TRUE), by = .(year(date), month(date), Week)]
names(ET_Weekly) <- c("Year", "Month", "Week", "ET")

ET_Weekly <-  ET_Weekly |>
                  mutate(beginning = lubridate::ymd(str_c(Year, "-01-01")),
                         final_date = beginning + lubridate::weeks(Week))

ET_Weekly[,c("beginning", "Year", "Month")] <- NULL
ET_Weekly <- ET_Weekly[,c("final_date", "Week", "ET")]
names(ET_Weekly) <- c("date", "Week", "ET")
ET_Weekly |> ggplot() +  geom_line(aes(x = date, y = ET))


# Monthly data
Pre_monthly <- ET_Daily[, Month := lubridate::month(date)]
ET_Month <- Pre_monthly[, sum(ET, na.rm = TRUE), by = .(Month,year(date))]
ET_Month$date <- as.yearmon(paste(ET_Month$year, ET_Month$Month), format =  "%Y %m")
names(ET_Month) <- c("Month", "year", "ET", "date")
ET_Month[,c("Month", "year")] <- NULL
ET_Month <- ET_Month[,c("date", "ET")]
ggplot(ET_Month) + geom_line(aes(x = date, y = ET))

# names(ET_30min) <- c("date", "DateTime", "ET")
names(ET_Month) <- c("date", "ET")  #[mm+1hour-1]

# # Saving
# ET_30min[,"date"] <- NULL
# saveRDS(ET_30min, './output/ET_30min.rds')
# 
# ET_Daily[, c("Week", "Month")] <- NULL
# saveRDS(ET_Daily, './output/ET_Daily.rds')
# 
# saveRDS(ET_Weekly, './output/ET_Weekly.rds')
# 
# saveRDS(ET_Month, './output/ET_Monthly.rds')


# test 
ET_30min <- readRDS('./Eddy/output/ET_30min.rds')
ggplot(ET_30min, aes(x = DateTime, y = ET)) + 
  geom_line() +
  labs(y = 'ET 30 min [mm]')

ET_Daily <- readRDS('./Eddy/output/ET_Daily.rds')
ggplot(ET_Daily, aes(x = date, y = ET)) + 
  geom_line()+
  labs(y = 'ET daily [mm]')

ET_Weekly <- readRDS('./Eddy/output/ET_Weekly.rds')
ET_Weekly |> ggplot() +  geom_line(aes(x = date, y = ET)) +
  labs(y = 'ET weekly [mm]')

ET_Monthly <- readRDS('./Eddy/output/ET_Monthly.rds')
ggplot(ET_Monthly) + geom_line(aes(x = date, y = ET)) +
  labs(y = 'ET monthly [mm]')








