library(rstudioapi)
library(data.table)
library(ggplot2)
library(chron)
library(forecastML)
library(imputeTS)
library(stringr)
library(zoo)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

# EC data has two lines of headers
# name <- scan("./data/summary_report_2022-01-01_2022-10-04.txt", nlines = 1, what = character())
# name <- scan("./data/summary_report_2020-01-01_2022-11-06.txt", nlines = 1, what = character())
name <- scan("./data/summary_report_2020-01-01_2022-11-14.txt", nlines = 1, what = character())

# summary_data <- read.delim("./data/summary_report_2022-01-01_2022-10-04.txt")
# dta <- read.table("./data/summary_report_2022-01-01_2022-10-04.txt", skip = 2, header = FALSE)
dta1 <- read.delim("./data/summary_report_2020-01-01_2022-11-14.txt")

dta <- read.table("./data/summary_report_2020-01-01_2022-11-06.txt", skip = 2, header = FALSE) |>
  as.data.table()

names(dta) <- name



# ggplot(data = dta) + geom_line(aes(x = date, y = h2o_flux))

# creating date time column to identify the gaps
# dta$DateTime <- chron(dates. = dta$date, times. = dta$time, format = c(dates = "Y-m-d", times = "h:m:s"))
dta$DateTime <- 
  as.POSIXct(paste(dta$date, dta$time), format="%Y-%m-%d %H:%M:%S", tz = "GMT")

dta <- as.data.table(dta)

which(is.na(dta$DateTime))
names(dta)
which(is.na(dta$ET))
# dta$DateTime <- as.Date(dta$DateTime)

dta[,225]
class(dta$DateTime)

dta <- 
  fill_gaps(dta, date_col = 225, frequency = "30 min")

which(is.na(dta$ET))
which(is.na(dta$DateTime))

# filling the gaps in the date column
dta <- as.data.table(dta)
dta[, date := lubridate::date(dta$DateTime)]
which(is.na(dta$date))

ggplot_na_distribution(dta$ET)

# remove the first part of data where there are lots of spikes
subset_dta <- dta[min(which(dta$date == as.Date("2020-07-01"))):length(dta$date)]
ggplot_na_distribution(subset_dta$ET)


# saveRDS(subset_dta, file = './output/Eddy_30_min.rds')
Eddy_data <- readRDS('./output/Eddy_30_min.rds') |> as.data.table()
ggplot(Eddy_data, aes(x = DateTime, y = ET)) + geom_line()


# Cheking the missing values
# ggplot_na_distribution(Eddy_data$h2o_flux)
ggplot_na_distribution(Eddy_data$ET)



# ET data 

# 30 minute data
# Replacing the negative values with NA
ET_30min <- Eddy_data[ , .(date, DateTime, ET)][, ET := ET * 0.5][, ET:= ifelse(ET <0, NA, ET)]
ggplot_na_distribution(ET_30min$ET)
ggplot(ET_30min, aes(x = DateTime, y = ET)) + geom_line()
ET_30min[,"date"] <- NULL
# saveRDS(ET_30min, './output/ET_30min.rds')


# Daily data
# Pre_Daily <- ET_30min[, ET:= ifelse(is.na(ET), 0, ET)]
ET_Daily <- ET_30min[, sum(ET, na.rm = TRUE), by = date]
names(ET_Daily) <- c("date", "ET")
ggplot_na_distribution(ET_Daily$ET)
ggplot(ET_Daily, aes(x = date, y = ET)) + geom_line()
# saveRDS(ET_Daily, './output/ET_Daily.rds')

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
# saveRDS(ET_Weekly, './output/ET_Weekly.rds')


# Monthly data
Pre_monthly <- ET_Daily[, Month := lubridate::month(date)]
ET_Month <- Pre_monthly[, sum(ET, na.rm = TRUE), by = .(Month,year(date))]
ET_Month$date <- as.yearmon(paste(ET_Month$year, ET_Month$Month), format =  "%Y %m")
names(ET_Month) <- c("Month", "year", "ET", "date")
ET_Month[,c("Month", "year")] <- NULL
ET_Month <- ET_Month[,c("date", "ET")]
ggplot(ET_Month) + geom_line(aes(x = date, y = ET))
# saveRDS(ET_Month, './output/ET_Monthly.rds')


names(ET_30min) <- c("date", "DateTime", "ET")
names(ET_Daily) <- c("date", "ET")  #[mm+1hour-1]



# test 
ET_30min <- readRDS('./output/ET_30min.rds')
ggplot(ET_30min, aes(x = DateTime, y = ET)) + geom_line()

ET_Daily <- readRDS('./output/ET_Daily.rds')
ggplot(ET_Daily, aes(x = date, y = ET)) + geom_line()

ET_Weekly <- readRDS('./output/ET_Weekly.rds')
ET_Weekly |> ggplot() +  geom_line(aes(x = date, y = ET))

ET_Monthly <- readRDS('./output/ET_Monthly.rds')
ggplot(ET_Monthly) + geom_line(aes(x = date, y = ET))

################################################################################
# LE H RH(relative humidity) 

# 30 minute data
# Replacing the negative values with NA

Eddy_subset <- Eddy_data[ , .(date, 
                       DateTime, 
                       LE, H, RH, 
                       wind_speed, 
                       LWIN_1_1_1,
                       LWOUT_1_1_1,
                       P_RAIN_1_1_1,
                       TCNR4_C_1_1_1,
                       SWIN_1_1_1,
                       SWOUT_1_1_1, 
                       TS_1_1_1, 
                       TS_2_1_1, 
                       TS_3_1_1)][, `:=` (LE = ifelse(LE <0, NA, LE) ,
                                          H = ifelse(H <0, NA, H),
                                          RH = ifelse(RH <0, NA, RH),
                                          LWIN_1_1_1 = ifelse(LWIN_1_1_1 <0, NA, LWIN_1_1_1),
                                          LWOUT_1_1_1 = ifelse(LWOUT_1_1_1 <0, NA, LWOUT_1_1_1),
                                          P_RAIN_1_1_1 = ifelse(P_RAIN_1_1_1 <0, NA, P_RAIN_1_1_1),
                                          TCNR4_C_1_1_1 = ifelse(TCNR4_C_1_1_1 <0, NA, TCNR4_C_1_1_1),
                                          SWIN_1_1_1 = ifelse(SWIN_1_1_1 <0, NA, SWIN_1_1_1),
                                          SWOUT_1_1_1 = ifelse(SWOUT_1_1_1 <0, NA, SWOUT_1_1_1),
                                          TS_1_1_1 = ifelse(TS_1_1_1 <0, NA, TS_1_1_1),
                                          TS_2_1_1 = ifelse(TS_2_1_1 <0, NA, TS_2_1_1),
                                          TS_3_1_1 = ifelse(TS_3_1_1 <0, NA, TS_3_1_1)
                                          )]


Eddy_subset[, P_RAIN_1_1_1 := ifelse(P_RAIN_1_1_1 > 1, NA, P_RAIN_1_1_1)]

# [, ET := ET * 0.5]
# [, ET := ET * 0.5]
# [, ET := ET * 0.5]
ggplot_na_distribution(Eddy_subset$LE)
ggplot_na_distribution(Eddy_subset$H)
ggplot_na_distribution(Eddy_subset$RH)
ggplot(Eddy_subset, aes(x = DateTime, y = LE)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = H)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = RH)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = P_RAIN_1_1_1)) + geom_line()

# saveRDS(ET_30min, './output/ET_30min.rds')
# Daily data
# Pre_Daily <- ET_30min[, ET:= ifelse(is.na(ET), 0, ET)]
Eddy_subset_Daily <- Eddy_subset[, .(sum(LE, na.rm = TRUE), 
                       sum(H, na.rm = TRUE),
                       mean(RH, na.rm = TRUE),
                       mean(wind_speed, na.rm = TRUE),
                       mean(LWIN_1_1_1, na.rm = TRUE),
                       mean(LWOUT_1_1_1, na.rm = TRUE),
                       sum(P_RAIN_1_1_1  * 1000, na.rm = TRUE),
                       mean(TCNR4_C_1_1_1, na.rm = TRUE),
                       mean(SWIN_1_1_1, na.rm = TRUE),
                       mean(SWOUT_1_1_1, na.rm = TRUE),
                       mean(TS_1_1_1, na.rm = TRUE),
                       mean(TS_2_1_1, na.rm = TRUE),
                       mean(TS_3_1_1, na.rm = TRUE)), by = date]


names(Eddy_subset_Daily) <- c("date", "LE", "H", "RH", "wind_speed", "LWIN_1_1_1",
                              "LWOUT_1_1_1", "P_RAIN_1_1_1", "TCNR4_C_1_1_1", 
                              "SWIN_1_1_1", "SWOUT_1_1_1", "TS_1_1_1", "TS_2_1_1", "TS_3_1_1")
Eddy_subset_Daily$ET <- ET_Daily$ET
ggplot_na_distribution(Eddy_subset_Daily$LE)
ggplot_na_distribution(Eddy_subset_Daily$H)
ggplot_na_distribution(Eddy_subset_Daily$RH)

ggplot(Eddy_subset_Daily, aes(x = date, y = LE)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = H)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = RH)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = wind_speed)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = LWIN_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = LWOUT_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = P_RAIN_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = TCNR4_C_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = SWIN_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = SWOUT_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = TS_1_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = TS_2_1_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = TS_3_1_1)) + geom_line()

saveRDS(Eddy_subset_Daily, './output/Eddy_subset_Daily.rds')



