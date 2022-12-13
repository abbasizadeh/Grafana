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
# dta1 <- read.delim("./data/summary_report_2020-01-01_2022-11-14.txt")
# head(dta1$P_RAIN_1_1_1)
dta <- read.table("./data/summary_report_2020-01-01_2022-11-06.txt", skip = 2, header = FALSE) |>
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
Eddy_data <- readRDS('./output/Eddy_30_min.rds') |> as.data.table()
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
ggplot(ET_Daily, aes(x = date, y = ET)) + geom_line()

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
ET_30min <- readRDS('./output/ET_30min.rds')
ggplot(ET_30min, aes(x = DateTime, y = ET)) + geom_line()

ET_Daily <- readRDS('./output/ET_Daily.rds')
ggplot(ET_Daily, aes(x = date, y = ET)) + geom_line()

ET_Weekly <- readRDS('./output/ET_Weekly.rds')
ET_Weekly |> ggplot() +  geom_line(aes(x = date, y = ET))

ET_Monthly <- readRDS('./output/ET_Monthly.rds')
ggplot(ET_Monthly) + geom_line(aes(x = date, y = ET))

################################################################################

# 30 minute data
Eddy_data <- readRDS('./output/Eddy_30_min.rds') |> 
  as.data.table()
# Replacing the negative values with NA

Eddy_subset <- Eddy_data[ , .(date, 
                       DateTime, 
                       P_RAIN_1_1_1,
                       air_temperature,
                       ET,
                       RH,
                       wind_speed, 
                       LWIN_1_1_1,
                       LWOUT_1_1_1,
                       SWIN_1_1_1,
                       SWOUT_1_1_1, 
                       RN_1_1_1, 
                       LE, 
                       H,
                       SHF_1_1_1, 
                       SHF_2_1_1, 
                       SHF_3_1_1,
                       TCNR4_C_1_1_1,
                       TS_1_1_1, 
                       TS_2_1_1, 
                       TS_3_1_1)]


# Replace negative values by NA
Eddy_subset[, `:=` (
  P_RAIN_1_1_1 = ifelse(P_RAIN_1_1_1 > 1, NA, P_RAIN_1_1_1),
  ET = ifelse(ET < 0, NA, ET),
  RH = ifelse(RH < 0, NA, RH),
  wind_speed = ifelse(wind_speed < 0, NA, wind_speed),
  LWIN_1_1_1 = ifelse(LWIN_1_1_1 < 0, NA, LWIN_1_1_1),
  LWOUT_1_1_1 = ifelse(LWOUT_1_1_1 < 0, NA, LWOUT_1_1_1),
  SWIN_1_1_1 = ifelse(SWIN_1_1_1 < 0, NA, SWIN_1_1_1),
  SWOUT_1_1_1 = ifelse(SWOUT_1_1_1 < 0, NA, SWOUT_1_1_1),
  # RN_1_1_1 = ifelse(RN_1_1_1 < 0, NA, RN_1_1_1),
  LE = ifelse(LE < 0, NA, LE),
  H = ifelse(H < 0, NA, H),
  SHF_1_1_1 = ifelse(SHF_1_1_1 < 0, NA, SHF_1_1_1),
  SHF_2_1_1 = ifelse(SHF_2_1_1 < 0, NA, SHF_2_1_1),
  SHF_3_1_1 = ifelse(SHF_3_1_1 < 0, NA, SHF_3_1_1)
  )][, `:=` (TS_3_1_1 = ifelse(TS_1_1_1 > 320, NA, TS_1_1_1))]


# ggplot_na_distribution(Eddy_subset$LE)
# ggplot_na_distribution(Eddy_subset$H)
# ggplot_na_distribution(Eddy_subset$RH)
ggplot(Eddy_subset, aes(x = DateTime, y = P_RAIN_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = air_temperature)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = ET)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = RH)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = wind_speed)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = LWIN_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = LWOUT_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = SWIN_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = SWOUT_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = RN_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = LE)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = H)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = SHF_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = SHF_2_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = SHF_3_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = TCNR4_C_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = TS_1_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = TS_2_1_1)) + geom_line()
ggplot(Eddy_subset, aes(x = DateTime, y = TS_3_1_1)) + geom_line()


# saveRDS(ET_30min, './output/ET_30min.rds')
# Daily data
Eddy_subset_Daily <- Eddy_subset[, .(
  Prec = sum(P_RAIN_1_1_1, na.rm = TRUE),
  ET = sum(ET, na.rm = TRUE),
  Relative_Humidity = mean(RH, na.rm = TRUE),
  Wind_Speed = mean(wind_speed, na.rm = TRUE),
  Lwave_in = sum(LWIN_1_1_1, na.rm = TRUE),
  Lwave_out = sum(LWOUT_1_1_1, na.rm = TRUE),
  Swave_in = sum(SWIN_1_1_1, na.rm = TRUE),
  Swave_out = sum(SWOUT_1_1_1, na.rm = TRUE),
  Net_Radiation = sum(RN_1_1_1, na.rm = TRUE),
  Latent_Heat_Flux = sum(LE, na.rm = TRUE), 
  sensible_Heat_Flux = sum(H, na.rm = TRUE),
  Soil_Heat_Flux_1 = sum(SHF_1_1_1, na.rm = TRUE),
  Soil_Heat_Flux_2 = sum(SHF_2_1_1, na.rm = TRUE),
  Soil_Heat_Flux_3 = sum(SHF_3_1_1, na.rm = TRUE),
  Net_Radiometer_Temp = mean(TCNR4_C_1_1_1, na.rm = TRUE),
  Soil_Temp_1 = mean(TS_1_1_1, na.rm = TRUE),
  Soil_Temp_2 = mean(TS_2_1_1, na.rm = TRUE),
  Soil_Temp_3 = mean(TS_3_1_1, na.rm = TRUE)
  ), by = date][, `:=` (Prec = Prec * 1000,
                        Soil_Temp_1 = Soil_Temp_1 -273.15,
                        Soil_Temp_2 = Soil_Temp_2 -273.15,
                        Soil_Temp_3 = Soil_Temp_3 -273.15)]


ggplot(Eddy_subset_Daily, aes(x = date, y = Prec)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = ET)) + geom_line()


# divide the Le, which should be in an energy unit such as W/m-2 by the latent 
#heat of evaporation (i.e. the amount of energy required to evaporate 1g or 1ml of water) 
#which is 2257 J/g . For example, if you have a total LE of 500 W/m-2 for one hour 
#this would be 1800 000 J of energy (with watts equal to jouls per second). 
#Enough to evaporate 798 g of water per m-2 (1800000/2257). 
#This is equal to 0.798 mm of evaporation (1 kg H20 per m-2 = 1 mm)

ggplot(Eddy_subset_Daily, aes(x = date, y = Latent_Heat_Flux)) + 
  geom_line(aes(y = ET)) +
  geom_line(aes(y = Latent_Heat_Flux/2257), color = "red")  # 

ggplot(Eddy_subset_Daily, aes(x = date, y = Relative_Humidity)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Wind_Speed)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Lwave_in)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Lwave_out)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Swave_in)) + geom_line()
ggplot(Eddy_subset_Daily) + geom_line(aes(x = date, y = Swave_out))
ggplot(Eddy_subset_Daily, aes(x = date, y = Net_Radiation)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Latent_Heat_Flux)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = sensible_Heat_Flux)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Heat_Flux_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Heat_Flux_2)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Heat_Flux_3)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Net_Radiometer_Temp)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Temp_1)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Temp_2)) + geom_line()
ggplot(Eddy_subset_Daily, aes(x = date, y = Soil_Temp_3)) + geom_line()

# saveRDS(Eddy_subset_Daily, './output/Eddy_subset_Daily.rds')
Eddy_subset_Daily <- readRDS('./output/Eddy_subset_Daily.rds')

test <- Eddy_subset_Daily[, .(date, Net_Radiation, 
                              Lwave_in - Lwave_out + Swave_in - Swave_out, 
                              Soil_Heat_Flux_1 + Latent_Heat_Flux + sensible_Heat_Flux)]
ggplot(test, aes(x = date)) + 
  geom_line(aes(y = Net_Radiation)) +
  geom_line(aes(y = V3), color = "red", linetype = "dashed") +
  geom_line(aes(y = V4), color = "blue") 
  scale_color_manual(values = c("red"="Eddy" , "black" = "dHRUM" ))










