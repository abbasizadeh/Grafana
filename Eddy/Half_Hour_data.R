library(rstudioapi)
library(data.table)
library(ggplot2)
library(chron)
library(forecastML)
library(imputeTS)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

# EC data has two lines of headers
# name <- scan("./data/summary_report_2022-01-01_2022-10-04.txt", nlines = 1, what = character())
name <- scan("./data/summary_report_2020-01-01_2022-11-06.txt", nlines = 1, what = character())

# summary_data <- read.delim("./data/summary_report_2022-01-01_2022-10-04.txt")
# dta <- read.table("./data/summary_report_2022-01-01_2022-10-04.txt", skip = 2, header = FALSE)
dta <- read.table("./data/summary_report_2020-01-01_2022-11-06.txt", skip = 2, header = FALSE)
# dta1 <- read.delim("./data/summary_report_2022-01-01_2022-10-04.txt")

names(dta) <- name
dta = as.data.table(dta)



# ggplot(data = dta) + geom_line(aes(x = date, y = h2o_flux))


# creating date time column to identify the gaps
# dta$DateTime <- chron(dates. = dta$date, times. = dta$time, format = c(dates = "Y-m-d", times = "h:m:s"))
dta$DateTime <- 
  as.POSIXct(paste(dta$date, dta$time), format="%Y-%m-%d %H:%M:%S", tz = "GMT")

which(is.na(dta$DateTime))
names(dta)
# dta$DateTime <- as.Date(dta$DateTime)

dta[,225]
class(dta$DateTime)

dta2 <- 
  fill_gaps(dta, date_col = 225, frequency = "30 min")
which(is.na(dta2$DateTime))
which(is.na(dta2$ET))

saveRDS(dta, file = './output/Eddy_30_min.rds')

Eddy_data <- readRDS('./output/Eddy_30_min.rds')


# ggplot_na_distribution(Eddy_data$h2o_flux)
ggplot_na_distribution(Eddy_data$ET[2000:6000])
ggplot_na_distribution(Eddy_data$LE)
ggplot_na_distribution(Eddy_data$LE_scf)

# t <- dta$DateTime[2] - dta$DateTime[1]
# ET data 
ET_30min <- Eddy_data[ ,.(date, DateTime, ET)][, ET := ET * 0.5] 
ET_Daily <- ET_30min[, sum(ET), by = date]

names(ET_30min) <- c("date", "DateTime", "ET [mm]")
names(ET_Daily) <- c("date", "ET [mm]")  #[mm+1hour-1]

saveRDS(ET_30min, './output/ET_30min.rds')
saveRDS(ET_Daily, './output/ET_Daily.rds')

ET_30min <- readRDS('./output/ET_30min.rds')
ET_Daily <- readRDS('./output/ET_Daily.rds')


