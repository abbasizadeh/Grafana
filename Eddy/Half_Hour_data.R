library(rstudioapi)
library(data.table)
library(ggplot2)
library(chron)
library(forecastML)
library(imputeTS)

setwd(dirname(getActiveDocumentContext()$path))
getwd()

name <- scan("./data/summary_report_2022-01-01_2022-10-04.txt", nlines = 1, what = character())
# summary_data <- read.delim("./data/summary_report_2022-01-01_2022-10-04.txt")
dta <- read.table("./data/summary_report_2022-01-01_2022-10-04.txt", skip = 2, header = FALSE)

names(dta) <- name
dta = as.data.table(dta)

# ggplot(data = dta) + geom_line(aes(x = date, y = h2o_flux))



# creating date time column to identify the gaps
# dta$DateTime <- chron(dates. = dta$date, times. = dta$time, format = c(dates = "Y-m-d", times = "h:m:s"))
dta$DateTime <- as.POSIXct(paste(dta$date, dta$time), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
which(is.na(dta$DateTime))
names(dta)
# dta$DateTime <- as.Date(dta$DateTime)

dta[,224]
class(dta$DateTime)

dta2 <- fill_gaps(dta, date_col = 224, frequency = "30 min")
which(is.na(dta2$DateTime))
saveRDS(dta, file = './output/Eddy_30_min.rds')

Eddy_data <- readRDS('./output/Eddy_30_min.rds')

ggplot_na_distribution(Eddy_data$h2o_flux)
ggplot_na_distribution(Eddy_data$ET)

# t <- dta$DateTime[2] - dta$DateTime[1]


