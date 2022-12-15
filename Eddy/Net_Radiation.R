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



# 30 minute data
Eddy_data <- readRDS('./output/Eddy_30_min.rds') |> 
  as.data.table()


# selcting the required columns 
Eddy_subset <- Eddy_data[ , .(date, 
                              DateTime, 
                              P_RAIN_1_1_1,       # [m]
                              air_temperature,    # [°k]
                              ET,                 # Evapotranspiration flux [mm/hour]
                              RH,                 # Ambient relative humidity [%]
                              wind_speed,         # [m/s] 
                              LWIN_1_1_1,         # Long wave radiation (in) [w/m^2]
                              LWOUT_1_1_1,        #           "        (out) [w/m^2]
                              SWIN_1_1_1,         # Short wave radiation (in) [w/m^2]
                              SWOUT_1_1_1,        #           "         (out) [w/m^2]
                              RN_1_1_1,           # Net radiation [w/m^2]
                              LE,                 # Corrected latent heat flux [w/m^2]
                              H,                  # Corrected sensible heat flux [w/m^2]
                              SHF_1_1_1,          # Soil Heat Flux [w/m^2]
                              SHF_2_1_1,          
                              SHF_3_1_1,          
                              TCNR4_C_1_1_1,      # CNR4 or NR01 Net Radiometer Temperature  [°C]
                              TS_1_1_1,           # Soil Temperature
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
  TS_3_1_1 = ifelse(TS_1_1_1 > 320, NA, TS_1_1_1)
  # H = ifelse(H < -1000, NA, H),
  # RN_1_1_1 = ifelse(RN_1_1_1 < 0, NA, RN_1_1_1),
  # LE = ifelse(LE < 0, NA, LE),
  # H = ifelse(H < 0, NA, H),
  # SHF_1_1_1 = ifelse(SHF_1_1_1 < 0, NA, SHF_1_1_1),
  # SHF_2_1_1 = ifelse(SHF_2_1_1 < 0, NA, SHF_2_1_1),
  # SHF_3_1_1 = ifelse(SHF_3_1_1 < 0, NA, SHF_3_1_1)
)]

# changing the radiation units from w/m^2 to MJ/m^2 day
Eddy_subset[, `:=`(ET = ET * 0.5,                          # [mm/half-hour]
                   LWIN_1_1_1 = LWIN_1_1_1 * 0.08640,     # [MJ/m2 day] 86400*1e-6
                   LWOUT_1_1_1 = LWOUT_1_1_1 * 0.08640,   
                   SWIN_1_1_1 = SWIN_1_1_1 * 0.08640,
                   SWOUT_1_1_1 = SWOUT_1_1_1 * 0.08640,
           RN_1_1_1 = RN_1_1_1 * 0.08640,
           LE = LE * 0.08640,
           H = H * 0.08640,
           SHF_1_1_1 = SHF_1_1_1 * 0.08640,
           SHF_2_1_1 = SHF_1_1_1 * 0.08640,
           SHF_3_1_1 = SHF_1_1_1 * 0.08640,
           P_RAIN_1_1_1 = P_RAIN_1_1_1 * 1000, # [mm]
           TS_1_1_1 = TS_1_1_1 - 273.15,
           TS_2_1_1 = TS_2_1_1 - 273.15,
           TS_3_1_1 = TS_3_1_1 - 273.15
           )]



# visualization

#
ggplot(Eddy_subset) + 
  geom_histogram(aes(ET), binwidth = 0.005)

# Chacking gaps in data
# ggplot_na_distribution(Eddy_subset$LE)
# ggplot_na_distribution(Eddy_subset$H)
# ggplot_na_distribution(Eddy_subset$RH)

ggplot(Eddy_subset, aes(x = DateTime, y = P_RAIN_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = air_temperature)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = ET)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = RH)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = wind_speed)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = LWIN_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = LWOUT_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = SWIN_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = SWOUT_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = RN_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = LE)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = H)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = SHF_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = SHF_2_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = SHF_3_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = TCNR4_C_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = TS_1_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = TS_2_1_1)) + 
  geom_line()

ggplot(Eddy_subset, aes(x = DateTime, y = TS_3_1_1)) + 
  geom_line()


# saveRDS(ET_30min, './output/ET_30min.rds')
# Daily data

Eddy_subset_Daily <- 
  Eddy_subset[,
              .(
                Prec = sum(P_RAIN_1_1_1, na.rm = TRUE),
                ET = sum(ET, na.rm = TRUE),
                Relative_Humidity = mean(RH, na.rm = TRUE),
                Wind_Speed = mean(wind_speed, na.rm = TRUE),
                Lwave_in = mean(LWIN_1_1_1, na.rm = TRUE),
                Lwave_out = mean(LWOUT_1_1_1, na.rm = TRUE),
                Swave_in = mean(SWIN_1_1_1, na.rm = TRUE),
                Swave_out = mean(SWOUT_1_1_1, na.rm = TRUE),
                Net_Radiation = mean(RN_1_1_1, na.rm = TRUE),
                Latent_Heat_Flux = mean(LE, na.rm = TRUE),
                sensible_Heat_Flux = mean(H, na.rm = TRUE),
                Soil_Heat_Flux_1 = mean(SHF_1_1_1, na.rm = TRUE),
                Soil_Heat_Flux_2 = mean(SHF_2_1_1, na.rm = TRUE),
                Soil_Heat_Flux_3 = mean(SHF_3_1_1, na.rm = TRUE),
                Net_Radiometer_Temp = mean(TCNR4_C_1_1_1, na.rm = TRUE),
                Soil_Temp_1 = mean(TS_1_1_1, na.rm = TRUE),
                Soil_Temp_2 = mean(TS_2_1_1, na.rm = TRUE),
                Soil_Temp_3 = mean(TS_3_1_1, na.rm = TRUE)
              ),
              by = date]



ggplot(Eddy_subset_Daily, aes(x = date, y = Prec)) + 
  geom_line()

ggplot(Eddy_subset_Daily, aes(x = date, y = ET)) + 
  geom_line()

ggplot(Eddy_subset_Daily, aes(x = date, y = Latent_Heat_Flux)) + 
  geom_line()




ggplot(Eddy_subset_Daily, aes(x = date, y = Latent_Heat_Flux)) +
  geom_line(aes(y = ET, color = "Measured"), 
            size = 0.7) +
  geom_line(aes(y = Latent_Heat_Flux * 0.408, color = "LatentHeat * 0.408"),
            size = 0.7, 
            linetype = "dashed") +  #
  scale_color_manual(values = c("Measured" = "black", "LatentHeat * 0.408" = "red")) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "date", 
       y = "ET [mm]")



ggplot(Eddy_subset_Daily,
       aes(x = ET, y = Latent_Heat_Flux * 0.408)) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    size = 0.8,
    color  = "red"
  ) +
  labs(x = "Measured ET [mm]",
       y = "LatentHeat * 0.408 [mm]") +
  xlim(c(0, 6)) +
  ylim(c(0, 6))

cor(Eddy_subset_Daily$ET,
    Eddy_subset_Daily$Latent_Heat_Flux,
    use =  "na.or.complete")
# ggsave("plot.png", p, width = 20, height = 20, units = "cm")


# Visualization of daily data
ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Relative_Humidity)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Wind_Speed)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Lwave_in)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Lwave_out)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Swave_in)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Swave_out)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Net_Radiation)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Latent_Heat_Flux)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = sensible_Heat_Flux)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Heat_Flux_1)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Heat_Flux_2)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Heat_Flux_3)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Net_Radiometer_Temp)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Temp_1)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Temp_2)) + 
  geom_line()

ggplot(Eddy_subset_Daily, 
       aes(x = date, y = Soil_Temp_3)) + 
  geom_line()

# saveRDS(Eddy_subset_Daily, './output/Eddy_subset_Daily.rds')

Eddy_subset_Daily <- readRDS('./output/Eddy_subset_Daily.rds')

test <- Eddy_subset_Daily[, 
                          .(date, Net_Radiation, 
                              WaveIn_WaveOut = Lwave_in + Swave_in - Swave_out - Lwave_out, 
                              flux = Soil_Heat_Flux_1 + Latent_Heat_Flux + sensible_Heat_Flux)
                          ] # + Soil_Heat_Flux_2 + Soil_Heat_Flux_3

ggplot(test, aes(x = date)) + 
  geom_line(aes(y = Net_Radiation, color = "Measured NetRad"), 
            size = 0.6) +
  geom_line(aes(y = WaveIn_WaveOut, color = "WaveIn - WaveOut"), 
            size = 0.6) +           # , linetype = "dashed"
  geom_line(aes(y = flux, color = "SoilHeat+LatentHeat+SensibleHeat"), 
            size = 0.5) +
  scale_color_manual(values = c("Measured NetRad" = "black", 
                                "WaveIn - WaveOut"="red" , 
                                "SoilHeat+LatentHeat+SensibleHeat" = "blue" )) +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) + 
  labs(x = "date [day]", y = "NetRad [MJ m^-2 day^-1]")


ggplot(Eddy_subset_Daily,
       aes(x = Net_Radiation, y = Lwave_in - Lwave_out + Swave_in - Swave_out)) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    size = 0.8,
    color  = "red"
  ) +
  labs(x = "NetRad", y = "WaveIn - WaveOut")






