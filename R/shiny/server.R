######VERSION 5.1.0
library(shiny)
options(shiny.port = 3676) # in questo modo usiamo sempre questa porta per il sito
#library(shinyTime)
######
shinyServer(function(input, output, session) {
  
  # Dati in ingresso, inserire il path corretto della cartella che contiene
            # i file:
  boschSens <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_boschBME280_sensor_2019-09-26.txt", header = FALSE, skip = 0, fill = TRUE)
  CO2Sens <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_CO2ppm_sensor_2019-09-26.txt", header = FALSE, skip = 0, fill=TRUE)
  DHT_high <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_T_RH_gpio20_sensor_2019-09-26.txt", header = FALSE, skip=0, fill=TRUE)
  DHT_low <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_T_RH_gpio16_sensor_2019-09-26.txt", header = FALSE, skip=0, fill=TRUE)
  IR_T <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_T_IR_sensor_2019-09-26.txt", header = FALSE, skip = 0, fill=TRUE)
  lux <- read.table("/home/USER/Documents/..../acquarioClimatico/acquariumData/acquarium_lux_sensor_2019-09-26.txt", header = FALSE, skip = 0, fill = TRUE)
  offset.df_IR_T_air   <- -0.08301205  #mean(df_IR_T$TEMP_air)    - T_lab_air_ref 
  offset.df_IR_T_obj   <- -0.6107229  #mean(df_IR_T$TEMP_obj)    - T_lab_air_ref 
  offset.df_DHT_low    <- -0.8221519  #mean(df_DHT_low$TEMP)     - T_lab_air_ref
  offset.df_DHT_high   <- -1.11875  #mean(df_DHT_high$TEMP)    - T_lab_air_ref
  offset.df_boschSense <- -0.2345312  #mean(df_boschSense$TEMP)  - T_lab_air_ref


  # da qui' in avanti non ci dovrebbe essere niente da modificare

  ###################
  df_IR_T <- data.frame(TIME=paste(IR_T$V1, IR_T$V2),
                        TEMP_air=IR_T$V3,
                        TEMP_obj=IR_T$V4)
  df_IR_T$TIME <- as.POSIXct(strptime(df_IR_T$TIME, format = "%Y-%m-%d %H:%M:%S"))
  
  df_DHT_low <- data.frame(TIME=paste(DHT_low$V1, DHT_low$V2),
                           TEMP=DHT_low$V4,
                           RH=DHT_low$V7)
  df_DHT_low$TIME <- as.POSIXct(strptime(df_DHT_low$TIME, format = "%Y-%m-%d %H:%M:%S")) 
  
  df_DHT_high <- data.frame(TIME=paste(DHT_high$V1, DHT_high$V2),
                            TEMP=DHT_high$V4,
                            RH=DHT_high$V7)
  df_DHT_high$TIME <- as.POSIXct(strptime(df_DHT_high$TIME, format = "%Y-%m-%d %H:%M:%S")) 
  
  df_CO2 <- data.frame(TIME=paste(CO2Sens$V1, CO2Sens$V2),
                       CO2ppm=CO2Sens$V3)
  df_CO2$TIME <- as.POSIXct(strptime(df_CO2$TIME, format = "%Y-%m-%d %H:%M:%S"))
  
  df_boschSense <- data.frame(TIME=paste(boschSens$V1, boschSens$V2),
                              TEMP=boschSens$V3,
                              RH=boschSens$V4,
                              PRES=boschSens$V5)
  df_boschSense$TIME <- as.POSIXct(strptime(df_boschSense$TIME, format = "%Y-%m-%d %H:%M:%S"))
  
  df_lux <- data.frame(TIME=paste(lux$V1, lux$V2),
                       LUX=lux$V5)
  df_lux$TIME <- as.POSIXct(strptime(df_lux$TIME, format = "%Y-%m-%d %H:%M:%S"))
  
  T_airK <- df_DHT_high$TEMP + 273.15
  P_H2O <- exp( (77.3450 + 0.0057*T_airK) - 7235/T_airK ) / T_airK^(8.2)
  RH_air <- df_DHT_high$RH
  P_atm <- mean(df_boschSense$PRES[ length(df_boschSense$PRES)-12 : length(df_boschSense$PRES) ] )
  ppmH2O <- (P_H2O*RH_air/100 / (P_atm*100 - P_H2O*RH_air/100) ) * 10^6
  df_ppmH2O <- data.frame(TIME = df_DHT_high$TIME, ppmH2O = ppmH2O)
  #derivata della concentrazione dell'acqua
  diff_ppmH2O <- diff(df_ppmH2O$ppmH2O)
  
  ###################
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  output$raw_temp <- renderPlot({
    invalidateLater(5000, session)
    plot(df_boschSense$TIME, df_boschSense$TEMP, pch=19, col=1, ylim = c(20,max(df_boschSense$TEMP)+5), type = "l")
    lines(df_DHT_high$TIME, df_DHT_high$TEMP, col=4, pch=19)
    lines(df_DHT_low$TIME, df_DHT_low$TEMP, pch=19, col=2)
    lines(df_IR_T$TIME, df_IR_T$TEMP_air, pch=19, col=3)
    lines(df_IR_T$TIME, df_IR_T$TEMP_obj, pch=19, col=6)
    legend("topright", 
           c("bosch", "DHT_h", "DHT_l", "IR_air", "IR_obj"), col = c(1, 4, 2, 3, 6),
           lty = c(1,1,1,1,1))
  })
 
  output$norm_temp <- renderPlot({
    plot(df_boschSense$TIME, df_boschSense$TEMP - offset.df_boschSense, pch=19, col=1, ylim = c(20,max(df_boschSense$TEMP)+5), main="Temperatura",type = "l", lwd=1 ,xlab = "Tempo", ylab=" Temperatura °C")
    lines(df_DHT_high$TIME, df_DHT_high$TEMP - offset.df_DHT_high, col=4, pch=19)
    lines(df_DHT_low$TIME, df_DHT_low$TEMP - offset.df_DHT_low, pch=19, col=2)
    lines(df_IR_T$TIME, df_IR_T$TEMP_air - offset.df_IR_T_air, pch=19, col=3, lwd = 5)
    lines(df_IR_T$TIME, df_IR_T$TEMP_obj - offset.df_IR_T_obj, pch=19, col=6)
    legend("topright", 
           c("bosch", "DHT_h", "DHT_l", "IR_air", "IR_obj"), col = c(1, 4, 2, 3, 6),
           lty = c(1,1,1,1,1))
  })
  
  output$ppmH2O <- renderPlot({
    plot(df_ppmH2O$TIME, df_ppmH2O$ppmH2O, main="Vapore acqueo", type = "l", lwd = 5, xlab = "Tempo", ylab = "Concentrazione (ppm)", col="navy")
  })
  
  output$diff_ppmH2O <- renderPlot({
    plot(diff_ppmH2O)
  })
  
  output$raw_co2 <- renderPlot({
    plot(df_CO2$TIME, df_CO2$CO2ppm, col="steelblue", main="Anidride Carbonica (CO2 o diossido di carbonio)", pch=19, type = "l", lwd = 5, xlab = "Tempo", ylab = "Concentrazione (ppm)")
  })
  
  output$lux <- renderPlot({
    plot(df_lux$TIME, df_lux$LUX, pch=19, type="l", main="Illuminazione", lwd = 5, xlab = "Tempo", ylab = "Intensità (lx)")
  })
  
  #sensore bosch
  last_bosch_time <- df_boschSense$TIME[length(df_boschSense$TIME)]
  output$last_bosch_time <- renderText(paste("Bosch ultima lettura: ", paste(as.character(last_bosch_time)), " UTC + 2" ))
  last_boschT <- df_boschSense$TEMP[length(df_boschSense$TEMP)]
  output$last_boschT <- renderText(paste("Bosch Raw Temperature: ", paste(sprintf("%.1f", last_boschT )), " °C" ))
  last_boschRH <- df_boschSense$RH[length(df_boschSense$RH)]
  output$last_boschRH <- renderText(paste("Bosch Raw Umidità relativa: ", paste(sprintf("%.1f", last_boschRH )), " %" ))
  last_boschPres <- df_boschSense$PRES[length(df_boschSense$PRES)]
  output$last_boschPres <- renderText(paste("Bosch Pressione Atm: ", paste(sprintf("%.1f", last_boschPres )), " hPa" ))
  
  #sensore CO2
  last_CO2_time <- df_CO2$TIME[length(df_CO2$TIME)]
  output$last_CO2_time <- renderText(paste("CO2 ultima lettura: ", paste(as.character(last_CO2_time)), " UTC + 2" ))
  last_CO2_ppm <- df_CO2$CO2ppm[length(df_CO2$CO2ppm)]
  output$last_CO2_ppm <- renderText(paste("Concentrazione CO2: ", paste(sprintf("%.1f", last_CO2_ppm )), " ppm, (400 < CO2 < 5000)" ))
  
  #acqua
  last_H2O_time <- df_ppmH2O$TIME[length(df_ppmH2O$TIME)]
  output$last_H2O_time <- renderText(paste("H2O ultima lettura: ", paste(as.character(last_H2O_time)), " UTC + 2" ))
  last_H2O_ppm <- df_ppmH2O$ppmH2O[length(df_ppmH2O$ppmH2O)]
  output$last_H2O_ppm <- renderText(paste("Concentrazione H2O: ", paste(sprintf("%.1f", last_H2O_ppm )), " ppm" ))
  
  #lux
  last_lux_time <- df_lux$TIME[length(df_lux$TIME)]
  output$last_lux_time <- renderText(paste("Lux ultima lettura: ", paste(as.character(last_lux_time)), " UTC + 2" ))
  last_lux <- df_lux$LUX[length(df_lux$LUX)]
  output$last_lux <- renderText(paste("Illuminazione (lx): ", paste(sprintf("%.1f", last_lux )), " lx" ))
  
})
