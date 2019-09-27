######VERSION 5.1.0
library(shiny)
options(shiny.port = 3676) # in questo modo usiamo sempre questa porta per il sito
#library(shinyTime)
######
shinyUI(fluidPage(
  
  theme = ("bootswatch-cerulean.css"),
  #HTML('<meta http-equiv="refresh" content="60; URL=http://127.0.0.1:3676/">'), #togliere il commento se si vuole che la pagina si aggiorni ogni 60 secondi
  
  titlePanel("Acquario Climatico, EU Researchers' Night 2019"),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(
      h4("Ultime Letture:"),
      h5("Oraro locale: ", textOutput("currentTime"), align="left"),
      tags$hr(),
      h5(textOutput("last_bosch_time"), align="left"),
      h5(textOutput("last_boschT"), align="left"),
      h5(textOutput("last_boschRH"), align="left"),
      h5(textOutput("last_boschPres"), align="left"),
      tags$hr(),
      h5(textOutput("last_CO2_time"), align="left"),
      h5(textOutput("last_CO2_ppm"), align="left"),
      tags$hr(),
      h5(textOutput("last_H2O_time"), align="left"),
      h5(textOutput("last_H2O_ppm"), align="left"),
      tags$hr(),
      h5(textOutput("last_lux_time"), align="left"),
      h5(textOutput("last_lux"), align="left")
    ),
    mainPanel(
      tabsetPanel(type="pills", id="TABS",
                  tabPanel("Serie Temporali", id="TS",
                           fluidRow(
                             column(12, plotOutput("norm_temp"))
                           ),
                           fluidRow(
                             column(12, plotOutput("raw_co2"))
                           ),
                           fluidRow(
                             column(12, plotOutput("ppmH2O"))
                           ),
                           fluidRow(
                             column(12, plotOutput("diff_ppmH2O"))
                           ),
                           fluidRow(
                             column(12, plotOutput("lux"))
                           )
                           ),
                  tabPanel("IR cam", id="IRCAM",
                           fluidRow(
                             column(12, align="center", img(src="lastPic.png", width="100%"))
                           )
                           )
                  )
    )
  )
))
