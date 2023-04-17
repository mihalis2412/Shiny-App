#rsconnect::setAccountInfo(name='mih2412',
#                          token='E6ADE59D426185CC0D6752CFA3A76B06',
#                          secret='RG9FlaZPzrCzkCXVpoPDvGVc5pfH3Ki0H+K/ZiMT')
#
#library(rsconnect)
#rsconnect::deployApp('C:/Users/mihal/OneDrive/my_shiny_app_final_v4', appName = 'Breast_Cancer_Metabric',
#                     account = 'mih2412')


library(shiny)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = 'C:/Users/mihal/OneDrive/my_shiny_app_final_v3'
  host = '0.0.0.0',
  port = as.numeric(port)
  
)




















