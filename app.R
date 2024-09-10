library(shiny)
library(seacarb)
library(DBI)
library(RPostgreSQL)

con <- dbConnect(
  drv = PostgreSQL(),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  host = Sys.getenv("DB_HOST"),
  dbname = Sys.getenv("DB_NAME")
)



ui <-fluidPage(
  selectInput('crmSelect', label = 'CRM', choices = c()),
  numericInput("temperature", label = "temperature", value = 12),
  verbatimTextOutput("result")
  
)

  
server <- function(input, output, session){
  
  observe({
    opts <- dbGetQuery(con, "SELECT DISTINCT crm_number FROM lu_oa_crm ORDER BY crm_number;")
    updateSelectInput(session, 'crmSelect', label = 'CRM (From DB)', opts$crm_number)
  })


  output$result <- renderText({
    curr_CRM = input$crmSelect  
    curr_temp = input$temperature

    library(readr)
    
    df.CRM = read.csv("CRM_info.csv")
    
    library(dplyr)
    
    df.CRM.curr <- df.CRM %>%
      filter(CRM == curr_CRM) %>% 
      mutate_at(c(3:6), ~(./10^6))
    
    
    # Function
    fxn.CRMpH=function(temp, df){
      
      fxn_temp = as.numeric(temp)
      
      #flag 15 = ALK & DIC, i.e. var 1 = ALK, var 2 = DIC
      fullcarb = carb(flag = 15, var1 = df$TA_umolkg.1, var2 = df$DIC_umolkg.1, S = df$Salinity, T = fxn_temp, Pt = df$Phosphate_umolkg.1, Sit = df$Silicate_umolkg.1, k1k2 = "l", ks = "d", b = "l10")
      
      CRMpH = fullcarb["pH"][[1]]
      
      return(CRMpH)
    }
    
    
    if(nrow(df.CRM.curr) == 0){
      result <- paste("No info on CRM batch")
    }else{
      pH_currtemp = fxn.CRMpH(curr_temp, df.CRM.curr)
      
      result <- paste0("Expected pH at current temperature (",curr_temp," C) is: ", round(pH_currtemp, digits = 3))
    }
    paste(result)
  })
}


shinyApp(ui = ui, server = server)
  