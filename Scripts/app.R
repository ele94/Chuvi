library(shiny)
library(shinyjs)
library(RMySQL)


options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = "diao94chan"
))

databaseName <- "lpro2"
table <- "cita"
table_pacientes <- "pacientes"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

saveData_pacientes <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table_pacientes, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  query <- sprintf("SELECT * FROM %s", table)
  
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


fields <- c("referencias", "Fecha", "Bateria_V","ESTIMA","STIMVD","STIMVI","CHOQUE","ATP","FC_min","R","ms_A","VD","ms_VD","VI","ms_VI","Episodios")
fields_pacientes <- c("SN", "N_camaras", "Fecha_Implante","Fecha_Ultimo_Seguimiento")


shinyApp(
  ui = fluidPage(
    
    shinyjs::useShinyjs(),
    
    div(
      
      id="inicio",
      h3("Indique la accion que desea realizar"),
      actionLink("submit_nuevacita","Quiero registrar una nueva cita."),
      actionLink("submit_nuevopaciente","Quiero registrar un nuevo paciente.")
    ),
    
    shinyjs::hidden(
    
      div(
      
       id="form",
      
        DT::dataTableOutput("cita", width = 300), tags$hr(),
        textInput("referencias", "Referencias", ""),
        textInput("Fecha", "Fecha", "2017-04-18"),
        textInput("Bateria_V", "Bateria", ""),
        textInput("ESTIMA", "ESTIMA", ""),
        textInput("STIMVD", "STIMVD", ""),
       textInput("STIMVI", "STIMVI", ""),
        textInput("CHOQUE", "CHOQUE", ""),
        textInput("ATP", "ATP", ""),
        textInput("FC_min", "FC_min", ""),
        textInput("R", "R", ""),
        textInput("VA", "VA", ""),
        textInput("ms_A", "ms_A", ""),
       textInput("VD", "VD", ""),
       textInput("ms_VD", "ms_VD", ""),
       textInput("VI", "VI", ""),
       textInput("ms_VI", "ms_VI", ""),
        textInput("Episodios", "Episodios", ""),
    
        actionButton("submit", "Enviar")),
      
      div(
        
        id="form_pacientes",
        
        
        textInput("SN", "SN", ""),
        textInput("N_camaras", "Numero de camaras", ""),
        textInput("Fecha_Implante", "Fecha del implante", ""),
        textInput("Fecha_Ultimo_Seguimiento", "Fecha_Ultimo_Seguimiento", ""),
        
        
        actionButton("submit_pacientes", "Enviar")),
    
    
      
      div(
        
        id="gracias_msg",
        h3("Gracias, la informacion de la cita se ha registrado correctamente"),
        actionLink("submit_another","Quiero registrar una nueva cita"),
        actionLink("submit_vueltainicio","Volver al inicio")
      ),
      
      div(
        
        id="gracias_msg_pacientes",
        h3("Gracias, la informacion del paciente se ha registrado correctamente"),
        actionLink("submit_vueltainicio_pacientes","Volver al inicio")
      )
    )
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    formData_pacientes <- reactive({
      data <- sapply(fields_pacientes, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      #AQU
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("gracias_msg")
    })
    
    observeEvent(input$submit_pacientes, {
      saveData_pacientes(formData_pacientes())
      shinyjs::reset("form_pacientes")
      shinyjs::hide("form_pacientes")
      shinyjs::show("gracias_msg_pacientes")
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("gracias_msg")
    })    
    
    observeEvent(input$submit_vueltainicio, {
      shinyjs::show("inicio")
      shinyjs::hide("gracias_msg")
    })  
    
    observeEvent(input$submit_nuevacita, {
      shinyjs::show("form")
      shinyjs::hide("inicio")
    }) 
    
    observeEvent(input$submit_nuevopaciente, {
      shinyjs::show("form_pacientes")
      shinyjs::hide("inicio")
    })
    
    observeEvent(input$submit_vueltainicio_pacientes, {
      shinyjs::show("inicio")
      shinyjs::hide("gracias_msg_pacientes")
    })  
    
  
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$cita <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)