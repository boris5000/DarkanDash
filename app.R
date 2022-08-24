library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

api_GET <- function(
  base_url = "https://darkan.org:8443/v1", 
  section="", search=""){
  url <- sprintf("%s/%s/%s", base_url, section, search)
  cont <- httr::content(httr::GET(url))
  return(cont)
}

header = dashboardHeader()

sidebar = dashboardSidebar(
  sidebarMenu(
    id = 'lefttabs',
    menuItem('About', tabName = 'about'),
    menuItem('Adventure Log', tabName = 'adventurelog'),
    #menuItem('Highscores', tabName = 'highscores'),
    menuItem('Grand Exchange', tabName = 'ge')
  )
)

body = dashboardBody(
  uiOutput('tabView')
)

rightsidebar = dashboardControlbar()

ui <- dashboardPage(
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=rightsidebar
)

server <- function(input, output, session) {

  output$tabView = renderUI({
    switch(input$lefttabs,
      'about' = {fluidPage(h3('This is an about page you scum!'))},
      'ge' = {fluidPage(
        box(title='Current Offers', collapsible=TRUE, closable=FALSE, width=12,
          DT::dataTableOutput('geDF')
        )
      )},
      'adventurelog' = {
        fluidPage(
          fluidRow(
            textInput('alog_player', "Player Name:", value="")
          ),
          fluidRow(
            infoBoxOutput('days_played')
          ),
          box(title='Drops And Stuff', collapsible=TRUE, closable=FALSE, width=12,
            DT::dataTableOutput('dropDF')
          ),
          box(title='NPC Kills', collapsible=TRUE, closeable=FALSE, width=12,
            DT::dataTableOutput('killDF')
          )
        )
      }
    )
  })

  session_data = reactiveValues()
  session_data$ge <- NULL
  session_data$alog_player <- NULL
  session_data$drops <- NULL

  output$days_played <- renderInfoBox({
    infoBox(value=signif(session_data$alog_player$timePlayed / 86400, 4), title='Days Played...')
  })

  output$dropDF <- DT::renderDataTable({
    if(!is.null(session_data$alog_player)) DT::datatable(data.frame(Number=unlist(session_data$alog_player$variousCounter)))
  })

  output$killDF <- DT::renderDataTable({
    if(!is.null(session_data$alog_player)){
      DT::datatable(
        data.frame(Number=unlist(session_data$alog_player$npcKills))
      )
    }
  }, options=list(columnDefs = list(list(targets=c(3,4,5,6)))))

  output$geDF <- DT::renderDataTable({
    if(!is.null(session_data$ge)) DT::datatable(session_data$ge)
  })

  observeEvent(input$alog_player, {
    session_data$alog_player <- api_GET(section='players', search=input$alog_player)
  })

  observeEvent(input$lefttabs, {
    if(input$lefttabs == 'ge'){
      data <- data.frame(do.call('rbind', api_GET(section='ge')))
      session_data$ge <- data[!unlist(data$aborted), c(3,4,5,6,7)]
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
