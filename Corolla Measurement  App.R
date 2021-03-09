library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)

phlox_long <- read_csv(here(phlox_long))

ui <- fluidPage(
  selectInput("x", "Select Variable", choices = c("genus", "pollinator", "color"),
              selected = "mean_corolla_length_cm"),
  selectInput("y", "Select Y Variable", choices = c("mean_corolla_length_cm", "mean_corolla_width_throat_cm", "mean_length_width_ratio"),
              selected = "mean_corolla_width_throat_cm"),
  plotOutput("plot", width = "800px", height = "400px")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(phlox_long, aes_string(x = input$x, y = input$y)) + 
      scale_fill_manual(values = palette) +
      geom_boxplot(show.legend = FALSE)+
      xlab(paste(input$x,collapse=""))+
      ylab(paste(input$y,collapse=""))+
      
      labs(title = "Corolla Measurements"#,
           #x = "Genus"#,
           #y = "Corolla Length:Width"
      )+
      theme_gray(base_family = "Palatino")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            plot.title=element_text(size = rel(1.5), face="bold", hjust=.5))
  })
  
  session$onSessionEnded(stopApp)
} 

shinyApp(ui, server)
