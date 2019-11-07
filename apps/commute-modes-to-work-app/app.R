#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

commute_mode <- read_csv("commute_mode.csv")

variables <- commute_mode %>% 
    select(city_size, state_region, city_type) %>% 
    colnames()

names(variables) <-variables %>% 
    str_replace_all("_", " ") %>% 
    str_to_title()
    
    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Commute Modes to Work"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variable",
                        "Variable:",
                        choices = variables)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("commute_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$commute_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        # non-numeric variable needs to be passed as sym
        var <- sym(input$variable)
        
        commuter_mode_by_input_variable <- commute_mode %>% 
            group_by(!!var, mode) %>% 
            summarize(total = sum(n)) %>% 
            ungroup() %>% 
            group_by(!!var) %>% 
            mutate(prop = 100 * total / sum(total)) %>% 
            mutate(prop_text = paste0(round(prop,1),"%")) %>% 
            ungroup() %>% 
            na.omit()
        
        commuter_mode_by_input_variable %>% 
            ggplot(aes(!!var, total, fill = mode)) +
            geom_col(position = "dodge") +
            geom_label(aes(label = prop_text), position = position_dodge(0.9)) +
            labs(title = paste("Commute Mode by", input$variable %>% str_replace_all("_", " ") %>% str_to_title()),
                 x = input$variable %>% str_replace_all("_", " "), 
                 y = "# commuters") +
            scale_y_continuous(labels = scales::dollar_format(prefix = "")) +
            theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
