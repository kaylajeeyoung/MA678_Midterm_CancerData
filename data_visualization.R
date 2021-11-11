source("data_reading.R")
pacman::p_load(knitr,
        arm,
        data.table,
        foreign,
        gridExtra,
        rstan,
        rstanarm,
        zoo,
        lme4, 
        magrittr,
        maps,
        shiny)

year <- df %>% distinct(Year)
year <- year$Year
MainStates <- map_data("state")
MainStates$region <- str_to_title(MainStates$region)
colnames(MainStates) <- c("long", "lat", "group", "order", "States", "subregion")


ui_2 <- fluidPage(
  selectInput("Year", "select year", year), 
  plotOutput("plot")
)
server_2 <- function(input, output, session) {
  output$plot <- renderPlot({
    df_mean <- df %>% subset(Year == input$Year) %>% group_by(States)
    
    
    plot_data <- inner_join(df_mean, MainStates, by = "States") %>% 
      dplyr::select("States", "long", "lat", "Age-Adjusted Rate", "Year", "group")
    
    plot_data$rate <- plot_data$`Age-Adjusted Rate`
    
    ggplot() + 
      geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),color="black", fill="seashell1", size = .3) + 
      geom_polygon(data = plot_data, aes(x = long, y = lat, group = States, fill = rate), 
                   color = "grey", size = .3) + 
      scale_color_gradient(name="Age Adjusted Rate", 
                            low = "blue2", 
                            high = "brown3", 
                            na.value = "grey50") + labs(title="Childhood CNS Cancer rates")
  }, res = 96, height = 550, width = 950)
}

shinyApp(ui_2, server_2)
