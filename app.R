# Loading libraries

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(openxlsx)
library(readxl)
library(htmlwidgets)
library(highcharter)
library(lmtest)
library(leaflet)
library(plotly)
library(kableExtra)

# Loading datasets

overall_migration <- read_csv("data_clean/migration_totals.csv")
tooltip_data <- read_xlsx("data_clean/tooltip_data.xlsx")
weekly_earning_state <- read_xlsx("data_clean/weekly_earning_state.xlsx")
overall_participation_rate <- read_xlsx("data_clean/overall_participation_rate.xlsx")
lm_part_time <- read.xlsx("data_clean/lm_model_part_time.xlsx")
lm_full_time <- read.xlsx("data_clean/lm_model_full_time.xlsx")
state_data <- read.xlsx("data_clean/state_data.xlsx")

plot_list <- lapply(unique(overall_participation_rate$year), function(year) {
  overall_participation_rate %>%
    filter(year == !!year) %>%
    ggplot(aes(x = year, y = participation)) +
    geom_boxplot() +
    labs(title = paste("Average Participation Rate in", year),
         x = "Year",
         y = "Participation Rate") +
    theme_minimal() +
    theme_bw() +
    theme(axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          plot.title = element_text(size = 22))
})

# Define UI for application
ui <- 
  dashboardPage(title = "Rshiny Dashboard",
  dashboardHeader(title = span(
    tags$style(HTML("
      .main-header .logo {
        white-space: normal;
        font-family: 'Times New Roman', Times, serif;
        font-wight: bold;
      }
    ")),
    "Dashboard on Migration & Education for Australia's Labour Force"
  ),
  titleWidth = "97%"
  ),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .sidebar-heading {
          font-size: 18px;
          font-weight: bold;
          text-align: center;
          padding: 10px;
          color: #ffffff;
          background-color: #2b2b2b;
        }
      "))
    ),
    div(class = "sidebar-heading", "Navigation"),
    sidebarMenu(
    menuItem(" Introduction", tabName = "intro"),
    menuItem("Overall", tabName = "slide1"),
    menuItem("Employment", tabName = "slide2"),
    menuItem("Education", tabName = "slide3")
    )),
  dashboardBody(
    shinyDashboardThemes(theme = "poor_mans_flatly"),
    tabItems(
      tabItem(tabName = "intro",
              h1("Impact of Migration and Education Levels on Australia’s Labour Force",
                 br(),
                 br(),
                 style = "text-align: center; font-weight: bold"),
              h2("Purpose of This Dashboard", 
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h4("This dashboard
                          aims to display some statistics and visualizations of the different states
                          in Australia. Overseas migration and education levels are amongst 
                          the many factors that affect the various aspects of the labour force 
                          such as unemployment & participation rates and income levels. With this 
                          visualization, the aim is to portray a message to the public who wish 
                          to become more educated on the matter, and potential policy makers to 
                          be notified to the issues at hand, to which they may go forth and 
                          deeper into the issues highlighted here.", 
                          style = "font-size: 28px; font-family: 'Times New Roman'; 
                          text-align: center;")),
                column(width = 2)),
              
              h2("How To Use",
                 style = "text-align: center; font-weight: bold"),
              fluidRow(
                column(width = 2),
                column(width = 8,
                         h4("Using this dashboard is very simple, locate the navigation
                            tab which is visible on the left-hand side of this screen, where the user 
                            may toggle between different pages related to the title mentioned on the 
                            menu. Within these pages are various visualizations that are interactive
                            to the user's will. This includes 
                       hovering, filtering,
                          and playing a short animation. Within these tabs there are explanations as
                          to the current display on the page demonstrates.",
                          style = "font-size: 28px; font-family: 'Times New Roman'; 
                          text-align: center;")),
                column(width = 2)
                ),
              
              fluidRow(
                column(width = 2),
                column(width = 8,
                       h4("Note: Kindly allow sufficient time for the graphs and diagrams to 
                          load, often they may take more than a couple seconds.",
                          style = "font-size: 28px; font-family: 'Times New Roman'; 
                          text-align: center; font-weight: bold")),
                column(width = 2)
              )
              ),
      
      tabItem(tabName = "slide1",
              div(style = "height: 800px; overflow-y: auto; overflow-x: hidden;",
              h1("Overall Migration Statistics"),
              fluidRow(
                column(width = 6,
                box(width = 12,
                    title = "Sankey Digram of Migration Types",
                    status = "primary",
                    solidHeader = TRUE,
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey1'",
                      highchartOutput("sankey1")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey2'",
                      highchartOutput("sankey2")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey3'",
                      highchartOutput("sankey3")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey4'",
                      highchartOutput("sankey4")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey5'",
                      highchartOutput("sankey5")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey6'",
                      highchartOutput("sankey6")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey7'",
                      highchartOutput("sankey7")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey8'",
                      highchartOutput("sankey8")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey9'",
                      highchartOutput("sankey9")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey10'",
                      highchartOutput("sankey10")
                    ),
                    conditionalPanel(
                      condition = "input.sankeySelector == 'sankey11'",
                      highchartOutput("sankey11")
                    ))),
                column(width = 6,
                       box(width = 12,
                           title = "Interactive Map of Australia",
                           status = "primary",
                           solidHeader = TRUE,
                           leafletOutput("map")))),
              fluidRow(
                column(width = 3,
                box(
                  width = 12,
                  title = "Select State or Migration Type",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("sankeySelector", "Select Plot:",
                               choices = list("Australian Capital Territory" = "sankey1", 
                                              "New South Wales" = "sankey2", 
                                              "Northern Territory" = "sankey3", 
                                              "Queensland" = "sankey4",
                                              "South Australia" = "sankey5",
                                              "Tasmania" = "sankey6",
                                              "Victoria" = "sankey7",
                                              "Western Australia" = "sankey8",
                                              "Change Over Previous Quarter" = "sankey9",
                                              "Natural Increase" = "sankey10",
                                              "Net Overseas Migration" = "sankey11"),
                               selected = "sankey1"))),
                column(width = 4,
                       h2("The sankey diagram on the left displays various migration types and
                          the number of years in which a net increase/decrease was recorded in a 
                          particular state. Furthermore, the user may also toggle for specific states
                          and view the categories that saw the most/least migration. In general, all
                          of the states saw an increase in migration across the majority of the years.
                          The state of New South Wales is the only state that sees the most net  
                          decrease in interstate migration across the years.",
                       style = "font-size: 23px; font-family: 'Times New Roman'; 
                          text-align: justify;")),
                
                column(width = 4,
                       h2("However, this does not mean that the population of NSW has not increased, 
                       the natural rate
                          of increase is also a big contributor towards the population. This simply
                          suggests that more people are leaving NSW and going to other states. 
                          Furthermore, the map of Australia provides hover-able tooltips of more
                          accurate information of various statistics. The radius is determined by the
                          over change in the population over the years.",
                          style = "font-size: 23px; font-family: 'Times New Roman'; 
                          text-align: justify;"))
                  
                ))),
      
      tabItem(tabName = "slide2",
              div(style = "height: 800px; overflow-y: auto; overflow-x: hidden;",
              h1("Employment and Participation Rates"),
              fluidRow(
                column(width = 6,
                       box(width = 12,
                           title = "Interactive Bar Graph of Weekly Income Across Genders",
                           status = "primary",
                           solidHeader = TRUE,
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot9'",
                         plotlyOutput("plot9")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot1'",
                         plotlyOutput("plot1")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot2'",
                         plotlyOutput("plot2")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot3'",
                         plotlyOutput("plot3")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot4'",
                         plotlyOutput("plot4")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot5'",
                         plotlyOutput("plot5")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot6'",
                         plotlyOutput("plot6")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot7'",
                         plotlyOutput("plot7")
                         ),
                       conditionalPanel(
                         condition = "input.plotSelectorSlide2 == 'plot8'",
                         plotlyOutput("plot8")
                         )
                       )
                       ),
                column(width = 6,
                       box(width = 12,
                           title = "Participation Rate",
                           status = "primary",
                           solidHeader = TRUE,
                           plotOutput("animation")))
                ),
              fluidRow(
                column(width = 2,
                       box(
                         width = 12,
                         title = "Select State",
                         status = "primary",
                         solidHeader = TRUE,
                         radioButtons("plotSelectorSlide2", "Select Plot:",
                                      choices = list("All of Australia" = "plot9",
                                                     "Australian Capital Territory" = "plot1", 
                                                     "New South Wales" = "plot2", 
                                                     "Northern Territory" = "plot3", 
                                                     "Queensland" = "plot4",
                                                     "South Australia" = "plot5",
                                                     "Tasmania" = "plot6",
                                                     "Victoria" = "plot7",
                                                     "Western Australia" = "plot8"),
                                      selected = "plot9"))),
                column(width = 2,
                       box(width = 12,
                           title = "Play Animation",
                           status = "primary",
                           solidHeader = TRUE,
                           actionButton("play_pause", "Play/Pause"))),
                
                column(width = 7,
                       h3("The bar chart is displaying the weekly earnings over the years of males
                          and females for the entirety of Australia. The user may select to study
                          individual states too, however, looking at the whole country we can see
                          that in full time employments, the males tend to earn more than the females,
                          however, when it comes to part time employment, it is the females who are
                          earning more. Furthermore, the animation of the boxplot on the right is
                          displaying how the participation rate changes over the years. Overall, we
                          do not see it change too much regarding total participation but the total
                          spread varies.",
                          style = "font-size: 23px; font-family: 'Times New Roman'; 
                          text-align: justify;")
                       ),
                column(width = 1)
                )
              )
              ),
      
      tabItem(tabName = "slide3",
              div(style = "height: 800px; overflow-y: auto; overflow-x: hidden;",
              h1("Income and Education"),
              fixedRow(
                column(width = 4,
                       box(width = 12,
                           title = "Linear Regression Model",
                           status = "primary",
                           solidHeader = TRUE,
                           tableOutput("enhancedTable"),
                           tags$style(HTML("
                            #enhancedTable table {
                              width: 100% !important;
                            }
                          "))
                       )
                       ),
                column(width = 6,
                       box(width = 12,
                           title = "Violin Plot",
                           status = "primary",
                           solidHeader = TRUE,
                           plotOutput("violinPlot")
                       )
                ),
                column(width = 2,
                       box(width = 12,
                           title = "Violin Plot",
                           status = "primary",
                           solidHeader = TRUE,
                           radioButtons("state", "Select State/Territory:",
                                        choices = unique(state_data$state_and_territory))
                       ))),
                
                fixedRow(
                column(width = 2,
                       box(width = 12,
                           title = "Employment Type",
                           status = "primary",
                           solidHeader = TRUE,
                           radioButtons("data_select", "Select Type:",
                                        choices = list("Full Time" = "Full Time", 
                                                       "Part Time" = "Part Time"),
                                        selected = "Full Time"))),
                column(width = 2,
                       box(width = 12,
                           title = "State",
                           status = "primary",
                           solidHeader = TRUE,
                           selectInput("state_select", "Select State:",
                                        choices = unique(lm_full_time$state),
                                        selected = "Australia")
                           )
                       ),
               
                column(width = 4,
                       h3("The linear regression model shows that the coefficients of the different
                          education types and the difference they make in the weekly income. The table
                          displays these models for both part-time and full-time employment types. 
                          Looking at the entirety of Australia, postgraduate  and graduate diplomas
                          bring forth the highest increase in weekly earning for full-time employment.
                          Whilst, the same can also be said for part-time employment, the numbers
                          are significantly lower. All p-values are below 0.05 in the majority of the
                          cases, suggesting the specific education levels are indeed impactful.",
                          style = "font-size: 23px; font-family: 'Times New Roman'; 
                          text-align: justify;")),
                column(width = 3,
                       h3("Furthermore, the violin plot displays the major concentrations of incomes,
                          hence increases the width of the plot is multiple locations. The graph
                          extends beyond its maximum value because it is predicting the occurances
                          for which the values may spread even further. The violins include both 
                          part-time and full-time employment types, this may vary the results of 
                          the impact of education type.",
                          style = "font-size: 23px; font-family: 'Times New Roman'; 
                          text-align: justify;"))
                )
              )
              )
      )
    )
  )
  
# Define server logic
server <- function(input, output, session) {

  # Sankey 1
  
  output$sankey1 <- renderHighchart({
    sankey1 <- overall_migration %>%
      filter(State == "Australian Capital Territory") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey1), "sankey") %>%
      hc_title(text = "Net Change Migration Types in ACT") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 2
  
  output$sankey2 <- renderHighchart({
    sankey2 <- overall_migration %>%
      filter(State == "New South Wales") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey2), "sankey") %>%
      hc_title(text = "Net Change Migration Types in NSW") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 3
  
  output$sankey3 <- renderHighchart({
    sankey3 <- overall_migration %>%
      filter(State == "Northern Territory") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey3), "sankey") %>%
      hc_title(text = "Net Change Migration Types in NT") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 4
  
  output$sankey4 <- renderHighchart({
    sankey4 <- overall_migration %>%
      filter(State == "Queensland") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey4), "sankey") %>%
      hc_title(text = "Net Change Migration Types in QLD") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 5
  
  output$sankey5 <- renderHighchart({
    sankey5 <- overall_migration %>%
      filter(State == "South Australia") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey5), "sankey") %>%
      hc_title(text = "Net Change Migration Types in SA") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 6
  
  output$sankey6 <- renderHighchart({
    sankey6 <- overall_migration %>%
      filter(State == "Tasmania") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey6), "sankey") %>%
      hc_title(text = "Net Change Migration Types in TAS") %>%
      hc_add_theme(hc_theme_ggplot2())
  })

  # Sankey 7
  
  output$sankey7 <- renderHighchart({
    sankey7 <- overall_migration %>%
      filter(State == "Victoria") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey7), "sankey") %>%
      hc_title(text = "Net Change Migration Types in VIC") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 8
  
  output$sankey8 <- renderHighchart({
    sankey8 <- overall_migration %>%
      filter(State == "Western Australia") %>%
      select(`Measurement Type`, net_increase)
    
    hchart(data_to_sankey(sankey8), "sankey") %>%
      hc_title(text = "Net Change Migration Types in WA") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 9
  
  output$sankey9 <- renderHighchart({
    sankey9 <- overall_migration %>%
      filter(`Measurement Type` == "Change Over Previous Quarter") %>%
      select(State, net_increase)
    
    hchart(data_to_sankey(sankey9), "sankey") %>%
      hc_title(text = "Change Over Previous Quarter in All States and Territories") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 10
  
  output$sankey10 <- renderHighchart({
    sankey10 <- overall_migration %>%
      filter(`Measurement Type` == "Natural Increase") %>%
      select(State, net_increase)
    
    hchart(data_to_sankey(sankey10), "sankey") %>%
      hc_title(text = "Natural Increase in All States and Territories") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Sankey 11
  
  output$sankey11 <- renderHighchart({
    sankey11 <- overall_migration %>%
      filter(`Measurement Type` == "Net Overseas Migration") %>%
      select(State, net_increase)
    
    hchart(data_to_sankey(sankey11), "sankey") %>%
      hc_title(text = "Net Overseas Migration in All States and Territories") %>%
      hc_add_theme(hc_theme_ggplot2())
  })
  
  # Leaflet map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 133.8807, 
              lat = -23.6980,
              zoom = 3.5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        data = tooltip_data,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~(tooltip_data$change_over_previous_quarter) / 1000,
        color = "red",
        label = lapply(1:nrow(tooltip_data), function(i) {
          paste("State: ", tooltip_data$state[i], "<br>",
                "Average Unemployment: ", tooltip_data$average_unemployment[i], "%", "<br>",
                "Average Participation: ", tooltip_data$average_participation[i], "%",  "<br>",
                "Change Over Previous Quarter: ", tooltip_data$change_over_previous_quarter[i], "<br>",
                "Natural Increase: ", tooltip_data$natural_increase[i], "<br>",
                "Net Overseas Migration: ", tooltip_data$net_overseas_migration[i], "<br>",
                "Net Interstate Migration: ", tooltip_data$net_interstate_migration[i], "<br>",
                "Full Time: ", tooltip_data$full_time[i], "<br>",
                "Part Time: ", tooltip_data$part_time[i])}) %>%
          lapply(HTML))
  })
  
  # Plot 1
  
  output$plot1 <- renderPlotly({
    act_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Australian Capital Territory") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in ACT",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(act_income)
    
  })
  
  # Plot 2
  output$plot2 <- renderPlotly({
    nsw_income <- weekly_earning_state %>% 
      filter(state_and_territory == "New South Wales") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in NSW",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(nsw_income)
    
  })
  
  # Plot 3
  output$plot3 <- renderPlotly({
    nt_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Northern Territory") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in NT",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(nt_income)
    
  })
  
  # Plot 4
  output$plot4 <- renderPlotly({
    qld_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Queensland") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in QLD",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(qld_income)
    
  })
  
  # Plot 5
  output$plot5 <- renderPlotly({
    sa_income <- weekly_earning_state %>% 
      filter(state_and_territory == "South Australia") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in SA",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(sa_income)
    
  })
  
  # Plot 6
  output$plot6 <- renderPlotly({
    tas_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Tasmania") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in TAS",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(tas_income)
    
  })
  
  # Plot 7
  output$plot7 <- renderPlotly({
    vic_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Victoria") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in VIC",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(vic_income)
    
  })
  
  # Plot 8
  output$plot8 <- renderPlotly({
    wa_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Western Australia") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in WA",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.8))
    
    ggplotly(wa_income)
    
  })
  
  # Plot 9
  output$plot9 <- renderPlotly({
    aus_income <- weekly_earning_state %>% 
      filter(state_and_territory == "Australia") %>%
      ggplot(aes(x = year,
                 y = weekly_income,
                 fill = category)) +
      geom_col(color = "black",
               position = "dodge") +
      geom_smooth(method = "lm", 
                  se = FALSE, 
                  size = 1,
                  color = "black") +
      labs(x = "Year", 
           y = "Weekly Earning", 
           title = "Time Series of Weekly Earning by Employment Type in AUS",
           fill = "Cateogry") +
      theme_bw() +
      facet_wrap(~ gender) +
      scale_x_continuous(breaks = seq(2014,2023, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.7))
    
    ggplotly(aus_income)
    
  })
  
  # Boxplot animation
  rv <- reactiveValues(current_plot = 1, playing = FALSE)
  
  # Render the current plot
  output$animation <- renderPlot({
    plot_list[[rv$current_plot]]
  })
  
  # view play/pause button
  observeEvent(input$play_pause, {
    rv$playing <- !rv$playing
  })
  
  # Timer to cycle through plots
  observe({
    invalidateLater(1000, session)
    isolate({
      if (rv$playing) {
        rv$current_plot <- (rv$current_plot %% length(plot_list)) + 1
      }
    })
  })
  
  # Kable Table
  output$enhancedTable <- renderTable({
    # Select the appropriate dataset based on user input
    data <- if (input$data_select == "Full Time") lm_full_time else lm_part_time
    
    # Filter the dataset for the selected state
    filtered_data <- data %>% filter(state == input$state_select)
    
    # Use kableExtra to create table
    filtered_data %>%
      kbl(caption = paste(input$data_select, "Linear Regression Model for", input$state_select)) %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
      row_spec(0, background = "white")
  }, sanitize.text.function = function(x) x)
  
  # Violin plot
  output$violinPlot <- renderPlot({
    filtered_data <- state_data %>%
      filter(state_and_territory == input$state)
    
    ggplot(filtered_data, aes(x = year, y = weekly_income, 
                              fill = level_of_highest_non_school_qualification)) +
      geom_violin(trim = FALSE, scale = "width") +
      theme_bw() +
      scale_x_continuous(breaks = seq(2014, 2023, by = 1)) +
      scale_y_continuous(breaks = seq(
        min(filtered_data$weekly_income, na.rm = TRUE),
        2000, 
        by = 400)) +
      labs(title = paste("Violin Plot of Income Distribution by Education Type in", input$state),
           x = "Year",
           y = "Weekly Income",
           fill = "Education Type") +
      theme(axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 18),
            plot.title = element_text(size = 22),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())  
    
    })
}
  

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
  