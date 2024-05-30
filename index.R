library(shiny)
library(shinydashboard)

library(tidyverse)
library(ggplot2)
library(dplyr)
US_trend_states_BR <- read.csv("NCHS_-_U.S._and_State_Trends_on_Teen_Births.csv")

if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}

ui <- dashboardPage(
  dashboardHeader(title = "Teen Pregnancy Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Exploration", tabName = "data_explore", icon = icon("dashboard")),
      menuSubItem("Average Birth Rate by State", tabName = "average_state"),
        menuSubItem("U.S. Births Rates Over Year", tabName = "us_br_over_year"),
        menuSubItem("Trends in Rate by State", tabName = "trends_rate_state"),
      menuItem("Insights & Conclusions", tabName = "conclusions", icon = icon("flag-checkered"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidPage(
                titlePanel("Teenagers Pregnancy Rate Analysis and Solutions in US"),
                tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQM4N6eOa4Q-pwupp8uMwU0kMuRnLuwRxzVOA&s", height = "300px", width = "500px"),
                h3("Introduction"),
                p("Our goal would be aiming on the trends and determinants of teenage pregnancy rates in each state in the US, focusing on teens between the ages of 15 and 19 years old. By discovering and pairing to different abortion laws and educational conditions in each state, the goal is to reveal the correlation between these factors and the rate of teenage pregnancy."),
                p("As social attitudes toward teen pregnancy become more critical, we believe there is necessary to explore deeper in this social situation. Teenage pregnancy would bring out lots of negative results, for example, affecting teenagers' mental development and physical health etc. We plan to utilize up-to-date and dynamic data from the recent decade to ensure that our research is relevant and current. This approach will allow us to further explore how different laws and educations affect teenage mental and physical health."),
                p("In addition, we hope the project can highlight broader implications for public health and social policy, providing improvements for the development of targeted interventions and solutions. By identifying the key factors that influence teen pregnancy rates, our project aims call on action for policymakers to develop effective strategies to address this challenging social issue. Overall, our main goal is to provide actionable solutions that could help in reduce the teen pregnancy rates, thus improving and ensuring the well-being of teenagers across the United States."),
                a("Here is the dataset.",
                href = "https://www.cdc.gov/nchs/data-visualization/teen-births/index.htm"),
                h3("Our Research Questions"),
                p("1.How have teenage pregnancy rates changed across different states over approximately the past decade, and what can we tell from these trends?"),
                p("2.What is the relationship between state abortion laws and teenage pregnancy rates from different states?"),
                p("3.How do sex education influences teenage pregnancy rates in different states?")
             )
      ),
      tabItem(tabName = "data_explore",
              fluidPage(
                h1("Let's analysis the data"),
                h3("About this Dataset"),
                p("- We directly found and accessed this dataset can from National Center for Health Statistics, a U.S. federal agency responsible for collecting and analyzing health data to provide statistical information that guides public health and policy decisions."),
                p("- We've found that this dataset was collected by NCHS, which it's the part of the CDC, a principle agency in the US."),
                p("- NCHS collects all the data from a variety of health and demographic surveys, along with vital statistics records such as birth certificates. This collection offers extensive information on birth rates, featuring detailed demographic categories that enable trend analysis over time based on race, age group, and Hispanic origin."),
                p("- We believe that their purpose of collecting these teen birth rate data is for monitoring and assessing the teens' health trends to inform public health policy and plans. This dataset would help to evaluate the effectiveness of health interventions for reducing teen pregnancies, also and can influence educational programs and resource allocation for family planning and sex education."),
                h3("Our Implications"),
                p("We think all the results we could get by analyzing this dataset could assist technologists in developing data-driven solutions, such as using big data analysis and AI technology to predict the trends in teenage pregnancy rates and provide more accurate support for policymakers. "),
                p("In addition, technologists can develop online platforms or social media tools to provide teenage health counseling and support services and promote teenage health education and awareness. Secondly, designers can design and improve teenage health education publicity materials according to the research results, making them more attractive and easy to understand, and increasing the participation and acceptance of adolescents."),
                p("In addition, designers can work with educational institutions and community organizations to develop innovative educational games, apps, or virtual reality experiences that stimulate youth interest and engagement in health issues, thereby promoting an understanding of sex education. Also, the research results could provide important references for developing more direct and efficient policy measures to reduce teenage pregnancy rates and ensure the teenage health.")
              )
      ),
      tabItem(tabName = "average_state",
              fluidPage(
                h1("Average Rate"),
                h3("Average Pregnancy Rate By State"),
                titlePanel("Average Teenage Pregnancy Rates"),
                selectInput("selectState", "Select State:", 
                choices = c("All States" = "All", "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"), 
                selected = "All States", 
                multiple = TRUE),
    actionButton("updatePlot", "Update Plot"),
                plotOutput("avgRatesPlot"),
                h3("Summary for the graph"),
                p("- We've chose a bar chart to do this visualization for the average teen birth rates per state because it is particularly effective for comparing categorical data. Each bar represents a state, allowing for straightforward comparisons of their average rates. This visualization is excellent for assessing and comparing the magnitude of teen birth rates across different states at a glance. The orientation and length of the bars make it easy to identify which states have higher or lower averages, highlighting areas that might require more focused interventions or further study. By using distinct colors for each state and rotating the x-axis labels, the chart ensures clarity and readability, enhancing the viewer's ability to digest and interpret the data efficiently. ")
              )
      ),
      tabItem(tabName = "us_br_over_year",
              fluidPage(
              h1("U.S. Birth Rates Over the Years"),
              h3("Explore how birth rates have changed in the U.S. over the years."),
              sliderInput("yearRange", "Select Year Range:", 
                min = 1990, max = 2020, 
                value = c(1990, 2020), step = 1),
                plotOutput("usBirthRatePlot"),
                h3("Summary for the graph"),
                p("- We decided to choose the line and point plot for visualizing the U.S. birth rate over the years primarily because it could effectively and clearly illustrate the trends over time. Line and point plots are excellent for time series data, allowing clear visualization of changes in the birth rate—whether it's rising, declining, or remaining stable. This type of plot provides a continuous view of data, highlighting relationships and movements over the years, which is crucial for understanding demographic dynamics. It can showcase the trends, detect volatility, and identify anomalies, offering a comprehensive overview of how external factors might influence birth rates.")
              )
      ),
      tabItem(tabName = "trends_rate_state",
              fluidPage(
                h1("Trends in Rates by State"),
                h3("Trends in Teenage Pregnancy Rates by State"),
                selectInput("stateInput", "Select a State:", choices = c("All States" = "All", "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")),
                plotOutput("pregnancyTrendsPlot"),
                h3("Summary for the graph"),
                p("-  We believe this line plot would be an ideal for visualizing the trends in teen birth rates by state over time, due to its ability to clearly show changes through connected data points for each state. By incorporating different colors for each state, the plot facilitates easy comparisons, allowing observers to quickly discern which states have higher or lower rates and how these rates evolve relative to each other. This visualization is particularly valuable for identifying trends, fluctuations, and the impacts of different state policies or healthcare access on teen birth rates.")
              )
      ),
      tabItem(tabName = "conclusions",
              fluidPage(
                h1("Conclusions & Takeaways"),
                h3("Takeaway 1:"),
                p("Based on the analysis of the data analyses, we found the Average US teen pregnancy rate. According to the data exposed in the recent few decades teen pregnancy rates have been dragged down and decreased. For instance, in respect of the period from 2000-2020, ever of the teen pregnancy rate has decreased each year. In that, from 62 percent in 1990-1991, in 2019-2020 it has decreased to 18.1 percent. Depending on these successful elements some education and interventions that have been proceeded or organized in the past helped to decrease the teenage pregnancy rate in any way."),
                p("Despite the overall positive trend, there are significant differences in teen pregnancy rates between states. The 2019-2020 teen pregnancy rate was 14.1 in California and 12.7 in New York, but 26.3 in Mississippi and 24.8 in Alabama. California and New York have significantly lower teen pregnancy rates than some southern states such as Mississippi and Alabama. This disparity reflects differences in sex education policies across states, access to contraception, and cultural and socioeconomic factors. Comparing policies and outcomes in different states can provide a reference for policy improvements across the country."),
                h3("Takeaway 2(Main):"),
                p("Further analysis showed that teen pregnancy rates are not determined by a single factor but by many social factors. For example, states with higher rates of teen pregnancy tend to have higher rates of poverty and lower levels of education. These findings suggest that in addition to direct sex education and contraception, improving overall socioeconomic status is an important way to reduce teen pregnancy rates.
Our most important insight from this analysis is that comprehensive sex education and widespread access to contraception play a key role in reducing teen pregnancy rates. However, these measures alone are not enough, and socio-economic factors such as poverty and education levels also need to be taken into account and addressed. This insight highlights the need for policymakers to take a multi-layered, multi-dimensional approach when designing interventions. We suggest ways to further reduce the teen pregnancy rate."),
                h4("Solutions we could think of:"),
                p("1. Implement comprehensive and accurate sex education curricula in schools, including contraception, prevention and control of sexually transmitted diseases, sexual consent, etc. Educate adolescents on how to make informed sexual decisions, including rejecting unsafe sex and recognizing potential risks. Comprehensive sex education can help adolescents make safer and more responsible sexual decisions."),
                p("2. Increase the availability of contraceptive knowledge and resources. Provide free or low-cost contraceptive tools and information through schools, community health centers, and online platforms. Accessible counseling: Professional sexual health counseling services are available so that young people can find support if they need help or advice."),
                p("3. Encourage parents to have an open and honest dialogue with their children and improve parents' knowledge and communication skills about sex education. In addition, building community support networks to increase awareness and prevention of teenage pregnancy through events and workshops."),
                p("4. Develop and implement policies that support the health and rights of adolescents, such as ensuring the quality and coverage of sex education curricula in schools and ensuring the teenagers have adequate funding to use contraceptives. There is a legal need to ensure the protection of adolescents' right to privacy when accessing contraceptive and counseling services."),
                h3("Takeaway 3(Broad Influence)"),
                p("The broad implications of our analysis are that it applies not only to reducing teen pregnancy rates, but also to other public health issues. By integrating educational, economic and health considerations, more effective public health policies can be designed. Such multidimensional strategies can achieve health improvements across a wider population, thereby enhancing overall social well-being."),
                p("With these specific analyses and insights, we can provide strong support and recommendations for future policy development and interventions to help further reduce teen pregnancy rates and improve adolescent health in the United States.")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(read_csv("NCHS_-_U.S._and_State_Trends_on_Teen_Births.csv"))
  })

  observe({
    updateSelectInput(session, "selectState", choices = c("All" = "All", unique(data()$State)))
    updateSelectInput(session, "stateInput", choices = c("All" = "All", unique(data()$State)))
  })
  
  output$pregnancyTrendsPlot <- renderPlot({
    df <- req(data())
    if (input$stateInput != "All") {
      df <- df[df$State == input$stateInput, ]
    }
    ggplot(df, aes(x = Year, y = `State Rate`, color = State)) +
      geom_line() +
      labs(title = paste("Trends in Teenage Pregnancy Rates by State:", input$stateInput),
           x = "Year", y = "Teenage Pregnancy Rate") +
      theme_minimal()
  })

  average_rates <- reactive({
    df <- req(data())
    if (input$selectState != "All") {
      df <- df %>% filter(State %in% input$selectState)
    }
    df %>%
      group_by(State) %>%
      summarise(AverageRate = mean(`State Rate`, na.rm = TRUE))
  })

  observeEvent(input$updatePlot, {
    output$avgRatesPlot <- renderPlot({
      df_avg <- req(average_rates())
      ggplot(df_avg, aes(x = reorder(State, -AverageRate), y = AverageRate, fill = State)) +
        geom_bar(stat = "identity") +
        labs(title = "Average Teenage Pregnancy Rates by State", x = "State", y = "Average Pregnancy Rate") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  })
  output$usBirthRatePlot <- renderPlot({
    df <- data()
    filtered_df <- df %>% filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
    ggplot(filtered_df, aes(x = Year, y = `U.S. Birth Rate`)) +
      geom_line() + 
      geom_point() + 
      labs(title = "U.S. Birth Rate Over the Years", x = "Year", y = "Birth Rate (per 1,000 people)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

