setwd("D:/바탕 화면/SKKU/4-1/대용량자료관리시각화/Project/shiny_proj_agn")


# Load packages

library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)

# Load Data

admin_district_EQK1978to2015 <- fread("data/admin_district_EQK1978to2015.csv")
admin_district_EQK2015to2017 <- fread("data/admin_district_EQK2015to2017.csv")
admin_district_EQK2018to2021 <- fread("data/admin_district_EQK2018to2021.csv")

# Explore data
# str(admin_district_EQK1978to2015)
# str(admin_district_EQK2015to2017)
# str(admin_district_EQK2018to2021)


final1978 <-
  admin_district_EQK1978to2015 %>%
  rename(시간 = 발생시각) %>%
  dplyr::select(c(3,4,6,7,8))

final2015 <-
  admin_district_EQK2015to2017 %>%
  dplyr::select(c(2,3,5,6,7))

final2018 <-
  admin_district_EQK2018to2021 %>%
  dplyr::select(c(2,3,5,6,7))



# combine 3 data 
combine1978to2015 <- rbind(final1978, final2015)

fin_df1 <-
  combine1978to2015 %>%
  separate(시간, c("날짜", "시각"), 10) %>%
  separate(위도, c("lat", NA), -1) %>%
  separate(경도, c("lon", NA), -1) %>%
  dplyr::select(-시각)

fin_df2 <-
  final2018 %>%
  separate(시간, c("날짜", "시각"), 10) %>%
  separate(위도, c("lat", NA), -1) %>%
  separate(경도, c("lon", NA), -1) %>%
  dplyr::select(-시각)

# final data
totaldata <- rbind(fin_df1, fin_df2)


# --------------------------------------
# 지역따라 데이터

district_data <- 
  totaldata %>%
  group_by(날짜, 행정구역) %>%
  summarise(how_many = n())




# =============================================

# Define UI ----
ui <- fluidPage(
  titlePanel("날짜별 행정구역의 지진 발생 횟수"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(p("날짜를 조절하여 행정구역별 지진 발생 횟수를 확인하세요. 1978년 8월 30일부터 2021년 4월 17일까지의 행정구역별 지진 발생 횟수 총합 데이터를 볼 수 있습니다."),
               em("TIPS: 상세한 변화 추이를 눈으로 보고 싶다면, 날짜 구간의 폭을 5-10년 정도로 좁혀보세요!")),
      
      
      dateRangeInput("daterange", "Date Range : ",
                     start = min(district_data$날짜),
                     end = max(district_data$날짜))
      
    ),
    
    mainPanel(
      plotOutput("plot1")
    )
  )
)


# Define server logic ----
server <- function(input, output) {
  

  
  dateRangeInput<-reactive({
    dataset <- subset(district_data, 날짜 >= input$daterange[1] & 날짜 <= input$daterange[2])
    dataset
  })
  

  

  output$plot1 <- renderPlot({
    
    
    ggplot(data = dateRangeInput(), aes(x = 행정구역, y = how_many)) +
      geom_col(fill = 'lightblue2') +
      theme_minimal() +
      #ylim(0, max(district_data$how_many*10))+
      ggtitle("행정구역별 지진 발생 횟수") +
      xlab("행정구역") + ylab("지진발생횟수") +
      theme(
        plot.title = element_text(size = 21, face = 'bold'),
        axis.title = element_text(size = 18, face = "bold", color = 'darkgrey'),
      )
    
  })
  

  
}

# Run the app ----
shinyApp(ui = ui, server = server)


