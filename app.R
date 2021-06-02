setwd("D:/바탕 화면/SKKU/4-1/대용량자료관리시각화/Project/shiny_project")
library(shiny)
library(tidyverse)
library(ggplot2)


# Data pre-process
# 조선시대 data
choseon_unique <- read.csv("data/choseon_unique.csv")

choseon_state <- c("경기", "충청", "전라", "경상", "강원", "황해", "영안", "함경", "평안")

count_state_num <- 
  choseon_unique %>%
  dplyr::select(번역) %>%
  str_match_all(choseon_state)

names(count_state_num) <- choseon_state

choseon_count_state <- data.frame(count = lengths(count_state_num))
choseon_count_state$region <- row.names(choseon_count_state)

choseon_count_state



# 오늘날

# 1st data
count_state_1978 <-
  admin_district_EQK1978to2015 %>%
  dplyr::select(행정구역) %>%
  str_match_all(today_state)

names(count_state_1978) <- today_state

count_state_1978to2015 <- data.frame(count = lengths(count_state_1978))
count_state_1978to2015


# 2nd data
count_state_2015 <-
  admin_district_EQK2015to2017 %>%
  dplyr::select(행정구역) %>%
  str_match_all(today_state)

names(count_state_2015) <- today_state

count_state_2015to2017 <- data.frame(count = lengths(count_state_2015))
count_state_2015to2017


# 3rd data
count_state_2018 <-
  admin_district_EQK2018to2021 %>%
  dplyr::select(행정구역) %>%
  str_match_all(today_state)

names(count_state_2018) <- today_state

count_state_2018to2021 <- data.frame(count = lengths(count_state_2018))
count_state_2018to2021


# Final 오늘날 data
count_state_1978to2021 <- count_state_1978to2015 + count_state_2015to2017 + count_state_2018to2021

count_state_1978to2021$region <-
  row.names(count_state_1978to2021)

count_state_1978to2021


# -----------------------------------------------------------------------------

# plot
choseon_p <- ggplot(choseon_count_state, aes(x = region, y = count))+
  geom_col(fill = 'lightblue2') + 
  theme_minimal() +
  ylim(0, max(choseon_count_state$count*1.25))+
  geom_text(aes(label = count), vjust = -0.3, size = 4) +
  ggtitle("행정구역별 지진 발생 횟수", "조선시대(1392-1493)\n") +
  xlab("\n조선시대 행정구역") + ylab("지진발생횟수") +
  theme(
    plot.title = element_text(size = 21, face = 'bold'),
    plot.subtitle = element_text(size = 19, face = 'bold'),
    axis.title = element_text(size = 18, face = "bold", color = 'darkgrey'),
    axis.text.x = element_text(colour = 'mediumblue', face='bold')
  )

today_p <- 
  ggplot(count_state_1978to2021, aes(x = region, y = count))+
  geom_col(fill = 'lightgreen', width = 0.8) + 
  theme_minimal() +
  ylim(0, max(count_state_1978to2021$count*1.25))+
  # xlim()
  geom_text(aes(label = count), vjust = -0.3, size = 4) +
  ggtitle("행정구역별 지진 발생 횟수", "1978년 ~ 2021년\n") +
  xlab("\n오늘날 행정구역") + ylab("지진발생횟수") +
  theme(
    plot.title = element_text(size = 20, face = 'bold'),
    plot.subtitle = element_text(size = 18, face = 'bold'),
    axis.title = element_text(size = 18, face = "bold", color = 'darkgrey'),
    axis.text.x = element_text(colour = 'darkgreen', face='bold')
  )

# list of graph
graph_list = list(choseon_p, today_p)


### ===============================================================




# Define UI ----
ui <- fluidPage(
  titlePanel("시기별 지진 발생 양상"),
  
  sidebarLayout(
    sidebarPanel(
      # helptext 위젯
      helpText("조선시대(1392-1493)과 오늘날(1978-2021)의 행정구역별 지진발생횟수 정보를 볼 수 있습니다."),
      
      # selectbox 위젯
      selectInput("selecthist",
                  label = "시기 선택",
                  choices = c("1.조선시대(1392-1493)",
                              "2.오늘날(1978-2021)"),
                  selected = "조선시대(1392-1493)")
      
    ),
    
    mainPanel(plotOutput("hist"))
    
  )
  
)



# Define server logic ----
server <- function(input, output) {
  ind <- reactive({
    
    as.numeric(substr(input$selecthist,1,1))
    
  })
  
  
  output$hist <- renderPlot({
    graph_list[ind] + theme(axis.text.x = element_text(color = "mediumblue", face = "bold", size = 13))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
