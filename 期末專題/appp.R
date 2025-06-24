install.packages(c("shiny", "shinydashboard", "DT", "lubridate"))
install.packages("shinyTime")

library(shiny)
library(shinyTime)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "打工薪資預估工具"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("薪資紀錄", tabName = "records", icon = icon("table")),
      menuItem("統計圖表", tabName = "charts", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "records",
              fluidRow(
                box(width = 4, title = "輸入資訊", status = "primary", solidHeader = TRUE,
                    numericInput("hourly_rate", "💰 時薪（元）", value = 190, min = 0),
                    numericInput("ot1_rate", "1.33 倍加班倍數", value = 1.33),
                    numericInput("ot2_rate", "1.66 倍加班倍數", value = 1.66),
                    numericInput("ot3_rate", "2.66 倍加班倍數（第六天）", value = 2.66),
                    dateInput("date", "📅 日期", value = Sys.Date()),
                    timeInput("start_time", "🕘 上班時間", value = strptime("09:00", "%H:%M")),
                    timeInput("break_start", "🍱 外出時間", value = strptime("12:00", "%H:%M")),
                    timeInput("break_end", "🍵 返回時間", value = strptime("13:00", "%H:%M")),
                    timeInput("end_time", "🕔 下班時間", value = strptime("18:00", "%H:%M")),
                    selectInput("edit_mode", "✏️ 模式", choices = c("新增", "編輯")),
                    actionButton("submit", "✅ 執行"),
                    downloadButton("downloadData", "📥 下載 CSV")
                ),
                box(width = 8, title = "紀錄", status = "info", solidHeader = TRUE,
                    tableOutput("records"),
                    verbatimTextOutput("total_hours"),
                    verbatimTextOutput("total_salary")
                )
              )
      ),
      tabItem(tabName = "charts",
              fluidRow(
                box(width = 6, title = "📊 每週薪資統計", status = "success", solidHeader = TRUE, plotOutput("weekly_plot")),
                box(width = 6, title = "📈 每月薪資統計", status = "warning", solidHeader = TRUE, plotOutput("monthly_plot"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  records <- reactiveVal(data.frame(
    日期 = character(),
    上班 = character(),
    下班 = character(),
    `外出-返回` = character(),
    工作時數 = numeric(),
    薪資 = numeric(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$submit, {
    date <- input$date
    date_str <- format(date, "%Y-%m-%d")
    
    start <- as.POSIXct(paste(date, format(input$start_time, "%H:%M:%S")))
    break_start <- as.POSIXct(paste(date, format(input$break_start, "%H:%M:%S")))
    break_end <- as.POSIXct(paste(date, format(input$break_end, "%H:%M:%S")))
    end <- as.POSIXct(paste(date, format(input$end_time, "%H:%M:%S")))
    
    work_seconds <- as.numeric(difftime(end, start, units = "secs")) -
      as.numeric(difftime(break_end, break_start, units = "secs"))
    work_hours <- round(work_seconds / 3600, 2)
    
    week_day_num <- wday(date, week_start = 1)
    
    hourly <- input$hourly_rate
    ot1 <- input$ot1_rate
    ot2 <- input$ot2_rate
    ot3 <- input$ot3_rate
    
    pay <- 0
    if (week_day_num < 6) {
      if (work_hours <= 8) {
        pay <- work_hours * hourly
      } else if (work_hours <= 10) {
        pay <- 8 * hourly + (work_hours - 8) * hourly * ot1
      } else if (work_hours <= 12) {
        pay <- 8 * hourly + 2 * hourly * ot1 + (work_hours - 10) * hourly * ot2
      } else {
        pay <- 8 * hourly + 2 * hourly * ot1 + 2 * hourly * ot2 + (work_hours - 12) * hourly * ot3
      }
    } else if (week_day_num == 6) {
      if (work_hours <= 2) {
        pay <- work_hours * hourly * ot1
      } else if (work_hours <= 8) {
        pay <- 2 * hourly * ot1 + (work_hours - 2) * hourly * ot2
      } else {
        pay <- 2 * hourly * ot1 + 6 * hourly * ot2 + (work_hours - 8) * hourly * ot3
      }
    } else {
      pay <- work_hours * hourly * ot3
    }
    
    new_record <- data.frame(
      日期 = date_str,
      上班 = format(start, "%H:%M"),
      下班 = format(end, "%H:%M"),
      `外出-返回` = paste0(format(break_start, "%H:%M"), "-", format(break_end, "%H:%M")),
      工作時數 = work_hours,
      薪資 = round(pay, 0),
      stringsAsFactors = FALSE
    )
    
    current <- records()
    
    if (input$edit_mode == "新增") {
      if (date_str %in% current$日期) {
        showModal(modalDialog("❗ 此日期已存在紀錄，請切換為編輯模式或選擇其他日期。", easyClose = TRUE))
        return()
      }
      records(rbind(current, new_record))
    } else {
      current <- current[current$日期 != date_str, ]
      records(rbind(current, new_record))
    }
  })
  
  output$records <- renderTable(records())
  
  output$total_hours <- renderText({
    paste("🕒 總工作時數：", sum(records()$工作時數), "小時")
  })
  
  output$total_salary <- renderText({
    paste("💴 總預估薪資：", sum(records()$薪資), "元")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("工作紀錄_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- records()
      df_out <- rbind(df, data.frame(
        日期 = "總計", 上班 = "", 下班 = "", `外出-返回` = "",
        工作時數 = sum(df$工作時數), 薪資 = sum(df$薪資),
        stringsAsFactors = FALSE
      ))
      write.csv(df_out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$weekly_plot <- renderPlot({
    df <- records()
    if (nrow(df) == 0) return()
    df$週次 <- isoweek(as.Date(df$日期))
    df %>%
      group_by(週次) %>%
      summarise(總薪資 = sum(薪資)) %>%
      ggplot(aes(x = factor(週次), y = 總薪資)) +
      geom_bar(stat = "identity", fill = "#4e79a7") +
      labs(title = "每週薪資統計", x = "週次", y = "薪資") +
      theme_minimal()
  })
  
  output$monthly_plot <- renderPlot({
    df <- records()
    if (nrow(df) == 0) return()
    df$月份 <- format(as.Date(df$日期), "%Y-%m")
    df %>%
      group_by(月份) %>%
      summarise(總薪資 = sum(薪資)) %>%
      ggplot(aes(x = 月份, y = 總薪資)) +
      geom_col(fill = "#f28e2b") +
      labs(title = "每月薪資統計", x = "月份", y = "薪資") +
      theme_minimal()
  })
}

shinyApp(ui, server)
