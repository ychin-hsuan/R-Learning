library(shiny)
library(pdftools)
library(stringr)
library(lubridate)
library(ical)

server <- function(input, output) {
  # 讀取 PDF 並解析內容
  parsed_data <- reactive({
    req(input$pdf_file)
    text <- pdf_text(input$pdf_file$datapath)
    
    lines <- unlist(strsplit(text, "\n"))
    schedule <- list()
    
    for (line in lines) {
      if (str_detect(line, "[0-9]{2}:[0-9]{2}")) {  # 偵測時間
        parts <- unlist(strsplit(line, " "))
        time <- parts[1]
        course <- paste(parts[-1], collapse = " ")
        schedule <- append(schedule, list(list(time = time, course = course)))
      }
    }
    
    return(schedule)
  })
  
  # 產生 .ics 檔案
  output$download_ics <- downloadHandler(
    filename = function() { "schedule.ics" },
    content = function(file) {
      schedule <- parsed_data()
      
      ics_content <- "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//My Schedule//EN\n"
      
      for (entry in schedule) {
        time_match <- str_match(entry$time, "(\\d{2}):(\\d{2})")
        if (is.na(time_match[1])) next 
        
        start_hour <- as.numeric(time_match[2])
        start_minute <- as.numeric(time_match[3])
        
        start_time_local <- ymd_hms(sprintf("2025-03-25 %02d:%02d:00", start_hour, start_minute), tz = "Asia/Taipei")
        start_time_utc <- with_tz(start_time_local, "UTC")
        
        end_time_local <- start_time_local + hours(1)
        end_time_utc <- with_tz(end_time_local, "UTC")
        
        event_summary <- trimws(entry$course)
        if (event_summary == "") next 
        
        ics_event <- paste0(
          "BEGIN:VEVENT\n",
          "DTSTART:", format(start_time_utc, "%Y%m%dT%H%M%SZ"), "\n",
          "DTEND:", format(end_time_utc, "%Y%m%dT%H%M%SZ"), "\n",
          "SUMMARY:", event_summary, "\n",
          "END:VEVENT\n"
        )
        
        ics_content <- paste0(ics_content, ics_event)
      }
      
      ics_content <- paste0(ics_content, "END:VCALENDAR")
      
      writeLines(ics_content, file)
    }
  )
  
  output$status <- renderText({
    if (is.null(input$pdf_file)) {
      "請上傳 PDF 課表"
    } else {
      paste("成功解析課表，共", length(parsed_data()), "堂課")
    }
  })
}
