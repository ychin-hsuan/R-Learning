
# 安裝並載入必要的套件
install.packages("pdftools")
install.packages("tidyverse")
library(pdftools)
library(tidyverse)

# 指定 PDF 檔案的路徑
pdf_file <- "個人課表.pdf"  # 將 "your_file.pdf" 替換為您的檔案名稱

# 從 PDF 中提取文字
text <- pdf_text(pdf_file)

# 由於 pdf_text 會將每一頁作為一個元素返回，我們假設課表在第一頁
text <- text[[1]]

# 將文字分割成行
lines <- str_split(text, "\n")[[1]]

# 檢視提取的文字
print(lines)

# 範例：清理和提取資料 (這部分需要根據您的 PDF 輸出來調整)
# 假設課表資料從第 X 行開始，到第 Y 行結束
start_row <- 8  # 根據您的 PDF 調整
end_row <- 22    # 根據您的 PDF 調整

schedule_lines <- lines[start_row:end_row]

# 將文字轉換為表格 (這是一個簡化的範例，您可能需要更複雜的邏輯)
schedule_data <- str_split_fixed(schedule_lines, "\\s{2,}", n = 8)  # 使用兩個或更多空格分割
schedule_df <- as.data.frame(schedule_data, stringsAsFactors = FALSE)
colnames(schedule_df) <- c("節次(時間)", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")

print(schedule_df)




# 載入 lubridate 以便於處理日期和時間
install.packages("lubridate")
library(lubridate)

# 輔助函數：將星期幾轉換為對應的日期 (需要根據您的課表開始日期進行調整)
get_dates_for_weekdays <- function(start_date, weekdays = c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")) {
  dates <- c()
  for (i in seq_along(weekdays)) {
    day_of_week <- weekdays[i]
    if (!is.na(day_of_week) && day_of_week != "") {
      day_num <- which(c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日") == day_of_week)
      dates <- c(dates, start_date + days(day_num - wday(start_date, week_start = 1)))
    } else {
      dates <- c(dates, NA)
    }
  }
  return(dates)
}

# 假設您的學期開始於 2024 年 9 月 2 日 (星期一)
start_date <- as.Date("2024-09-02")  # 請根據您的實際開始日期更改
dates_of_week <- get_dates_for_weekdays(start_date)

# 輔助函數：將時間字串轉換為開始和結束時間
parse_time_slot <- function(time_str) {
  times <- str_match(time_str, "(\\d{2}):(\\d{2})-(\\d{2}):(\\d{2})")
  if (!is.na(times[1])) {
    start_time <- paste(times[2], times[3], sep = "")
    end_time <- paste(times[4], times[5], sep = "")
    return(c(start_time, end_time))
  } else {
    return(c(NA, NA))
  }
}

# 產生 ICS 內容
ics_content <- c(
  "BEGIN:VCALENDAR",
  "VERSION:2.0",
  "PRODID:-//YourOrganization//YourAppName//EN",
  "CALSCALE:GREGORIAN"
)

for (row in 1:nrow(schedule_df)) {
  time_slot <- schedule_df[row, 1]
  times <- parse_time_slot(time_slot)
  start_time <- times[1]
  end_time <- times[2]
  
  for (col in 2:ncol(schedule_df)) {
    day_of_week <- colnames(schedule_df)[col]
    course_info <- schedule_df[row, col]
    
    if (!is.na(course_info) && course_info != "") {
      # 從課程資訊中提取課程名稱和地點
      course_details <- str_split(course_info, "\n")[[1]]
      course_name <- course_details[1]
      location <- course_details[length(course_details)]  # 假設地點在最後一行
      
      event_date <- dates_of_week[col - 1]
      if (!is.na(event_date)) {
        dtstart <- paste0(format(event_date, "%Y%m%d"), "T", start_time, "00")
        dtend <- paste0(format(event_date, "%Y%m%d"), "T", end_time, "00")
        uid <- paste0(digest::digest(paste(course_name, dtstart, dtend)), "@yourdomain.com")  # 產生唯一ID
        
        ics_content <- c(ics_content,
                         "BEGIN:VEVENT",
                         paste0("UID:", uid),
                         paste0("DTSTAMP:", format(Sys.time(), "%Y%m%dT%H%M%SZ")),
                         paste0("DTSTART:", dtstart),
                         paste0("DTEND:", dtend),
                         paste0("SUMMARY:", course_name),
                         paste0("LOCATION:", location),
                         "END:VEVENT")
      }
    }
  }
}

ics_content <- c(ics_content, "END:VCALENDAR")

# 將 ICS 內容寫入檔案
writeLines(ics_content, "schedule.ics")