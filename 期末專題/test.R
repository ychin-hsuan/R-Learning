
install.packages("pdftools")
install.packages("readr")
install.packages("rstudioapi")
install.packages("lubridate")

# 安裝必要套件（只需要安裝一次）
# 安裝必要套件（只需要安裝一次）
install.packages(c("pdftools", "readr", "stringr"))
library(pdftools)
library(readr)
library(stringr)

# 1. 讀取 PDF 檔案
pdf_file <- "個人課表.pdf" # 改成你的檔案路徑
pdf_text_data <- pdf_text(pdf_file)

# 2. 將PDF每頁內容以行為單位切開
lines <- strsplit(pdf_text_data, "\n")[[1]]

# 3. 找出表格開始的地方
# 範例中通常會有「節次(時間) 星期一 星期二 ...」這種字眼，找到它
start_index <- which(grepl("節次", lines))

# 只保留課表部分的資料（課表結束後通常會有備註，不需要）
table_lines <- lines[(start_index + 1):length(lines)]

# 4. 手動設定星期欄
weekdays <- c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")

# 5. 手動設定節次時間
periods <- c("06:10~07:00", "07:10~08:00", "08:10~09:00", 
             "09:10~10:00", "10:10~11:00", "11:10~12:00",
             "12:10~13:00", "13:10~14:00", "14:10~15:00",
             "15:10~16:00", "16:10~17:00", "17:10~18:00",
             "18:10~19:00", "19:10~20:00", "20:10~21:00",
             "21:10~22:00")

# 6. 準備儲存課表的資料框
timetable <- data.frame(
  節次時間 = periods,
  星期一 = rep("", length(periods)),
  星期二 = rep("", length(periods)),
  星期三 = rep("", length(periods)),
  星期四 = rep("", length(periods)),
  星期五 = rep("", length(periods)),
  星期六 = rep("", length(periods)),
  星期日 = rep("", length(periods)),
  stringsAsFactors = FALSE
)

# 7. 逐行解析，填入課表
current_period <- 0

for (line in table_lines) {
  line <- str_trim(line)  # 去除前後空白
  if (line == "") next  # 空行跳過
  
  # 如果是節次時間 (有小括號時間)
  if (grepl("\\(.*\\)", line)) {
    current_period <- current_period + 1
    next
  }
  
  # 如果是課程內容：根據位置大概估計星期
  # 假設不同課程是以固定間距排列的（簡單估算）
  courses <- str_split_fixed(line, "\\s{2,}", n = 8) # 以2個以上空白切割
  
  if (ncol(courses) >= 2) {
    for (i in 2:8) { # 從星期一到星期日
      content <- str_trim(courses[i])
      if (content != "") {
        # 抓取第一行（如果有換行符號）
        first_line <- strsplit(content, "\\s")[[1]][1]
        timetable[current_period, i] <- first_line
      }
    }
  }
}

# 8. 刪除整行都是空白的節次（如果有）
timetable <- timetable[rowSums(timetable[, -1] != "") > 0, ]

# 9. 輸出CSV
write_csv(timetable, "課表轉換結果.csv")

cat("✅ 成功轉成課表 CSV 檔案！存成 '課表轉換結果.csv'。\n")
