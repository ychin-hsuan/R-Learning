library(shiny)

ui <- fluidPage(
  titlePanel("東華大學課表 .pdf 轉 .ics"),
  sidebarLayout(
    sidebarPanel(
      fileInput("pdf_file", "上傳你的課表 PDF", accept = ".pdf"),
      downloadButton("download_ics", "下載行事曆 (.ics)")
    ),
    mainPanel(
      verbatimTextOutput("status")
    )
  )
)
