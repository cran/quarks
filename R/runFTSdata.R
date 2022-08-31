#' Application for downloading data from Yahoo Finance
#'
#' @export
#' @import shiny
#' @importFrom shinyjs 'useShinyjs'
#' @import dygraphs
#' @import ggplot2
#'
#' @return
#' None

runFTSdata <- function() {
  options(show.error.messages = FALSE)
  app <-
    list(ui = fluidPage(useShinyjs(),
                              sidebarLayout(
                                sidebarPanel(
                                  selectizeInput("ticker",
                                                 label = "Enter a ticker symbol or name",
                                                 choices = NULL,
                                                 options = list(create = TRUE)),

                                  dateRangeInput("Date",
                                                 "Period",
                                                 start = Sys.Date()-750,
                                                 end = Sys.Date()),

                                  br(),
                                  br(),
                                  # p(actionButton("loadData", "Load", icon = icon("r-project")),
                                  #   br(),
                                  #   "Selected data is loaded into global R-environment.",
                                  #   style = "font-family: 'consolas'; font-si16pt"),
                                  # br(),
                                  # br(),
                                  p(downloadButton("downloadData", "Save", icon = icon("floppy-disk")),
                                    br(),
                                    "Save selected data on local hard drive.",
                                    style = "font-family: 'consolas'; font-si16pt"),
                                  br(),
                                  br(),
                                  p(actionButton("closeApp", "Close", icon = icon("power-off")),
                                    br(),
                                    "Close Application.",
                                    style = "font-family: 'consolas'; font-si16pt"),
                                  br()
                                ),

                                mainPanel(br(),
                                          br(),
                                          dygraphOutput("plot1"),
                                          br(),
                                          br(),
                                          br(),
                                          dygraphOutput("plot2"))
                              )
  ),
  server = function(input, output, session){
    updateSelectizeInput(session, "ticker",
                         choices = c(paste0(ticker_stocks$Ticker[-1], " (", ticker_stocks$Name[-1], ")"),
                                     paste0(ticker_indices$Ticker, " (", ticker_indices$Name, ")")),
                         selected = '',
                         server = TRUE)

    dataInput <- reactive({
      req(input$ticker)
      req(input$Date[1])
      req(input$Date[2])
      ticker = strsplit(input$ticker, " ")[[1]][1]
      suppressWarnings(
        suppressMessages(
          yfR::yf_get(tickers = ticker, first_date = input$Date[1],
                      last_date = input$Date[2], freq_data = "daily",
                      cache_folder = file.path(tempdir(), 'BGS_Cache'))
        )
      )
    })


    output$plot1 <- renderDygraph({
      req(input$ticker)
      req(input$Date[1])
      req(input$Date[2])
      req(dataInput())
      dat = dataInput()
      plot_data = xts::xts(dat$price_close, dat$ref_date)
      dygraphs::dygraph(plot_data, main = paste(input$ticker, "- Closing prices (daily)"),
                        ylab = "X<sub>t</sub>", group = "FZS") %>%
        dygraphs::dyUnzoom()
    })

    output$plot2 <- renderDygraph({
      req(input$ticker)
      req(input$Date[1])
      req(input$Date[2])
      req(dataInput())

      dat2 = dataInput()[-1, ]
      plot_data2 = xts::xts(dat2$ret_closing_prices, dat2$ref_date)
      dygraphs::dygraph(plot_data2, main = paste(input$ticker, "- Log-returns (daily)"),
                        ylab = "Y<sub>t</sub>", xlab = "Daily observations (t)", group = "FZS") %>%
        dygraphs::dyRangeSelector(height = 20) %>%
        dygraphs::dyUnzoom()
    })

    # observeEvent(input$loadData, {
    #   data_raw = dataInput()
    #   ticker = strsplit(input$ticker, " ")[[1]][1]
    #   if (length(strsplit(ticker, "^", fixed = TRUE)[[1]]) != 1) {
    #     ticker = strsplit(ticker, "^", fixed = TRUE)[[1]][2]
    #   }
    #   assign(ticker, data_raw)#, envir = globalenv())
    #   cat("", fill = TRUE)
    #   cat("Data for", input$ticker, "loaded as object", ticker, "in global environment.", fill = TRUE)
    #   cat("", fill = TRUE)
    # })

    output$downloadData <- downloadHandler(
      filename = function() {
        ticker = strsplit(input$ticker, " ")[[1]][1]
        if (length(strsplit(ticker, "^", fixed = TRUE)[[1]]) != 1) {
          ticker = strsplit(ticker, "^", fixed = TRUE)[[1]][2]
        }
        paste0(ticker, ".csv")
      },
      content = function(file) {
        utils::write.csv(dataInput(), file, row.names = FALSE)
      }
    )
    observeEvent(input$closeApp, {
      stopApp()
    })
    }
)
shiny::runApp(app, display.mode = 'normal')
}
