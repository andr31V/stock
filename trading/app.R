#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("quantmod")
library(tidyverse)
library(rvest)
library(DT)
library(shinycssloaders)

library(jsonlite)

horizon <- "DAILY"#"INTRADAY"
symbol <- "VOO"
interval <- ""#"&interval=1min"
#only for intraday, allowable: 1-5-15-30-60min

scrty <- fromJSON(
    paste0(
        "https://www.alphavantage.co/query?function=TIME_SERIES_",
        horizon,
        "&symbol=",
        symbol,
        interval,
        "&outputsize=full&apikey=SEZGQTSOZXF24IBJ"
        )
    ) %>% 
    .[[2]] 
df <- data.frame(as.POSIXct(names(scrty),tz="America/New_York"),matrix(unlist(scrty), nrow=length(scrty), byrow=T),stringsAsFactors = FALSE) %>% 
    mutate_if(is.character,as.numeric) %>% 
    arrange(.[[1]])

l <- tsla %>% 
    .[[2]] 

df <- data.frame(as.POSIXct(names(l),tz="America/New_York"),matrix(unlist(l), nrow=length(l), byrow=T),stringsAsFactors = FALSE) %>% 
    mutate_if(is.character,as.numeric) %>% 
    arrange(.[[1]]) #%>% 
    #mutate(min=row_number()) %>% 
    #select(min,everything())

colnames(df) <- c(#"minute",
    "date","open","high","low","close","vol")

qxts <- xts(df[,-1], order.by=df[,1])
#chart_Series(qxts)
macd = MACD(df[,"close"], nFast=12, nSlow=26,nSig=9,maType=SMA, percent = FALSE)

chartSeries(qxts, TA="addMACD()")

ggplot(dat=df) +
    geom_point(aes(minute,close,size=vol))

getSymbols("GS")
chartSeries(GS) 

#should make these user interactive/inputable
qt <- c("TSLA","GILD","GRVY","FXI","SH","TTWO","BP","IMLFF","SPXS","ERY","SMDD","SDOW")
cb <- c(265.77,74.37,22.93,34.36,31.61,102.25,39.53,.45,17.02,117.65,10.78,51.67)

dat <- data.frame(stock=character(),quote=numeric())

#for(i in 1:length(qt)) {
#
#html <- paste0("https://finance.yahoo.com/quote/",qt[i],"?p=",qt[i],"&.tsrc=fin-srch") %>% 
#    read_html() %>% 
#    html_nodes(xpath = "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div/div[4]/div/div/div/div[3]/div[1]/div/span[1]") %>% 
#    html_text()
#dat <- dat %>% 
#    bind_rows(data.frame(stock=qt[i],quote=as.numeric(html)))
#}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Portfolio Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton('rfrsh','Refresh')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("quotes") %>% withSpinner(color="#1A4C64")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    tbl <- eventReactive(input$rfrsh,{
        tbl <- dat
        for(i in 1:length(qt)) {
            
            html <- paste0("https://finance.yahoo.com/quote/",qt[i],"?p=",qt[i],"&.tsrc=fin-srch") %>% 
                read_html() %>% 
                html_nodes(xpath = "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div/div[4]/div/div/div/div[3]/div[1]/div/span[1]") %>% 
                html_text()
            tbl <- tbl %>% 
                bind_rows(data.frame(stock=qt[i],quote=as.numeric(html)))
        }
        tbl
    })
    
    
    output$quotes <- renderDataTable({
        tbl <- tbl() %>% 
            mutate(cost=cb) %>% 
            mutate(diff=round(quote-cost,2),flag=if_else(diff<0,1,0))
        
        datatable(tbl, options=list(columnDefs = list(list(visible=FALSE, targets=5)))
                  ) %>% 
            formatStyle(
                'diff', 'flag',
                color = styleEqual(1, 'red')
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
