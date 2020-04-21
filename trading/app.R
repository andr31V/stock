#
# Let's create a model to take advantage of price swings, e.g.
# 3% daily sustained (1 week?) upswing would suggest we should have bought a stock
# create binary variable (1|0) that flags buys (consider one for sales too)
# Autoregression, logit, probit, neural networks
# Start by just flagging swings using MACD
# Add/Investigate RSI
# Start building a predictive model and check assumptions

library(shiny)
library("shinyWidgets")
library("quantmod")
library(tidyverse)
library(tidyquant)
library(rvest)
library(DT)
library(shinycssloaders)
library("RcppRoll")
library(jsonlite)
library(data.table)
library(zoo)
library(plotly)

library("BatchGetSymbols")

options(scipen=999)

Sys.setenv(TZ='EST')

#horizon <- "DAILY"
##"INTRADAY"
#symbol <- "TSLA"
#interval <- ""
#  #"&interval=60min"
##only for intraday, allowable: 1-5-15-30-60min
#
#scrty <- fromJSON(
#    paste0(
#        "https://www.alphavantage.co/query?function=TIME_SERIES_",
#        horizon,
#        "&symbol=",
#        symbol,
#        interval,
#        "&outputsize=full&apikey=SEZGQTSOZXF24IBJ"
#        )
#    ) %>% 
#    .[[2]] 
#df <- data.frame(as.POSIXct(names(scrty),tz="America/New_York"),matrix(unlist(scrty), nrow=length(scrty), byrow=T),stringsAsFactors = FALSE) %>% 
#    mutate_if(is.character,as.numeric) %>% 
#    arrange(.[[1]]) 
#
#colnames(df) <- c("date","open","high","low","close","vol")

ticks <- c("TSLA","GILD","GRVY")#,"FXI","SH","TTWO","BP","IMLFF","SPXS","ERY","SMDD","SDOW")


market <- GetSP500Stocks()  
market %>% distinct(GICS.Sector) %>% .[[1]] -> sectors 
market %>% distinct(GICS.Sub.Industry) %>% .[[1]] -> industries 

tickers <- unique(c(ticks,market$Tickers))


#add buy/sell signals strat: https://www.dailyfx.com/forex/education/trading_tips/daily_trading_lesson/2020/01/09/macd-histogram.html
## strategy: check last 5 slope and if we are increasing (decreasing) and max (min) value for buy (sell)
#last_n <- 5


## Sustained Return Trend Analysis ##

#y <- 2020
#per <- 12 #5 hours or 5 days of sustained increase/decrease
#del <- .03 #delta cutoff, e.g. 3% daily swings
#
#algo <- df %>% 
#    filter(year(date)==y) %>% 
#    mutate(delta=close/lag(close)-1) %>% 
#    drop_na() 
##algo %>% filter(delta>0)
#
##cutoff .375 and .625???
#leads <- shift(algo$delta,n=1:per, type = "lead")
#return <- leads[[1]] + 1
#for (i in 2:per) {
#  return <- return*(leads[[i]]+1) #multiply first element of each list to each other, e.g. leads1-5
#}
#
#algo$return <- return-1
#algo <- algo %>% 
#    #mutate(upswing=as.numeric(per==roll_sum(lead(delta)>0,per, fill=NA, align="left"))) %>% #check for sustained positive trend too
#    drop_na()
#
#ggplot(algo,aes(x=hist_n,y=return)) +
#    geom_point() +
#  geom_smooth()



#ggplot(df %>% filter(year(date)==2020), aes(x=date)) +
#  geom_line(aes(y=close)) +
#    geom_bar(aes(y=hist*10),stat="identity") +
#    theme_minimal() +
#    scale_y_continuous(
#        
#        # Features of the first axis
#        name = "First Axis",
#        
#        # Add a second axis and specify its features
#        sec.axis = sec_axis(~./10,name="Second Axis")
#    )

##test significance of hist indicator
#model_dat <- df %>% 
#    filter(!is.na(hist))
#test <- tibble(lags=numeric(),p_value=numeric())
#for (i in 0:500) {
#add_row <-  data.frame(
#  lags=i, p_value=
#      summary(lm(close ~ lag(hist,i),model_dat))$coefficients[,4][2] %>% 
#        .[[1]]
#  )
#test <- test %>% 
#    bind_rows(add_row)
#}
#plot(test$lags,test$p_value)
#
#summary(lm(close ~ lag(hist,14),model_dat))
    
## Check tidyquant code ##
#macd <- FANG %>%
#    group_by(symbol) %>% 
#    tq_mutate(select     = close, 
#              mutate_fun = MACD, 
#              nFast      = 12, 
#              nSlow      = 26, 
#              nSig       = 9, 
#              maType     = SMA) %>%
#    mutate(diff = macd - signal)
    


## Messing with quantmod charts ##
#
#qxts <- xts(df[,-1], order.by=df[,1])
##chart_Series(qxts)
#macd = MACD(df[,"close"], nFast=12, nSlow=26,nSig=9,maType=SMA, percent = FALSE)
#
#chartSeries(qxts, TA="addMACD()")
#
#ggplot(dat=df) +
#    geom_point(aes(minute,close,size=vol))
#
#getSymbols("GS")
#chartSeries(GS) 

#should make these user interactive/inputable
#qt <- c("TSLA","GILD","GRVY","FXI","SH","TTWO","BP","IMLFF","SPXS","ERY","SMDD","SDOW")
#cb <- c(265.77,74.37,22.93,34.36,31.61,102.25,39.53,.45,17.02,117.65,10.78,51.67)

#dat <- data.frame(stock=character(),quote=numeric())

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
            #actionButton('rfrsh','Refresh')
          numericInput("lookback","Select Historical Horizon",value=120,min=120,max=120),
          selectInput("freq","Select Quote Frequency",choices='daily',selected='daily'),
          selectInput("price","Select Quote Type",choices="close",selected="close"),
          column(12,h5("*Select Time Lengths")),
          fluidRow(column(3,numericInput("short","ShortTerm",value=12,min=12,max=12)),
                   column(3,numericInput("long","LongTerm",value=26,min=26,max=26)),
                   column(3,numericInput("sign","Signal",value=9,min=9,max=9)),
                   column(3,numericInput("lag","Lag",value=1,min=1,max=1))
          ),
          selectInput("sec","Select Quote Sector(s)",choices=sectors
                      ,selected=sectors
                      ,multiple=T
          ),
          #selectInput("ind","Select Quote Industry(ies)",choices=industries
          #            ,selected=industries
          #            ,multiple=T
          #),
          selectInput("ticks","Portfolio Stocks",choices=tickers
                      ,selected=ticks
                      ,multiple=T
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(textOutput("test"),
           h3("Quotes: Sell, Buy, ?"),
           dataTableOutput("sell") %>% withSpinner(color="#1A4C64"),
           dataTableOutput("buy") %>% withSpinner(color="#1A4C64"),
           dataTableOutput("tbd") %>% withSpinner(color="#1A4C64"),
           h3("Portfolio"),
           plotlyOutput("plot") %>% withSpinner(color="#1A4C64")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

   #tbl <- eventReactive(input$rfrsh,{
   #    tbl <- dat
   #    for(i in 1:length(qt)) {
   #        
   #        html <- paste0("https://finance.yahoo.com/quote/",qt[i],"?p=",qt[i],"&.tsrc=fin-srch") %>% 
   #            read_html() %>% 
   #            html_nodes(xpath = "/html/body/div[1]/div/div/div[1]/div/div[2]/div/div/div[4]/div/div/div/div[3]/div[1]/div/span[1]") %>% 
   #            html_text()
   #        tbl <- tbl %>% 
   #            bind_rows(data.frame(stock=qt[i],quote=as.numeric(html)))
   #    }
   #    tbl
   #})
    
  #observeEvent(input$sec,{
  #  market %>% 
  #    filter(GICS.Sector %in% input$sec) %>%  
  #    distinct(GICS.Sub.Industry) %>% 
  #    .[[1]] -> choices
  #  
  #  updateSelectInput(session,"ind",choices=choices,selected=choices)
  #  
  #})
  
  dat <- reactive({
    
    market %>% 
      filter(GICS.Sector %in% input$sec#,GICS.Sub.Industry %in% input$ind
             ) %>%  
      distinct(Tickers) %>% 
      .[[1]]-> choices 
    
    tickers <- unique(c(input$ticks,choices))
    
    end <- Sys.Date() + 1 #check on this!!!
    str <- end - input$lookback
    price <- input$price
    freq <- input$freq
    #test moving average theory
    m <- input$short
    n <- input$long
    p <- input$sign
    a <- 2/(m+1)
    l <- input$lag
    withProgress(message = 'Gathering Quote Details',
                 detail = 'This may take a while...', value = 0, {

    l.out <- BatchGetSymbols(tickers = tickers, 
                             first.date = str,
                             last.date = end, 
                             freq.data = freq) 

    incProgress(1, detail = "Query Complete")
                
    })
    
    df <- l.out$df.tickers %>% 
     select(ticker,ref.date, contains(price), everything()) %>% 
     rename(price=3) %>% 
     arrange(ticker,ref.date) %>% 
     group_by(ticker) %>%
     #simple moving averages
     mutate(sma_m=roll_sum(lag(price,l),m, fill=NA, align="right")/m,
            sma_n=roll_sum(lag(price,l),n, fill=NA, align="right")/n,
            macd=sma_m-sma_n
     ) %>% 
     #exponential moving averages + macd
     mutate(ema_m=EMA(price,m),
            ema_n=EMA(price,n),#a*close+(1-a)*sma_n
            macd=ema_m-ema_n
     ) %>% 
     # signal + histogram    
     mutate(signal=EMA(macd,p),
            hist=macd-signal
     ) %>% 
     drop_na() %>% 
     mutate(hist_n=(hist-min(hist))/(max(hist)-min(hist)), #Normalization - consider standardization instead
            cross_0=if_else(lag(hist)<=0&hist>=0|lag(hist)>=0&hist<=0,1,0),
            cross_w=if_else(as.numeric(ref.date-lag(ref.date))>1,1,0),
            cross=if_else(cross_0==1|cross_w==1,1,0)
     ) %>%  
     drop_na() %>% 
     ## Hist Signal Analysis ##      
     mutate(
       #hwm=roll_min(abs(hist),last_n,fill=NA,align="right"),
       #slope=(hist-lag(hist,last_n-1))/last_n,
       interval=cumsum(cross)
     ) %>% 
     group_by(ticker,interval) %>% 
     mutate(
       hwm=rollapply(abs(hist)#if_else(test==row_number(),abs(hist),0)
                     ,row_number(),FUN=max,na.rm = F, align="right")
     ) %>% 
     mutate(
       signal=if_else(cross==0,if_else(abs(hist)<abs(lag(hist))&abs(lag(hist))==hwm
                                       ,if_else(hist<=0,"buy","sell")
                                       ,"neutral"
       ),"cross")
     ) %>% 
     drop_na() 
    
    df
    
  })
  
  #output$test <- renderText({as.character(
  #  max(dat()$ref.date)
  #)})
  
  output$sell <- renderDataTable({

    datatable(dat() %>% filter(signal=="sell",ref.date==Sys.Date()))
    
  })
  output$buy <- renderDataTable({
    
    datatable(dat() %>% filter(signal=="buy",ref.date==Sys.Date()))
    
  })
  output$tbd <- renderDataTable({
    
    datatable(dat() %>% filter(signal=="cross",ref.date==Sys.Date()))
    
  })
  
  output$plot <- renderPlotly({
    
    df <- dat()
    
    gplot <- ggplot(df %>% filter(ticker %in% input$ticks),aes(x=ref.date)) +
      geom_line(aes(y=price)) +
      geom_point(data=df %>% filter(ticker %in% input$ticks,signal!="neutral"),aes(y=price,color=signal#,size=volume
      )) +
      geom_bar(stat="identity",aes(y=hist,fill=signal)) + 
      scale_y_continuous(
        
        # Features of the first axis
        name = "Close",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~., name="MACD Histogram")
      ) +
      facet_wrap(vars(ticker),scales = 'free_y') +
      theme_minimal()
    
    ggplotly(gplot)
    
  })
    
   #output$quotes <- renderDataTable({
   #    tbl <- tbl() %>% 
   #        mutate(cost=cb) %>% 
   #        mutate(diff=round(quote-cost,2),flag=if_else(diff<0,1,0))
   #    
   #    datatable(tbl, options=list(columnDefs = list(list(visible=FALSE, targets=5)))
   #              ) %>% 
   #        formatStyle(
   #            'diff', 'flag',
   #            color = styleEqual(1, 'red')
   #        )
   #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp("/home/andr31/R/github/stock/trading/")
