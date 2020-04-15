#
# Let's create a model to take advantage of price swings, e.g.
# 3% daily sustained (1 week?) upswing would suggest we should have bought a stock
# create binary variable (1|0) that flags buys (consider one for sales too)
# Autoregression, logit, probit, neural networks
# Start by just flagging swings using MACD
# Add/Investigate RSI
# Start building a predictive model and check assumptions

library(shiny)
library("quantmod")
library(tidyverse)
library(tidyquant)
library(rvest)
library(DT)
library(shinycssloaders)
library("RcppRoll")
library(jsonlite)

options(scipen=999)

horizon <- "DAILY"#"INTRADAY"
symbol <- "DIA"
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

colnames(df) <- c("date","open","high","low","close","vol")

#test moving average theory
m <- 12
n <- 26
p <- 9
a <- 2/(m+1)
l <- 1

df <- df %>% 
    #arrange(desc(date)) %>% 
    #slice(1:(100)) %>% 
    arrange(date) %>% 
    #simple moving averages
    mutate(sma_m=roll_sum(lag(close,l),m, fill=NA, align="right")/m,
           sma_n=roll_sum(lag(close,l),n, fill=NA, align="right")/n,
           macd=sma_m-sma_n
           ) %>% 
    #exponential moving averages + macd
    mutate(ema_m=EMA(close,m),
           ema_n=EMA(close,n),#a*close+(1-a)*sma_n
           macd=ema_m-ema_n
           ) %>% 
    # signal + histogram    
    mutate(signal=EMA(macd,p),
           hist=macd-signal
    ) %>% 
    drop_na() %>% 
    mutate(hist_n=(hist-min(hist))/(max(hist)-min(hist)))
#Normalization - consider standardization instead

y <- 2020
per <- 5 #5 hours or 5 days of sustained increase/decrease
del <- .03 #delta cutoff, e.g. 3% daily swings

algo <- df %>% 
    filter(year(date)==y) %>% 
    mutate(delta=close/lag(close)-1) %>% 
    drop_na() 
#algo %>% filter(delta>0)

#cutoff .375 and .625???
leads <- shift(algo$delta,n=1:per, type = "lead")
return <- leads[[1]] + 1
for (i in 2:per) {
  return <- return*(leads[[i]]+1) #multiply first element of each list to each other, e.g. leads1-5
}

algo$return <- return-1
algo <- algo %>% 
    drop_na()

ggplot(algo) +
    geom_point(aes(x=hist_n,y=return)) 


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
