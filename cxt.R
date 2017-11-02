library(lubridate)
library(plotly)
library(dplyr)
library(fpp)
library(quantmod)

setwd("C:/Users/jy/Desktop/cxt")

df <- read.csv("Data Set.csv", stringsAsFactors = F)

for(i in c(2,3,4,5)){
      df[,i] <- as.numeric(df[,i])
}

df[,1] <- ymd(df[,1])

summary(df)

u1 <- abs(df[,2]) < 1000
df <- df[u1,]

df <- df[complete.cases(df),]

summary(df)

bbands <- BBands(df[,c("H","L","C")])
df <- cbind(df, data.frame(bbands[,1:3]))
# plot candlestick chart
p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~O, close = ~C,
          high = ~H, low = ~L, name = "Price") %>%
add_lines(y = ~up , name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none") %>%
  add_lines(y = ~dn, name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands",
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none") %>%
  layout(yaxis = list(title = "Price"))


# plot volume bar chart
pp <- df %>%
  plot_ly(x=~Date, y=~V, type='bar', name = "Volume") %>%
  layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# subplot with shared x axis
p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
             shareX = TRUE, titleY = TRUE) %>%
  layout(title = paste("price"),
         xaxis = list(rangeselector = rs),
         legend = list(orientation = 'h', x = 0.5, y = 1,
                       xanchor = 'center', yref = 'paper',
                       font = list(size = 10),
                       bgcolor = 'transparent'))

p



#we cant possibly know low and high
df2 <- df[,c("Date", "O", "C")]
n <- nrow(df2)
df2$return <- ( df2[1:n,"C"]/c(NA, df2[1:(n-1), "C"]) ) - 1
df2$closedivopen <- ( df2[1:n,"C"]/df2[1:n,"O"] ) - 1

summary(df2)

plot(df2$return)


wkret <- function(cl, op){
      L <- length(cl)
      cl[L]/op[1]-1
}
wkclose <- function(v){
      L <- length(v)
      v[L]
}

signal <- function(wkreturn, percent = 0 ){
      wkreturn < percent
}


roundtripcost <- 0.0 #1% round trip cost
df4 <- as.data.frame( df2 %>% 
                            mutate(yr = year(Date), wk = week(Date)) %>% 
                            group_by(yr, wk) %>% 
                            summarise(Date = Date[length(Date)], wkreturn = wkret(C, O), wkclose = wkclose(C)) 
                      )
df5 <- df4 %>%   mutate(signal = lag(signal(wkreturn, 0), 1) ) %>%
          mutate(signal2 = lag(signal(wkreturn, 0.02), 1) ) %>%
          mutate(adj_wkreturn = wkreturn - roundtripcost)

                  
actual <- xts(df5$adj_wkreturn, df5$Date); 
signal_1 <- xts(df5$adj_wkreturn*df5$signal, df5$Date)
signal_2 <- xts(df5$adj_wkreturn*df5$signal2, df5$Date)

compare <- na.omit(cbind(actual, signal_1, signal_2))
colnames(compare) <- c("Actual", "Signal 1", "Signal 2")
charts.PerformanceSummary(compare)





df3$buy <- df3$wkreturn > 0
df3$buy <- c(NA, df3$buy[1:(nrow(df3)-1)])

df3 <- df3[1:25,]
prod(1+df3$wkreturn*df3$buy, na.rm = T)
prod(1+df3$wkreturn, na.rm = T)



#backtest
N <- nrow(df3)

attach(df3)
#Strategies: if weekly return of the previous week is positive, then buy at open at the start of this week, and sell at the end of the week. Else do nothing.
for(i in 1:N){
      if(index[i] == 1 & wk[i] > 1){
            u1 <- wk == wk[i]-1; prev_wk_return <- wkreturn[u1][1]
            if(prev_wk_return > 0){
                  df3[i, "cashflow"] <- O[i] #buy at opening price     
            }else{
                  df3[i, "cashflow"] <- 0         
            }
      }else{
            df3[i, "cashflow"] <- 0     
      }
}
