library(tidyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(plotly)

df <- read.csv('GovernmentSwedenYields.csv',sep = ";")
df$Group <- NULL
names(df) <- c('Date','duration','interest')
df$Date <- paste(df$Date, "01")
df$Date <- ymd(df$Date)
levels(df$duration) <- factor(c('year10','year2','year5','year7'))

p <- ggplot(df, aes(Date,interest,color=duration)) +
  geom_line() + 
  labs(title="Swedish Government Yield Rates")

print(p)

t2 <- filter(df,duration=='year2')
t5 <- filter(df,duration=='year5')
t7 <- filter(df,duration=='year7')
t10 <- filter(df,duration=='year10')



p <- plot_ly(df, x = ~Date, y = ~duration, z = ~interest, type = 'mesh3d',opacity=1,intensity=~interest,showscale=FALSE)%>%
  layout(title = 'Swedish Government Bond Yields',
         scene = list(xaxis = list(title = ''),
                      yaxis = list(title = 'Binding Period'),
                      zaxis = list(title = '% Interest'),
         aspectratio = list(x = 2, y = 1, z = 0.5)))

p <- p%>% add_trace(x = t2$Date, y = t2$duration, z=t2$interest, type = "scatter3d",mode='lines',showlegend=FALSE)
p <- p%>% add_trace(x = t5$Date, y = t5$duration, z=t5$interest, type = "scatter3d",mode='lines',showlegend=FALSE)
p <- p%>% add_trace(x = t7$Date, y = t7$duration, z=t7$interest, type = "scatter3d",mode='lines',showlegend=FALSE)
p <- p%>% add_trace(x = t10$Date, y = t10$duration, z=t10$interest, type = "scatter3d",mode='lines',name="10 year fix",showlegend=FALSE)


ggplotly(p)
