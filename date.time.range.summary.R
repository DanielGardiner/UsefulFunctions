


approx.start = seq(as.POSIXct("04/02/2017 12:00", format = "%d/%m/%Y %H:%M"),
                   as.POSIXct("04/02/2017 23:00", format = "%d/%m/%Y %H:%M"), 60*60)

approx.end = seq(as.POSIXct("05/02/2017 12:00", format = "%d/%m/%Y %H:%M"),
                 as.POSIXct("06/02/2017 15:00", format = "%d/%m/%Y %H:%M"), 60*60)


temp = expand.grid(approx.start, approx.end)

colnames(temp) = c("approx.start", "aprox.end")

temp$diff = difftime(temp$aprox.end, temp$approx.start)

range(temp$diff)


temp$aprox.end = as.character(temp$aprox.end)

temp$approx.start = as.character(temp$approx.start)


library(ggplot2)


ggplot(temp, aes(x = aprox.end, y = approx.start)) + 
  geom_tile(aes(fill = diff)) +
  geom_text(aes(label = diff)) +
  ggtitle(paste("Hour range:", as.numeric(min(temp$diff)), 
                "-", as.numeric(max(temp$diff)))) +
  theme(axis.text.x = element_text(angle = 90)) 










