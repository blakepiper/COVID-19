## Fever Time Series Data While Infected with COVID-19 ##

library(ggplot2)

Dad = c(100.7, 99.3, 99.5, 100, 99.1, 99.1, 98.7, 98.9, 100.0, 98.8, 97.8, 99.7, 99.6, 98.6, 97.7, 99.4, 100.7, 
        98.5, 97.7, 97.4, 97.4)
Blake = c(99.5, 99.5, 99.0, 99.1, 99.5, 98.5, 98.3, 98.7, 98.2, 98.3, 98.2, 97.1, 97.4, 98.6, 98.1, 98.7, 97.9, 97.0,
          97.5, 97.6, 97.5)
blake_name_s = c(rep("Blake", 21))
bruce_name_s = c(rep("Bruce", 21))
names_ = c(blake_name_s, bruce_name_s)
temp_F = c(Blake, Dad)
time = c(1:21)
data = data.frame(names, temp_F, time)
plot = ggplot(data, aes(x=time, y=temp_F, color = names)) + geom_line(linetype = "solid", size = .8) + 
  geom_point(color = "blue", size = .5) +
  scale_color_manual(values = c("red", "blue"))+
  xlab("Time (Measured Irregularly)") + 
  ylab("Temperature (Fahrenheit)") +
  geom_text(aes(x = 17, y =100.8, label = "Bruce Max Temp = 100.7"))+
  geom_text(aes(x = 6.7, y =99.6, label = "Blake Max Temp = 99.5"),color = "red")
plot

# Our fevers did not correlate
cor.test(Dad, Blake)

# Our fevers went down over time (I run cool)
cor.test(Dad, time)
cor.test(Blake, time)
