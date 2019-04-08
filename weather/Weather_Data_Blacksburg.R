library(tidyverse)   # install.packages("tidyverse")

# ------------------------------------------------------------------------------------------

weather <- read.csv("https://raw.githubusercontent.com/erodb/datasets/master/Weather_Data_Blacksburg.csv")
weather <- na.omit(weather)
weather <- weather %>% separate(Day, sep = '-', c('Day', 'Month')) %>% mutate(Day = as.numeric(Day))

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = Month), size = 3) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = Month), size = 3)

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = Month), size = 3) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = Month), size = 3) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top")

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = Month), size = 3) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = Month), size = 3) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top")

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest), size = 1, color = 'red', alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest), size = 1, color = 'blue', alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Normal_Low, yend = Normal_High), size = 1, color = 'black', alpha = 0.5) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top") +
  scale_y_continuous(sec.axis = sec_axis(~(.-32)*(5/9), name = "Temperature [°C]")) + 
  labs(title = "Normal and Record Temperatures for Blacksburg, Virginia", 
       caption = "Source: National Weather Service", 
       y = "Temperature [°F]", x = "Date") +
  theme_bw() +
  theme(strip.background = element_blank())

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = 'Record Hottest Temperature Range'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = 'Record Coldest Temperature Range'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Normal_Low, yend = Normal_High, color = 'Normal Temperature Range'), size = 1, alpha = 0.5) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top") +
  scale_y_continuous(sec.axis = sec_axis(~(.-32)*(5/9), name = "Temperature [°C]")) + 
  labs(title = "Normal and Record Temperatures for Blacksburg, Virginia", 
       caption = "Source: National Weather Service", 
       color = "",
       y = "Temperature [°F]", 
       x = "Date") +
  theme_bw() +
  theme(strip.background = element_blank())

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = 'Record Hottest'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = 'Record Coldest'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Normal_Low, yend = Normal_High, color = 'Normal'), size = 1, alpha = 0.5) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top") +
  scale_y_continuous(sec.axis = sec_axis(~(.-32)*(5/9), name = "Temperature [°C]")) + 
  labs(title = "Normal and Record Temperatures for Blacksburg, Virginia", 
       caption = "Source: National Weather Service", 
       color = "Temperature Range",
       y = "Temperature [°F]", 
       x = "Date") +
  theme_bw() +
  theme(strip.background = element_blank())

# ------------------------------------------------------------------------------------------

weather %>%
  ggplot(aes(group = Month)) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Hottest, yend = High_Temp_Hottest, color = 'Record Hottest'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Low_Temp_Coldest, yend = High_Temp_Coldest, color = 'Record Coldest'), size = 1, alpha = 0.5) +
  geom_segment(aes(x = Day, xend = Day, y = Normal_Low, yend = Normal_High, color = 'Normal'), size = 1, alpha = 0.5) +
  geom_hline(aes(yintercept = 32), alpha = 0.3) +
  scale_color_manual(values = c('black','blue','red')) +
  facet_wrap(~factor(Month, levels = unique(Month)), nrow = 1, strip.position = "top") +
  scale_y_continuous(sec.axis = sec_axis(~(.-32)*(5/9), name = "Temperature [°C]")) + 
  labs(title = "Normal and Record Temperatures for Blacksburg, Virginia", 
       caption = "Source: National Weather Service", 
       color = "Temperature Range",
       y = "Temperature [°F]", 
       x = "Date") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 8))
