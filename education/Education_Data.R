library(tidyverse)

# ------------------------------------------------------------------------------------------------------------------------------------------------
A <- read.csv("https://raw.githubusercontent.com/erodb/datasets/master/education/Education_Data.csv", header = TRUE)
B <- gather(A, value = 'Data', key = 'Year', -c('Country_Name','Country_Code','Indicator_Name','Indicator_Code'))
B$Year <- as.numeric(sapply(B$Year, gsub, pattern = "X", replacement = "")) 
B <- B %>% mutate(Country_Name = as.character(Country_Name), 
                  Country_Code = as.character(Country_Code), 
                  Indicator_Name = as.character(Indicator_Name),
                  Indicator_Code = as.character(Indicator_Code))
colname(B)[5] <- "Year" # Rename due to gsub
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Output Number of Countries
length(unique(B$Country_Name))

# Output Number of Indicators
length(unique(B$Indicator_Name))

# Sample Subset (Demographics by Age Group in the United States)
pop <- c('SP.POP.0014.TO.ZS',
         'SP.POP.1564.TO.ZS')
USAdata <- filter(B, 
                  Country_Name == 'United States',
                  Indicator_Code %in% pop)

# Sample Plot
ggplot(data = USAdata, aes(x = Year, y = Data, fill = Indicator_Name)) +
  geom_bar(stat = "identity") +
  theme_bw()