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

# Output Country Name
length(unique(B$Country_Name))

# Sample Subset
peruData <- filter(B, Country_Name == "Peru")