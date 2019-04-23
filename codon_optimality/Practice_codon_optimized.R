library(tidyverse)   # install.packages("tidyverse")
# ------------------------------------------------------------------------------------------
data <- read.csv("https://tinyurl.com/codon-eric")

data <- na.omit(data) %>% group_by(Image) %>%
  mutate(strain_Mean = mean(BGS_Intensity),
         strain_SD = sd(BGS_Intensity),
         sample_Size = length(unique(Nuclei)))

ggplot() +
  geom_density(data = filter(data, Category == 'A'), aes(x = Intensity, y = stat(count), color = Strain), size = 2) +
  geom_density(data = filter(data, Category == 'A'), aes(x = Intensity, y = stat(count), color = Strain, group = Image)) +
  facet_wrap(~Strain) +
  theme_bw()

ggplot(data = data) +
  geom_jitter(aes(x = Image, y = BGS_Intensity), alpha = 0.5) +
  geom_errorbar(aes(x = Image, 
                    ymin = strain_Mean - strain_SD, 
                    ymax = strain_Mean + strain_SD)) + 
  geom_errorbar(aes(x = Image, 
                    ymin = strain_Mean, 
                    ymax = strain_Mean), 
                color = 'red', 
                size = 2) +
  facet_wrap(~Strain, scales = "free_x") +
  theme_bw()

ggplot(data = data) +
  geom_jitter(aes(x = Image, y = BGS_Intensity), alpha = 0.5) +
  facet_wrap(~Strain, scales = "free_x") +
  theme_bw()

ggplot(data = data) +
  geom_point(aes(x = sample_Size, y = strain_SD)) +
  stat_smooth(aes(x = sample_Size, y = strain_SD)) +
  facet_wrap(~Strain) +
  theme_bw()