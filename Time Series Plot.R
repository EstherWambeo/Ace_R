library(readxl)
data_water<-read_xlsx("Water_levels_on_rivers_and_on_a_lake.xlsx")
str(data_water)


#########
#Time series analysis
library(ggplot2)

# Convert Year column to numeric
data_water$Year <- as.numeric(data_water$`Year (from 1961 to 2021)`)

# Create the time series plot
ggplot(data = data_water, aes(x = Year)) +
  geom_line(aes(y = `Gnevsdorf EP / cm`, color = "Gnevsdorf EP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Quitzöbel OP / cm`, color = "Quitzöbel OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Neuwerben Elbe / cm`, color = "Neuwerben Elbe"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Havelberg Stadt / cm`, color = "Havelberg Stadt"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Grütz OP / cm`, color = "Grütz OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Rathenow OP / cm`, color = "Rathenow OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Bahnitz OP / cm`, color = "Bahnitz OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Plaue OP / cm`, color = "Plaue OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Brandenburg OP / cm`, color = "Brandenburg OP"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Ketzin P / cm`, color = "Ketzin P"), linetype = "solid", size = 1, alpha = 0.8) +
  geom_line(aes(y = `Potsdam P / cm`, color = "Potsdam P"), linetype = "solid", size = 1, alpha = 0.8) +
  labs(x = "Year", y = "Water Level (cm)", title = "Time Series of Water Levels") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "brown", "pink", "gray", "cyan", "magenta", "black"),
                     labels = c("Gnevsdorf EP", "Quitzöbel OP", "Neuwerben Elbe", "Havelberg Stadt", "Grütz OP",
                                "Rathenow OP", "Bahnitz OP", "Plaue OP", "Brandenburg OP", "Ketzin P", "Potsdam P")) +
  theme_minimal()
