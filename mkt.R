#Install the libraries
install.packages("ChannelAttribution")
install.packages("ggplot2")
install.packages("reshape")
install.packages("dplyr")
install.packages("plyr")
install.packages("reshape2")
install.packages("markovchain")
install.packages("plotly")

#Load the libraries
library("ChannelAttribution")
library("ggplot2")
library("reshape")
library("dplyr")
library("plyr")
library("reshape2")
library("markovchain")
library("plotly")

#Read the data into R
channel = read.csv("Channel_attribution.csv", header = T)
head(channel)
for(row in 1:nrow(channel))
{
  if(21 %in% channel[row,]){channel$convert[row] = 1}
}
column = colnames(channel)
channel$path = do.call(paste, c(channel[column], sep = " > "))
head(channel$path)

for(row in 1:nrow(channel))
{
  channel$path[row] = strsplit(channel$path[row], " > 21")[[1]][1]
}
channel_fin = channel[,c(23,22)]
channel_fin = ddply(channel_fin,~path,summarise, conversion= sum(convert))
head(channel_fin)

Data = channel_fin
head(Data)

H <- heuristic_models(Data, 'path', 'conversion', var_value='conversion')
H

M <- markov_model(Data, 'path', 'conversion', var_value='conversion', order = 1)> M

# Merges the two data frames on the "channel_name" column.
R <- merge(H, M, by='channel_name')

# Select only relevant columns
R1 <- R[, (colnames(R) %in %c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion'))]

# Transforms the dataset into a data frame that ggplot2 can use to plot the outcomes
R1 <- melt(R1, id='channel_name')

# Plot the total conversions
ggplot(R1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') +
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  ylab("")
  
  
