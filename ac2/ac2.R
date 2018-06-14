#hyang390
#903320189

#install.packages("ggplot2")
#install.packages("ggmap")

library(ggplot2)
library(ggmap)

setwd("C:/Users/Batmachine/Dropbox/OMSCS/CSE6242 - DVA/Assignments/ac2") #change to dir of .csv file
raw_data = read.csv("datascienceuni.csv")

aggregated_schools = aggregate(cbind(count = SCHOOL) ~ STATE, data = raw_data, FUN = NROW)
states_data = map_data("state")
missing_states = data.frame(setdiff(state.name, aggregated_schools$STATE), 0)
names(missing_states) = c("STATE","count")
aggregated_data = rbind(aggregated_schools, missing_states)

aggregated_data$region = tolower(aggregated_data$STATE)
plotting_data = merge(states_data, aggregated_data, by="region")

ggplot(plotting_data, aes(long, lat)) +
geom_polygon(aes(group = group, fill = count)) + 
scale_fill_continuous(low='thistle2', high='darkred', guide='colorbar') +
coord_map("albers",  at0 = 45.5, lat1 = 29.5)

