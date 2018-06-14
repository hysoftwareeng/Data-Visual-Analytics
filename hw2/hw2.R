#hyang390
#903320189

########### 1. Professional Education by State  ###############
library(ggplot2)
data(midwest)

#plot distribution of percprof per state
ggplot(midwest, aes(reorder(state, -percprof, mean), percprof)) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete("State") +
  ggtitle("Distribution of Education Percentage by State") +
  xlab("State") + 
  ylab("Percentage of People with Professional Education")

population_stats  = aggregate(list(midwest$poptotal, midwest$poptotal * midwest$percprof/100), by=list(Category=midwest$state), FUN=sum)
colnames(population_stats) = c("state", "total_pop", "edu_pop")
population_stats$percentage = population_stats$edu_pop / population_stats$total_pop * 100

ggplot(data=population_stats, aes(x=state, y=percentage)) + geom_bar(stat="identity") +
  ggtitle("Percentage of Education by State")

########### 2. School and College Education by State  ###############
#install.packages("GGally")
library(GGally)
data(midwest)
school_college_df=data.frame(midwest$state, midwest$perchsd, midwest$percollege)
ggpairs(school_college_df, aes(color=midwest$state, alpha=0.7))

########### 4. Random Scatterplots  ###############
library(ggplot2)
#install.packages("reshape")
library(reshape)
scatter_data_df = data.frame(matrix(nrow = 17, ncol = 5))
colnames(scatter_data_df) = c("n_size", "ps_size", "pdf_size", "jpeg_size", "png_size")

getFileSizes = function(n_size) {
  scatter_df = data.frame(x=runif(n_size), y=runif(n_size))
  plot = ggplot(scatter_df, aes(x=x, y=y)) + geom_point()
  ggsave("ps_file.ps", plot)
  ps_size = file.size("ps_file.ps")/1000
  file.remove("ps_file.ps")
  
  ggsave("pdf_file.pdf", plot)
  pdf_size = file.size("pdf_file.pdf")/1000
  file.remove("pdf_file.pdf")
  
  ggsave("jpeg_file.jpeg", plot)
  jpeg_size = file.size("jpeg_file.jpeg")/1000
  file.remove("jpeg_file.jpeg")
  
  ggsave("png_file.png", plot)
  png_size = file.size("png_file.png")/1000
  file.remove("png_file.png")
  
  return (c(n_size, ps_size, pdf_size, jpeg_size, png_size))
}

row_count = 1
for (i in c(50, 100, 150, 250, 500, 1000, 1500, 2000, 3500, 5000, 7500, 10000, 15000, 25000, 40000, 50000, 100000)){
  vector = getFileSizes(i)
  scatter_data_df[row_count, ] = vector
  row_count = row_count + 1
}

scatter_data_formatted = melt(scatter_data_df, id="n_size")
ggplot(data=scatter_data_formatted,
       aes(x=n_size, y=value, colour=variable)) +
  geom_line() +
  xlab("Size of N") +
  ylab("File Size in KB") +
  ggtitle("File Size Comparison with Respect to N")


########### 5. Diamonds  ###############
library(ggplot2)
library(GGally)
data(diamonds)

#plot historgrams
ggplot(diamonds, aes(carat)) + geom_histogram() + ggtitle("Diamonds Carat Histogram")

ggplot(diamonds, aes(price)) + geom_histogram() + ggtitle("Diamonds Price Histogram")

ggplot(diamonds, aes(color)) + geom_bar() + ggtitle("Diamonds Color Bar Graph")

diamonds_df=data.frame(diamonds$color, diamonds$carat, diamonds$price)
ggpairs(diamonds_df, aes(color=diamonds$color, alpha=0.7))



