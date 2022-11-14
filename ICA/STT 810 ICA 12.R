
library(ggplot2)
library(tidyverse)
cars_multi <- read_csv(file = "C:\\Users\\Adi\\Desktop\\Fall-22 Study Material\\STT 810\\ICA\\cars_multi.csv", col_names = TRUE)

# question 1

g = ggplot(data = cars_multi, aes(x = weight)) + geom_histogram(fill = "deepskyblue4") 
g + labs(x = "Car Weight", y = "Count", title = "Car Weight Histogram")

# question 2

g = ggplot(data = cars_multi, aes(x = acceleration,fill = as.character(cylinders))) + geom_histogram() + facet_grid(cylinders ~.)
g + labs(x = "Car Acceleration", y = "Count", title = "Car Acceleration Histogram by # of Cylinders")

# question 3

g = ggplot(data = cars_multi, aes(y = as.integer(horsepower))) + geom_boxplot() 
g + labs(y = "Horsepower",title = "Box and Whisker Plot of Horsepower")

# question 4
g = ggplot(data = cars_multi, aes(x = mpg, y = weight, color = as.character(cylinders))) + geom_point()
g + labs(x = "mpg", y = "weight", title = "Mpg vs Weight Scatter Plot")

