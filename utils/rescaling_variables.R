# load required libraries
library(scales)

# generate test data
init_vector <-  rnorm(1000, mean = 0, sd = 1)  # c(-10, -9, -5, 2, 6) 

# visualize density distribution of the initial vector
den <- density(init_vector) 
plot(den, frame = FALSE, col = "blue",main = "Density plot", ylim = c(0, 0.5))

# changing the scale initial vector
rescaled_vector <- scales::rescale(init_vector, to = c(0, 100))

# visualize density distribution of the rscaled vector
den <- density(rescaled_vector) 
plot(den, frame = FALSE, col = "blue",main = "Density plot")
