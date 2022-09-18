library(scales)
rescale(c(-10, -9, -5, 2, 6), to = c(0, 100)) ## Use scales:::rescale() if you have several packages loaded using the same function name
#[1]   0.00   6.25  31.25  75.00 100.00