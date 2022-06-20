dat <- readLines("/home/friday/Coding/AdventOfCode_2021_R/day_20/ex_day_20.txt", warn = F)

split = match(0, nchar(dat))

algo = paste0(dat[1:(split - 1)], collapse = "")

image = dat[(split + 1):length(dat)]


rw = nchar(image[1])

image = matrix(unlist(strsplit(paste0(image, collapse = ""), split = "")), ncol = rw, byrow = TRUE)


top = rep(".", ncol(image) + 2)

image[1,]