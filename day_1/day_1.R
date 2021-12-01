input_1 <- as.numeric(readLines("day_1/data/day_1.txt"))

# Part 1 
sum(diff(input_1) > 0)

# Answer 1715

# Part 2
input_roll <- zoo::rollsum(input_1, 3)

sum(diff(input_roll) > 0)

# Answer 1739