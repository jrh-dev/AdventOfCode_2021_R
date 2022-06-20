
.end_on <- function(x) x - (x - (x %% 10))

get_ans = function(p1_start, p2_start){
  rolls = rep(1:100, 9000)
  
  p1 = cumsum(rolls[c(T,T,T,F,F,F)])
  p2 = cumsum(rolls[c(F,F,F,T,T,T)])
  
  p1 <- .end_on(p1[seq(3, length(p1), 3)] + p1_start)
  p2 <- .end_on(p2[seq(3, length(p2), 3)] + p2_start)
  
  p1[p1 == 0] <- 10
  p2[p2 == 0] <- 10
  
  p1_win_n = sum(cumsum(p1) < 1000) + 1
  
  p2_win_n = sum(cumsum(p2) < 1000) + 1
  
  if (min(p1_win_n, p2_win_n) == p1_win_n) {
    # if p1 wins
    rolls = (p1_win_n * 3) + ((p1_win_n - 1) * 3)
    lose_score = sum(p2[1:(p1_win_n - 1)])
  } else {
    # if p2 wins
    rolls = (p2_win_n * 3) + ((p2_win_n - 1) * 3)
    lose_score = sum(p1[1:(p2_win_n - 1)])
  }
  return(lose_score * rolls)
}


# Part 1 answer
get_ans(7,9)


# A tibble: 7 × 5
space_1 score_1 space_2 score_2 universes
<dbl>   <dbl>   <dbl>   <dbl>     <dbl>
  1       1       1       8       0         6
2       2       2       8       0         3
3       3       3       8       0         1
4       7       7       8       0         1
5       8       8       8       0         3
6       9       9       8       0         6
7      10      10       8       0         7


di_combs = as.data.frame(table(rowSums(expand.grid(1:3,1:3,1:3))), stringsAsFactors = FALSE)

names(di_combs) <- c("roll_total", "freq")

di_combs$roll_total = as.numeric(di_combs$roll_total)

turn_combs = expand.grid(3:9,3:9)

names(turn_combs) <- c("p1","p2")

state = data.frame(p1_space = 4, p1_score = 0, p2_space = 8, p2_score = 0, universes = 1)

play <- function(input, player) {
  
  if (player == 1) {
    tmp = data.frame(
      p1_space = .end_on(input$p1_space + di_combs$roll_total), 
      p2_space =  input$p2_space
    )
    tmp$p1_space[tmp$p1_space == 0] <- 10 
    tmp$p1_score = input$p1_score + tmp$p1_space
    tmp$p2_score = input$p2_score
    tmp$universes = input$universes + di_combs$freq
  } else {
    tmp = data.frame(
      p2_space = .end_on(input$p2_space + di_combs$roll_total), 
      p1_space =  input$p1_space
    )
    tmp$p2_score = input$p2_score + tmp$p2_space
    tmp$p1_score = input$p1_score
    tmp$universes = input$universes + di_combs$freq
  }
  return(tmp)
}

.turn_switch <- function(x) if (x == 1) 2 else 1

player = 1

win_count = list(0,0)

# play out all possible outcomes but start with p1s first turn
state = play(state, player = player)

player = .turn_switch(player)

iter = 1

# now enter loop


state = lapply(split(state,row.names(state)),play,player)

state = do.call(rbind, state)

state = aggregate(.~p1_space+p2_space+p1_score+p2_score, state, sum)

catch_uni <- sum(state$universes)

state <- state[state$p1_score < 21 & state$p2_score < 21,]

win_uni <- catch_uni - sum(state$universes)

win_count[[player]] <- win_count[[player]] win_uni * iter

if (nrow(state) == 0) stop("Finished")

iter = iter + 1

player = .turn_switch(player)

# needs work, suspect its to do with the way universes are counted