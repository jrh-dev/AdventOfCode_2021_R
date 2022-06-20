UsefulR::set_dir()

input = as.data.frame(readLines("ex_day_22.txt", warn = FALSE))

names(input) = "str"

action = grepl("on", input$str)

input$x = gsub("\\.\\.", ":", substr(input$str, 
                 stringr::str_locate(pattern="x=", input$str)[,1] + 2, 
                 stringr::str_locate(pattern="y=", input$str)[,1] - 2
))

input$y = gsub("\\.\\.", ":", substr(input$str, 
                 stringr::str_locate(pattern="y=", input$str)[,1] + 2, 
                 stringr::str_locate(pattern="z=", input$str)[,1] - 2
))

input$z = gsub("\\.\\.", ":",substr(input$str, 
                 stringr::str_locate(pattern="z=", input$str)[,1] + 2, 
                 nchar(input$str)
))

input = cbind(
data.frame(do.call('rbind', strsplit(as.character(input$x),':',fixed=TRUE))),
data.frame(do.call('rbind', strsplit(as.character(input$y),':',fixed=TRUE))),
data.frame(do.call('rbind', strsplit(as.character(input$z),':',fixed=TRUE)))
)

names(input) = c("x1", "x2", "y1", "y2", "z1", "z2")

input = as.data.frame(sapply(input, as.double))

input = input + 51

input$action <- action

input = input[
  ((input$x1 < 1 & input$x2 > 101) |
    (input$x1 %in% 1:101 | input$x2 %in% 1:101)) &
    ((input$y1 < 1 & input$y2 > 101) |
       (input$y1 %in% 1:101 | input$y2 %in% 1:101)) &
    ((input$z1 < 1 & input$z2 > 101) |
       (input$z1 %in% 1:101 | input$z2 %in% 1:101)),
  ]

action = input$action


core = array(rep(0,101),dim = c(101,101,101))

for (ii in seq_along(action)) {
  
  if (action[ii] == TRUE) {
    
    core[input$x1[ii]:input$x2[ii], input$y1[ii]:input$y2[ii], input$z1[ii]:input$z2[ii]] <- 1
    
  } else {
    
    core[input$x1[ii]:input$x2[ii], input$y1[ii]:input$y2[ii], input$z1[ii]:input$z2[ii]] <- 0
    
  }
  
}

# answer part 1
sum(core)


# part 2




















