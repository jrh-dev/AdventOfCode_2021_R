

dat <- readLines("/home/friday/Coding/AdventOfCode_2021_R/day_19/ex_day_19_dat.txt", warn = F)

sl = grep("^--",dat)

scan <- vector(mode = "list", length = length(sl))


for (ii in seq_len(length(scan))){
  if (ii != length(sl)){
    
    scan[[ii]] <- read.table(text = dat[(sl[ii]+1):(sl[ii+1]-2)], sep = ",", header = FALSE)
    
  } else {
    
    scan[[ii]] <- read.table(text = dat[(sl[ii]+1):length(dat)], sep = ",", header = FALSE)
    
  }
}

pro_1 <- function(input) {
  
  input$id <- 1:nrow(input) 
  
  tmp_list <- vector(mode = "list", length = nrow(input))
  
  for (ii in seq_len(nrow(input))) {
 
    x = input[ii,1]
    y = input[ii,2]
    z = input[ii,3]
  
    tmp <- input[input$id != ii,]
    
    tmp$dist <- paste(abs(tmp$V1 - x),abs(tmp$V2 - y),abs(tmp$V3 - z), sep=",")
       
    tmp_list[[ii]] <- tmp
      
  }
  
  return(do.call(rbind, tmp_list))
  
}

pro_2 <- function(input_1, input_2) {
  
  uni <- unique(input_1[input_1$dist %in% unique(input_1$dist[input_1$dist %in% input_2$dist]),]$id)
  
  input_1 <- input_1[input_1$id %in% uni,]
  
  input_1 <- input_1[!names(input_1) %in% c("dist")]
  
  input_1 <- input_1[!duplicated(input_1[,c("V1", "V2", "V3", "id")]),]
  
  return(input_1)

}

# shared_beacons 

# initialise lists

share_bea <- vector(mode = "list", length = length(scan))

share_bea <- lapply(share_bea, function(x) {
  vector(mode = "list", length = length(scan))
})

for (ii in seq_len(length(scan))) {
  
  ref_scan = pro_1(scan[[ii]])
  
  for (jj in seq_len(length(scan))) {
    
    tar_scan = pro_1(scan[[jj]])
    
    if (identical(ref_scan, tar_scan)) next
    
    share_bea[[ii]][[jj]] <- pro_2(ref_scan, tar_scan)
    
  }
  
  share_bea[[ii]] <- do.call(rbind, share_bea[[ii]])
  
}




















