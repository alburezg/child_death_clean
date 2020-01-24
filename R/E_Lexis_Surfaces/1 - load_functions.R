source("../functions.R")

# Data wrangling
wrangling <- c("tidyverse", "scales")

library2(wrangling)



# New functions ----

find_min_value <- function(min_val, df){
  
  fun <- function(val, df) {
    df %>% 
      group_by(region, cohort) %>% 
      filter(value >= val) %>% 
      slice(1) %>% 
      mutate(value = val)  
  } 
  data.frame(do.call(rbind, lapply(min_val, fun, df)), stringsAsFactors = F) 
}

label_min_value <- function(min_val, df, x = 1952, shift_y = -3, as_share = F){
  
  fun <- function(val, df) {
    df %>% 
      group_by(region, cohort) %>% 
      filter(value >= val) %>% 
      slice(1) %>% 
      filter(cohort == x) %>% 
      mutate(
        value = val
        , age = age + shift_y
      ) 
    # ungroup %>% 
    
  } 
  out <- data.frame(do.call(rbind, lapply(min_val, fun, df)), stringsAsFactors = F) 
  if(as_share) out <- out %>% mutate(value = paste0(value*100, "%"))
  return(out)
}

# lexis_coord_cohort <- function(row){
#   # browser()
#   
#   id <- row['id']
#   n <- as.integer(row[2:3])
#   names(n) <- names(row)[2:3]
#   
#   xcoord <- c(n['cohort'], n['cohort'] + 1, n['cohort'] + 1, n['cohort'])
#   ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
#   
#   # if(n['cohort'] == 1950) {
#   #   xcoord[4] <- n['cohort']
#   # } else if(n['cohort'] == 1999) {
#   #   xcoord[3] <- xcoord[4]
#   # } else if(n['cohort'] == 2000) {
#   #   xcoord <- rep(n['cohort'], 4)  
#   # } 
#   
#   data.frame(
#     id = id
#     , cohort = xcoord
#     , age = ycoord
#     , row.names = NULL, stringsAsFactors = F)
#   
# }

lexis_coord_cohort <- function(row){
  # browser()
  
  id <- row['id']
  n <- as.numeric(row[c("cohort", "age")])
  names(n) <- c("cohort", "age")
  
  xcoord <- c(n['cohort'], n['cohort'] + 1, n['cohort'] + 1, n['cohort'])
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  # if(n['cohort'] == 1950) {
  #   xcoord[4] <- n['cohort']
  # } else if(n['cohort'] == 1999) {
  #   xcoord[3] <- xcoord[4]
  # } else if(n['cohort'] == 2000) {
  #   xcoord <- rep(n['cohort'], 4)  
  # } 
  
  data.frame(
    id = id
    , cohort = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}


lexis_coord_period <- function(row){
  # browser()
  
  id <- row['id']
  n <- as.integer(row[2:3])
  names(n) <- names(row)[2:3]
  
  xcoord <- c(n['year'], n['year'] + 1, n['year'] + 2, n['year'] + 1)
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  data.frame(
    id = id
    , year = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}

lexis_coord_shrink <- function(row){
  # browser()
  
  id <- row['id']
  n <- as.numeric(row[c("cohort", "age", "shrink")])
  names(n) <- c("cohort", "age", "shrink")
  
  xcoord <- c(n['cohort'], n['cohort'] + 1, n['cohort'] + 1, n['cohort'])
  # shrink
  if(!n["shrink"] %in% c(0,1)) {
    low <- mean(xcoord[1:2]) - (n["shrink"]/2)
    high <- mean(xcoord[1:2]) + (n["shrink"]/2)
    
    xcoord[c(1,4)] <- low
    xcoord[c(2,3)] <- high 
  }
  
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  data.frame(
    id = id
    , cohort = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}

myfxHCLramp <- function(H,C=95,L,N=5){
  # H and L must be of equal length
  colsi <- c()
  for (i in 1:(length(H)-1)){
    Hi <- seq(H[i],H[i+1],length=(N+1))
    Hi[Hi<0] <- Hi[Hi<0]+360
    colsi <- c(colsi,hcl(h=Hi,c=C,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
  }
  colsi
}