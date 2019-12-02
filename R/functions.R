
# *~^**~^**~^**~^**~^**~^**~^*#
# Code by                     #
# Diego Alburez-Gutierrez     #
# gatemonte@gmail.com         #
# @d_alburez                  #
# unless stated otherwise.    #
# GNU GENERAL PUBLIC LICENSE  #
# Version 3, 29 June 2007     #
# *~^**~^**~^**~^**~^**~^**~^*#



# Apply cohort life tables to real-life populatinos
# with the intention of getting the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data
apply_lt <- function(female_births, LTCF, numCores) {
  
  countries <- unique(LTCF$Country)
  cohorts <- 1950:1999
  
  cl <- makeCluster(numCores)
  
  clusterExport(
    cl
    , varlist = c("countries", "cohorts", "female_births", "LTCF", 'lt_mx')
    , envir = environment()
  )
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(data.table)
  })
  
  print(system.time(
    est_l <- parLapply(cl, countries, worker_apply_lt, countries, cohorts, female_births, LTCF) 
  ))
  
  stopCluster(cl)
  
  # est_l <- lapply(countries, worker_apply_lt)
  
  data.frame(rbindlist(est_l, use.names = T))  
  
}

# Change period labels
# UN calendar year periods are non-exlusive; ie 1950-1955 and 1955-1960
# The should actually be only 5 yers long: 1950-1954 and 1955-1959
change_period_labels <- function(col) {
  
  per_old <- unique(col)
  
  per_new <- unlist(lapply(per_old, function(p) {
    pe <- as.numeric(unlist(strsplit(p, "-")))
    pe[2] <- pe[2] -1
    paste(pe, collapse = "-")
  }))
  
  plyr::mapvalues(col, from = per_old, to = per_new)  
}

child_loss <- function(countries, reference_years, ages_keep = 15:100, path = "../../Data/derived", ASFRC) {
  
  df_l <- lapply(
    countries
    , worker_child_loss
    , reference_years = reference_years
    , sex_keep = F
    , ages_keep = ages_keep
    , ASFRC
    , path = path
  )
  
  data.frame(rbindlist(df_l, use.names = T))
  
}

child_survival <- function(countries, reference_years, ages_keep = 15:100, path = "../../Data/derived", ASFRC) {
  
  df_l <- lapply(
    countries
    , worker_child_survival
    , reference_years = reference_years
    , sex_keep = F
    , ages_keep = ages_keep
    , ASFRC
    , path = path
  )
  
  data.frame(rbindlist(df_l, use.names = T))
  
}

expand_asfr_age <- function(l_5_5, grouped_ages, method = "linear", col = "value") {
  
  all_ages <- min(grouped_ages):max(grouped_ages)
  
  names(l_5_5) <- paste0(names(l_5_5), ".")
  
  print("interpolating...")
  # Use a linear regression to interpolate the value
  estimates_list <- lapply(l_5_5, function(df) {
    
    # the number of ages to interpolate depends on the number of 
    # age groups present in the data
    
    val <- approx(df[ , col], method = method, n = length(all_ages))$y
    
    
    # if(length(years) != length(val)) browser()
    data.frame(
      country = unique(df$country)
      , year = unique(df$year)
      , age = all_ages
      , val = val
    )
    
  })
  
  # Save as data frame
  
  print("converting to data table...")
  yearly_estimates <- data.frame(rbindlist(estimates_list, use.names = T))
  
  yearly_estimates <- 
    yearly_estimates %>% 
    # Since year-age combinations that did not have any values for a given country were returned as NA
    # in the previous function:
    filter(!is.na(val)) %>% 
    dplyr::arrange(country, year, age)
  
  colnames(yearly_estimates)[ncol(yearly_estimates)] <- col
  
  return(yearly_estimates)
  
}

# Deprecated on 20191009 in favour of 
# expand_LT_age_by_mx_linear
# Still works but overestimated child mortality by trying to fit
# one single line between all points
# expand_LT_age_by_mx <- function(l_5_5, grouped_ages, method = "linear") {
#   
#   all_ages <- min(grouped_ages):max(grouped_ages)
#   
#   names(l_5_5) <- paste0(names(l_5_5), ".")
#   
#   print("interpolating...")
#   # Use a linear regression to interpolate the value
#   estimates_list <- lapply(l_5_5, function(df) {
#     
#     if(method == "linear") {
#       val <- approx(df$mx[order(df$age)], method = method, n = length(all_ages))$y
#     } else if (method == "spline") {
#       # Interpolate separaelty for 0-4, 5-10 and 10+
#       
#       # pos_old <- list(1:3, 4:length(grouped_ages))
#       # pos_new <- list(1:6, 7:length(all_ages)) 
#       # 
#       # old_ages <- grouped_ages[pos_old[[1]]]
#       # new_ages <- all_ages[pos_new[[1]]]
#       # 
#       # mx <- df$mx[pos_old[[1]]]
#       # 
#       # val <- predict(interpSpline(old_ages, mx), new_ages)$y      
#       
#       spline_mx <- interpSpline(grouped_ages, df$mx)
#       val <- predict(spline_mx, all_ages)$y
#     }
#       
#     # Return a df 
#       data.frame(
#         country = unique(df$country)
#         , year = unique(df$year)
#         , age = all_ages
#         , mx = val
#       )
#     
#   })
#   
#   # Save as data frame
#   
#   print("converting to data.table...")
#   
#   yearly_estimates <- rbindlist(estimates_list, use.names = T) %>% 
#     dplyr::arrange(country, year, age)
#   
#   return(yearly_estimates)
#   
# }

expand_LT_age_by_mx_linear <- function(l_5_5, grouped_ages) {
  
  all_ages <- min(grouped_ages):max(grouped_ages)
  
  names(l_5_5) <- paste0(names(l_5_5), ".")
  
  print("interpolating...")
  # Use a linear interpolation value within each interval
  # i.e. do not fit one line through all the intervals, but separate lines 
  # making sure that all the actual observed mx values remain in the interpolated
  # version
  
  # Note that the grouped nmx values correspond to the middle of the interval
  # This is straightforward for ages over 5, but creates an issue for age group 
  # 1-4, which needs to be treated separately to avoid underestimating child mortality
  # There are two ways of doing this: smoothing or simple linear interpolation
  
  # In practice, this means that the nmx value for a given interval is assumed to be the value
  # of the mid-point full age range. For exampele, the value for 5M10 is assumed to be the same as
  # 1M12 and the linear interpolation is then carried out between 1M12 and 1M17
  
  estimates_list <- lapply(l_5_5, function(df) {
    
    # print(paste(unique(df$country), unique(df$year)))
    # if(df$country == "europe") browser()
    
    mx_df <- 
      # a. expand df
      df %>% 
      arrange(age) %>% 
      select(age, interval, mx) %>% 
      mutate(interval = abs(interval))
    
    mx_new <- 
      mx_df[rep(seq_len(nrow(mx_df)), mx_df$interval), ] %>% 
      mutate(
        id = age
        , age = 0:100
        # 'default' says that for age 100, use mx value of age 100
        # , mx_lead = lead(mx, 1, default = .$mx[n()])
      ) 
    
    # a. For age 0
    # ~~~~~~~~~~~~~~~
    # Keep unchanged
    
    mx_0 <- mx_new %>% 
      filter(age %in% 0) %>% 
      select(age, mx)
    
    # b. For ages 1-6
    # ~~~~~~~~~~~~~
    # Assume that mid-point of age interval 1-4 is 3, and extrapolate to ages 1-2
    # using a linear model trained on the two existing points: 
    # nmx values for 4M1 (assumed to be equivalent to 1M3) and
    # nmx values for 5M5 (assumed to be equivalent to 1M7) and
    
    # The assumption that the middle point of the interval is 3 can be changed below
    # middle_point <- 2
    middle_point <- 3
    
    mx_1_6 <- 
      mx_new %>% 
      filter(age %in% c(middle_point, 7)) %>% 
      select(age, mx)
    
    mx_1_6_extrap <- data.frame(age = 1:6, mx = NA)
    
    mx_1_6_extrap$mx <- predict(lm(mx ~ age, data = mx_1_6), newdata = mx_1_6_extrap)
    
    # c. For ages 7-96
    # ~~~~~~~~~~~~~~~~~
    # Interpolate values linearly, assuming that 5Mx values represent the value
    # for the mid-interval age; ie 5_M_x = 1_M_{(x+2)}.
    # Therefore, the ages for which 1Mx values are 'known' are:
    
    known_ages <- seq(5, 95, 5) + 2
    
    mx_7_96 <- 
      mx_new %>% 
      filter(age %in% known_ages) %>% 
      select(age, mx) %>% 
      mutate(mx_lead = lead(mx, 1)) %>% 
      filter(!is.na(mx_lead))
    
    mx_interp <- unlist(lapply(split(mx_7_96, mx_7_96$age), function(d) {
      seq(from = d$mx, to = d$mx_lead, length.out = 6)[seq_len(5)]
    }))
    
    mx_7_96_interpol <-  data.frame(
      age = 7:96
      , mx = mx_interp
    )
    
    # Check that points actually match in mid-interval
    # plot(x=mx_7_96_interpol$age, y=mx_7_96_interpol$mx)
    # points(x = mx_7_96$age, y = mx_7_96$mx, pch= 0)
    
    # d. For ages 97-99
    # ~~~~~~~~~~~~~~~~~
    # Also extrapolate to ages 98-99 in a similar way as in point b above
    
    mx_97_99 <- 
      mx_new %>% 
      filter(age %in% c(92, 97)) %>% 
      select(age, mx)
    
    mx_97_99_extrap <- data.frame(age = 97:99, mx = NA)
    
    mx_97_99_extrap$mx <- predict(lm(mx ~ age, data = mx_97_99), newdata = mx_97_99_extrap)
    
    # e. Age 100+
    # ~~~~~~~~~~~~~
    # Keep unchanged
    
    mx_100 <- mx_new %>% 
      filter(age %in% 100) %>% 
      select(age, mx)
    
    # Consolidate everything, adding rows for ages 0 and 100
    # which remain unchanged
    
    bind_rows(
      mx_0
      , mx_1_6_extrap
      , mx_7_96_interpol
      , mx_97_99_extrap
      , mx_100
    ) %>% 
      mutate(
        country = unique(df$country)
        , year = unique(df$year)  
      ) %>% 
      select(country, year, age, mx) %>% 
      data.frame
    
  })
  
  # Save as data frame
  
  print("converting to data.table...")
  
  yearly_estimates <- rbindlist(estimates_list, use.names = T) %>% 
    dplyr::arrange(country, year, age)
  
  return(yearly_estimates)
  
}

expand_LT_year_by_mx <- function(df_5_1, method = "linear", parallel = T, numCores = 4) {
  # browser()
  ages_old <- as.character(df_5_1$year)
  df_5_1$year <- ages_old
  df_l <- split(df_5_1, df_5_1[c("country", "age")])  
  names(df_l) <- paste0(names(df_l), ".")
  
  years <- unique(sort(unlist(lapply(df_l[[1]]$year, function(d) {
    n <- as.numeric(unlist(strsplit(d, "-")))
    min(n):max(n)
  }))))
  y_range_length <- length(years)
  
  print("interpolating...")
  # Use a linear regression to interpolate the value for each age groups separately
  
  if(!parallel){
    estimates_list <- lapply(df_l, worker_expand_LT_year_by_mx)  
  } else{
    print("parallelising...")
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("method", "years", "y_range_length")
      , envir = environment()
    )
    
    clusterEvalQ(cl, library(splines))
    
    estimates_list <- parLapply(cl, df_l, worker_expand_LT_year_by_mx, method, y_range_length, years) 
    stopCluster(cl)
  }
  
  # Save as data frame
  
  print("Saving as data.table...")
  
  rbindlist(estimates_list, use.names = T) %>% 
    dplyr::arrange(country, year, age)
  
}

expected_child_death <- function(ASFRSC, lx.kids.arr, xs, mas, cos, ages_keep = NA) {
  # browser()
  # Child loss matrix
  ECLC.mat.coh<-matrix(NA,length(mas), length(cos), dimnames=list(paste(mas), paste(cos)))
  
  # Child survival matrix
  ECSC.mat.coh<-matrix(NA,length(mas), length(cos), dimnames=list(paste(mas), paste(cos)))
  
  # Get estimates
  
  for (co in cos){
    # print(co)
    for (ma in mas){
      # print(ma)
      fert.vec <- c()
      # Cohort ASFR up to a given age
      fert.vec <- ASFRSC[ASFRSC$Cohort == co & ASFRSC$Age <= ma, "ASFR"]
      
      # This condition evalutes whether all are missing
      probs <- lx.kids.arr[1:length(fert.vec),paste(ma),paste(co)]
      condition <- sum(is.na(probs)) != length(probs)
      
      if (condition){	
        # This is actually implementing Equation 1 in original paper
        children_born <- sum(fert.vec, na.rm=TRUE)
        survival_probs <- lx.kids.arr[1:length(fert.vec),paste(ma),paste(co)]
        children_surviving <- sum(fert.vec * survival_probs, na.rm=TRUE)
        
        ECLC.mat.coh[paste(ma), paste(co)]<- children_born - children_surviving
        ECSC.mat.coh[paste(ma), paste(co)] <- children_surviving
      }
    }
  }
  
  list_out <- list(ECLC.mat.coh = ECLC.mat.coh, ECSC.mat.coh = ECSC.mat.coh)
  
  # Filter only relevant ages
  if(!all(is.na(ages_keep))) {
    
    list_out <- lapply(list_out, function(mat) {
      mat[rownames(mat) %in% ages_keep, ]
    })
    
  }
  
  return(list_out)
  
}

fix_un_countries <- function(x) {
  x <- gsub("\\.|\\?", "", x)
  x <- gsub("/", "_", x)
  tolower(x)
}

format_births <- function(df) {
  
  old_names <- colnames(df)
  
  new_names <- c(
    "index", "variant", "country", "notes", "country_code", 
    "type","parent_code"
    , paste0("X", old_names[8:length(old_names)])
  )
  
  # Change colnames
  colnames(df) <- new_names
  
  # Save for future reference
  # births_codebook <- data.frame(old = old_names, new = new_names)
  # print(births_codebook)
  
  # 0.2. To long
  
  df[df == "..."] <- NA
  
  df %>% 
    filter(type != "Label/Separator") %>% 
    select(country, dplyr::starts_with("X")) %>% 
    # gather(., year, value, `X1950-1955`:`X2095-2100`) %>%
    reshape2::melt(id = c("country")) %>%
    select(country, year = variable, value) %>% 
    mutate_all(list(as.character)) %>% 
    # Remove unwanted punctuation
    mutate(
      country = fix_un_countries(country)
      , year = gsub("X", "", year)
      , value = as.numeric(value)
    ) %>% 
    arrange(country, year) 
}

# Formats table for latex visualisation
format_table <- function(df, row_keep = 29, ages = c(20,45,100), cohorts = c(1950, 1975, 1999), extra_header = T) {
  df$area[df$region != ""] <- df$region[df$region != ""]
  df$region <- NULL
  colnames(df) <- gsub("^X", "", colnames(df))
  df$area[df$area == "oceania (excluding australia and new zealand)"] <- "oceania (exc. Aus and NZ)"
  if(nchar(colnames(df)[2]) == 4)
    colnames(df)[1] <- "Birth Cohort"
  else
    colnames(df)[1] <- "Birth Cohort and Age"
  
  # Keep only first 30 rows and add extra row
  if(!is.na(row_keep)) {
    df <- df[1:row_keep,]
    df[row_keep+1, ] <- rep("...", ncol(df)) 
  }
  # Format colnames
  if(extra_header) {
    colnames(df) <- c("Age", rep(ages, length(cohorts)))
  }
  
  return(df)  
  
}

get_lx_array <- function(country_keep, reference_years, sex_keep, path = "../../Data/derived"){
  print(country_keep)
  
  file <- paste0(paste0(path, "/lx.kids.arr_", country_keep, ".RDS"))
  lx.kids.arr <- readRDS(file)  
  lx_array_temp <- lx.kids.arr[ , , paste(reference_years)]
  return(lx_array_temp)
}

interpolate_births_calendar_years <- function(df_5, method = "linear") {
  # browser()
  df_5$year <- as.character(df_5$year)
  df_l <- split(df_5, df_5[c("country")])  
  names(df_l) <- paste0(names(df_l), ".")
  
  print("interpolating...")
  # Use a linear regression to interpolate the value for each age groups separately
  estimates_list <- lapply(df_l, function(df) {
    
    years <- unique(sort(unlist(lapply(df$year, function(d) {
      n <- as.numeric(unlist(strsplit(d, "-")))
      min(n):max(n)
    }))))
    
    y_range_length <- length(years)
    
    # Get this to make sure that they are ordered proberly by year
    y <- as.numeric(str_extract(df$year, "^[0-9]{4}"))
    val <- approx(df[order(y) , 'value'], method = method, n = y_range_length)$y
    
    data.frame(
      country = unique(df$country)
      , year = years
      , value = val
    )
  })
  
  # Save as data frame
  
  print("Saving as data.table...")
  
  rbindlist(estimates_list, use.names = T) %>% 
    data.frame %>% 
    dplyr::arrange(country, year)
}

interpolate_COLUMN_calendar_years <- function(df_5_1, method = "linear", col = "mx") {
  # browser()
  df_5_1$year <- as.character(df_5_1$year)
  df_l <- split(df_5_1, df_5_1[c("country", "age")])  
  names(df_l) <- paste0(names(df_l), ".")
  
  print("interpolating...")
  # Use a linear regression to interpolate the value for each age groups separately
  estimates_list <- lapply(df_l, function(df) {
    if(nrow(df) == 0) {
      data.frame(
        country = NA
        , year = NA
        , age = NA
        , val = NA
      )
    } else {
      years <- unique(sort(unlist(lapply(df$year, function(d) {
        n <- as.numeric(unlist(strsplit(d, "-")))
        min(n):max(n)
      }))))
      y_range_length <- length(years)
      
      if(nrow(df) == 1) {
        #as the years are grouped by 5 years
        val <- rep(df[ , col], 5)
        
      } else if(nrow(df) > 1) {
        # browser()
        # Get this to make sure that they are ordered proberly by year
        y <- as.numeric(str_extract(df$year, "^[0-9]{4}"))
        val <- approx(df[order(y) , col], method = method, n = y_range_length)$y
      }
      # if(length(years) != length(val)) browser()
      data.frame(
        country = unique(df$country)
        , year = years
        , age = unique(df$age)
        , val = val
      )
    }
    
  })
  
  # Save as data frame
  
  print("Saving as data.table...")
  
  # yearly_estimates <- data.frame(do.call(rbind, estimates_list), stringsAsFactors = F)
  yearly_estimates <- rbindlist(estimates_list, use.names = T)
  
  yearly_estimates <- 
    yearly_estimates %>% 
    # Since year-age combinations that did not have any values for a given country were returned as NA
    # in the previous function:
    filter(!is.na(val)) %>% 
    dplyr::arrange(country, year, age)
  
  colnames(yearly_estimates)[ncol(yearly_estimates)] <- col
  
  return(yearly_estimates)  
  
}

# adapted from pacman package
library2 <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}

# Builds a life table by using the mortality rate schedule to calculate 
# the subsequent columns of the table
# Adapted from LifeTables::lt.mx
lt_mx <- function (nmx, sex = "female", age = c(0, 1, seq(5, 110, 5)), 
                   nax = NULL, radix = 1E5) 
  
{
  n <- c(diff(age), 999)
  if (is.null(nax)) {
    nax <- 0.5 * n
    if (n[2] == 4) {
      if (sex == "male") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.33
          nax[2] <- 1.352
        }
        else {
          nax[1] <- 0.045 + 2.684 * nmx[1]
          nax[2] <- 1.651 - 2.816 * nmx[1]
        }
      }
      if (sex == "female") {
        if (nmx[1] >= 0.107) {
          nax[1] <- 0.35
          nax[2] <- 1.361
        }
        else {
          nax[1] <- 0.053 + 2.8 * nmx[1]
          nax[2] <- 1.522 - 1.518 * nmx[1]
        }
      }
    }
  }
  nqx <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx <- c(nqx[-(length(nqx))], 1)
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1) 
      nqx[i] <- 1
  }
  nage <- length(age)
  npx <- 1 - nqx
  l0 = radix
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  Tx <- c(rev(cumsum(rev(nLx[-length(nLx)]))), 0)
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # With rounding
  # lt <- data.frame(
  #   age = age
  #   , ax = c(round(nax[-length(nax)], 3), NA)
  #   , mx = round(nmx, 4)
  #   , qx = round(nqx, 4)
  #   , px = round(npx, 4)
  #   , dx = ndx
  #   , lx = lx
  #   , Lx = c(round(nLx[-length(nLx)]), NA)
  #   , Tx = c(round(Tx[-length(Tx)]), NA)
  #   , ex = c(round(ex[-length(ex)], 2), NA)
  #   )
  
  # Without rounding
  lt <- data.frame(
    age = age
    , ax = c(nax[-length(nax)], NA)
    , mx = nmx
    , qx = nqx
    , px = npx
    , dx = ndx
    , lx = lx
    , Lx = c(nLx[-length(nLx)], NA)
    , Tx = c(Tx[-length(Tx)], NA)
    , ex = c(ex[-length(ex)], NA)
  )
  
  lt.top.age <- min(which(nqx == 1))
  lt <- lt[1:lt.top.age, ]
  return(lt)
}

lt_mx_parallel <- function(l_mx_1_1_l, numCores) {
  
  cl <- makeCluster(numCores)
  
  clusterExport(
    cl
    , varlist = c("lt_mx")
    , envir = environment()
  )
  
  estimates_list <- parLapply(cl, l_mx_1_1_l, worker_lt_mx) 
  stopCluster(cl)
  
  return(estimates_list)
}

LT_period_to_cohort <- function(df, years, ages, parallel = F, numCores = 4) {
  # Constructs pseud-cohort life tables from period life tables
  
  # where df is a period life table with columns mx, qx, and ax
  
  l <- split(df, df$Country)
  
  if(!parallel) {
    print(system.time(
      out <- lapply(l, worker_LT_period_to_cohort, years, ages)
    ))
  } else {
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("years", "ages")
      , envir = environment()
    )
    
    print(system.time(
      out <- parLapply(cl, l, worker_LT_period_to_cohort, years, ages) 
    ))
    
    stopCluster(cl)  
  }
  
  # Convert to data.table
  out_df <- data.table::rbindlist(out, use.names=TRUE)
  
  return(out_df)
}

# fun = worker_survival_probs
survival_probs_parallel <- function(l, xs, mas, cos, numCores = 4) {
  
  print(paste("Parallelising every country using", numCores, "cores."))
  
  cl <- makeCluster(numCores)
  
  # Load packages
  # clusterEvalQ(cl, library(dplyr))
  
  clusterExport(
    cl
    , varlist = c("xs", "mas", "cos")
    , envir = environment()
  )
  
  clusterEvalQ(cl, library(dplyr))
  
  print(system.time(
    out <- parLapply(cl, l, worker_survival_probs, xs, mas, cos) 
  ))
  
  stopCluster(cl)  
  
  print("Done! This function returns no object since save_output == T")
  
}

worker_apply_lt <- function(con, countries, cohorts, female_births, LTCF) {
  print(con)
  
  l <- lapply(cohorts, function(coh) {
    
    radix <- female_births %>% 
      filter(country %in% con) %>% 
      filter(year %in% coh) %>% 
      pull(value)
    
    nmx <- LTCF %>% 
      filter(Country %in% con) %>% 
      filter(Cohort %in% coh) %>% 
      pull(mx)
    
    lt_mx(nmx = nmx, age = 0:100, radix = radix) %>% 
      mutate(country = con, cohort = coh, age = 0:100) %>% 
      select(country, cohort, age, lx)  
    
  })
  
  rbindlist(l, use.names = T)
  
}

worker_child_loss <- function(country_keep, reference_years, sex_keep = F, ages_keep, ASFRC, path) {
  
  # 2.1. Get LT for chosen years
  
  lx_array_temp <- get_lx_array(
    country_keep = country_keep
    , reference_years = reference_years
    , sex_keep = sex_keep
    , path = path
  )
  
  # 2.2. Chose ASFR for chosen years
  
  ASFR_df <- 
    ASFRC %>% 
    filter(country %in% country_keep) %>% 
    filter(Cohort %in% reference_years)
  
  # 2.3. Expected child loss and child survival
  
  estimates <- expected_child_death(
    ASFR_df
    , lx.kids.arr = lx_array_temp
    , xs
    , mas
    , cos = reference_years
    , ages_keep = ages_keep 
  )
  
  ECLC.mat.coh <- estimates[[1]]
  
  # 2.4. FOrmat as data frame
  
  eclc <- as.data.frame(ECLC.mat.coh, stringsAsFactors = F)
  eclc$age <- as.numeric(rownames(eclc))
  
  eclc %>% 
    reshape2::melt(id = c("age")) %>% 
    dplyr::mutate(
      variable = as.character(variable)
      , value = as.numeric(value)
      , country = country_keep
    )
  
}

worker_child_survival <- function(country_keep, reference_years, sex_keep = F, ages_keep, ASFRC, path) {
  
  # 2.1. Get LT for chosen years
  
  lx_array_temp <- get_lx_array(
    country_keep = country_keep
    , reference_years = reference_years
    , sex_keep = sex_keep
    , path = path
  )
  
  # 2.2. Chose ASFR for chosen years
  
  ASFR_df <- 
    ASFRC %>% 
    filter(country %in% country_keep) %>% 
    filter(Cohort %in% reference_years)
  
  # 2.3. Expected child loss and child survival
  
  estimates <- expected_child_death(
    ASFR_df
    , lx.kids.arr = lx_array_temp
    , xs
    , mas
    , cos = reference_years
    , ages_keep = ages_keep 
  )
  
  ECLS.mat.coh <- estimates[[2]]
  
  # 2.4. FOrmat as data frame
  
  ecls <- as.data.frame(ECLS.mat.coh, stringsAsFactors = F)
  ecls$age <- as.numeric(rownames(ecls))
  
  ecls %>% 
    reshape2::melt(id = c("age")) %>% 
    dplyr::mutate(
      variable = as.character(variable)
      , value = as.numeric(value)
      , country = country_keep
    )
  
}

worker_expand_LT_year_by_mx <- function(df, method, y_range_length, years) {
  
  # df=df_l[[1]]
  # Test for Poland, age 100
  # df <- df_l[[which(names(df_l) %in% "Poland.100.")]]
  
  # Get this to make sure that they are ordered proberly by year
  # y <- seq(1950, 2095, 5)
  
  if(method == "linear") {
    val <- approx(df$mx, method = method, n = y_range_length)$y    
  } else if(method == "spline") {
    spline_mx <- interpSpline(seq(1, 150, 5), df$mx)
    val <- predict(spline_mx, 1:150)$y
  }
  
  data.frame(
    country = unique(df$country)
    , year = years
    , age = unique(df$age)
    , mx = val
  )
  
}

worker_lt_mx <- function(df) {
  lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E5)
  
  cbind(
    df[1:nrow(lt) , 1:2]
    , lt
  )
}

worker_LT_period_to_cohort <- function(df, years, ages) {
  # browser()
  # Constructs pseud-cohort life tables from period life tables
  
  # where df is a period life table with columns mx, qx, and ax
  
  print(unique(df$Country))
  
  # Makes sure that maximum cohort is the one for which 
  # all age groups are available in the projected data
  year_limit_max <- max(years) - max(ages)
  age_limit_max <- max(ages)
  
  # 1. lx, dx, nLx columns (ages 0-99)
  
  for (year in years){
    # print(year)
    for (age in ages){
      # print(age)
      # Get lagged and normal indices
      current_position <- which(df$Cohort == year & df$Age == age)
      lag_position <- which(df$Cohort == year & df$Age == (age - 1))
      
      # Assign new lx
      # Since the vector 'ages' starts at 1, the lagged qx value will be that of age 0 always
      df$lx[current_position] <- df$lx[lag_position] * (1 - df$qx[lag_position])
      
      # Assign new dx
      df$dx[lag_position] <- df$lx[lag_position] * df$qx[lag_position]
      
      # Assign nLx column
      if(age < age_limit_max + 1 & year < year_limit_max) {
        df$nLx[lag_position] <- 
          df$lx[current_position] + df$dx[lag_position] * df$ax[lag_position]
      }
    }
  }
  
  # 2. dx and nLx columns for for open age interval
  
  open_age <-  which(df$Age == age_limit_max & df$Cohort < year_limit_max)
  last_age <- which(df$Age== age_limit_max - 1  & df$Cohort < year_limit_max)
  open_age2 <- which(df$Age == age_limit_max)
  
  df$dx[open_age] <- df$lx[last_age] * df$qx[last_age]
  df$nLx[open_age2] <- df$ax[open_age2] * df$dx[open_age2]
  
  # 3. Tx column (persons alive at any point in time above age x)
  
  for (year in years){
    if (year < year_limit_max)
      df$Tx[df$Cohort == year] <- rev(cumsum(rev(df$nLx[df$Cohort == year])))
  }
  
  # 4. Compute ex
  
  df$ex<-df$Tx/df$lx
  
  # 5. Return only relevant rows
  
  # df <- df[df$Cohort <= year_limit_max, ]
  df$Year <- NULL
  
  return(df)
}

worker_survival_probs <- function(life_table, xs, mas, cos) {
  # browser()
  
  pais <- unique(life_table$Country)
  life_table$Country <- NULL
  
  # Create array of 2x2 matrices
  # one matrix for every cohort
  # each matrix has rows = xs and cols = mas
  
  lx.kids.arr <- array(
    NA
    , dim=c(length(xs), length(mas), length(cos))
    , dimnames=list(paste(xs), paste(mas), paste(cos))
  )
  
  for (co in cos){ 
    # For every column
    for (ma in mas){
      lx.kids <- c()
      # for every row
      for (x in xs){
        tmp <- c()
        
        # This is the important bit: 
        # It only applies for cases where ma >= x
        
        # Get the survival probability for a child given:
        child_age <- ma - x # because 
        child_cohort <- co + x
        
        tmp <- life_table$lx[life_table$Age == child_age & life_table$Cohort == child_cohort]
        
        if (length(tmp) > 0){
          lx.kids<-c(lx.kids, tmp)
        }
        
      }
      if (length(lx.kids)>0)
        lx.kids.arr[1:length(lx.kids),paste(ma),paste(co)] <- lx.kids
    }
  }
  
  # Export to file in pc
  
  nombre <- paste0("lx.kids.arr_", pais)
  # assign(x = nombre, value = lx.kids.arr)
  
  file <- paste0("../../Data/derived/", nombre, ".RDS")
  
  saveRDS(object = lx.kids.arr, file = file)
  print(paste(file, "saved"))
  
}