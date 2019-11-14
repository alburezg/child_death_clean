
# Add new functions alphabetically to this file

age_at_death <- function(death_l, p, parallel = T, numCores = 8) {
  
  if(!is.data.table(p)) p <- as.data.table(p)
  p <- p[, c("profileid", "birth_year")]
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    out_l <- lapply(death_l, worker_get_ages, p)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("death_l", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    print('Start computations...')
    
    out_l <- parLapply(cl, death_l, worker_get_ages, p) 
    
    stopCluster(cl)
  }
  
  return(out_l)
  
}

age_at_death_all <- function(
  death_l
  , p
  , parallel = T
  , numCores = 8
  , worker_function = worker_get_ages
  , adjust_own_mortality = F
) {
  
  if(!is.data.table(p)) p <- as.data.table(p)
  p <- p[, c("profileid", "birth_year", "death_year")]
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    out_l <- lapply(death_l, worker_function, p, adjust_own_mortality)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("death_l", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    print('Start computations...')
    
    out_l <- parLapply(cl, death_l, worker_function , p, adjust_own_mortality) 
    
    stopCluster(cl)
  }
  
  return(out_l)
  
}

age_frequency <- function(df, low = 0, high = 100) {
  
  keep1 <- df$age_death >= low & df$age_death <= high
  df <- df[keep1, ]
  keep2 <- !is.infinite(df$age_death)
  df <- df[keep2, ]
  
  n <- sum(!keep1, na.rm = T) + sum(!keep2, na.rm = T)
  print(paste("Dropped", n, "values (age out of bounds)."))
  
  f <- df %>% dplyr::count(age_death, coh)
  
  # f <- table(df$age_death, df$age_death)
  # f <- as.data.frame(f)
  # f$a <- as.numeric(f$a)
  
  return(f)
  
}

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

# Get psuedo census of population alive in given year or period
# from genealogical data
census <- function(df, from, to = NA, return_ids = T, group_by_sex = F, group_by_age_sex = F, replace_greater_100 = "100+") {
  
  if (is.na(to)) to <- from
  
  if (from == to) df$census <- from
  else df$census <- paste(from,to,sep = "-")
  
  df$death_temp <- df$death
  df$death_temp[df$is_alive == 1] <- 2100
  
  year_df <- 
    df %>% 
    dplyr::filter(birth <= to & death_temp >= from)
  
  if(return_ids) {
    out <- year_df$profileid
  } else {
    if(group_by_sex) {
      out <- year_df %>% 
        dplyr::count(gender, census)
    } else if(group_by_age_sex) {
      out <- year_df %>% 
        dplyr::mutate(
          age_at_census = census - birth
          , age_at_census = ifelse(age_at_census > 100, replace_greater_100, age_at_census) 
        ) %>% 
        dplyr::count(gender, age_at_census, census) %>% 
        tidyr::complete(gender, age_at_census, census, fill = list(n = 0))
    }
    else{
      out <- year_df
    }
    
  } 
  
  return(out)
  
}

change_cohort <- function(l1, p, breaks, labels, 
                          parallel = T, numCores = 8) {
  # browser()
  p <- p[,c("profileid", 'birth_year')]
  
  if(!parallel) {
    l3 <- lapply(l1, worker_recohort, p, breaks, labels)   
  } else {
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    
    clusterExport(
      cl
      , varlist = c("l1","p", "breaks", "labels")
      , envir = environment()
    )
    
    l3 <- parLapply(cl, l1, worker_recohort, p, breaks, labels)
  }
  return(l3)
}

# Use to change fertility levels without altering mean age at childbearing given a vector of ASFR
change_fertility_levels <- function(multiply_by, base_year, ASFRSC) {
  
  ASFR <- ASFRSC[ASFRSC$Cohort == base_year, ]
  ASFR$ASFR <- ASFR$ASFR*multiply_by
  ASFR$Cohort<- paste(multiply_by)
  return(ASFR)
  
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

# 20190927
# use in "experimental_tricolore.R' to create (using lapply)
# A series of maps for each cohort
cl_tric <- function(coh, cl_temp) {
  
  # 1.1. Denominator  
  
  # Total number of child deaths at mother's death
  # for each country and cohort
  
  cl_denom <- 
    cl_temp %>% 
    filter(cohort == coh) %>% 
    filter(age == max_age) %>% 
    select(country, denom = value)
  
  # 1.2. Enumerator 
  
  # Since CL is cumulative, just take the values at the end of the given age intervals
  
  cl_enumer <- 
    cl_temp %>% 
    filter(cohort == coh) %>% 
    filter(age %in% agegr) %>% 
    # Now substract previous amount to get only deaths in given age group
    group_by(country) %>% 
    mutate(enum = value - lag(value)) %>% 
    ungroup %>% 
    filter(!is.na(enum)) %>% 
    select(country, age, enum)
  
  
  # 1.3. Share ==
  
  cl_share <- merge(
    cl_enumer
    , cl_denom
    , by = "country"
    , all.x = T
  ) %>% 
    mutate(value = enum/denom) %>% 
    select(country, age, value)
  
  # As wide
  
  cl_w <- spread(cl_share, age, value) 
  colnames(cl_w) <- c("country", c_names)
  
  # 1.4. Plot ===
  
  # https://github.com/jschoeley/tricolore
  
  # Get colours 
  
  colours <- Tricolore(
    df = cl_w
    , c_names[3]
    , c_names[2]
    , c_names[1]
    # , breaks = Inf
  )
  
  cl_w$rgb <- colours$rgb
  
  # Map edits
  
  # Convert to isocodes
  
  # Merge
  
  w <- merge(
    world
    , cl_w
    , by = "country"
    , all.x = T
  ) %>% 
    select(ID, geometry, rgb) %>% 
    filter(!is.na(rgb))
  
  # Plot
  p <- 
    ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = rgb)) +
    scale_fill_identity() +
    annotation_custom(
      ggplotGrob(
        colours$key +
          labs(L = c_names[3], T = c_names[2], R = c_names[1]))
      , xmin = -180, xmax = -100, ymin = -60, ymax = 20
    ) +
    labs(
      subtitle = paste("Woman's birth cohort:", coh)
      # , title = "Age group at which women experience the death of a child"
      
    ) +
    theme_void() +
    coord_sf(datum = NA)
  
  # Export ===
  
  ggsave(paste0('map/map_', coh, ".pdf"), p, width = 8, height =  3)
  print(paste0("Map saved", coh))
  
}

# Apply statistical test to compare familinx and model estimates
compare_series <- function(df1, df2, fun, extract = "") {
  
  if(extract == "") {
    vapply(2:ncol(df1), function(n) {
      fun(df1[ , n], df2[, n])
    }, numeric(1) )
  } else {
    vapply(2:ncol(df1), function(n) {
      fun(df1[ , n], df2[, n])[[paste(extract)]]
    }, numeric(1) )
  }
  
}

cummulative_no_deaths <- function(age_at_death, age_range) {
  cumsum(age_range %in% age_at_death)  
}

death_date_rel <- function(p, rel, p_full) {
  
  lines <- match(p[ ,rel], p_full$profileid)
  DoD_rel <- p_full$death_year[lines]
  
  return(DoD_rel)
}

# For legacy only, use active version below 
# 20190627
expand_LT_age_by_col <- function(l_5_5, grouped_ages, method = "linear", col = "mx") {

  all_ages <- min(grouped_ages):max(grouped_ages)

  names(l_5_5) <- paste0(names(l_5_5), ".")

  print("interpolating...")
  # Use a linear regression to interpolate the value
  estimates_list <- lapply(l_5_5, function(df) {
    if(nrow(df) == 0) {
      data.frame(
        country = NA
        , year = NA
        , age = NA
        , val = NA
      )
    } else {
      if(nrow(df) == 1) {
        #as the years are grouped by 5 years
        val <- rep(df[ , col], 5)
        age <- unique(df$age)
      } else if(nrow(df) > 1) {
        # the number of ages to interpolate depends on the number of
        # age groups present in the data
        # Add 4 to get upper boudary of age group

        # age <- age_list[[which(names(age_list) %in% as.character(max(df$age)))]]
        age <- all_ages
        val <- approx(df[order(df$age) , col], method = method, n = length(age))$y

      }
      # if(length(years) != length(val)) browser()
      data.frame(
        country = unique(df$country)
        , year = unique(df$year)
        , age = age
        , val = val
      )
    }
  })

  # Save as data frame

  print("converting to data.table...")
  yearly_estimates <- rbindlist(estimates_list, use.names = T)
  # yearly_estimates <- data.frame(do.call(rbind, estimates_list), stringsAsFactors = F)

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

# This must be the one that is used for expanding abridged life tables
expand_LT_age_by_qx <- function(l_5_5, grouped_ages) {
  
  all_ages <- min(grouped_ages):max(grouped_ages)
  
  names(l_5_5) <- paste0(names(l_5_5), ".")
  
  print("interpolating...")
  # Use a linear regression to interpolate the value
  estimates_list <- lapply(l_5_5, function(df) {
    
    # Ignore open interval
    
    index <- nrow(df) - 1 # ignore open interval
    
    # Get value for 1_q_x 
    # Assuming that the probability of dying is connstant over the 5-year 5_q_x
    # interval. Ergos, I assume the values estaimated below to refer to the middle
    # 1_q_x for the grouped 5_q_x, so that for example:
    # 1_q_1 = 1_q_2 = 1_q_3 = 1_q_4 = 1 - (1 - 5_q_0)^(1/5)
    middle_qx <- unlist(
      lapply(1:index, function(row) {
        # Interval lengths in grouped age bands
        1 - (1 - df$qx[row])^(1/df$interval[row])
      })
    )
    
    # Expand by interval length to create a new data frame of qx values
    # print(unique(df$country))
    
    if(unique(df$country) =='europe') browser()
    
    expand_qx <- rep(middle_qx, df$interval[1:index])
    
    data.frame(
      country = unique(df$country)
      , year = unique(df$year)
      , age = all_ages
      , qx = c(expand_qx, NA) # Add qx value for open age interval
    )
  })
  
  # Save as data frame
  
  print("converting to data.table...")
  
  yearly_estimates <- rbindlist(estimates_list, use.names = T) %>% 
    dplyr::arrange(country, year, age)
  
  return(yearly_estimates)
  
}

# expand_LT_age_by_qx <- function(l_5_5, grouped_ages, method = "linear") {
#   
#   all_ages <- min(grouped_ages):max(grouped_ages)
#   
#   names(l_5_5) <- paste0(names(l_5_5), ".")
#   
#   print("interpolating...")
#   # Use a linear regression to interpolate the value
#   estimates_list <- lapply(l_5_5, function(df) {
#     
#     # Ignore open interval
#     
#     index <- nrow(df) - 1 # ignore open interval
#     
#     # Get value for 1_q_x 
#     # Assuming that the probability of dying is connstant over the 5-year 5_q_x
#     # interval. Ergos, I assume the values estaimated below to refer to the middle
#     # 1_q_x for the grouped 5_q_x
#     middle_qx <- unlist(
#       lapply(1:index, function(row) {
#         # Interval lengths in grouped age bands
#         1 - (1 - df$qx[row])^(1/df$interval[row])
#         
#       })
#     )
#     
#     # Interpolate using log scale since I am interpolating rates
#     # only 100 values, not 101, since age 100 is NA
#     val <- exp(approx(log(middle_qx), method = method, n = 100)$y)
#     
#       data.frame(
#         country = unique(df$country)
#         , year = unique(df$year)
#         , age = all_ages
#         , qx = c(val, NA) # Add qx value for open age interval
#       )
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

expand_LT_year_by_qx <- function(df_5_1, method = "linear") {
  # browser()
  df_5_1$year <- as.character(df_5_1$year)
  df_l <- split(df_5_1, df_5_1[c("country", "age")])  
  names(df_l) <- paste0(names(df_l), ".")
  
  years <- unique(sort(unlist(lapply(df_l[[1]]$year, function(d) {
    n <- as.numeric(unlist(strsplit(d, "-")))
    min(n):max(n)
  }))))
  y_range_length <- length(years)
  
  print("interpolating...")
  # Use a linear regression to interpolate the value for each age groups separately
  estimates_list <- lapply(df_l, function(df) {
    
    # Get this to make sure that they are ordered proberly by year
    y <- as.numeric(str_extract(df$year, "^[0-9]{4}"))
    
    if(all(is.na(df$qx))) {
      # qx is always 1 for open interval
      val <- rep(1, y_range_length)
    } else {
      val <- approx(df$qx[order(y)], method = method, n = y_range_length)$y  
    }
    
    # if(length(years) != length(val)) browser()
    data.frame(
      country = unique(df$country)
      , year = years
      , age = unique(df$age)
      , qx = val
    )
    
  })
  
  # Save as data frame
  
  print("Saving as data.table...")
  
  # yearly_estimates <- data.frame(do.call(rbind, estimates_list), stringsAsFactors = F)
  yearly_estimates <- rbindlist(estimates_list, use.names = T) %>% 
    dplyr::arrange(country, year, age)
  
  return(yearly_estimates)  
  
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

find_kommun <- function(point_l, kom_l, parallel = T, numCores = 8) {
  # browser()
  
  if(!parallel) {
    print("Locating kommuner (non-parallelised).")
    
    kommuner <- lapply(point_l, worker_find_kommun, parallel, numCores)
    
  } else{
    
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    clusterEvalQ(cl, library(sp))
    
    clusterExport(
      cl
      , varlist = c("kom_l", "point_l")
      , envir = environment()
    )
    
    kommuner <- parLapply(cl, point_l, worker_find_kommun, kom_l) 
    
    stopCluster(cl)
  }
  
  kom_name <- unlist(kommuner, use.names = F)
  
  return(kom_name)
  
}

find_relatives <- function(p, type, rm_ego = T, 
                           rm_parent = T,
                           parallel = T, numCores = 8) {
  
  if(is.data.table(p)) p <- as.data.frame(p)
  
  if(type == 'mother') {
    
    rel_out <- as.list(p$mother)
    names(rel_out) <- p$profileid
    
  }
  
  # Of both parents
  if(type == 'sister') {
    
    rel_out <- keep_sib(
      prof
      , rm_ego
      , keep_sex = 'female'
      , parallel
      , numCores
    )
    
  }
  
  if(type == 'grandmother_maternal') {
    
    through <- "mother" # Maternal line
    rel_type <- "mother" # get data on (grand)mother
    
    parent <- p[, through]
    parents_id <- match(parent, p$profileid)
    grandpas <- p[parents_id, rel_type]
    
    keep <- !is.na(grandpas) #Only those with known grandparents
    rel_out <- as.list(grandpas[keep])
    names(rel_out) <- p$profileid[keep]
    
    # p %>% 
    #   dplyr::filter(profileid %in% c(264, 69836161)) %>% 
    #   select(profileid, mother)
    
  }
  
  if(type == 'aunt_maternal') {
    
    rel_out <- keep_aunt_uncle(
      prof
      , through = 'mother'
      , rm_parent
      , keep_sex = 'female'
      , parallel
      , numCores
    )
  } 
  
  if(type == 'children_maternal') {
    
    rel_out <- keep_children(
      prof
      , through = 'mother'
      , keep_sex = NA
      , parallel
      , numCores
    )
  }
  
  if(type == 'children_paternal') {
    
    rel_out <- keep_children(
      prof
      , through = 'father'
      , keep_sex = NA
      , parallel
      , numCores
    )
  }
  
  return(rel_out)
  
}

first_death <- function(r_list, p, parallel = T, numCores = 8, 
                        cohort_breaks, cohort_labels) {
  
  ids <- names(r_list)
  
  # Get birth cohort 
  
  birth <- as.numeric(p$birth_year[p$profileid %in% ids])
  # birth[birth < min(cohort_breaks) & birth > max(cohort_breaks)] <- NA
  
  coh <- cut(
    birth
    , breaks = cohort_breaks
    , include.lowest = TRUE
    , labels = cohort_labels
    , right = F
  )
  
  # Filter out out-of tear boundary inds
  drop <- unlist(lapply(coh, is.na))
  
  coh <- coh[!drop]
  r_list <- r_list[!drop]
  
  if(!is.data.table(p)) p <- as.data.table(p)
  col <- "death_year"
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    dates <- lapply(r_list, worker_get_dates, col, p)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("col", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    dates <- parLapply(cl, r_list, worker_get_dates, col, p) 
    
    stopCluster(cl)
  }
  
  out_l <- list(dates = dates,coh = coh)
  
  return(out_l)
  
}

fix_swedish_chars <- function(txt) {
  txt <- gsub("Ã¥", "å", txt)
  txt <- gsub("Ã¤", "ä", txt)
  txt <- gsub("Ã¶", "ö", txt)
  txt <- gsub("Ã…", "Å", txt)
  txt <- gsub("Ã„", "Ä", txt)
  txt <- gsub("Ã–", "Ö", txt)
  # Danish
  txt <- gsub('Ã¸', "ø", txt)
  # Estonian
  txt <- gsub('Ãµ', "õ", txt)
  # Icelandic
  txt <- gsub('Ãƒ', "ƒ", txt)
  txt <- gsub('Â¤', "â", txt)
  return(txt)
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

format_doExtrapolate <- function(df, age_span) {
  X <- df %>% 
    rename(
      LocID = country
      , ReferencePeriod = period
      , DataValue = mx
      , AgeStart = age
    ) %>% 
    mutate(
      SexID = 2
      , IndicatorID = "nMx"
    ) %>% 
  select(LocID, ReferencePeriod, DataValue, AgeStart, SexID, IndicatorID) %>% 
  arrange(LocID, ReferencePeriod, AgeStart)
  
  X$AgeSpan <- age_span
  X
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
  df <- df[1:row_keep,]
  df[row_keep+1, ] <- rep("...", ncol(df))
  # Format colnames
  if(extra_header) {
    colnames(df) <- c("Age", rep(ages, length(cohorts)))
  }
  
  return(df)  
  
}

get_asfr <- function(df, sex_keep, y_range, age_breaks) {
  
  # 1. Denominator - women in reproductive years
  den <- lapply(y_range, get_women_reproductive_age
                , df = df
                , age_breaks = age_breaks
                , sex_keep = "female"
  ) 
  
  denominator <- data.frame(do.call(rbind, den))
  
  # 2. Enumerator - births to women in given age group
  
  enumerator <- yearly_birth_by_age(df, sex_keep, y_range)
  
  # 3. Rate
  
  asfr <- 
    bind_cols(denominator %>% rename(den = n),
              enumerator %>% select(enu = n)) %>% 
    dplyr::mutate(
      asfr = enu/den
    )
  
  return(asfr)
  
}


# Estimates total number of children reported for profiles
# for which children were found
# NOTE that it ignores those without children
get_completed_fertility <- function(prof_df, type, relatives_ids_list) {
  
  who <- ifelse(type == "mother", "mat", "pat")
  sex_keep <- ifelse(type == "mother", "female", "male")
  
  parent_type_df <- 
    prof_df %>% 
    dplyr::filter(gender == sex_keep)
  
  # These are the women for which children are known
  children_of_parent <- relatives_ids_list[[paste0('r_child_',who)]]
  
  # Subset df to keep only eligible mothers
  # ie only mothers for whom children could be found 
  
  parent_df <- 
    parent_type_df %>% 
    dplyr::filter(profileid %in% names(children_of_parent))
  
  # 1.1.3. Get lifespan of children by mother's birth year
  # For this, each mother has to be assigned to a unique cohort
  
  parents_and_children <- children_of_parent[names(children_of_parent) %in% parent_df$profileid]
  
  no_children <- unlist(lapply(parents_and_children, length))
  
  children_df <- data.frame(
    profileid = as.numeric(names(no_children))
    , completed_fert = no_children
    , stringsAsFactors = F
  )
  
  return(children_df)
  
}

get_correlation <- function(df, use="complete.obs") {
  
  tryCatch({
    cor(df, use = use)[2]
  }, error = function(e) {
    NA
  })
  
}

# ADapted from first_death, for all relative death's
get_death_dates <- function(r_list, p, parallel = T, numCores = 8, 
                            cohort_breaks, cohort_labels
                            , worker_function = worker_get_dates_all
) {
  
  ids <- names(r_list)
  
  # Get birth cohort 
  
  birth <- as.numeric(p$birth_year[p$profileid %in% ids])
  # birth[birth < min(cohort_breaks) & birth > max(cohort_breaks)] <- NA
  
  coh <- cut(
    birth
    , breaks = cohort_breaks
    , include.lowest = TRUE
    , labels = cohort_labels
    , right = F
  )
  
  # Filter out out-of tear boundary inds
  drop <- unlist(lapply(coh, is.na))
  
  coh <- coh[!drop]
  r_list <- r_list[!drop]
  
  if(!is.data.table(p)) p <- as.data.table(p)
  col <- "death_year"
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding data (non-parallelised).")
    dates <- lapply(r_list[1:1000], worker_function, col, p)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("col", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    dates <- parLapply(cl, r_list, worker_function, col, p) 
    
    stopCluster(cl)
  }
  
  out_l <- list(dates = dates,coh = coh)
  
  return(out_l)
  
}

get_lx_array <- function(country_keep, reference_years, sex_keep, path = "../../Data/derived"){
  print(country_keep)
  
  file <- paste0(paste0(path, "/lx.kids.arr_", country_keep, ".RDS"))
  lx.kids.arr <- readRDS(file)  
  lx_array_temp <- lx.kids.arr[ , , paste(reference_years)]
  return(lx_array_temp)
}

# Get share tha each age group are of total poulation given a df
get_pop_share <- function(pop_df) {
  totals <- sum(pop_df$pop) 
  share <- pop_df$pop / totals
  return(share)
}

# Gets YEARLY breakdown of women (or men) in reproductive age
# For estimating ASFR
get_women_reproductive_age <- function(df
                                       , year
                                       , age_breaks
                                       , sex_keep = "female"
) {
  
  df$census <- year
  
  df$death_temp <- df$death_year
  df$death_temp[df$is_alive == 1] <- 2100
  
  out <- 
    df %>% 
    dplyr::filter(birth_year <= year & death_temp >= year) %>% 
    dplyr::filter(gender %in% sex_keep) %>%  
    dplyr::mutate(
      age_at_census = census - birth_year
      , agegr_at_census = cut(age_at_census, age_breaks, include.lowest = TRUE, 
                              right = F)
      , agegr_at_census = as.character(agegr_at_census)
    ) %>% 
    dplyr::filter(!is.na(agegr_at_census)) %>% 
    dplyr::count(agegr_at_census, census) %>% 
    tidyr::complete(agegr_at_census, census, fill = list(n = 0)) %>% 
    select(year = census, agegr = agegr_at_census, n) %>% 
    arrange(year, agegr)
  
  return(out)
}

# Get ex summary measure by region using WPP life table data
# Input are cohort LT data, which were created manually from 
# the WPP-given Period LT data
get_wpp_ex_region <- function(LT, un, sex_lab, default_region, old_sdg8) {
  
  LT_m <- merge(
    LT %>% filter(Cohort < 2000) %>% filter(Age == 0) 
    , un %>% filter(type == 'country')
    , by.x = 'Country'
    , by.y = 'level1'
    # , all.y = T
  ) %>% 
    mutate(
      region = factor(default_region, levels = old_sdg8)
    ) %>% 
    select(region, type, country = Country, year = Cohort, value = ex) %>% 
    arrange(country, year)
  
  sum_cols(df = LT_m) 
  
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

# # DEPRECATED 20190619 IN FAVOUR OF interpolate_COLUMN_calendar_years
# interpolates age-specific rates over a projection horizon applying
# linear regression to each vector of country/agr group
# Returns a long data frame with yearly age-specific values in the given age intervals
# present in the original data
# interpolate_calendar_years <- function(df_5_5, year_range, method = "linear") {
#   
#   df_l <- split(df_5_5, df_5_5[c("country", "age")])  
#   names(df_l) <- paste0(names(df_l), ".")
#   y_range_length <- length(year_range)
#   
#   # Use a linear regression to interpolate the value
#   estimates_list <- lapply(df_l, function(df) {
#     approx(df$value, method = method, n = y_range_length)$y
#   })
#   
#   # Save as data frame
#   
#   estimates <- unlist(estimates_list)
#   yearly_estimates <- data.frame(do.call(rbind, strsplit(names(estimates), "\\.")), stringsAsFactors = F)
#   yearly_estimates$value <- unlist(estimates)
#   
#   colnames(yearly_estimates) <- c("country", "age", "year", "value")
#   yearly_estimates$year <- year_range
#   
#   yearly_estimates <- 
#     yearly_estimates %>% 
#     dplyr::arrange(country, year, age)
#   
#   return(yearly_estimates)
#   
# }

# interpolates age-specific rates over a projection horizon applying
# linear regression to each vector of country/agr group
# Returns a long data frame with yearly age-specific values in the given age intervals
# present in the original data
interpolate_lx_calendar_years <- function(df_5_5, year_range, method = "linear") {
  
  df_l <- split(df_5_5, df_5_5[c("country", "age")])  
  names(df_l) <- paste0(names(df_l), ".")
  y_range_length <- length(year_range)
  
  # Use a linear regression to interpolate the value
  estimates_list <- lapply(df_l, function(df) {
    approx(df$lx, method = method, n = y_range_length)$y
  })
  
  # Save as data frame
  
  estimates <- unlist(estimates_list)
  yearly_estimates <- data.frame(do.call(rbind, strsplit(names(estimates), "\\.")), stringsAsFactors = F)
  yearly_estimates$value <- unlist(estimates)
  
  colnames(yearly_estimates) <- c("country", "age", "year", "lx")
  yearly_estimates$year <- year_range
  
  yearly_estimates <- 
    yearly_estimates %>% 
    mutate(
      age = as.numeric(age)
      , year = as.numeric(year)
      ) %>% 
    dplyr::arrange(country, year, age)
  
  return(yearly_estimates)
  
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


# If correct_errors, then uses linear interpolation in the 2-4 age group for cases
# where there are large variations in mortlaity at younger ages, thus preventing 
# increases in lx
interpolate_lx_age <- function(lx, old_ages, new_ages = 0:100, correct_errors = T) {
  # browser()
  spline_lx <- interpSpline(old_ages, lx)
  lx_extended <- predict(spline_lx, 0:100)$y
  
  if(correct_errors) {
    # Check that lx is still monotonos decreasing function
    # non_negative_change indicates the number of ages for which this is not true
    # therefore, should be 0!!
    error_present <- sum((diff(lx_extended)>0)) > 0
    
    # If there a non-decreasing value, this is likely due to the fact that 
    # there were large changes in mortality at younger ages
    # One solution is to interpolate these values linearly in the first two age groups
    # which is where the largest changes in mortality are to be found
    if(error_present) {
      spline_lx <- interpSpline(old_ages[-c(1,2)], lx[-c(1,2)])
      
      lx_extended = c(
        # first two age groups
        lx = c(lx[1], lx[2]
         # linear interpolation for 1-4 agr group
        , lx[2] - (lx[2] - lx[3]) / 4 * 1:3
        # spline interpolation for the rest
        , predict(spline_lx, 5:100)$y)
      )
    }
  } 
  
  return(lx_extended)
  
}


keep_aunt_uncle <- function(p, through = 'mother', rm_parent, 
                            keep_sex, parallel, numCores) {
  
  # browser()
  print("Keeping only relevant cases...")
  #Filter out non-applicable cases
  p <- p %>% dplyr::mutate(parents = paste0(father, "_",mother))
  
  p2 <- p %>% 
    dplyr::filter(!is.na(father) & !is.na(mother))
  
  # Keep only those where mother has known granparent of given sex
  
  parent <- p2[, through]
  
  grandpas <- p2[match(parent, p2$profileid), ] %>% 
    select(grandma = mother, grandpa = father) 
  
  p3 <- cbind(
    p2 ,
    grandpas
  ) %>% 
    select(profileid, parents, grandma, grandpa, parent_ref = dplyr::contains(through)) %>% 
    na.omit %>% 
    dplyr::mutate(
      grandpas = paste(grandpa, grandma, sep = "_")
    ) %>% 
    select(profileid, parents, grandpas, parent_ref)
  
  print("Done.")
  
  print("Start locating aunts or uncles...")
  
  args <- split(p3, seq(nrow(p3)))
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding auts (non-parallelised).")
    out <- lapply(args[1:100], worker_find_aunts_uncles, p, rm_parent, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "rm_parent", "keep_sex")
      , envir = environment()
    )
    
    out <- parLapply(cl, args, worker_find_aunts_uncles
                     , p, rm_parent, keep_sex) 
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}


keep_children <- function(p, through, keep_sex, parallel, numCores) {
  # browser()
  print("Keeping only relevant cases...")
  
  p2 <- p %>% dplyr::rename(parent = dplyr::contains(through)) 
  
  # Keep only those who are mothers/fathers
  parents_id <- na.omit(unique(unlist(p2$parent)))
  
  p3 <- p2 %>% 
    dplyr::filter(profileid %in% parents_id)
  
  args <- p3$profileid
  
  print("Done.")
  print("Start locating children...")
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding children (non-parallelised).")
    out <- lapply(args, worker_find_children, p2, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "p2", "keep_sex")
      , envir = environment()
    )
    
    print(system.time(
      out <- parLapply(cl, args, worker_find_children, p2, keep_sex) 
    ))
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}

# Get siblings ids as list
keep_sib <- function(p, rm_ego, keep_sex, parallel, numCores) {
  # browser()
  print("Keeping only relevant cases...")
  #Filter out non-applicable cases
  p2 <- 
    p %>% 
    dplyr::filter(!is.na(father) & !is.na(mother)) %>% 
    dplyr::mutate(parents = paste0(father, "_",mother))
  
  keep <- 
    p2 %>% 
    dplyr::count(parents) %>% 
    dplyr::filter(n > 1) %>% 
    pull(parents)
  
  p3 <- p2 %>% dplyr::filter(parents %in% keep)
  
  print("Done.")
  
  print("Start locating siblings...")
  
  args <- split(select(p3, profileid, parents), seq(nrow(p3)))
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding siblings (non-parallelised).")
    out <- lapply(args, worker_find_sib, p, p3, rm_ego, keep_sex)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    # Load packages
    # clusterEvalQ(cl, library(dplyr))
    
    clusterExport(
      cl
      , varlist = c("p", "p3", "rm_ego", "keep_sex")
      , envir = environment()
    )
    
    print(system.time(
      out <- parLapply(cl, args, worker_find_sib, p, p3, rm_ego, keep_sex) 
    ))
    
    stopCluster(cl)
  }
  
  names(out) <- p3$profileid
  
  return(out)
  
}

kommun_to_laen <- function(kommuner) {
  
  codes <- 
    swemaps::map_kn %>% 
    select(knnamn, lnnamn) %>% 
    dplyr::distinct(knnamn, lnnamn) %>% 
    dplyr::mutate_all(funs(fix_swedish_chars))
  
  lookup <- codes$lnnamn
  names(lookup) <- codes$knnamn
  
  laener <- lookup[kommuner]
  
  return(laener)
  
}

# Gives a matrix with projected populations from a leslie matrix
leslie_get_share <- function(LT, ASFR, base_year, max_age, pop, projection_horizon, ages_keep, one.sex, rescale_by_age = T) {
  
  # Get lx - vector  of  either  age-specific  cumulative  survival  or  person-years  lived  in  the interval
  
  # For Sweden, this can be obtained from the lifwe tables that we have been using so far
  # Note that lx values are given in 1-year intervals
  
  lx <- 
    LT %>% # Cohort life table
    # dplyr::filter(Cohort %in% base_year) %>%
    select(Age, lx = nLx)
  
  # Get mx - age-specific fertility rates
  
  mx <- ASFR %>% 
    # dplyr::filter(Cohort %in% base_year) %>% 
    select(Age, mx = ASFR) 
  
  input <- merge(lx, mx, by = "Age", all.x = T)
  input$mx[is.na(input$mx)] <- 0
  
  # Get matrix
  
  A <- leslie.matrix(
    lx = input$lx
    , mx = input$mx
    , L = T # if ’FALSE’,lxis taken to be cumulative survival to exact age x+n.
    , peryear = 1
    , one.sex = one.sex
    , SRB = 1.05
    , infant.class = T # this should be F, but I think there is a mistake in the source code of this function, 
    # since the second value of mx is always deleted, irrespective of whether infant class is T or F
    # but it doesn't really matter in practical terms
  )
  
  # Project the leslie matrix over n years
  
  # Get baseline population distribution in Sweden
  
  age_range <- unique(input$Age)[-2] # to account for the error described above. These are non-reproductive ages, so 
  # it doesn't matter or the empirical analysis
  
  suppressWarnings(
    no <- pop %>% 
      dplyr::mutate(
        Age = as.numeric(Age)
        , Female = Female/10E3
      ) %>% 
      dplyr::filter(Year %in% base_year) %>% 
      dplyr::filter(Age %in% age_range) %>% 
      dplyr::pull(Female)
  )
  
  N <- project.leslie(
    A = A
    , no = no
    , tmax = projection_horizon
  )
  
  
  
  # Get population distribution by year
  # keeping only relevant ages. ie.25-54
  # It matters whether specific ages will be kept or not
  # The reason is that the total share of the population that each age group 
  # corresponds to can be estiamted respective to the TOTAL population or
  # the population existnig in the chosen age groups
  # This is only relevant when returning the share of the population, not the
  # actual population numbers
  # The next steps defines the appropriate denominator for estimating the populatino shares:
  if(rescale_by_age) {
    # Filter ages by row in matrix
    totals <- colSums(N[ages_keep, ])
  } else {
    totals <- colSums(N)
  }
  
  share <- as.data.frame(t(N)/totals)
  colnames(share) <- 1:ncol(share)  
  share <- share[ , as.numeric(colnames(share)) %in% ages_keep]
  share$year <- 0:projection_horizon  
  
  return(share)
  
}

# Create leslie matrix
# Adapted from demogR package
leslie.matrix <- function (lx, mx, L = TRUE, peryear = 5, one.sex = TRUE, SRB = 1.05, 
                           infant.class = TRUE, max_age = 105) {
  len1 <- length(lx)
  len2 <- length(mx)
  if (len1 > len2) {
    warning("length of lx greater than the length of mx,\n lx truncated to length of mx")
    lx <- lx[1:len2]
  }
  if (len2 > len1) {
    mx <- mx[1:len1]
  }
  if (infant.class) {
    mx <- mx[-2]
  }
  fages <- which(mx > 0)
  # k <- max(fages)
  k <- max_age
  mx <- mx[1:k]
  if (L) {
    L1 <- lx[1] + lx[2]
    s <- L1
    lx <- c(L1, lx[-c(1, 2)])
  }
  else {
    lx <- lx[-2]
    s <- sqrt(lx[2]) * peryear
  }
  px <- exp(diff(log(lx)))
  px <- px[1:(k - 1)]
  Fx <- NULL
  for (i in 1:k - 1) {
    Fx[i] <- s * (mx[i] + px[i] * mx[i + 1])/2
  }
  Fx <- c(Fx, mx[k])
  if (one.sex) 
    Fx <- Fx/(1 + SRB)
  A <- matrix(0, nrow = k, ncol = k)
  A[row(A) == col(A) + 1] <- px
  A[1, ] <- Fx
  class(A) <- "leslie.matrix"
  A
}

# Project population n number of years using leslie matrices
# The function returns a vector of either population totals or distributino of population over age groups
# In the last year of the projection horizon given the input parameters
leslie_project <- function(LT, ASFR, base_year, max_age, pop, projection_horizon, ages_keep, one.sex, return_share = F, rescale_by_age = T) {
  
  # browser()
  # Get lx - vector  of  either  age-specific  cumulative  survival  or  person-years  lived  in  the interval
  
  # For Sweden, this can be obtained from the lifwe tables that we have been using so far
  # Note that lx values are given in 1-year intervals
  
  lx <- 
    LT %>% # Cohort life table
    # dplyr::filter(Cohort %in% base_year) %>%
    select(Age, lx = nLx)
  
  # Get mx - age-specific fertility rates
  
  mx <- ASFR %>% 
    # dplyr::filter(Cohort %in% base_year) %>% 
    select(Age, mx = ASFR) 
  
  input <- merge(lx, mx, by = "Age", all.x = T)
  input$mx[is.na(input$mx)] <- 0
  
  # Get matrix
  
  A <- leslie.matrix(
    lx = input$lx
    , mx = input$mx
    , L = T # if ’FALSE’,lxis taken to be cumulative survival to exact age x+n.
    , peryear = 1
    , one.sex = one.sex
    , SRB = 1.05
    , infant.class = T # this should be F, but I think there is a mistake in the source code of this function, 
    # since the second value of mx is always deleted, irrespective of whether infant class is T or F
    # but it doesn't really matter in practical terms
  )
  
  # Project the leslie matrix over n years
  
  # Get baseline population distribution in Sweden
  
  age_range <- unique(input$Age)[-2] # to account for the error described above. These are non-reproductive ages, so 
  # it doesn't matter or the empirical analysis
  
  suppressWarnings(
    no <- pop %>% 
      dplyr::mutate(
        Age = as.numeric(Age)
        , Female = Female/10E3
      ) %>% 
      dplyr::filter(Year %in% base_year) %>% 
      dplyr::filter(Age %in% age_range) %>% 
      dplyr::pull(Female)
  )
  
  N <- project.leslie(
    A = A
    , no = no
    , tmax = projection_horizon
  )
  
  if(return_share) {
    
    # Get population distribution by year
    # keeping only relevant ages. ie.25-54
    # It matters whether specific ages will be kept or not
    # The reason is that the total share of the population that each age group 
    # corresponds to can be estiamted respective to the TOTAL population or
    # the population existnig in the chosen age groups
    # This is only relevant when returning the share of the population, not the
    # actual population numbers
    # The next steps defines the appropriate denominator for estimating the populatino shares:
    if(rescale_by_age) {
      # Filter ages by row in matrix
      totals <- colSums(N[ages_keep, ])
    } else {
      totals <- colSums(N)
    }
    
    share <- as.data.frame(t(N)/totals)
    colnames(share) <- 1:ncol(share)  
    share <- share[ , as.numeric(colnames(share)) %in% ages_keep]
    share$year <- 0:projection_horizon  
    
    age_dist <- as.numeric(share[share$year == 200, ])
    age_dist <- age_dist[-length(age_dist)]
    names(age_dist) <- colnames(share)[-ncol(share)]
  }
  
  else {
    N_df <- as.data.frame(N)
    age_dist <- N_df[ages_keep , ncol(N_df)]
  }
  
  return(age_dist)
  
}

# Create a lexis grid
# Adapted from package LexisPlotR
lexis_grid <- function (year.start, year.end, age.start, age.end,int_length = 5, alpha = 1) {
  if (!is.numeric(year.start)) {
    stop("No numeric value for year.start")
  }
  if (!is.numeric(year.end)) {
    stop("No numeric value for year.end")
  }
  if (!is.numeric(age.start)) {
    stop("No numeric value for age.start")
  }
  if (!is.numeric(age.end)) {
    stop("No numeric value for age.end")
  }
  if (year.start >= year.end) {
    stop("year.start >= year.end")
  }
  if (age.start >= age.end) {
    stop("age.start >= age.end")
  }
  
  year.seq <- seq(year.start, year.end, by = int_length)
  age.seq <- seq(age.start, age.end, by = int_length)
  dia <- data.frame()
  
  for (i in 1:length(year.seq[-length(year.seq)])) {
    for (j in 1:length(age.seq[-length(age.seq)])) {
      a <- year.seq[i]
      b <- year.seq[i + 1]
      c <- age.seq[j]
      d <- age.seq[j + 1]
      dia <- rbind(dia, c(a, b, c, d))
    }
  }
  
  colnames(dia) <- c("a", "b", "c", "d")
  
  gg <-
    ggplot() + 
    geom_segment(aes(x = year.seq, xend = year.seq, y = age.start, yend = age.end), alpha = alpha) + 
    geom_segment(aes(x = year.start, xend = year.end, y = age.seq, yend = age.seq), alpha = alpha) + 
    geom_segment(aes(x = dia$a, xend = dia$b, y = dia$c, yend = dia$d), alpha = alpha) +
    scale_x_continuous(expand = c(0,0), breaks = year.seq, name = "Year") +
    scale_y_continuous(expand = c(0,0), breaks = age.seq, name = "Age") + 
    coord_fixed(xlim = c(year.start, year.end), ylim = c(age.start, age.end)) + 
    theme_bw() + 
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , legend.position = "none"
    )
  
  return(gg)
}

lexis_lifeline <- function (lg, entry, exit = NA, lineends = F, lineends_size = 2, colour = "red", alpha = 1, lwd = 0.5) {
  if (!is.ggplot(lg)) {
    stop("No valid ggplot object.")
  }
  # entry <- as.Date(entry, origin = "1970-01-01")
  # exit <- as.Date(exit, origin = "1970-01-01")
  year.start <- ggplot_build(lg)$data[[1]][1, 1]
  year.end <- tail(ggplot_build(lg)$data[[1]]$xend, 1)
  age.start <- ggplot_build(lg)$data[[1]][1, 3]
  age.end <- tail(ggplot_build(lg)$data[[1]]$yend, 1)
  x <- NULL
  y <- NULL
  xend <- NULL
  yend <- NULL
  case <- data.frame(entry, exit)
  case$x <- entry
  case$xend <- ifelse(is.na(exit), year.end, exit)
  case$xend <- case$xend
  case$y <- 0
  case$yend <- ifelse(is.na(case$exit), year.end - case$entry, case$exit - case$entry)
  
  lg <- lg + 
    geom_segment(data = case, aes(x = x, xend = xend, y = y, yend = yend), colour = colour, alpha = alpha, lwd = lwd)
  
  if (lineends == TRUE) {
    lg <- lg + 
      geom_point(data = case[!is.na(case$exit), ], aes(x = xend, y = yend), size = lineends_size, shape = 3)
  }
  return(lg)
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

# https://www.ssa.gov/OACT/NOTES/as116/as116_IV.html
# http://demographer.com/dsitl/10-d101-life-table-basics/DSITL10-d101-life-table-basics.pdf
LT_from_lx <- function(lx) {
  
  dx <- as.vector(abs( c(0, diff(lx)) ) )
  qx <- dx/lx
  px <- 1 - qx
  Lx <- lx - 0.5 * dx
  # Lx <- 1*lx + ax*dx
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  mx <- dx/Lx
  
  # ax <- (Lx-1*lx)/(lx-lx[-1])
  b0 <- 0.07
  b1<- 1.7
  
  ax <- 1/mx - 1/qx + 1
  ax[1] <- b0 + b1 *mx[1]    # from Keyfitz & Flieger(1968)
  ax[length(ax)] <- ex[length(ax)] 	  # e_x at open age interval
  
  data.frame(
    age = 0:100
    , mx
    , qx
    , ax
    # , px
    , lx
    , dx
    , Lx
    , Tx
    , ex
  ) 
  
}

# based on Sarah's code'
LT_from_nMX <-  function(nMx, years) {
  colnames(nMx) <- c("Age", paste0("X", colnames(nMx)[-1]))
  ltdf(nMx = nMx, years = years) %>% data.frame %>% dplyr::rename(Lx = nLx)
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

## Make life table data base akin to what is available in the HMD life tables 
ltdf<-function(nMx, years){
  lt.df<-NULL
  for (year in years){
    tmp<-NULL
    tmp<-mxlt(nMx[ ,paste("X",year,sep="")])
    tmp<-cbind(year, tmp)
    lt.df<-rbind(lt.df,tmp)
  }
  return(lt.df)
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

# Used to check whether interpolated values match to real values
# lx was interpolated and the plots compare life expectancy
lx_graphic_test <- function(i) {
  
  lx <- lx_interp_l[[i]]
  names_l <- names(lx_interp_l)[i]
  filt <- unlist(strsplit(names_l, "\\."))
  
  estimated <- LT_from_lx(lx) %>% 
    select(age, lx) 
  
  # Compare to WPP Afghanistan 1950
  
  real <- lx_long_1_5 %>% 
    dplyr::filter(country == filt[1]) %>% 
    dplyr::filter(year == filt[2]) %>% 
    select(age, lx) 
  
  both <- merge(
    real
    , estimated
    , by = "age"
    , all = T
  ) %>% 
    select(age, wpp = lx.x, estimates = lx.y)
  
  gather(both, source, value, wpp:estimates) %>% 
    mutate(country = names_l)
}

#Functions for model estimates
macohort<-function(cohort){
  print(paste("Working on cohort:", cohort))
  Mavec<-NULL
  for (a in 0:90){
    t=cohort+a
    xs=c(15:49)
    
    fertvec<-NULL
    for (x in xs){
      co=t-a-x
      fert<-NULL
      fert<-ASFRSC[ASFRSC$Cohort==co&ASFRSC$Age==x,"ASFR"]
      fertvec<-c(fertvec,fert)
    }
    
    popvec<-NULL
    for (x in xs){
      co=t-a-x
      poptmp<-NULL
      poptmp<-pop[pop$Cohort==co&pop$Age==x, "Female"]
      popvec<-c(popvec,poptmp)
    }
    
    ##ASSUMPTION	
    ##Making simplifying assumption that lx is 0 above 105
    survvec<-NULL
    for (x in xs){
      co=t-a-x
      survtmp<-NULL
      if (x+a<=105){
        survtmp<-LTSCF[LTSCF$Cohort==co&LTSCF$Age==x+a,"lx"]/LTSCF[LTSCF$Cohort==co&LTSCF$Age==x,"lx"]
      }
      if (x+a>105){	
        survtmp<-0
      }
      survvec<-c(survvec, survtmp)
    }
    Ma<-NULL
    Ma<-sum(fertvec*popvec/sum(fertvec*popvec)*survvec)
    Mavec<-c(Mavec, Ma)
  }
  return(Mavec)
}


multiply_ASFR <- function(multiply_by, ASFRSC, base_TFR, base_year) {
  
  labs <- base_TFR * multiply_by
  labs[labs == base_TFR] <- paste0(labs[labs == base_TFR], " (base)")
  # labs <- paste(multiply_by)
  labs <<- labs
  
  ASFR_list <- lapply(multiply_by, change_fertility_levels, ASFRSC = ASFRSC, base_year = base_year)
  ASFR_df <- data.frame(do.call(rbind, ASFR_list), stringsAsFactors = F)
  
  return(ASFR_df)
}

## Function to produce life table from series of one year Mx's
mxlt<-function(mx){
  ax=rep(.5,length(mx))
  ax[length(mx)]<-1/mx[length(mx)]
  ax[1]<-.07+1.7*mx[1]
  nqx<-mx/(1+(1-ax)*mx)
  lx<-1
  for (i in 1:(length(mx)-1)){
    lx[i+1]<-lx[i]*(1-nqx[i])
  }
  dx<-c(-diff(lx), lx[length(lx)])
  #dx2<-lx[1:(length(lx)-1)]-lx[2:length(lx)]
  nLx<-lx[2:length(lx)]+ax[1:length(lx)-1]*dx[1:length(lx)-1]
  nLx[length(lx)]<-ax[length(ax)]*dx[length(ax)]
  Tx<-rev(cumsum(rev(nLx)))
  ex<-Tx/lx
  Mx<-dx/nLx
  x<-c(0:(length(mx)-1))
  lt<-cbind(x,mx,ax,nqx,lx,dx,nLx,Tx,ex,Mx)
  return(lt)
}

open_excel <- function(what, ext = ".csv", row.names = F) {
  dir <- paste0(tempdir(),"\\",sample(1:100,1),"R",ext)
  if (ext == ".csv") write.csv(what,dir, row.names)
  else if (ext == ".txt") write(what,dir)
  shell.exec(paste0(dir))
}


parent_child_correlations <- function(children_df, variable = "fert", parent_type_keep = "all", child_gender_keep = "all") {
  # browser()
  if(child_gender_keep != "all") {
    children_df <- 
      children_df %>% 
      dplyr::filter(child_gender == child_gender_keep)
  }
  
  if(parent_type_keep != "all") {
    children_df <- 
      children_df %>% 
      dplyr::filter(parent_type == parent_type_keep)
  }
  
  yearly_children <- split(
    children_df %>% select(dplyr::ends_with(variable))
    , children_df$parent_cohort
  )
  
  parent_children <- unlist(lapply(yearly_children, get_correlation, use="complete.obs"))
  
  return(parent_children)
  
}

parents_cols <- function(links, prof, infer_gender = T) {
  
  # To add gender of parent
  l <- merge(
    links,
    prof %>% select(profileid, gender),
    by.x = 'parent',
    by.y = 'profileid'
  ) 
  
  parents <- reshape2::dcast(
    l
    , ... ~ gender
    , value.var = 'parent'
    , fun.aggregate = sum
    , na.rm = F
  )
  
  colnames(parents) <- c("child", "mother", "father", "parent_sex_unknown")
  
  parents[parents==0] <- NA
  
  # Fill in missing parents if their gender can be inferred
  if(infer_gender) {
    
    add_f <-  !is.na(parents$parent_sex_unknown) & 
      is.na(parents$father) & !is.na(parents$mother)
    
    add_m <-  !is.na(parents$parent_sex_unknown) & 
      !is.na(parents$father) & is.na(parents$mother)
    
    parents$father[add_f] <- parents$parent_sex_unknown[add_f]
    parents$mother[add_m] <- parents$parent_sex_unknown[add_m]
    
    parents$parent_sex_unknown[add_f | add_m] <- NA
  }
  
  parents <- merge(
    prof
    , parents
    , by.x = 'profileid'
    , by.y = 'child'
    , all.x = T
  ) %>% 
    select(father, mother, parent_sex_unknown)
  
  return(parents)
  
}

# Adapted from demogR
project.leslie <- function (A, no, tmax, pop.sum = FALSE) 
{
  if (length(no) != dim(A)[1]) 
    stop("Projection matrix and population vector have different number of states!")
  N <- matrix(0, nrow = length(no), ncol = tmax + 1)
  N[, 1] <- no
  pop <- no
  for (t in 1:tmax) {
    pop <- A %*% pop
    N[, t + 1] <- pop
  }
  if (pop.sum) {
    N <- apply(N, 2, sum)
  }
  return(N)
}

# Deprecated
relatives_death <- function(r_list, p, parallel = T, numCores = 8) {
  
  ids <- names(r_list)
  
  if(!is.data.table(p)) p <- as.data.table(p)
  col <- "death_year"
  
  if(!parallel) {
    # Option A. Non parallelised
    print("Finding auts (non-parallelised).")
    out <- lapply(r_list, worker_get_dates, col, p)
  } else {
    # Option B. Parallelise using sockets for Windows
    # http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
    # browser()
    
    # numCores <- detectCores()
    
    print(paste("Parallelising (forking) on", numCores, "cores."))
    
    cl <- makeCluster(numCores)
    
    clusterExport(
      cl
      , varlist = c("col", "p")
      , envir = environment()
    )
    
    # Load packages
    clusterEvalQ(cl, library(data.table))
    
    out <- parLapply(cl, r_list, worker_get_dates, col, p) 
    
    stopCluster(cl)
  }
  
  return(out)
  
}

remove_outliers <- function(v) {
  
  outliers <- boxplot(v, plot=FALSE)$out
  v[-which(v %in% outliers)]
  
}

recode_extreme_values <- function(v, minval = 0, maxval = 100) {
  v[v < minval] <- minval
  v[v > maxval] <- maxval
  return(v)
}

save_lexis_plots <- function(year.start, mother_age_death, mother_age_at_birth, child_age_at_death, alpha = 0.4, lwd = 1.5, animate = F, name = "") {
  
  # browser()
  age.start = 0
  age.end = 100
  
  year.end = year.start + age.end
  year_range <- year.start:year.end
  
  mother_birth <- year.start
  
  mother_death <- mother_birth + mother_age_death
  
  # mother_age_at_birth <- sort(sample(rep_age, child_no))
  
  child_birth <- mother_birth + mother_age_at_birth
  # child_age_at_death <- sample(range_child_death_ages, child_no)
  child_death_year <- child_birth + child_age_at_death
  
  # Define life lines
  entry <- c(mother_birth, child_birth)
  exit <- c(mother_death, child_death_year)
  colour <- c("red", rep("blue", length(mother_age_at_birth)))
  
  lines <- data.frame(entry, exit, colour, stringsAsFactors = F)
  
  # Define base grid
  
  lg_base <-
    lexis_grid(
      year.start = year.start
      , year.end = year.end
      , age.start = age.start
      , age.end = age.end
      , int_length = 10
      , alpha = alpha
    )
  
  lg <- 
    lexis_lifeline(
      lg = lg_base
      , entry = lines$entry
      , exit = lines$exit
      , colour = lines$colour
      , lwd = lwd
      , alpha = 1
      , lineends = F
      , lineends_size = 4
    )
  
  
  # ggsave("lexis_child_death.png", lg)
  
  # Add lines to indicate child deaths
  
  lg2 <- 
    lg +
    geom_vline(xintercept = exit[exit <= mother_death][-1], linetype = "dashed", size = 1)
  
  
  # ggsave("lexis_child_death2.png", lg2)
  if(!animate) ggsave(paste0("lexis_",name,".pdf"), lg2)
  
  if(animate) {
    
    print(paste("Saving animated plots."))
    
    # IN which interval to animate
    dates <- range(unique(c(lines$entry, lines$exit)))
    year_range <- min(dates):max(dates)
    year_range <- year_range[!year_range > year.end]
    
    for(n in 1:length(year_range)) {
      
      # For mothers
      mother_exit <- lines$entry[1]
      mother_exit <- mother_exit + n
      
      # For kids
      kids_exit <- lines$entry[-1] + n - mother_age_at_birth
      kids_exit[kids_exit < year.start] <- year.start 
      
      # both
      ex <-  c(mother_exit, kids_exit)
      
      # # Make sure to stop lines when people die
      deaths <- ex > lines$exit
      ex[deaths] <- lines$exit[deaths]
      
      l_temp <- 
        lexis_lifeline(
          lg = lg_base
          , entry = lines$entry
          , exit = ex
          , colour = lines$colour
          , lwd = lwd
          , alpha = 1
          , lineends = F
          , lineends_size = 5
        )
      
      # ggsave(paste0("lexis_animate", n, ".png"), l_temp)
      ggsave(paste0("lexis_animate_",name,"_", n,".pdf"), l_temp)
      
    }
    
    ggsave(paste0("lexis_animate_",name,"_", n+1,".pdf"), lg2)
    
    print(paste("Animated plots saved."))
    
  }
  
}


share_is_number <- function(x, nums = c(0,5)) {
  x <- na.omit(x)
  denominator_length <- length(x)
  enumerator_cases <- sum(x %in% nums)
  enumerator_cases/denominator_length
}

share_saw_rel_death <- function(p, p_full, rel, probs, min_age, max_age) {
  
  col <- paste0("age_death_", rel)
  levs <- as.character(min_age:max_age)
  y <- unique(p$birth_year)
  
  denominator <- nrow(p)
  
  freq_cum <-
    p %>% 
    dplyr::rename(age_death = dplyr::contains(col)) %>% 
    dplyr::mutate(age_death = factor(age_death, levels = levs)) %>% 
    dplyr::count(age_death) %>% 
    tidyr::complete(age_death, fill = list(n = 0)) %>% 
    # Weight by number of obsevations
    dplyr::mutate(share = n/denominator) %>% 
    dplyr::arrange(age_death) %>% 
    dplyr::mutate(
      share_cum = cumsum(share)
      , age_death = as.numeric(as.character(age_death))
    )
  
  # Get minimum age at which the given percentile is reached
  percentiles <- unlist(
    lapply(probs, function(prob, df) {
      max(freq_cum$age_death[freq_cum$share_cum <= prob])  
    }, df = freq_cum)
  )
  
  return(percentiles)
  
}

# Use to change mean age at childbearing given a vector of ASFR
# without changing the TFR
shift_fertility <- function(shift_by, base_year, ASFRSC) {
  
  ASFR <- ASFR_original <- ASFRSC[ASFRSC$Cohort == base_year, ]
  ASFR$Age <- ASFR$Age + shift_by
  
  ASFR_new <- merge(
    ASFR
    , ASFR_original %>% select(Age)
    , by = "Age"
    , all.x = F
    , all.y = T
  )
  
  ASFR_new$ASFR[is.na(ASFR_new$ASFR)] <- 0
  ASFR_new$Cohort<- paste(shift_by)
  
  return(ASFR_new)
  
}

# Generate df of ASFR which are transformed by shifting the mean
# age at childbirth
shift_mu <- function(shift_by, ASFRSC, base_mu, base_year) {
  
  labs <- base_mu + shift_by
  labs[labs == base_mu] <- paste0(labs[labs == base_mu], " (base)")
  # Upadte in global env
  labs <<- labs
  
  ASFR_list <- lapply(shift_by, shift_fertility, ASFRSC = ASFRSC, base_year = base_year)
  ASFR_df <- data.frame(do.call(rbind, ASFR_list), stringsAsFactors = F)
  
  return(ASFR_df)
}

sign <- function(x) ifelse(x < 0.05, "*", "")

# Summarise columns from historical trensd
# Note that requires grouping columns to be named
# region and year
sum_cols <- function(df) {
  
  df %>% 
    group_by(region, year) %>% 
    summarise(
      mean = mean(value)
      , sd = sd(value)
      , median = median(value)
      , iqr = IQR(value)
    ) %>% 
    ungroup %>% 
    mutate(
      low_iqr = median - iqr
      , high_iqr = median + iqr
      , low_sd = mean + 1*sd
      , low_sd = mean + 1*sd
      , low_sd2 = mean + 2*sd
      , high_sd2 = mean - 2*sd
    )
}

# Used to estimate child survival probabilities
# I still don't fully understand how it works
survival_probs <- function(life_table, xs, mas, cos) {
  # browser()
  lx.kids.arr <- array(
    NA
    , dim=c(length(xs), length(mas), length(cos))
    , dimnames=list(paste(xs), paste(mas), paste(cos))
    )
  
  for (co in cos){ 
    # print(co)
    for (ma in mas){
      lx.kids <- c()
      for (x in xs){
        tmp <- c()
        
        # This is the important bit: 
        # It only applies for cases where ma >= x
        
        # Get the survival probability for a child given:
        child_age <- ma-x # because 
        child_cohort <- co+x
        
        tmp <- life_table$lx[life_table$Age == child_age & life_table$Cohort == child_cohort]
        
        if (length(tmp) > 0){
          lx.kids<-c(lx.kids, tmp)
        }
        
      }
      if (length(lx.kids)>0)
        lx.kids.arr[1:length(lx.kids),paste(ma),paste(co)]<-lx.kids
    }
  }
  
  return(lx.kids.arr)
  
}

# Works, but depracted in favour of
# expand_asfr_age 20190619
# Return a df with 1-year wide age intervals from UN ASFR projection data
# ungroup_age_groups <- function(df_5_5, countries, age_group_range, method = "linear") {
#   
#   # One list for every countr/year combination 
#   
#   fert_list <- lapply(
#     split(df_5_5, list(df_5_5$country, df_5_5$year))
#     , function(x) unlist(x$value)
#     )
#   
#   # Ungroup 5-year age specific rates
#   
#   age_groups_no <- length(age_groups_range)
#   
#   print("Starting approximation...")
#   
#   if(method == "poisson") {
#     # option 1
#     # Fit model to data
#     # One approach is to use the 'ungroup' package, which assumes an underlying poission distribution
#     # and performs a univariate penalized composite link model
#     # https://cran.r-project.org/web/packages/ungroup/vignettes/Intro.pdf
#     
#     age_groups <- seq(15, 45, 5)
#     n_last <- 5
#     
#     fert_n <- fert_list[[1]]
#     fert_est <- lapply(fert_list, function(fert_n) {
#       m <- ungroup::pclm(x = age_groups, y = fert_n, nlast = n_last, control = list(lambda = 10))
#       plot(m)
#       return(fitted(m))
#     })
#     
#   } else if(method == "linear") {
#     # Option 2
#     # use linaer interpolation
#     names(fert_list) <- paste0(names(fert_list), ".")
#     
#     fert_est <- lapply(fert_list, function(fert_n) {
#       approx(fert_n, method = method, n = age_groups_no)$y
#     }) 
#   } else if(method == "spline") {
#     
#     names(fert_list) <- paste0(names(fert_list), ".")
#     old_ages <- unique(df_1_5$age)
#     x <- 1:length(old_ages)
#     n <- age_groups_no
#     
#     fert_est <- lapply(fert_list, function(fert_n) {
#       spline(x = x, y = fert_n, n = n)$y
#     }) 
#     
#   }
#   
#   print("Estimates completed.")
#   
#   # Store as data frame
#   # Doing it like this ensure that correct order is preserved
#   
#   periods <- levels(factor(df_5_5$year))
#   periods_no <- length(periods)
#   countries_no <- length(countries)
#   
#   estimates <- unlist(fert_est)
#   yearly_fert_estimates <- data.frame(do.call(rbind, strsplit(names(estimates), "\\.")), stringsAsFactors = F)
#   yearly_fert_estimates$value <- unlist(fert_est)
#   
#   colnames(yearly_fert_estimates) <- c("country", "year", "age", "value")
#   
#   yearly_fert_estimates$age <- rep(age_groups_range, countries_no*periods_no)
#   
#   yearly_fert_estimates <- 
#     yearly_fert_estimates%>%
#     mutate(year = as.numeric(year)) %>% 
#     dplyr::arrange(country, age, year)
#   
#   return(yearly_fert_estimates)
# }

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

# Taken from https://community.rstudio.com/t/how-to-automatically-add-text-annotations-or-tags-outside-of-faceted-plots/13700/4
# Adds individual labels to every facet of a plot in ggplot
tag_facet_flex <- function(p, position = 'both', 
                           open = c("", ""), close = c("", ""),
                           tag_fun_top = function(i) LETTERS[i],
                           tag_fun_right = utils::as.roman,
                           x = c(0, 0), y = c(0.5, 1),
                           hjust = c(0, 0), vjust = c(0.5, 1),
                           fontface = c(1, 1), ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout  
  
  if (grepl(position, 'top')) {
    
    lay <- gb$layout$layout
    
    tags_top <- paste0(open[1], tag_fun_top(unique(lay$COL)), close[1])
    
    tl <- lapply(tags_top, grid::textGrob,
                 x = x[1], y = y[1],
                 hjust = hjust[1], vjust = vjust[1], 
                 gp = grid::gpar(fontface = fontface[1], ...)
    )
    
    g <- ggplot_gtable(gb)
    g <- gtable::gtable_add_rows(g, grid::unit(1, "line"), pos = 0)
    lm <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    g <- gtable::gtable_add_grob(g, grobs = tl, t = 1, l = lm)
    
  } else if (grepl(position, 'right')) {
    
    tags_right <- paste0(open[2], tag_fun_right(unique(lay$ROW)), close[2])
    
    rl <- lapply(tags_right, grid::textGrob,
                 x = x[2], y = y[2],
                 hjust = hjust[2], vjust = vjust[2], 
                 gp = grid::gpar(fontface = fontface[2], ...)
    )
    
    g <- ggplot_gtable(gb)
    l <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    
    wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
    g <- gtable::gtable_add_cols(g, wm, pos = max(l))
    t <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs = rl, t = t, l = max(l) + 1)
    g <- gtable::gtable_add_cols(g, unit(2, "mm"), pos = max(l))
    
  } else {
    
    print('Use default tagging option: both top and right sides')
    
    tags_top <- paste0(open[1], tag_fun_top(unique(lay$COL)), close[1])
    tags_right <- paste0(open[2], tag_fun_right(unique(lay$ROW)), close[2])
    
    tl <- lapply(tags_top, grid::textGrob,
                 x = x[1], y = y[1],
                 hjust = hjust[1], vjust = vjust[1], 
                 gp = grid::gpar(fontface = fontface[1], ...)
    )
    
    rl <- lapply(tags_right, grid::textGrob,
                 x = x[2], y = y[2],
                 hjust = hjust[2], vjust = vjust[2], 
                 gp = grid::gpar(fontface = fontface[2], ...)
    )
    
    g <- ggplot_gtable(gb)
    g <- gtable::gtable_add_rows(g, grid::unit(1, "line"), pos = 0)
    l <- unique(g$layout[grepl("panel", g$layout$name), "l"])
    g <- gtable::gtable_add_grob(g, grobs = tl, t = 1, l = l)
    
    wm <- do.call(grid::unit.pmax, lapply(rl, grid::grobWidth))
    g <- gtable::gtable_add_cols(g, wm, pos = max(l))
    t <- unique(g$layout[grepl("panel", g$layout$name), "t"])
    g <- gtable::gtable_add_grob(g, grobs = rl, t = t, l = max(l) + 1)
    g <- gtable::gtable_add_cols(g, unit(2, "mm"), pos = max(l))
  }
  
  if (!is.null(g)) {
    grid::grid.newpage()
    grid::grid.draw(g)    
  }
  
  return(g)
  
}

which_rel_died_first <- function(d_df) {
  
  d_cols <- grepl("^d_", colnames(d_df))
  
  rel_death <- d_df[,d_cols]
  
  d_df$first_death <- 
    unlist(
      apply(rel_death, 1, function(row, label) {
        if(any(!is.na(row))) {
          min_date <- min(row, na.rm = T)
          who <- match(min_date, row)
          out <- label[who]
        } else {
          out <- NA
        }
        return(out)
      }, label = colnames(rel_death) )
    )
  
  return(d_df)
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

worker_lt_mx <- function(df) {
  lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E5)
  
  cbind(
    df[1:nrow(lt) , 1:2]
    , lt
  )
}

worker_get_ages <- function(l1, p, adjust_own_mortality) {
  # print(1)
  # browser()
  
  coh = l1[['coh']]
  
  dates <- l1[["dates"]]
  ego <- names(dates)
  
  keep <- p$profileid %in% ego
  
  ego_birth <- as.numeric( unlist( p[keep, "birth_year"] ) )
  
  rel_death <- as.numeric(unlist(dates))[keep]
  
  age_death <- rel_death - ego_birth
  
  out_df <- data.frame(
    profileid = ego[keep]
    , age_death = age_death
    , coh = coh[keep]
    , stringsAsFactors = F
  )
  
  return(out_df)
}

worker_get_ages_all <- function(l1, p, adjust_own_mortality) {
  # print(1)
  
  coh = l1[['coh']]
  
  dates <- l1[["dates"]]
  ego <- names(dates)
  
  keep <- p$profileid %in% ego
  
  ego_birth <- as.numeric( unlist( p[keep, "birth_year"] ) )
  ego_death <- as.numeric( unlist( p[keep, "death_year"] ) )
  
  rel_death <- dates[keep]
  
  # Loop, since more than one relatives are possible
  
  age_death <- 
    lapply(1:length(rel_death), function(n, adjust_own_mortality) {
      # if(n==7) browser()
      # print(n)
      
      ego_b <- ego_birth[n]
      rel_d <- rel_death[[n]]
      age_d <- rel_d - ego_b
      
      # Keep only dates that were experienced by ego 
      # i.e. events that took place while ego was alive
      if(adjust_own_mortality) {
        ego_d <- ego_death[n]
        if(!is.na(ego_d) & any(!is.na(rel_d))) {
          not_experienced <- which(ego_d < rel_d)
          if(length(not_experienced > 0)) age_d <- age_d[-not_experienced]
        }
      }
      
      return(age_d)
      
    }, adjust_own_mortality)
  
  out_l <- 
    list(
      profileid = ego[keep]
      , age_death = age_death
      , coh = coh[keep]
    )
  
  return(out_l)
}


worker_get_dates <- function(ids, col, p) {
  
  if(all(is.na(ids))) {
    out <- NA
  } else {
    out <- as.numeric( unlist( p[ profileid %in% ids, col, with = FALSE] ) )
    out <- min(out)
  }
  
  return(out)
}

worker_get_dates_all <- function(ids, col, p) {
  
  if(all(is.na(ids))) {
    out <- NA
  } else {
    out <- as.numeric( unlist( p[ profileid %in% ids, col, with = FALSE] ) )
  }
  
  return(out)
}

worker_find_aunts_uncles <- function(arg, p, rm_parent, keep_sex) {
  
  # browser()
  id <- arg$profileid
  parents <- arg$parents
  grandpas <- arg$grandpas
  parent_ref <- arg$parent_ref
  
  aunts <- p$profileid[p$parents %in% grandpas]
  
  # Remove parent
  if(rm_parent) {
    aunts <- aunts[aunts != parent_ref]
  }
  
  if(length(aunts) > 0 & !is.na(keep_sex)) {
    sex <- p$gender[p$profileid %in% aunts]
    aunts <- aunts[sex == keep_sex]
  }  
  
  if(!length(aunts)) aunts <- NA
  
  return(aunts)
  
}


worker_find_children <- function(arg, p2, keep_sex) {
  # browser()
  parent <- arg
  
  child <- p2$profileid[p2$parent %in% parent]
  
  if(length(child) > 0 & !is.na(keep_sex)) {
    sex <- p2$gender[p2$profileid%in% child]
    child <- child[sex == keep_sex]
  }  
  
  if(!length(child)) child <- NA
  
  return(child)
  
}

worker_find_kommun <- function(p_df, kom_l, parallel, numCores) {
  print(p_df)
  
  matched <- lapply(kom_l, function(k, p_df) {
    point.in.polygon(
      point.x = p_df$long_b
      , point.y = p_df$lat_b
      , pol.x = k$long
      , pol.y = k$lat
    )
  }, p_df)
  
  kom_name <- names(matched)[matched == 1]
  
  if(!length(kom_name)) kom_name <- NA
  
  return(kom_name)
  
}

worker_find_sib <- function(arg, p, p3, rm_ego, keep_sex) {
  # browser()
  id <- arg[[1]]
  parents <- arg[[2]]
  
  sib <- p3$profileid[which( p3$parents == parents ) ]
  
  if(rm_ego) {
    sib <- sib[sib != id]
  }
  
  if(length(sib) > 0 & !is.na(keep_sex)) {
    sex <- p$gender[match(sib, p3$profileid)]
    sib <- sib[sex == keep_sex]
  }  
  
  if(!length(sib)) sib <- NA
  
  return(sib)
  
}


worker_recohort <- function(l2, p, breaks, labels) {
  # browser()
  ids <- as.numeric(names(l2[['dates']]))
  yob <- p[p$profileid %in% ids, 'birth_year']
  coh <- cut(yob, breaks = breaks, include.lowest = TRUE, 
             right = F, labels = labels)
  
  l2[['coh']] <- coh
  
  return(l2)
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


# Gets number of births to women by age group by looking at
# birth dates of mother/fathers 
yearly_birth_by_age <- function(df, sex_keep, y_range) {
  col <- ifelse(sex_keep == "male", "father", "mother")
  df$parent_birth <- df$birth_year[match(df[, col], df$profileid)]
  
  out <- 
    df %>% 
    select(birth_year, parent_birth) %>% 
    dplyr::mutate(
      parent_age = birth_year - parent_birth 
      , parent_agegr = cut(parent_age, age_breaks, include.lowest = TRUE, 
                           right = F)
      , parent_agegr = as.character(parent_agegr)
    ) %>% 
    dplyr::count(birth_year, parent_agegr) %>% 
    tidyr::complete(birth_year, parent_agegr, fill = list(n = 0)) %>% 
    dplyr::filter(
      !is.na(parent_agegr)
      , birth_year %in% y_range 
    ) %>% 
    select(year = birth_year, agegr = parent_agegr, n) %>% 
    arrange(year, agegr)
  
  
  return(out)
  
}

# Returns a list with the ids of all individuals that were alive at the start
# of a given year and died in that year or after thar year.
# This is, all who ever lived during this particular year
# Takes a data frame as input in which only births in a given year are nicluded
# This means that the compelte df needs to be split before passing it as a list to the functino
# via lapply
# Use return_ids to return not full df, but only ids of individuals alive in a given year
yearly_census <- function(from, to = NA, df, return_pop = T) {
  
  if(is.na(to))
    to <- from
  if(from == to)
    df$census <- from
  else df$census <- paste(from, to, sep = "-")
  
  print(paste("Subsetting data...", from))
  
  if(return_pop) {
    df2 <- df[df$birth <= to & df$death >= from, ]
    fem <- sum(df2$gender == 'female', na.rm = T)  
    male <- sum(df2$gender == 'male', na.rm = T)  
    total <- nrow(df2)
    out <- c(fem, male, total)
    
  } else {
    out<- df[df$birth <= to & df$death >= from, ]  
  }
  
  return(out)
}

print("All functions loaded")