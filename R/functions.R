
# *~^**~^**~^**~^**~^**~^**~^*#
# Code by                     #
# Diego Alburez-Gutierrez     #
# gatemonte@gmail.com         #
# @d_alburez                  #
# unless stated otherwise.    #
# Last edited 20200110        #
# GNU GENERAL PUBLIC LICENSE  #
# Version 3, 29 June 2007     #
# *~^**~^**~^**~^**~^**~^**~^*#
#       \   ^__^ 
#        \  (oo)\ ________ 
#           (__)\         )\ /\ 
#                ||------w|
#                ||      ||


# Apply cohort life tables to real-life populatinos
# with the intention of getting the lx column
# where radices are the initial size of birth cohorts 
# of women using wpp data
apply_lt <- function(female_births, LTCF, numCores) {
  
  countries <- unique(LTCF$Country)
  cohorts <- 1950:2000
  
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


# highest-level function.  Takes df of period ASFR in
# 1 age and 1 year groups from UNWPP and returns 
# cohort estimates
convert_period_asfr_to_cohort_asfr <- function(fert_per_1_1,variant_name,export = T, returnme = T){
  
  # 2. Get cohort asfr df
  
  # fert_per_1_1 was created in the previous script and can also be read from the corresponding csv file
  # it is the ungrouped age-specific fertility rates for 1-calendar-year intervals for all countries and regions
  # in the world, derived from the WPP 2019 data
  
  # In order to create pseudo-cohort fertility data, 
  # the first step is to create a 'cohort' column
  
  ASFRC_temp <- 
    fert_per_1_1 %>% 
    mutate(cohort = year - age) %>% 
    select(country, Cohort = cohort, Age = age, ASFR = value) %>% 
    arrange(country, Cohort, Age)
  
  # 2.2. Add all cohort/age combinations
  
  coh <- sort( unique(ASFRC_temp$Cohort) )
  a <- sort(unique(ASFRC_temp$Age))
  con <- as.character( unique(ASFRC_temp$country) )
  
  len_con <- length(a) * length(coh)
  len_a <- length(con) * length(coh)
  
  coh2 <- sort(rep(coh, length(a)))
  
  comb <- data.frame(
    country = sort(rep(con, len_con))
    , Cohort = rep(coh2, length(con))
    , Age = rep(a, len_a)
  ) %>% 
    arrange(country, Cohort, Age) 
  
  # Merge with created data structure
  ASFRC <- merge(
    ASFRC_temp
    , comb
    , all = T
  ) %>% 
    arrange(country, Cohort, Age)
  
  
  # 4. Export 
  if(export) {
    write.csv(x = ASFRC, file = paste0("../../Data/derived/", "ASFRC_",variant_name,".csv"), row.names = F)
  }
  
  if(returnme){
    ASFRC
  } else {
    print("Returning no value...")
  }
  
}

convert_period_LT_to_cohort_LT_robust <- 
  function(lt_1_1, sex = "B", export = T, years = 1951:2100, ages = 1:100, parallel = T, numCores = 4) {
    
    # 1. Create cohort life table 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # LTC ... Cohort life table
    LTC_df <- 
      lt_1_1 %>% 
      select(Country = country, Year = year, Age = age, mx, qx, ax) %>% 
      mutate(
        Cohort = Year - Age
        , Country = as.character(Country)
      ) %>% 
      arrange(Country, Cohort, Age) %>% 
      filter(Cohort > min(years) - 1) %>% 
      # filter(!Country %in% ignore) %>% 
      # Assign to new cohort life tables:
      # lx (number of people left alive)
      # dx (annual number of deaths)
      # nLx (person-live yeares lived in interval)
      mutate(
        lx = ifelse(Age == 0, 1, NA)
        , dx = NA
        , nLx = NA
        , Tx = NA
        , ex = NA
      ) %>% 
      select(Country, Cohort, Year, Age, mx, qx, ax, lx, dx, nLx, Tx, ex)
    
    # Sequentially, the function takes about 1 minute per country (for all years)
    # to exeute currently, so estimate around 2-3 hours for all countries
    # Parallelised on 4 cores is much faster, taking about 8-10 minutes
    
    # Return a df with new columns added
    
    closeAllConnections()
    
    LTC <- LT_period_to_cohort(
      df = LTC_df
      , years = years
      , ages = ages
      , parallel = parallel
      , numCores = numCores
    )
    
    rm("LTC_df")
    
    gc()
    
    # 3. Export 
    
    if(export) {
      
      # 3.1. Save all as one file
      file <- paste0("../../Data/derived/", "LTC", sex, ".csv")
      
      write.csv(x = LTC, file = file, row.names = F)
      
      print(paste("All cohort life tables saved to", file))
      
    }
    
    return(LTC)
    
  }

expand_asfr_age <- function(l_5_5, grouped_ages, method = "linear", col = "value") {
  
  all_ages <- min(grouped_ages):max(grouped_ages)
  
  names(l_5_5) <- paste0(names(l_5_5), ".")
  
  print("interpolating...")
  # Use a linear regression to interpolate the value
  estimates_list <- lapply(l_5_5, function(df) {
    # print(paste(df$country[1], df$year[1], df$variant[1], collapse = "-"))
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
  
  # years <- unique(sort(unlist(lapply(df_l[[1]]$year, function(d) {
  #   n <- as.numeric(unlist(strsplit(d, "-")))
  #   min(n):max(n)
  # }))))
  years <- 1950:2100
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
format_table <- function(df, row_keep = 29, ages = c(20,45,100), cohorts = c(1950, 1975, 2000), extra_header = T) {
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

get_world_pop_age <- function(fertility_variant){
  what <- "WPP2019_PopulationByAgeSex_"
  
  f <- paste0(
    "../../Data/wpp_data/"
    ,what
    , ifelse(fertility_variant == "medium", "Medium", "OtherVariants")
    ,".csv"
  )
  
  if(fertility_variant == "medium"){
    out <- 
      read.csv(file = f, stringsAsFactors = F) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location)  
  } else{
    # Split in two
    # for initial period get 'meiudm' estimates (ie estiamtes)
    old_period <- 1950 :2019
    
    out_1950_2020 <- 
      read.csv(
        file = paste0("../../Data/wpp_data/", what, "Medium", ".csv")
        , stringsAsFactors = F
      ) %>% 
      filter(Time %in% old_period) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location)
    
    # New period
    out_post2020 <- 
      read.csv(file = f, stringsAsFactors = F) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location) %>% 
      filter(Variant == fertility_variant)    
    
    out <- 
      bind_rows(out_1950_2020, out_post2020) %>% 
      mutate(Variant = fertility_variant)
    
  }
  
  return(out)
}

get_wpp_period_by_variant <- function(fertility_variant){
  
  
  
  f <- paste0(
    "../../Data/wpp_data/"
    ,"WPP2019_Period_Indicators_"
    , ifelse(fertility_variant == "medium", "Medium", "OtherVariants")
    ,".csv"
  )

  if(fertility_variant == "medium"){
    out <- 
      read.csv(file = f, stringsAsFactors = F) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location)  
  } else{
    # Split in two
    # for initial period get 'meiudm' estimates (ie estiamtes)
    old_period <- c("1950-1955", "1955-1960", "1960-1965", "1965-1970", "1970-1975", 
      "1975-1980", "1980-1985", "1985-1990", "1990-1995", "1995-2000", 
      "2000-2005", "2005-2010", "2010-2015", "2015-2020")
    
    out_1950_2020 <- 
      read.csv(
        file = "../../Data/wpp_data/WPP2019_Period_Indicators_Medium.csv"
        , stringsAsFactors = F
        ) %>% 
      filter(Time %in% old_period) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location)
    
    # New period
    out_post2020 <- 
      read.csv(file = f, stringsAsFactors = F) %>% 
      mutate(
        region = tolower(Location)
        , region = fix_un_countries(region)
        , Variant = tolower(Variant)
      ) %>% 
      select(-Location) %>% 
      filter(Variant == fertility_variant)    
    
    out <- 
      bind_rows(out_1950_2020, out_post2020) %>% 
      mutate(Variant = fertility_variant)
    
  }
    
  return(out)
  
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
      
      # years <- unique(sort(unlist(lapply(df$year, function(d) {
      #   n <- as.numeric(unlist(strsplit(d, "-")))
      #   min(n):max(n)
      # }))))
      
      years <- 1950:2100
      
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

  } 
  out <- data.frame(do.call(rbind, lapply(min_val, fun, df)), stringsAsFactors = F) 
  if(as_share) out <- out %>% mutate(value = paste0(value*100, "%"))
  return(out)
}

lexis_coord_cohort <- function(row){
  
  id <- row['id']
  n <- as.numeric(row[c("cohort", "age")])
  names(n) <- c("cohort", "age")
  
  xcoord <- c(n['cohort'], n['cohort'] + 1, n['cohort'] + 1, n['cohort'])
  ycoord <- c(n['age'], n['age'], n['age'] + 1, n['age'] + 1)
  
  data.frame(
    id = id
    , cohort = xcoord
    , age = ycoord
    , row.names = NULL, stringsAsFactors = F)
  
}

lexis_coord_period <- function(row){
  
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
                   nax = NULL, radix = 1E6) 
  
{
  # browser()
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
  # nqx[nqx > 1] <- 1
  for (i in 1:length(nqx)) {
    if (nqx[i] > 1) nqx[i] <- 1
  }
  nage <- length(age)
  npx <- 1 - nqx
  l0 = radix
  lx <- round(cumprod(c(l0, npx)))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLx <- n * lxpn + ndx * nax
  
  # Deal with open age interval for Lx:
  # Normally, I would just take it from the abriged life tables
  # but this is not possible since the abridged life tables are 
  # also grouped in 5-calendar-year groupps so that there is no
  # 'original' value to get this from and it must be computed
  # anew.
  # Lx = lx/(Inf_nmx_100)
  # Note: This method will only work if lx > 0 for the
  # open age interval. In some cases, this is not true if the radix
  # is too small and the mortality too high (eg niger 1950)
  # For this reason, I cahnged to radix from 1E5 to 1E6 on 20200108
  
  nLx[nage] <- lx[nage]/nmx[nage]
  
  
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  
  # Without rounding
  lt <- data.frame(
    age = age
    , ax = c(nax[-length(nax)], NA)
    , mx = nmx
    , qx = nqx
    , px = npx
    , dx = ndx
    , lx = lx
    , Lx = nLx
    , Tx = Tx
    , ex = ex
  )
  
  lt.top.age <- min(which(nqx == 1))
  lt <- lt[1:lt.top.age, ]
  return(lt)
}

# DEPRECATED 20200108 becuase it did not generate ex for upper age
# interval in expanded life tables
# Builds a life table by using the mortality rate schedule to calculate 
# the subsequent columns of the table
# Adapted from LifeTables::lt.mx
# lt_mx <- function (nmx, sex = "female", age = c(0, 1, seq(5, 110, 5)), 
#                    nax = NULL, radix = 1E5) 
#   
# {
#   # browser()
#   n <- c(diff(age), 999)
#   if (is.null(nax)) {
#     nax <- 0.5 * n
#     if (n[2] == 4) {
#       if (sex == "male") {
#         if (nmx[1] >= 0.107) {
#           nax[1] <- 0.33
#           nax[2] <- 1.352
#         }
#         else {
#           nax[1] <- 0.045 + 2.684 * nmx[1]
#           nax[2] <- 1.651 - 2.816 * nmx[1]
#         }
#       }
#       if (sex == "female") {
#         if (nmx[1] >= 0.107) {
#           nax[1] <- 0.35
#           nax[2] <- 1.361
#         }
#         else {
#           nax[1] <- 0.053 + 2.8 * nmx[1]
#           nax[2] <- 1.522 - 1.518 * nmx[1]
#         }
#       }
#     }
#   }
#   nqx <- (n * nmx)/(1 + (n - nax) * nmx)
#   nqx <- c(nqx[-(length(nqx))], 1)
#   nqx[nqx > 1] <- 1
#   # for (i in 1:length(nqx)) {
#   #   if (nqx[i] > 1) nqx[i] <- 1
#   # }
#   nage <- length(age)
#   npx <- 1 - nqx
#   l0 = radix
#   lx <- round(cumprod(c(l0, npx)))
#   ndx <- -diff(lx)
#   lxpn <- lx[-1]
#   nLx <- n * lxpn + ndx * nax
#   Tx <- c(rev(cumsum(rev(nLx[-length(nLx)]))), 0)
#   lx <- lx[1:length(age)]
#   ex <- Tx/lx
#   
#   # With rounding
#   # lt <- data.frame(
#   #   age = age
#   #   , ax = c(round(nax[-length(nax)], 3), NA)
#   #   , mx = round(nmx, 4)
#   #   , qx = round(nqx, 4)
#   #   , px = round(npx, 4)
#   #   , dx = ndx
#   #   , lx = lx
#   #   , Lx = c(round(nLx[-length(nLx)]), NA)
#   #   , Tx = c(round(Tx[-length(Tx)]), NA)
#   #   , ex = c(round(ex[-length(ex)], 2), NA)
#   #   )
#   
#   # Without rounding
#   lt <- data.frame(
#     age = age
#     , ax = c(nax[-length(nax)], NA)
#     , mx = nmx
#     , qx = nqx
#     , px = npx
#     , dx = ndx
#     , lx = lx
#     , Lx = c(nLx[-length(nLx)], NA)
#     , Tx = c(Tx[-length(Tx)], NA)
#     , ex = c(ex[-length(ex)], NA)
#   )
#   
#   lt.top.age <- min(which(nqx == 1))
#   lt <- lt[1:lt.top.age, ]
#   return(lt)
# }

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



map_burden_cd <- function(cohort_show, ...) {
  
  # Make sure that various cohorts share the same range of colours
  # in the legend colorbar
  bar_br <- seq(0, 30, 5)
  bar_lim <- c(0, 30)
  
  
  bar_name <- paste0("Regional Burden\nof child death")
  p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
  
  # Keep only people entering retirement age, defined as the 1955 cohort
  # currently, approaching age 65
  
  # Get share outlived mother:
  values <-
    abs_df %>%
    # Get generations burden from age-specific burden
    group_by(cohort, country) %>% 
    mutate(value = cumsum(absolute)) %>% 
    ungroup() %>% 
    filter(cohort == cohort_show) %>% 
    filter(age == retirement_age) %>% 
    filter(country != 'channel islands') %>% 
    mutate(
      value = value / 1e6
      , country = ifelse(country == "eswatini", "swaziland", country)
      , country = countrycode(country, "country.name", "iso3c")
    ) %>% 
    select(country, value) 
  
  # Join with map
  w <- left_join(
    world
    , values
    , by = "country"
  ) %>% 
    filter(! ID %in% "Antarctica")
  
  # Plot
  
  ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = country_line_size) +
    scale_fill_viridis(
      name = bar_name
      , option="viridis"
      , breaks = bar_br
      , limits = bar_lim
    ) +
    labs(
      title = ""
    ) +
    coord_sf(crs = "+proj=robin") +
    ggtitle(p_title) +
    theme_minimal(base_size = 6) +
    theme(
      axis.line = element_blank(), axis.text = element_blank()
      , axis.ticks = element_blank(), axis.title = element_blank()
      , plot.title = element_text(size = title_size, face="bold")
    ) +
    guides(fill = guide_colourbar(barwidth = 1))
  
}


# For plotting measure for SA materials
map_child_death <- function(cohort_show, ...) {
  
  # Note that this shifts all values by 0.5
  # to avoid very yellow colors that make
  # the map hard to read
  # There's a bug that the scale shows colour for negtive values,
  # which are impossible. The colors in the map are fine, it's just
  # the colourbar that is confusing.
  
  # Make sure that various cohorts share the same range of colours
  # in the legend colorbar
  # bar_br <- seq(shift_colors_by, 3 + shift_colors_by, 1)
  bar_br <- seq(0, 3, 1)
  bar_lim <- c(0, 3.5)
  
  bar_name <- paste0("Number of children\nlost by a woman\naged ", retirement_age ," years")
  # p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
  
  # Keep only people entering retirement age, defined as the 1955 cohort
  # currently, approaching age 65
  
  # Get share outlived mother:
  values <-
    df_cl_m_full %>%
    filter(cohort == cohort_show) %>% 
    filter(age == retirement_age) %>% 
    filter(country != 'channel islands') %>% 
    mutate(
      country = ifelse(country == "eswatini", "swaziland", country)
      , country = countrycode(country, "country.name", "iso3c")
      # to fix color scale
    ) %>% 
    select(country, value) 
  
  # Join with map
  w <- left_join(
    world
    , values
    , by = "country"
  ) %>% 
    filter(! ID %in% "Antarctica")
  
  # Plot
  
  ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = country_line_size) +
    scale_fill_viridis(
      name = bar_name
      , option="viridis"
      # , direction = -1
      , breaks = bar_br
      , limits = bar_lim
    ) +
    labs(
      title = ""
    ) +
    ggtitle(p_title) +
    coord_sf(crs = "+proj=robin") +
    theme_minimal(base_size = 6) +
    theme(
      axis.line = element_blank(), axis.text = element_blank()
      , axis.ticks = element_blank(), axis.title = element_blank()
      , plot.title = element_text(size = title_size, face="bold")
    ) +
    guides(fill = guide_colourbar(barwidth = 1))
  
}

# For plotting measure for SA materials
map_child_survival <- function(cohort_show, ...) {
  
  # Make sure that various cohorts share the same range of colours
  # in the legend colorbar
  bar_br <- seq(0, 6, 1)
  p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
  bar_lim <- c(0, 6.5)
  
  # bar_name <- paste0("Number of children\nsurviving for a woman\nretiring this year")
  bar_name <- paste0("Number of children\nalive for a woman\naged", retirement_age ," years")
  
  p_name <- paste0("../../Output/figS4-cs-",cohort_show,".pdf")
  
  # Keep only people entering retirement age, defined as the 1955 cohort
  # currently, approaching age 65
  
  # Get share outlived mother:
  values <-
    df_cs_m_full %>%
    filter(cohort == cohort_show) %>% 
    filter(age == retirement_age) %>% 
    filter(country != 'channel islands') %>% 
    mutate(
      country = ifelse(country == "eswatini", "swaziland", country)
      , country = countrycode(country, "country.name", "iso3c")
    ) %>% 
    select(country, value) 
  
  # Join with map
  w <- left_join(
    world
    , values
    , by = "country"
  ) %>% 
    filter(! ID %in% "Antarctica")
  
  # Plot
  
  ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = country_line_size) +
    scale_fill_viridis(
      name = bar_name
      , option="viridis"
      , breaks = bar_br
      , limits = bar_lim
    ) +
    labs(
      title = ""
    ) +
    coord_sf(crs = "+proj=robin") +
    ggtitle(p_title) +
    theme_minimal(base_size = 6) +
    theme(
      axis.line = element_blank(), axis.text = element_blank()
      , axis.ticks = element_blank(), axis.title = element_blank()
      , plot.title = element_text(size = title_size, face="bold")
    ) +
    guides(fill = guide_colourbar(barwidth = 1))
  
}



# For plotting measure for SA materials
map_share_outlived_mother <- function(cohort_show, ...) {
  
  # Make sure that various cohorts share the same range of colours
  # in the legend colorbar
  bar_br <- seq(0.70, 1, 0.05)
  bar_lim <- c(0.65, 1)
  
  # bar_name <- paste0("Offspring expected\nto outlive a woman\nretiring in ", cohort_show + 70)
  bar_name <- paste0("Offspring expected\nto outlive a woman")
  p_title <- paste0("Women born in ", cohort_show, " and retiring in ", cohort_show + 70)
  p_name <- paste0("../../Output/figS4-share-survive-",cohort_show,".pdf")
  
  
  # Keep only people entering retirement age, defined as the 1955 cohort
  # currently, approaching age 65
  
  # Get share outlived mother:
  cl_share <-
    ecl_ctfr %>%
    filter(cohort == cohort_show) %>% 
    filter(country != 'channel islands') %>% 
    mutate(
      share = 1 - value / tfr
      , country = ifelse(country == "eswatini", "swaziland", country)
      , country = countrycode(country, "country.name", "iso3c")
    ) %>% 
    select(country, value = share) 
  
  # Join with map
  w <- left_join(
    world
    , cl_share
    , by = "country"
  ) %>% 
    filter(! ID %in% "Antarctica")
  
  # Plot
  
  # p1 <- 
  ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = country_line_size) +
    scale_fill_viridis(
      name = bar_name
      # , option="magma"
      , option="viridis"
      , breaks = bar_br
      , limits = bar_lim
      , labels = function(br) paste0(round(br*100), "%")
    ) +
    labs(
      title = ""
    ) +
    coord_sf(crs = "+proj=robin") +
    ggtitle(p_title) +
    theme_minimal(base_size = 6) +
    theme(
      axis.line = element_blank(), axis.text = element_blank()
      , axis.ticks = element_blank(), axis.title = element_blank()
      , plot.title = element_text(size = title_size, face="bold")
    ) +
    guides(fill = guide_colourbar(barwidth = 1))
  
}


map_share_child_deaths_in_age_range <- function(cohort_show, col, bar_name, ...) {
  
  # Keep only people entering retirement age, defined as the 1955 cohort
  # currently, approaching age 65
  
  # Get value :
  values <-
    share_of_deaths_in_retirement %>%
    filter(cohort == cohort_show) %>%
    filter(country != 'channel islands') %>%
    mutate(
      country = ifelse(country == "eswatini", "swaziland", country)
      , country = countrycode(country, "country.name", "iso3c")
    ) %>%
    select(country, value = dplyr::starts_with(col))
  
  # Join with map
  w <- left_join(
    world
    , values
    , by = "country"
  ) %>%
    filter(! ID %in% "Antarctica")
  
  # Plot
  
  # p1 <-
  ggplot(data = w) +
    geom_sf(aes(geometry = geometry, fill = value), colour = alpha("white", 1 / 2), size = country_line_size) +
    scale_fill_viridis(
      name = bar_name
      , option="viridis"
      , breaks = bar_br
      , limits = bar_lim
      , direction = viridis_direction
      , labels = function(br) paste0(round(br*100), "%")
    ) +
    labs(
      title = ""
    ) +
    coord_sf(crs = "+proj=robin") +
    ggtitle(p_title) +
    theme_minimal(base_size = 6) +
    theme(
      axis.line = element_blank(), axis.text = element_blank()
      , axis.ticks = element_blank(), axis.title = element_blank()
      , plot.title = element_text(size = title_size, face="bold")
    ) +
    guides(fill = guide_colourbar(barwidth = 1))
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

# highest-level function.  Takes df of period ASFR grouped in
# 5 age and 5 year groups from UNWPP and and returns 
# Single year and single age estimates
# Function has no output but saves df to disk
ungroup_fertility_ASFR <- function(fert_df, variant_name, allowed_types = c("Country", "SDG region"), export = T, returnme = T){
  
  print(paste("Expanding ASFR for variant:", variant_name))
  
  # 1. Reformat fert data 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1.1. Change column names 
  
  old_names <- colnames(fert_df)
  
  new_names <- c("index", "variant", "country", "notes", "country_code", 
                 "type","parent_code", "year", "X15.19", "X20.24", 
                 "X25.29", "X30.34", "X35.39", "X40.44", "X45.49")
  
  # Change colnames
  colnames(fert_df) <- new_names
  
  # Save for future reference
  fert_codebook <- data.frame(old = old_names, new = new_names)
  # print(fert_codebook)
  
  # sum(is.na(fert_df$year))
  
  fert_df[fert_df == "..."] <- NA
  
  fert_5_5 <- 
    fert_df %>% 
    filter(type %in% allowed_types) %>% 
    select(country, year, dplyr::starts_with("X")) %>% 
    reshape2::melt(id = c("country", "year")) %>% 
    select(country, year, age = variable, value) %>% 
    mutate_all(list(as.character)) %>% 
    # Remove unwanted punctuation
    mutate(
      country = fix_un_countries(country)
      , age = gsub("\\.|\\?", "-", age)
      , value = as.numeric(value)
    ) %>% 
    arrange(country, year, age) 
  
  # 1.2. Parameters 
  
  # Change period labels
  # UN calendar year periods are non-exlusive; ie 1950-1955 and 1955-1960
  # The should actually be only 5 yers long: 1950-1954 and 1955-1959
  
  per_old <- unique(fert_5_5$year)
  
  per_new <- unlist(lapply(per_old, function(p) {
    pe <- as.numeric(unlist(strsplit(p, "-")))
    pe[2] <- pe[2] -1
    paste(pe, collapse = "-")
  }))
  
  fert_5_5$year <- plyr::mapvalues(fert_5_5$year, from = per_old, to = per_new)
  
  all_years <- as.numeric(unlist(strsplit(per_new, "-")))
  
  min_y <- min(all_years)
  max_y <- max(all_years)
  year_range <- min_y:max_y
  
  # grouped_ages <- seq(15, 45, 5)
  grouped_ages <- seq(15, 50, 5)
  age_groups_range <- 15:49
  
  countries <- unique(fert_df$country)
  
  # 2. Ungroup age groups 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  l_5_5 <- split(fert_5_5, list(fert_5_5$country, fert_5_5$year))
  
  # This returns a data frame with 1-year age bands but still grouped in 5-year long calendar year periods
  fert_5_1 <- expand_asfr_age(
    l_5_5 = l_5_5
    , grouped_ages = grouped_ages
    , method = "linear"
    , col = 'value'
  )
  
  rm("l_5_5")
  
  # View(fert_5_1)
  
  # 3. Interpolate calendar years 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  fert_per_1_1 <- interpolate_COLUMN_calendar_years(
    df_5_1 = fert_5_1
    , method = "linear"
    , col = 'value'
  )
  
  rm("fert_5_1")
  
  # 4. Visual exam 
  # Plot for same country, one 
  
  # if(visual_examination) {
  #   
  #   country_keep <- c("sweden", "guatemala", "israel", "sri lanka")
  #   
  #   # 4.1. Compare ASFR 
  #   
  #   (
  #     test_asfr_1_1 <- 
  #       cowplot::plot_grid(
  #         #  Original data
  #         fert_5_5 %>% 
  #           filter(country %in% country_keep) %>% 
  #           mutate(
  #             value = as.numeric(value)
  #           ) %>% 
  #           ggplot(aes(x = age, y = value, group = year, colour = year)) +
  #           geom_line(size=0.5) +
  #           facet_grid(~country) +
  #           theme(legend.position = "none")
  #         # Smoothed data
  #         , fert_per_1_1 %>% 
  #           filter(country %in% country_keep) %>% 
  #           mutate(source = "interpolated") %>% 
  #           ggplot(aes(x = age, y = value, group = year, colour = year)) +
  #           geom_line(size=0.5) +
  #           facet_grid(~country) +
  #           theme(legend.position = "none")
  #         , ncol = 1) 
  #   )
  #   
  #   # 4.2. Compare TFR
  #   
  #   # Random countries
  #   country_keep <- tolower( countries[sample(1:length(countries), 20)] )
  #   
  #   tfr_new <- 
  #     fert_per_1_1 %>% 
  #     mutate(year = year - 3) %>% 
  #     group_by(country, year) %>% 
  #     summarise(tfr = sum(value)/1000) %>% 
  #     ungroup %>% 
  #     mutate(
  #       country = as.character(country)
  #       , source= 'new'
  #     )
  #   
  #   tfr_old <- 
  #     fert_5_5 %>% 
  #     mutate(year = as.numeric(str_extract(year, "^[0-9]{4}"))) %>% 
  #     group_by(country, year) %>% 
  #     summarise(tfr = sum(value)/1000*5) %>% 
  #     ungroup %>% 
  #     mutate(
  #       source= 'old'
  #     )
  #   
  #   tfr_both <- 
  #     bind_rows(
  #       tfr_new
  #       , tfr_old
  #     ) %>% 
  #     filter(country %in% country_keep)
  #   
  #   (
  #     test_tfr_1_1 <- 
  #       tfr_both %>% 
  #       ggplot(aes(x = year, y = tfr, group = source, colour = source)) +
  #       geom_line(size = 0.5) +
  #       facet_wrap(~country)
  #   )
  #   
  #   #Export graphs
  #   
  #   ggsave("../../Output/A.3.test_asfr_1_1.pdf", test_asfr_1_1)
  #   ggsave("../../Output/A.3.test_tfr_1_1.pdf", test_tfr_1_1)
  #   
  # }
  
  # 5. Export 
  
  if(export) {
    write.csv(x = fert_per_1_1, file = paste0("../../Data/derived/", "fert_per_1_1_",variant_name,".csv"), row.names = F)
    print(paste(variant_name, "Exported!"))
  }
  
  if(returnme){
    fert_per_1_1
  } else {
    print("Returning no value...")
  }
  
}

# highest-level function.  Takes df of 
# 5 age and 5 year groups from UNWPP and and returns 
# Single year and single age estimates
# Function has no output but saves df to disk
ungroup_births <- function(births_obs_B, births_pred_B,sex_ratio_5, variant_name, export){
  
  # 0. Format 
  # ~~~~~~~~~~~~~~~~~~
  
  # 0.1. Births df
  
  births_5 <- bind_rows(
    format_births(df = births_obs_B)
    , format_births(df = births_pred_B)
  ) %>% 
    # Fix UN overlapping periods
    mutate(
      year = change_period_labels(year)
      # UN values are given in thousands
      , value = value*1000
    ) %>% 
    arrange(country, year)
  
  # 0.2. Sex ratio at birth df
  
  # sex_ratio_5 <- bind_rows(
  #   format_births(df = sex_ratio_at_birth_obs)
  #   , format_births(df = sex_ratio_at_birth_pred)
  # ) %>% 
  #   # Fix UN overlapping periods
  #   mutate(year = change_period_labels(year)) %>% 
  #   arrange(country, year)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # A. Women only 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1. 5-year number of female births 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  births_f_5 <- 
    births_5 %>% 
    rename(births = value) %>% 
    left_join(
      sex_ratio_5 %>% 
        rename(sex_ratio = value) 
      , by = c("country", "year")
    ) %>% 
    mutate(
      value = births / (sex_ratio + 1)
    ) %>% 
    select(country, year, value)
  
  # births_f_5 <- 
  #   births_5 %>% 
  #   mutate(
  #     value = value / (sex_ratio_5$value + 1)
  #   )
  
  # 2. 1-year number of female births 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Interpolate female births for every calendar year
  
  births_f_1 <- 
    interpolate_births_calendar_years(
      df_5 = births_f_5
      , method = "linear"
    ) %>% 
    mutate(
      country = as.character(country)
      , value = round(value, 0)
    )
  
  # 3. Checks 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # if(visual_examination) {
  #   
  #   country_keep <- c("sweden", "guatemala", "israel", "sri Lanka")
  #   
  #   cowplot::plot_grid(
  #     births_f_1 %>% 
  #       filter(country %in% country_keep) %>% 
  #       ggplot(aes(x = year, y = value, group = country, colour = country)) +
  #       geom_line(size=0.5) +
  #       scale_x_continuous(breaks = seq(1950, 2100, 50)) +
  #       theme(legend.position = "none")
  #     , 
  #     births_f_5 %>% 
  #       filter(country %in% country_keep) %>% 
  #       mutate(year = as.numeric(str_extract(year, '^[0-9]{4}'))) %>% 
  #       ggplot(aes(x = year, y = value, group = country, colour = country)) +
  #       scale_x_continuous(breaks = seq(1950, 2100, 50), labels = seq(1950, 2100, 50)) +
  #       geom_line(size=0.5) +
  #       # facet_grid(. ~ country) +
  #       theme(legend.position = "right")
  #   )
  #   
  # }
  
  # 4. Export 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(export) {
    n_out <- paste0('../../Data/derived/wpp_female_births_1_1_',variant_name,'.csv')
    write.csv(births_f_1, file = n_out, row.names = F)
    print(paste0("Exported - ",n_out))
    
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # B. Men and women
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 1. 1-year number of female births
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Interpolate female births for every calendar year
  
  births_1 <- 
    interpolate_births_calendar_years(
      df_5 = births_5
      , method = "linear"
    ) %>% 
    mutate(
      country = as.character(country)
      , value = round(value, 0)
    )
  
  # 2. Checks
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # if(visual_examination) {
  #   
  #   country_keep <- c("sweden", "guatemala", "israel", "sri Lanka")
  #   
  #   cowplot::plot_grid(
  #     births_1 %>% 
  #       filter(country %in% country_keep) %>% 
  #       ggplot(aes(x = year, y = value, group = country, colour = country)) +
  #       geom_line(size=0.5) +
  #       scale_x_continuous(breaks = seq(1950, 2100, 50)) +
  #       theme(legend.position = "none")
  #     , 
  #     births_5 %>% 
  #       filter(country %in% country_keep) %>% 
  #       mutate(year = as.numeric(str_extract(year, '^[0-9]{4}'))) %>% 
  #       ggplot(aes(x = year, y = value, group = country, colour = country)) +
  #       scale_x_continuous(breaks = seq(1950, 2100, 50), labels = seq(1950, 2100, 50)) +
  #       geom_line(size=0.5) +
  #       # facet_grid(. ~ country) +
  #       theme(legend.position = "right")
  #   )
  #   
  # }
  
  # 3. Export 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(export) {
    n_out <- paste0('../../Data/derived/wpp_all_births_1_1_',variant_name,'.csv')
    write.csv(births_f_1, file = n_out, row.names = F)
    print(paste0("Exported - ",n_out))
    # write.csv(births_1, file = paste0('../../Data/derived/wpp_all_births_1_1_',variant_name,'.csv'), row.names = F)
  }
}

# This function is stored here and not in _global_functions.R since it is pretty much 
# all the analysis

ungroup_mortality_from_mx_robust <- function(lt_per, sex = "F", parallel = T, numCores = 4, export = F) {
  
  # 1. Format life tables 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # I will do this for women initially, but the same can be repeated for men
  
  # lt_per is a df of period life tables
  # If running interactively, for women, this would be:
  # lt_per <- lt_per_F_small
  
  lt_5_5 <- lt_per_5_5 <- 
    lt_per %>% 
    select(country, year, age, interval, mx, qx, ax, lx, dx, Lx, Tx, ex) %>% 
    # filter out rows with no data (headings)
    filter(!is.na(age)) %>% 
    mutate(
      country = fix_un_countries(country)
      , age = as.numeric(gsub("\\+", "", age))
      # Change period labels
      # UN calendar year periods are non-exlusive; ie 1950-1955 and 1955-1960
      # The should actually be only 5 yers long: 1950-1954 and 1955-1959
      , year = change_period_labels(year)
      , mx = as.numeric(mx)
      , ax = as.numeric(ax)
    )
  
  
  # 1.1. Parameters 
  # ~~~~~~~~~~~~~~~~~~~~~
  
  all_years <- as.numeric(unlist(strsplit(unique(lt_5_5$year), "-")))
  
  min_y <- min(all_years)
  max_y <- max(all_years)
  year_range <- min_y:max_y
  
  grouped_years <- unique(lt_5_5$year)
  countries <- unique(lt_5_5$country)
  
  grouped_ages <- unique(lt_5_5$age)
  
  # Temporary 
  # change if you do chose to extrapolate to older ages
  grouped_ages_extrap <- grouped_ages
  
  # 2. Interpolate ages (mx col) 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Expand abridged life table to create a full life table
  # this is currently done by interpolating between the grouped mx values
  # A more sophisticated approach would expand on qx values (see below)
  
  # First, split tables into a list with country/year combinations
  
  lt_5_5_l <- split(lt_5_5, list(lt_5_5$country, lt_5_5$year))
  
  print("Interpolating ages from the mx column...")
  
  # Note on 20191009:
  # This inerpolates mx values within each age group
  # and returns an expanded df with mx values for ages 0:100
  # It assumes that 5Mx values represent the value
  # for the mid-interval age; ie 5_M_x = 1_M_{(x+2)}
  # (more details inside function)
  mx_5_1 <- expand_LT_age_by_mx_linear(
    l_5_5 = lt_5_5_l
    , grouped_ages = grouped_ages
  )
  
  rm("lt_5_5_l")
  
  # 3. Interpolate calendar years  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # This are qx per country/period
  # However, they are still grouped by 5-year calendar years
  # Therefore, we need to smooth these qx values over the 5 calendar years before 
  # gettin gthe values for the life tables
  
  print("Interpolating calendar years...")
  
  # Preferred option 20190628
  # Takes around 5-10 minutes
  # Spline is prefered as method since it gives more accurate values
  mx_1_1 <- expand_LT_year_by_mx(
    df_5_1 = mx_5_1
    , method = "spline"
    , parallel = parallel
    , numCores = numCores
  )
  
  print("mx_1_1 df created")
  rm("mx_5_1")
  
  # 4. Create period life tables from mx matrices
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now I have a data frame with all mx values for every country/year combination
  # the next step is to create life tables for each of these combinations
  
  print("Creating life tables from new mx column...")
  
  mx_1_1_l <- split(mx_1_1, list(mx_1_1$country, mx_1_1$year))
  
  if(parallel) {
    # Preferred option 20190628
    # Parallel version
    lt_1_1_l <- lt_mx_parallel(
      l_mx_1_1_l = mx_1_1_l
      , numCores = numCores
    )
  } else {
    # non-parallel verion
    lt_1_1_l <- lapply(mx_1_1_l, function(df) {
      lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E6)
      cbind(
        df[1:nrow(lt) , 1:2]
        , lt
      )
    })
  }
  
  # sAVE AS DF
  lt_1_1 <- data.frame( rbindlist(lt_1_1_l, use.names = T))
  
  rm("mx_1_1_l")
  rm("lt_1_1_l")
  gc()
  
  # 4.2. Final edits 
  
  # Set ax columns value for for upper age group
  # Currently it is NA, set to 1/qx
  
  lt_1_1$ax[lt_1_1$age == 100] <- 1/lt_1_1$mx[lt_1_1$age == 100]
  
  print("1x1 life tables created for all countries")
  
  # 7. Export 
  # ~~~~~~~~~~~~~~~~~~
  
  if(export) {
    print("Saving to csv file...")
    file <- paste0("../../Data/derived/", "lt_per_1_1_", sex,".csv")
    write.csv(x = lt_1_1, file = file, row.names = F)
    
    print(paste("All period life tables saved to", file))
    
  }  
  
  return(lt_1_1)
  
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
    # spline_mx <- interpSpline(seq(1, 150, 5), df$mx)
    # val <- predict(spline_mx, 1:150)$y
    # Grouped year intervals of the UN WPP data refer to mid-year estimates
    # so that the value reported for 1950-1955 actually refers to the period
    # 1950.5-1955.5. This means that the value can be interpreted as that 
    # corresponding to the middle of the period, in this case the 1 of January
    # of 1953.
    # I consider this in the spline interpolation below by considering that each
    # interval corresponds to the lower bound plus 3;
    spline_mx <- interpSpline(seq(3, y_range_length, 5), df$mx)
    val <- predict(spline_mx, 1:y_range_length)$y
  }
  
  data.frame(
    country = unique(df$country)
    , year = years
    , age = unique(df$age)
    , mx = val
  )
  
}

worker_lt_mx <- function(df) {
  lt <- lt_mx(nmx = df$mx, age = 0:100, radix = 1E6)
  
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
      if(age < age_limit_max + 1 & year <= year_limit_max) {
        df$nLx[lag_position] <- 
          df$lx[current_position] + df$dx[lag_position] * df$ax[lag_position]
      }
    }
  }
  
  # 2. dx and nLx columns for open age interval
  
  open_age <-  which(df$Age == age_limit_max & df$Cohort <= year_limit_max)
  last_age <- which(df$Age== age_limit_max - 1  & df$Cohort <= year_limit_max)
  open_age2 <- which(df$Age == age_limit_max)
  
  df$dx[open_age] <- df$lx[last_age] * df$qx[last_age]
  df$nLx[open_age2] <- df$ax[open_age2] * df$dx[open_age2]
  
  # 3. Tx column (persons alive at any point in time above age x)
  
  for (year in years){
    if (year <= year_limit_max)
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

# RIBUSTNESS FUNCTIONS -----------

process_mortality_robust_top <- function(lt_per, numCores){
  
  variant_current <- unique(lt_per$variant)
  
  lt_1_1 <- 
    ungroup_mortality_from_mx_robust(
      lt_per = lt_per
      , sex = "F"
      , parallel = T
      , numCores = numCores
      , export = F
    )
  
  # Convert to cohort
  LTC <- 
    convert_period_LT_to_cohort_LT_robust(
      lt_1_1 = lt_1_1
      , sex = "F"
      , export = F
      , years = 1950:2100
      , ages = 1:100
      , parallel = T
      , numCores = numCores
    ) %>% 
    mutate(variant = variant_current)
  
  # Create Matix of survival probs
  
  # This must be the same as the age groups in the asfr data
  # cos <- c(1950:2100) # cohorts
  cos <- c(2000:2001) # cohorts
  xs <- c(15:49) # reproductive age
  mas<-c(15:100) # mother ages
  
  matrix_of_survival_probabilities_robust(
    LTC = LTC
    , cos = cos
    , xs = xs
    , mas = mas
    , numCores = numCores
  )
  
}

matrix_of_survival_probabilities_robust <- function(LTC, run_checks = F, cos, xs, mas, numCores) {
  
  # 1. Get matrix of survival probabilities of kids 
  
  LTC_l <-  split(LTC, LTC$Country)
  
  # If done together, this returns a list sized 1.04 GB, which is inconvenient
  # This functino splits the data and saves it into a determined number of chunks
  # It return no object but saves no_chunks number of files that can later be loaded into R
  
  # Takes 15 MINUTES USING 25 CORES on a HPC.
  
  closeAllConnections()
  
  print("Getting matrix of survival probs and saving as separate files...")
  
  survival_probs_parallel_robust(
    l = LTC_l
    , xs
    , mas
    , cos
    , numCores = numCores
  )
  
}


survival_probs_parallel_robust <- function(l, xs, mas, cos, numCores = 4) {
  
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
    out <- parLapply(cl, l, worker_survival_probs_robust, xs, mas, cos) 
  ))
  
  stopCluster(cl)  
  
  print("Done! This function returns no object since save_output == T")
  
}


worker_survival_probs_robust <- function(life_table, xs, mas, cos) {
  browser()
  
  pais <- unique(life_table$Country)
  var <- unique(life_table$variant)
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
  
  nombre <- paste0("lx.kids.arr_", pais, "_", var)
  # assign(x = nombre, value = lx.kids.arr)
  
  file <- paste0("../../Data/derived/", nombre, ".RDS")
  
  saveRDS(object = lx.kids.arr, file = file)
  print(paste(file, "saved"))
  
}

# here ==============

child_loss_robust <- function(countries, reference_years, ages_keep = 15:100, path = "../../Data/derived", ASFRC, variant_fert, variant_mort) {
  
  df_l <- lapply(
    countries
    , worker_child_loss_robust
    , reference_years = reference_years
    , sex_keep = F
    , ages_keep = ages_keep
    , ASFRC
    , path = path
    , variant_mort = variant_mort
  )
  
  data.frame(rbindlist(df_l, use.names = T))
  
}

worker_child_loss_robust <- function(c, reference_years, sex_keep = F, ages_keep, ASFRC, path, variant_fert, variant_mort) {
  
  # 2.1. Get LT for chosen years
  
  lx_array_temp <- get_lx_array_robust(
    c = c
    , reference_years = reference_years
    , sex_keep = sex_keep
    , path = path
    , variant_mort = variant_mort
  )
  
  # 2.2. Chose ASFR for chosen years
  
  ASFR_df <- 
    ASFRC %>% 
    filter(country %in% c) %>% 
    filter(Cohort %in% reference_years)
  
  # 2.3. Expected child loss and child survival
  
  estimates <- expected_child_death(
    ASFRSC = ASFR_df
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
      , variant_fert = variant_fert
      , variant_mort = variant_mort
    )
  
}

get_lx_array_robust <- function(c, reference_years, sex_keep, path = "../../Data/derived", variant_mort){
  print(c)
  
  file <- paste0(paste0(path, "/lx.kids.arr_", c,"_",variant_mort, ".RDS"))
  lx.kids.arr <- readRDS(file)  
  lx_array_temp <- lx.kids.arr[ , , paste(reference_years)]
  return(lx_array_temp)
}
