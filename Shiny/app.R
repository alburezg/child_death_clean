# TO DO
# Fix encoding of Curacao, reunion and cote divore
# - Add logo
# - Make video

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
# library(plotly)


# Read data ----

# s1 <- as.data.frame( data.table::fread("Data/datasetS1.csv", stringsAsFactors = F) ) 
# s1v <- as.data.frame( data.table::fread("Data/datasetS1_variants.csv", stringsAsFactors = F) ) 

s1 <- 
  as.data.frame( data.table::fread("Data/datasetS1.csv", stringsAsFactors = F) ) %>% 
  mutate(variant = "mm-mf") %>% 
  bind_rows(
    as.data.frame( data.table::fread("Data/datasetS1_variants.csv", stringsAsFactors = F) )     
  )
  
s2 <- read.csv("Data/datasetS2.csv", stringsAsFactors = F)
region <- read.csv("Data/region.csv", stringsAsFactors = F)
lookup <- read.csv("Data/lookup.csv", stringsAsFactors = F)

equations <- readLines("Data/equations.txt")
captions <- readLines("Data/captions.txt")
measures <- read.csv("Data/measures.csv", stringsAsFactors = F)

# Text for intro

# f <- list.files(path = "Data/", pattern = "text_", full.names = T)
# text <- lapply(f, readLines)
# 
# names(text) <- gsub("\\.txt", "", gsub("Data/text_", "", f))

text <- 
  list(
    # introduction = "Recent population change has seen increases in life expectancy, reductions in family size, and postponement of fertility to older ages. We analyze the effect of these dynamics on the experience of child death over the life course for the 1950-2000 annual birth cohorts of women around the world.", 
    introduction ="The death of a child is a devastating experience for parents. Existing studies consider period measures of infant and young child mortality, but little is known about the experience of offspring mortality from a life course perspective. Using methods from mathematical demography, we project that child death will be more infrequent for younger cohorts of women around the world, who will increasingly experience the death of adult children. We expect the gap between the Global South and North to narrow over time but persisting international inequalities are troubling in a rapidly aging population where the loss of support from adult children can affect parental well-being. This is especially worrying for populations without access to effective institutional support for the older population.", 
    question = "We ask how a sustained fertility decline and rising life expectancy, driven by radical reductions in young child mortality, affect the experience of offspring mortality over the life course.",
    # methods = "The paper draws on age-specific fertility and mortality rates from the latest Revision of the United Nations World Population Prospects (2019, estimates and projections) to assess trends in the frequency and timing of child death using formal demographic methods. We discuss the variation in woman's exposure to offspring mortality according to the demographic regimes prevailing in different world regions.", 
    methods1 = "We extend a set of classic demographic methods known as the Goodman, Keyfitz and Pullum Kinship Equations (GKP equations) to analyze changes in offspring mortality for women using country-level demographic rates. The number of children ever born to a woman aged 'a' born in cohort 'c' standing before us will be equal to the number of children who are currently alive plus the children that died before the woman reached age 'a' is:",
    methods2 = "where F(x,c,p) represents age-specific fertility rates for cohort 'c' and country 'p', at age 'x' and l(a-x,c+x,p) represents the survival probability until age (a-x) for the cohort born in year (c+x).",
    methods3 = "We capture cohort changes in the lived experience of child death using data for the entire world population - 201 countries - from the 2019 Revision of the UN World Population Prospects for the 1950-2100 period.",
    findings = "We project a global reduction in the overall frequency of child death over the life course for younger cohorts of women. We expect the largest improvements in regions of the Global South where offspring mortality continues to be a common life event. In spite of persisting regional inequalities, we show evidence of a global convergence towards a future where the death of a child will become ever more infrequent for women. We anticipate that global population aging will be accompanied by an aging of generational relationships where life events such as the death of a child will be experienced at older ages. ", 
    interpretation = "We investigate the effect of changing mortality and fertility levels on the experience of offspring mortality for successive female cohorts. Even with the projected improvements in child mortality, women will continue to experience child death in the future.  The study emphasizes that, far from being restricted to a woman's reproductive age, child loss occurs throughout the life course.  Indeed, given the anticipated changes in global demography, 'child death' will increasingly come to mean the death of an adult offspring for younger cohorts of women."
  )


# Shiny Input options ----

region_type_choice <- lookup$pretty[lookup$type == "region_type"]

# countries <- unique(s1$country)
countries <- sort( lookup$pretty[lookup$type == "country"] )

variant_choices <- c(
  "medium variant"
  , "low mortality - low fertility"
  , "low mortality - high fertility"
  , "high mortality - low fertility"
  , "high mortality - high fertility"
  , "stable (constant at 2000 levels)"
  )

variant_lookup <- c("mm-mf", "lm-lf", "lm-hf", "hm-lf", "hm-hf", "cm-cf")
names(variant_lookup) <- c(variant_choices)

cohorts <- unique(s1$cohort)

names(equations) <- measures$original
names(captions) <- measures$original

# Note: 
# Warning in max(ids, na.rm = TRUE) :
#   no non-missing arguments to max; returning -Inf
# is expected. It happens when trying to plot confidence intervals for
# country level estimates
plotme <- function(
  measure_choice = "Cumulative Child Death"
  , region_type_choice = "UN SDG Regions"
  , regions_choice = "Sub-Saharan Africa"
  , countries_choice = c("Guatemala", "Sweden")
  , cohorts_choice = c(1950, 2000)
  , variant_choice = "medium variant"
  , quant_low = 0.4
  , quant_high = 0.6
  , y_lab_caption = "Cumulative Child Death"
  , region_line_size = 1
  , point_size = 3
  , age_br = c(seq(5, 100, 20), 100)
  , base_size = 20
  , export_df = F
) {
  
  # if(region_type_choice == "un_sdg_groups") browser()
  # browser()
  
  # recode regions
  
  if(all(regions_choice == "") & all(countries_choice == "")) {
    data.frame(x = 1, y = 1, label = "Please select a country or region.") %>% 
    ggplot() +
      geom_text(aes(x = x, y = y, label = label), size = 11) +
      theme_void()
  } else {
    
  
    measure_choice <- find_measure(measure_choice)
    region_type_choice <- find_region(region_type_choice, type = "region_type")
    regions_pretty <- regions_choice
    regions_choice <- find_region(regions_choice, type = region_type_choice)
    countries_pretty <- countries_choice
    countries_choice <- find_region(countries_choice, type = "country")
    variant_choice <- find_variant(variant_choice)
    
    # If in dataset1
    if(measure_choice %in% measures$original[1:4]) {
      estimates <- s1[ , c("variant","country", "cohort", "age", measure_choice)]
      names(estimates) <- c("variant","country", "cohort", "age", "value")
      
      estimates <- estimates %>% 
        filter(cohort %in% cohorts_choice) %>% 
        select(variant, country, age, cohort, value) %>% 
        mutate(low = NA, high = NA)
      
      country_estimates <- estimates %>% 
        filter(country %in% countries_choice) %>% 
        filter(variant %in% variant_choice)
      
      final_estimates <- country_estimates %>% dplyr::rename(region = country)
      
      # If regional estimates should be provided
      if(!all(is.na(regions_choice))) {
        # browser()
        reg <-  merge(estimates, region, by = "country") 
        reg$country <- NULL
        
        names(reg)[grep(region_type_choice, names(reg))] <- "region"
        
        # if(measure_choice == "first_difference_of_child_death") browser()
        
        # browser() 
        
        regional_estimates <- 
          reg %>%
          filter(variant %in% variant_choice) %>% 
          # select(-variant) %>% 
          filter(region %in% regions_choice) %>%
          # Remove age 100 for first difference and derived
          # filter(!is.na(value)) %>% 
          group_by(region, age, cohort) %>%
          summarise(
            median = median(value)
            , low = quantile(value, quant_low, na.rm = T)
            , high = quantile(value, quant_high, na.rm = T)
          ) %>%
          dplyr::rename(value = median) %>% 
          ungroup() 
        
        # append to country list
        final_estimates <- 
          bind_rows(
            country_estimates %>% dplyr::rename(region = country)
            , regional_estimates
            )
      }
      
      # Plot 
      
      if(!export_df) {
        
        # Create breaks for labels
        
        # browser()
        
        labs_pretty <- c(regions_pretty, countries_pretty) 
        labs_original <- c(regions_choice, countries_choice) 
        
        # suppressMessages(
        final_estimates %>% 
          mutate(
            low = suppressWarnings( as.numeric(low) )
            , high = suppressWarnings( as.numeric(high) )
            , cohort = paste0("Woman born in ", cohort)
          ) %>% 
          ggplot() +
          geom_line(
            aes(x = age, y = value, group = region, colour = region)
            , size = region_line_size
            , show.legend = FALSE
          ) +
          # Plot ECL quantiles as bands
          geom_ribbon(
            aes(x = age, ymin = low, ymax = high, group = region, fill = region)
            , alpha = 0.4, show.legend = F
          ) +
          # Plot ECL shapes to help distinguish regions
          geom_point(
            aes(x = age, y = value, group = region, colour = region
                # , size = share
                , shape = region
            )
            , size = point_size
            , data = . %>% filter(age %in% age_br)
          ) +
          scale_x_continuous("Woman's age") +
          scale_y_continuous(
            y_lab_caption
            , position = "left"
            , sec.axis = dup_axis()
          ) +
          scale_color_discrete("", breaks = labs_original, labels = labs_pretty) +
          scale_fill_discrete("", breaks = labs_original, labels = labs_pretty) +
          scale_shape_discrete("", breaks = labs_original, labels = labs_pretty) +
          facet_wrap(. ~ cohort, scales = 'fixed') +
          # Use with four measures
          theme_bw(base_size = base_size) +
          theme(
            legend.position = "bottom"
            # Remove space over legend
            , legend.margin=margin(t=-0.25, r=0, b=0, l=0, unit="cm")
            # Remove space between legends
            , legend.key.size = unit(0.1, "cm")
            # Remove title on left
            , axis.text.y.left = element_blank()
            , axis.ticks.y.left = element_blank()
            , axis.title.y.right = element_blank()
            # get rid of facet boxes
            , strip.background = element_blank()
            # , strip.text.y = element_blank()
            # Move y axis closer to the plot
            , axis.title.y = element_text(margin = margin(t = 0, r = -2, b = 0, l = 0))
          )
        # )
        
      } else if(export_df) {
        final_estimates %>% arrange(region, cohort, age)
      }
        
      # If in dataset2
    
      } else if(measure_choice %in% measures$original[5:6]) {
      
      estimates <- s2[ , c("country", "cohort", measure_choice)]
      names(estimates) <- c("country", "cohort", "value")
      
      estimates <- estimates %>% 
        select(country, cohort, value) %>% 
        mutate(low = NA, high = NA)
      
      country_estimates <- estimates %>% 
        filter(country %in% countries_choice) 
      
      final_estimates <- country_estimates %>% dplyr::rename(region = country)
      
      # If regional estimates should be provided
      if(!all(is.na(regions_choice))) {
        
        reg <-  merge(estimates, region, by = "country") 
        reg$country <- NULL
        
        names(reg)[grep(region_type_choice, names(reg))] <- "region"
        
        regional_estimates <- 
          reg %>%
          filter(region %in% regions_choice) %>%
          group_by(region, cohort) %>%
          summarise(
            median = median(value)
            , low = quantile(value, quant_low)
            , high = quantile(value, quant_high)
          ) %>%
          dplyr::rename(value = median) %>% 
          ungroup() 
        
        # append to country list
        final_estimates <- rbind(country_estimates %>% dplyr::rename(region = country), regional_estimates)
      }
     
      if(!export_df) {
      
        # Plot 
        
        labs_pretty <- c(regions_pretty, countries_pretty) 
        labs_original <- c(regions_choice, countries_choice) 
        
        final_estimates %>% 
          mutate(
            low = suppressWarnings( as.numeric(low) )
            , high = suppressWarnings( as.numeric(high) )
          ) %>% 
          ggplot() +
          # Region summary lines
          geom_line(
            aes(x = cohort, y = value, group = region, colour = region)
            , size = region_line_size
            , show.legend = F
          ) +
          geom_ribbon(
            aes(x = cohort, ymin = low, ymax = high, group = region, fill = region)
            , alpha = 0.4, show.legend = F
          ) +
          geom_point(
            aes(x = cohort, y = value, group = region, colour = region
                , shape = region
            )
            , size = point_size
            , data = . %>% filter(cohort %in% seq(1950, 2000, 15))
          ) +
          scale_x_continuous(
            "Woman's birth cohort"
          ) +
          scale_y_continuous(
            y_lab_caption
          ) +
          scale_color_discrete("", breaks = labs_original, labels = labs_pretty) +
          scale_fill_discrete("", breaks = labs_original, labels = labs_pretty) +
          scale_shape_discrete("", breaks = labs_original, labels = labs_pretty) +
          scale_size_continuous("Population share") +
          theme_bw(base_size = base_size) +
          theme(
            legend.position = "bottom"
            # Remove space over legend
            , legend.margin=margin(t=-0.25, r=0.5, b=0, l=0, unit="cm")
            # Remove space between legends
            , legend.key.size = unit(0.1, "cm")
            # Move y axis closer to the plot
            , axis.title.y = element_text(margin = margin(t = 0, r = - 0.5, b = 0, l = 0))
            , plot.margin = unit(c(t=0.2, r=0.25, b=0.1, l=0.1), unit="cm")
            # get rid of facet boxes
            , strip.background = element_blank()
          )
        
      } else if(export_df) {
        final_estimates %>% arrange(region, cohort, age)
    }
      
      
    } 
    
  }

}


find_measure <- function(x) {
  original <- measures$original 
  names(original) <- measures$pretty
  original[x]
}

find_region <- function(x, type = "country"){
  original <- lookup$original[lookup$type==type]
  names(original) <- lookup$pretty[lookup$type==type]
  out <- original[x]
  names(out) <- NULL
  out
}

find_region_undo <- function(x, type = "country"){
  pretty <- lookup$pretty[lookup$type==type]
  names(pretty) <- lookup$original[lookup$type==type]
  out <- pretty[x]
  names(out) <- NULL
  out
}

find_variant <- function(x){
  variant_lookup[ifelse(x=="", "medium variant", x)]
}

# 2. Create app ----

# 2.1. Ui ====

ui <- navbarPage(
  title = "Global experience of child death"
  , tabPanel(
    "Cover page"
    , h1("Women's experience of child death over the life course: a global demographic perspective")
    , h4("Companion web application")
    , h4(tags$a(href="https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/diego_alburez_gutierrez_3783", "Diego Alburez-Gutierrez*"), tags$sup(1)
         , "-", tags$a(href="https://www.su.se/profiles/mkolk-1.187699", "Martin Kolk"), tags$sup(2)
         , "-", tags$a(href="https://www.demogr.mpg.de/en/about_us_6113/staff_directory_1899/emilio_zagheni_2243","Emilio Zagheni"), tags$sup(1)
         )
    , h4(tags$sup("1"),"Lab of Digital and Computational Demography, Max Planck Institute for Demographic Research")
    , h4(tags$sup("2"),"Stockholm University Demography Unit, Stockholm University")
    , h4("* Correspondence: alburezgutierrez[at]demogr.mpg.de", "-", tags$a(href="https://twitter.com/d_alburez", "@d_alburez"))
    , h3("Citation:")
    , h3("Alburez-Gutierrez, D., Kolk, M. and Zagheni E. (Forthcoming). Women's experience of child death: A global demographic perspective. Demography. Temporary DOI:", tags$a(href="https://doi.org/10.31235/osf.io/s69fz", "https://doi.org/10.31235/osf.io/s69fz"))
    # , h4("Access the full paper here: https://doi.org/10.31235/osf.io/s69fz")
    , h3("Supplementary Materials:", tags$a(href="https://osf.io/jdvhw/", "https://osf.io/jdvhw/"))
    # , tags$hr()
    # , h3("Summary")
    # , h5(paste(text$introduction))
    # , h1("")
    # , HTML('<iframe width="840" height="473" src="https://www.youtube.com/embed/-8PQ-EQSBek" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    # , h1("")
    # , tags$hr()
    # , h4("Research question")
    # , h5(paste(text$question))
    # , tags$hr()
    # , h4("Methods")
    # , h5(paste(text$methods1))
    # , withMathJax()
    # , paste(equations[1])
    # , h5(paste(text$methods2))
    # , h5(paste(text$methods3))
    # , h4("Findings")
    # , h5(paste(text$findings))
    # , img(src='fig4.PNG', height = "45%", width = "45%", style="text-align: center;")
    # , tags$h4("Figure 1.", em("(A) Expected number of children expected to outlive an average woman. (B) Children outliving a woman as a fraction of her cohort's TFR."))
    # , tags$hr()
    # , h4("Interpretation")
    # , h5(paste(text$interpretation))
    # , img(src='MPIDR-EN.png', height = "25%", width = "25%", align = "right")
    )
  # UI main panel =============
  , tabPanel(
    title = "Interactive results",
    h3("Explore the results changing the parameters as you wish.")
    , h5("In this tab, you can explore the results from the study by changing between six of the indicators that we developed for the study. Choose an indicator from the drop-down menu and a short description of the indicator should appear at the bottom of your screen. You can explore the robustness of the results for each measure by changing:")
    , h1("")
    , h5(tags$ol(
      tags$li("How countries are grouped in regions"), 
      tags$li("The selection of birth cohorts for women"), 
      # tags$li("The UN WPP variant for projecting fertility and mortality (for Cumulative Child Death)"),
      tags$li("Percentile  choice to display heterogeneity as bands around median")
    ))
    , h1("")
    , sidebarLayout(
      sidebarPanel(
        selectInput("measure", "Chose measure", choices = measures$pretty, selected = measures$pretty[1], multiple = F),
        selectInput("country", "Chose a country", choices = countries, selected = c("Guatemala", "Sweden"), multiple = T),
        selectInput('region_type', "Group countries by", choices = region_type_choice, selected = region_type_choice[2], multiple = F),
        uiOutput("region"),
        # Only show slider if relevant
        conditionalPanel(
          condition = paste0("input.measure == ", paste0("'", measures$pretty[1:4], "'"), collapse = " | "),
          sliderInput("cohort", "Chose two birth cohorts to display", min = 1950, max = 2000, value = c(1960, 1990), step = 5, sep = "")
        ),
        sliderInput("spread", "Upper and lower percentiles to visualise regional heterogeneity", min = 0.05, max = 0.95, value = c(0.4, 0.6), step = 0.05),
        downloadButton("downloadData", "Download current selection")
      ),
      mainPanel(
        plotOutput("cdall"),
        textOutput("caption"),
        withMathJax(),
        uiOutput("formula")
        # , img(src='MPIDR-EN.png', height = "25%", width = "25%", align = "right")
      ) # Close main panel
    ) # close sidebarlayout 
  ) # cose tabpanel
  # UI Variant panel =============
  , tabPanel(
    title = "Alternative projection variants",
    h3("Cumulative Child Death: Robustness to alternative projection variants.")
    , h5("In this tab, you can explore the results from the study by changing the UN WPP variant for projecting fertility and mortality for the key outcome of the study: Cumulative Child Death for an average woman surviving to age 'a'.")
    , h5(tags$ul(
      tags$li("For mortality, 'low' is the UN WPP's lower 95 projection interval (PI) and 'high' is the upper 95 PI."),
      tags$li("For fertility, 'low' and 'high' are the corresponding UN WPP's projection variants."),
      tags$li("The 'stable' variant is similar to the UN WPP's 'constant' scenario, except that rates remain fixed at the values oberved in 2000 (see main text for more details).")
      
    ))
    , h1("")
    , sidebarLayout(
      sidebarPanel(
        # selectInput("measure_var", "Chose measure", choices = measures$pretty[1], selected = measures$pretty[1], multiple = F),
        selectInput("variant_var", "Chose a UN WPP projection variant", choices = variant_choices[-1], selected = variant_choices[2], multiple = F),
        selectInput("country_var", "Chose a country", choices = countries, selected = c("Guatemala", "Sweden"), multiple = T),
        selectInput('region_type_var', "Group countries by", choices = region_type_choice, selected = region_type_choice[2], multiple = F),
        uiOutput("region_var"),
        sliderInput("cohort_var", "Chose two birth cohorts to display", min = 1950, max = 2000, value = c(1960, 1990), step = 10, sep = ""),
        sliderInput("spread_var", "Upper and lower percentiles to visualise regional heterogeneity", min = 0.05, max = 0.95, value = c(0.4, 0.6), step = 0.05)
        # downloadButton("downloadData", "Download current selection")
      ),
      mainPanel(
        plotOutput("cdall_var"),
        textOutput("caption_var"),
        # withMathJax(),
        # uiOutput("formula")
      ) # Close main panel
    ) # close sidebarlayout
  ) #end UI Variant panel
) # close ui
  

server <- function(input, output, session) {
  
  # Reactive UI widgets
  
  output$region <- renderUI({
    # browser()
    region_type_temp <- find_region(input$region_type, "region_type")
    region_choices <- sort( find_region_undo(unique(region[ , region_type_temp]), region_type_temp) )
    selectInput("region", "Chose a region", choices = region_choices, selected = "", multiple = T)
  })
  
  
  output$region_var <- renderUI({
    # browser()
    region_type_temp <- find_region(input$region_type_var, "region_type")
    region_choices <- sort( find_region_undo(unique(region[ , region_type_temp]), region_type_temp) )
    selectInput("region_var", "Chose a region", choices = region_choices, selected = "", multiple = T)
  })
  
  # Redner formula
  
  output$formula <- renderUI({
    withMathJax(
      helpText(equations[find_measure(input$measure)])  
    )
  })
  
  # Render caption
  
  output$caption <- renderText({
    captions[find_measure(input$measure)]
  })

    
  output$cdall <- renderPlot({
    
    plotme(
      region_type_choice = input$region_type
      , measure_choice = input$measure
      , regions_choice = input$region
      , countries_choice = input$country
      , cohorts_choice = input$cohort
      , variant_choice = ""
      , quant_low = min(as.numeric(input$spread))
      , quant_high = max(as.numeric(input$spread))
      , y_lab_caption = input$measure
    )
  
  })
  
  output$cdall_var <- renderPlot({
    
    plotme(
      region_type_choice = input$region_type_var
      , measure_choice = "Cumulative Child Death"
      , regions_choice = input$region_var
      , countries_choice = input$country_var
      , cohorts_choice = input$cohort_var
      , variant_choice = input$variant_var
      , quant_low = min(as.numeric(input$spread_var))
      , quant_high = max(as.numeric(input$spread_var))
      , y_lab_caption = "Cumulative Child Death"
    )
    
  })
  

  # For exporting data frame
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("exported_data_",format(Sys.time(), "%Y%m%d_%H.%M.%S"), ".csv")
    },
    content = function(file) {
      df_out <-  plotme(
        region_type_choice = input$region_type
        , measure_choice = input$measure
        , regions_choice = input$region
        , countries_choice = input$country
        , cohorts_choice = input$cohort
        , variant_choice = input$variant
        , quant_low = min(as.numeric(input$spread))
        , quant_high = max(as.numeric(input$spread))
        , y_lab_caption = input$measure
        , export_df = T
      )
      write.csv(df_out, file, row.names = FALSE)
    }
  )
  
    
}

shinyApp(ui = ui, server = server)
