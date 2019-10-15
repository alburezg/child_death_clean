# Decide how countries will be groupped in the analysis
# On 20191010, we had decided to group all countries by UN SDG region 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data requirements: 
# un_regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

un_reg <- 
  un_regions %>% 
  # fix text formatting
  mutate_all(.funs = fix_un_countries) %>% 
  # Chose which regions should be used as default
  # Very important as this will shape the final analysis
  # [PREFERRED 20190814]: Use UN SDG regions but remove UAS/NZ and Ocenia (other)
  mutate(default_region = un_sdg_groups)

# Define labels for using in plots later on

# regions_long <- c(
#   "sub-saharan africa"
#   , "northern africa and western asia"
# 
#   , "latin america and the caribbean"
#   , "europe and northern america"
#   
#   , "eastern and south-eastern asia"
#   , "central and southern asia"
#   
#   , "australia_new zealand"
#   , "oceania (excluding australia and new zealand)"
# )
# 
# regions_short <- c(
#   "Sub-Sah Africa"
#   , "N Africa & W Asia"
#   
#   , "LATAM & Caribbean"
#   , "Europe & N America"
#   
#   , "E & SE Asia"
#   , "C & S Asia"
#   
#   , "AUS & NZ"
#   , "Oceania (other)"
# )

regions_long <- c(
  "sub-saharan africa"
  , "eastern and south-eastern asia"
  
  , "northern africa and western asia"
  , "latin america and the caribbean"
  
  , "central and southern asia"
  , "europe and northern america"
  
  , "australia_new zealand"
  , "oceania (excluding australia and new zealand)"
)

regions_short <- c(
  "Sub-Saharan Africa"
  , "East & SE Asia"
  
  , "North Africa & West Asia"
  , "LATAM & Caribbean"
  
  , "Central & South Asia"
  , "Europe & N America"
  
  , "AUS & NZ"
  , "Oceania (other)"
)

# These regions are not included in the plots, but estimates are presented 
# in the Supplementary Materials of the paper
regions_to_remove <- c("oceania (excluding australia and new zealand)", "australia_new zealand")
