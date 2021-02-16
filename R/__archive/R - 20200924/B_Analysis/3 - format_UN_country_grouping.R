# Decide how countries will be groupped in the analysis
# On 20191010, we had decided to group all countries by UN SDG region 


un_reg <- un_regions %>% 
  # fix text formatting
  mutate_all(.funs = fix_un_countries) %>% 
  # Chose which regions should be used as default
  # Very important as this will shape the final analysis
  # [PREFERRED 20190814]: Use UN SDG regions but remove UAS/NZ and Ocenia (other)
  mutate(default_region = un_sdg_groups)

# Define labels for using in plots later on

regions_long <- c(
  "sub-saharan africa"
  , "northern africa and western asia"
  , "central and southern asia"
  , "eastern and south-eastern asia"
  , "latin america and the caribbean"
  , "australia_new zealand"
  , "oceania (excluding australia and new zealand)"
  , "europe and northern america"
)

regions_short <- c(
  # "SS Africa"
  "Sub-Sah Africa"
  , "N Africa & W Asia"
  , "C & S Asia"
  , "E & SE Asia"
  , "LATAM & Caribbean"
  , "AUS & NZ"
  , "Oceania (other)"
  , "Europe & N America"
)
