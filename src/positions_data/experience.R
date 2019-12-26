# Libraries ====
options(tidyverse.quiet = TRUE)
library(tidyverse)

# Experience ====
# ├─ Positions tibble build ----
# │ ├─ Positions description ----
# │ │ ├─ Basic layout ----
# position_var <- tibble(
#   position = '', 
#   institucion = "", 
#   location = '',
#   start = '',
#   end = '',
#   desc = list(
#     tibble(
#       description = c(
#         "- description 1",
#         "- description 2",
#         "- description 3"
#       ), 
#       skills = c(
#         "skills for description 1",
#         "skills for description 2",
#         "skills for description 3"
#       )
#     )
#   )
# )

# │ │ ├─ Sr. Data Scientist ----
sr_ds <- tibble(
    position = 'Sr. Data Scientist', 
    institucion = "Mexico's Central Tax Administration Office  
    (Servicio de Administración Tributaria)", 
    location = 'Mexico City',
    start = 'Jan. 2019',
    end = 'present',
    desc = list(
        tibble(
            description = c(
                "- Development and deployment of a supervised model for classifying tax debt",
                "- Designed and taught three courses for the institution's staff training: introduction to R, basics of exploratory data analysis, building data science pipelines using Makefiles",
                "- Designed a pipeline for automatic generation of frequent data visualization reports",
                "- Perform network analysis to detect tax evasion communities",
                "- Version control of cloropleth maps for the geographical display of taxpayer's data"
            ), 
            skills = c(
                "R (tidyverse, ranger), Python (scikit-learn, pandas), SQL",
                "R (tidyverse, ggplot2), GNU Make",
                "R Markdown (ggplot2, shiny)",
                "R (tidyverse, visnetwork, ggraph)",
                "R (leaflet QGis)"
            )
        )
    )
)

# └ │ ├─ Jr. Data Scientist ----
jr_ds <-  tibble(
    position = 'Jr. Data Scientist | Economic Analyst', 
    institucion = "EnergeA (Energy Sector Consulting Firm)", 
    location = 'Mexico City',
    start = '2018',
    end = NA,
    desc = list(
        tibble(
            description = c(
                "- Developed a statistical model for identifying anti-competitive practices between the mid-stream natural gas providers",
                "- Neighboring gas station's competition analysis for identifying price setting mechanisms.",
                "- Built a PDF scrapping pipeline of 100+ files for ownership analysis of Mexico's natural gas industry."
            ), 
            skills = c(
                "R",
                "R",
                "R (stringr, selenium)"
            )
        )
    )
)

# ├─ Markdown preparation ----
# │ ├─ Positions collapse ----
# Collapses all the positions above into a single frame.
# The environment needs to have only the positions, else, the row bind may not work.
positions_frame <- map(ls(), ~ eval(sym(.x))) %>% bind_rows()

# └ ├─ Cleaning ----
# Adds details to the strings for printing as Markdown, ** for italics, line breaks, etc.
positions_frame <- positions_frame %>% 
    # Adds the encircling "* *" for the skills lists
    mutate(desc = map(desc, ~.x %>% mutate(skills = str_c('*', skills, '*')))) %>% 
    # Adds the encircling "* *" for the skills lists
    mutate(date = if_else(!is.na(end), str_c(start, ' - ', end), start)) %>% 
    # Collapses job description and skillset (unlist needed for the last mutate)
    mutate(desc = map(desc, ~pmap(.x, function(...) str_c(..., sep = ' | ')) %>% unlist(recursive = F))) %>% 
    # Adds the line break at the end.
    mutate(desc = map_chr(desc, ~ str_c(.x, collapse = '\n')))

# Output ====
# Writes out the ready-for-markdown-print frame on a csv
write_tsv(positions_frame, here::here('positions_data', 'experience.csv'))







