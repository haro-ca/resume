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
                "- Development of a **supervised model** for identification of tax evasion schemes using **cloud architecture**",
                "- Development and orchestration of a **bayesian model** for prediction of aggregate employment using tax receipts on **cloud architecture**",
                "- Development and deployment of a **supervised model** for classifying tax debt",
                "- Designed and taught three courses for the institution's **staff training**: intro to R, basics of EDA, building DS pipelines with Make",
                "- Designed a **pipeline for automatic** generation of frequent data visualization **reports**"
            ), 
            skills = c(
                "Spark (pySpark, koalas, SQL), python (scikit, pandas)",
                "R (tidyverse, STAN), Python (scikit-learn, pandas), Spark (SQL)",
                "R (tidyverse, ggplot2), GNU Make",
                "Rmarkdown (xaringan for slides)",
                "R (tidyverse, visnetwork, ggraph)"
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
                "- Developed a **statistical** model for identifying anti-competitive practices between the mid-stream natural gas providers"
            ), 
            skills = c(
                "base R"
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







