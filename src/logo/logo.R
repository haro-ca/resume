# Libraries ====
options(tidyverse.quiet = T)
library(tidyverse)
library(ggraph)
suppressMessages(library(tidygraph))
suppressMessages(library(igraph))

# Graphic parameters ====
# ├ Background color ----
bg_color <- "#cecece"

# └─ Color palette ----
blue <- '#545e75'
green <- '#3f826d'
red <- '#a31000'
skin <- '#f2a746'
grey <- '#adadad'
yellow <- '#ddd26a'
color_pal <- c(blue, green, red, yellow, skin)

# Letter C ====
# ├─ Base grid ----
n = 35 # number of nodes in the graph
grid = tibble(from = seq(1, n, by = 1)) %>% 
    # Adds the endpoint of the edges
    mutate(
        to = case_when(
            from < n*0.2 ~ list(c(seq(n*0.8, n, by = 1))), 
            from > n*0.8 ~ list(c(seq(1, n * 0.05, by = 1))), 
            TRUE ~ list(from)
        )
    ) %>% 
    unnest(to) %>% 
    # Adds and alpha variable for cotrolling it manually
    mutate(
        alpha = case_when(
            from < n*0.2 ~ 'normal', 
            from > n*0.8 ~ 'normal', 
            TRUE ~ 'transparent'
        )
    ) %>% 
    # Controls the color of the edges
    group_by(from) %>% 
    mutate(color_assign_vec = case_when(
        from == 1 ~ 2, 
        from == 2 ~ 1,
        from == 3 ~ 5,
        from == 4 ~ 4,
        from == 5 ~ 2,
        from == 6 ~ 3, 
        from == 29 ~ 1, 
        from == 30 ~ 3, 
        from == 31 ~ 2, 
        from == 32 ~ 3, 
        from == 33 ~ 5, 
        from == 34 ~ 1, 
        from == 35 ~ 2, 
        TRUE ~ sample(seq(1, 5, by = 1), 1, 
                      replace = T, 
                      prob = rep(1/5, 5))
    )) %>%
    ungroup() %>% 
    mutate(color_assign_vec = factor(color_assign_vec))

# ├─ Graph initialization ----
routes_igraph <- graph_from_data_frame(d = grid, directed = FALSE)

# └─ Graph ----
letter_c <- ggraph(routes_igraph, layout = "linear") + 
    geom_edge_arc(aes(color = color_assign_vec, alpha = alpha)) + 
    scale_edge_color_manual(values = color_pal) +
    scale_edge_alpha_manual(values = c(1, 0)) +
    scale_x_reverse() +
    scale_y_continuous(expand = expand_scale(c(0, 0))) +
    labs(edge_width = "Letters") +
    theme_graph() +
    theme(plot.background = element_rect(fill = bg_color), 
          legend.position = 'none') +
    coord_flip()

# Letter H ====
# ├─ Edge frame ----
# 7 edges connecting the 8 nodes: 4 on the left, 1 in the middle, 2 on the right
edges <- tribble(
    ~from, ~to, 
    # Left side 
    1, 3,
    3, 2,
    3, 7,
    3, 8,
    
    # Middle
    3, 4, 
    
    # Right
    5, 4, 
    4, 6
)


# ├─ Graph initialization ----
routes_igraph <- graph_from_data_frame(d = edges, directed = TRUE)
layout <- create_layout(routes_igraph, layout = 'drl')

# ├─ Layout modification ----
# Coordinates 
layout$x = case_when(
    # Left side
    layout$name == 1 ~ 1,
    layout$name == 2 ~ 1,
    layout$name == 3 ~ 0.8,
    layout$name == 7 ~ 0.7,
    layout$name == 8 ~ 0.8,
    
    # Right side
    layout$name == 5 ~ 2, 
    layout$name == 6 ~ 2, 
    layout$name == 4 ~ 2
)

layout$y = case_when(
    # Left side
    layout$name == 1 ~ 1,
    layout$name == 2 ~ -1,
    layout$name == 3 ~ 0,
    layout$name == 7 ~ 1,
    layout$name == 8 ~ -1,
    
    # Right side
    layout$name == 5 ~ 1, 
    layout$name == 6 ~ -1, 
    layout$name == 4 ~ 0
)

# Color and transparency
set.seed(17)
layout$color = sample(color_pal, nrow(layout), replace = T)
layout$alpha = sample(c(0.4, 0.6, 0.8), nrow(layout), replace = T)


# └─ Graph ----
letter_h <- ggraph(layout) + 
    geom_edge_link(color = 'black', 
                   width = 1, 
                   alpha = 0.5,
                   n = 3,
                   arrow = grid::arrow(type = 'closed', 
                                       ends = 'last',
                                       angle = 15, 
                                       length = unit(0.1, 'inches'))) + 
    geom_node_point(color = layout$color, size = 8,   alpha = 1) + 
    geom_node_point(fill = layout$color, size = 8,   alpha = 1, 
                    color = 'black', shape = 1) + 
    #ggraph::geom_node_text(aes(label = name), vjust = 2) +
    scale_x_continuous(expand = expand_scale(c(0.1, 0.1))) +
    scale_alpha_continuous(range = c(0.2, 0.8)) +
    theme_graph() + 
    theme(plot.background = element_rect(fill = bg_color), 
          legend.position = 'none')

# Logo ====
# Collapses the two letters into one image and saves it
#gridExtra::grid.arrange(letter_c, letter_h, nrow = 1)
ggsave('logo/ch.png', gridExtra::grid.arrange(letter_c, letter_h, nrow = 1),
       height = 4.5, width = 6)





