print_position <- function(tb, position) {
    tb %>% 
        filter(position == {{position}}) %>% 
        glue::glue_data(
            "### {position}", 
            '\n\n',
            '{institucion}', 
            '\n\n',
            '{location}', 
            '\n\n',
            '{date}', 
            '\n\n',
            '{desc}',
            '\n\n\n'
        )
}    