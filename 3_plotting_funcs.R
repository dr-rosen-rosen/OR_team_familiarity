################################################################
################################################################
######  Functions for plotting
################################################################
################################################################


library(ggplot2)
library(patchwork)
library(ggthemes)

get_histogram <- function(df, x, facet){
    # Makes variables usable in ggplot func
    x <- ensym(x)
    facet <- ensym(facet)
    p <- df %>%
        ggplot() +
        aes(x = !!x) +
        geom_histogram(bins = 100, fill = "#0c4c8a") +
        facet_wrap(~ !!facet) + #asa_rating_c
        theme_tufte()
    return(p)
}

get_scatter <- function(df, x, y, facet){
    # Makes variables usable in ggplot func
    x <- ensym(x)
    y <- ensym(y)
    facet <- ensym(facet)
    p <- df %>%
        ggplot() +
        aes(x = !!x, y = !!y) +
        geom_point(size = 1L, colour = "#0c4c8a") +
        geom_smooth() +
        facet_wrap(~ !!facet) + #asa_rating_c
        theme_tufte()
    return(p)
}
