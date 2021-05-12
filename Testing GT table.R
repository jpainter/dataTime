library( tidyverse )
library( gt )
library(paletteer)

# Use `countrypops` to create a gt table;
# Apply a color scale to the `population`
# column with `scales::col_numeric`,
# four supplied colors, and a domain
tab_1 <-
    countrypops %>%
    dplyr::filter(country_name == "Mongolia") %>%
    dplyr::select(-contains("code")) %>%
    tail(10) %>%
    gt() %>%
    data_color(
        columns = 'population',
        colors = scales::col_numeric(
            palette = c(
                "red", "orange", "green", "blue"),
            domain = c(0.2E7, 0.4E7))
    )
tab_1

tab_2 <-
    pizzaplace %>%
    dplyr::filter(
        type %in% c("chicken", "supreme")) %>%
    dplyr::group_by(type, size) %>%
    dplyr::summarize(
        sold = dplyr::n(),
        income = sum(price)
    ) %>%
    gt(rowname_col = "size") %>%
    data_color(
        columns = c('sold', 'income'),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::red_material"
            ) %>% as.character(),
            domain = NULL
        )
    )
tab_2
