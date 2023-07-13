library(dplyr)
library(tidyr)

who %>% pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"),
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
)

anscombe
anscombe_2 <- anscombe %>%
    pivot_longer(
        everything(),
        cols_vary = "slowest",
        names_to = c(".value", "set"),
        names_pattern = "(.)(.)"
    )

anscombe_2$set %>% table()



library(dplyr)
library(tidyr)

longer <- pivot_longer(
    dat,
    cols = -1,
    names_pattern = "(.*)(..)$",
    names_to = c("limit", "name")
) %>%
    mutate(limit = ifelse(limit == "", "value", limit))

answer <- pivot_wider(longer,
    id_cols = c(group, name),
    names_from = limit,
    values_from = value,
    names_repair = "check_unique"
)
