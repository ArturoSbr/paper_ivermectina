pacman::p_load(googlesheets4)

read_sheet("https://docs.google.com/spreadsheets/d/1VtXKW1IuCm4qRowlotXnTWZlhLoQYYmEsZp7ERUIeAQ") %>%
write_rds(here::here("02_out", "base_publica.rds"))
