library(vitae)
library(tidyverse)

pubs <- scholar::get_publications("xk9BhHUAAAAJ") %>% 
  dplyr::mutate(author = author %>% 
                  as.character,
                journal = journal %>% 
                  replace(journal %in% "bioRxiv", "BioRxiv") %>%
                  replace(journal %in% "medRxiv", "MedRxiv"), 
                first_author = case_when(stringr::str_starts(author, "A Hamlet|ATP Hamlet") ~ TRUE,
                                         TRUE ~ FALSE),
                preprint = case_when(journal %in% c("bioRxiv", "BioRxiv", "MedRxiv", "medRxiv", "") ~ TRUE,
                                     TRUE ~ FALSE),
                report = case_when(stringr::str_starts(title, "Report") ~ TRUE,
                                   TRUE ~ FALSE)) %>% 
  dplyr::arrange(desc(year), desc(cites))

pubs <- pubs[-which(pubs$cites == 0 & pubs$year < 2021), ]
if(any(duplicated(pubs$title))) pubs <- pubs[-which(duplicated(pubs$title)), ]

pubs %>% 
  dplyr::filter(first_author == T & preprint == F) %>% 
  dplyr::arrange(desc(year), desc(cites)) %>%
  detailed_entries(
    what = title,
    when = as.character(year),
    with = author,
    where = journal, 
    .protect = TRUE
  )
