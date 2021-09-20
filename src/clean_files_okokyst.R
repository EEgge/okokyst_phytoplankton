# Taxonomy
tax <- readxl::read_xlsx(here("data", "Okokyst_taxonomy_full_2017-2020_SG_EEG.xlsx"))
newtax <- tax %>% mutate(across(.cols = Classification, list(~ifelse(Class == "Bacillariophyta", "Diatoms", ifelse(Classification == "NA", NA, ifelse(Division == "Haptophyta" | Class == "Prymnesiophyceae", "Haptophyta",.))))))
totaldummy <- c(NA, NA, "total", "Total", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Total")
newtax <- rbind.data.frame(newtax, totaldummy)

write_delim(newtax, here("data", "okokyst_taxonomy.txt"), delim = "\t")

# Cellcounts table 
cellcounts <- read_xlsx(here("data", "Okokyst_cellcounts.xlsx"))


sumcounts <- cellcounts %>% dplyr::select(-Sample) %>% colSums()
length(which(sumcounts < 100))

stations <- read_xlsx(here("data", "Copy of OKOKYST_Hydrografi_Stasjoner_v5.xlsx"), sheet = "KML")

# names(cellcounts)
metadat <- read_xlsx(here("data", "Okokyst_metadata.xlsx"), col_types = c("text", "text", "date", "text", "text",
                                                                          "numeric", "text", "text", "text", rep("numeric", 11))) %>%
  mutate(sqrtKLFA = sqrt(KLFA),
         `sqrtN-NH4` = sqrt(`N-NH4`),
         `sqrtN-SNOX` = sqrt(`N-SNOX`),
         `sqrtN-TOT` = sqrt(`N-TOT`),
         `sqrtP-PO4` = sqrt(`P-PO4`),
         `sqrtP-TOT` = sqrt(`P-TOT`),
         `sqrtSI-SIO2` = sqrt(`SI-SIO2`),
         sqrtTEMP = sqrt(TEMP))

metadat2 <- left_join(metadat, stations, by = c("Station_code" = "StationCode")) %>% 
  dplyr::arrange(., by = Latitude.y)

### not working
# metadat <- read_delim(here("data", "Okokyst_metadata.txt"), col_names = T, locale = locale(decimal_mark = "."),
#                       delim = "\t", col_types = cols(Tid_provetak = col_datetime("%dd.%mm.%yyyy %H:%M"))) %>%
#   mutate(sqrtKLFA = sqrt(KLFA),
#          `sqrtN-NH4` = sqrt(`N-NH4`),
#          `sqrtN-SNOX` = sqrt(`N-SNOX`),
#          `sqrtN-TOT` = sqrt(`N-TOT`),
#          `sqrtP-PO4` = sqrt(`P-PO4`),
#          `sqrtP-TOT` = sqrt(`P-TOT`),
#          `sqrtSI-SIO2` = sqrt(`SI-SIO2`),
#          sqrtTEMP = sqrt(TEMP))
