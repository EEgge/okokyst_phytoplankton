cellcarbon0 <- read_xlsx(here("data", "okokyst_cellcarbon.xlsx")) 
View(cellcarbon)
cellcarbon1 <- cellcarbon0 %>% tidyr::separate(Tid_provetak, c("Year", "Month", "Day"), "-", remove = FALSE) %>% 
  tidyr::separate(Day, c("Day", "Time"), " ", remove = T) %>% mutate(verdi = as.numeric(Verdi)) %>% 
    mutate(across(Tid_provetak, ~ as.Date(as.character(.), format = '%Y-%m-%d')))

tax <- read_delim(here("data", "okokyst_taxonomy.txt"), delim = "\t")
cellcarbon_tax <- left_join(cellcarbon1, tax, by = c("name")) %>% 
  mutate(doy = lubridate::yday(Tid_provetak)) %>% #Add variable day-of-year
  left_join(., stations, by = c("station_code" = "StationCode")) %>% # Join with station file with Latitude and Longitude
  dplyr::arrange(., by = Latitude)

dim(cellcarbon)
sum(cellcarbon$name %in% tax$name) 
dim(cellcarbon_tax)
# ids in x for which there are multiple values in y - one duplicate in tax:
length(unique(cellcarbon$name))
dim(tax)
length(unique(tax$name))


#How often above bloom levels per year 
#Each taxon
#Each group (taxonomic, taxonomic+size category

bloomlevel <- 0.085 #ygC/L, cf. Lømsland & Johnsen 

level1 <- "Class"
level2 <- "name"

sel_groups <- c("Araphid-pennate", "Pennate", "Raphid-pennate")

#c("Araphid-pennate", "Pennate", "Raphid-pennate")

firstbloom <- cellcarbon_tax %>% mutate(bloom = ifelse(Verdi > bloomlevel, 1, 0)) %>% 
  filter(.data[[level1]] %in% c("Bacillariophyta")) %>% filter(bloom == 1) %>% 
  group_by(.data[[level2]], Year, station_code, StationName_new) %>% summarise_at(vars(doy), min) %>%  # add name when level2 != name
  mutate(Station_code = factor(station_code, levels = unique(cellcarbon_tax$station_code), ordered = T))

test2 %>% group_by(Year, station_code, Division) %>% summarise_at(vars(bloom), sum)
str(test2)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

firstbloom_plt <- ggplot(firstbloom, aes(x = Station_code, y = doy, color = .data[[level2]], shape = Year, 
                       text = sprintf("Taxon: %s<br>Station: %s<br> Doy: %s", .data[[level2]], StationName_new, doy))) +
  geom_point()+
  coord_flip()#+
  #scale_colour_manual(values = cbPalette)
ggplotly(firstbloom_plt, tooltip = "text")
