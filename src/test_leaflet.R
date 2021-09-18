library(terra)
options(digits = 15)

stations <- read_xlsx(here("data", "Copy of OKOKYST_Hydrografi_Stasjoner_v5.xlsx"), sheet = "KML")
cellcounts <- read_xlsx(here("data", "Okokyst_cellcounts.xlsx"))
names(cellcounts)
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

metadat2 <- left_join(metadat, stations, by = c("Station_code" = "StationCode"))

metadat2$`N-NH4` %>% range(., na.rm = T)
metadat2$`N-NH4`[c(1000:1355)]

#Transform UTM coordinates to lon lat for leaflet
lonlat_utm <- metadat %>% dplyr::select(lngnum, latnum) %>% as.data.frame()

names(lonlat_utm) <- c("lon", "lat")

lonlat_spatvec_utm <- vect(as.data.frame(lonlat_utm), crs="+proj=utm +zone=33 +datum=WGS84 +units=m")

lonlat_spatvec_lonlat <- project(lonlat_spatvec_utm, "+proj=longlat +datum=WGS84")
lonlat_spatvec_lonlat

lonlat <- terra::geom(lonlat_spatvec_lonlat)[, c("x", "y")] %>% 
  as.data.frame() %>% 
  mutate(x = as.character(x), y = as.character(y))
lonlat_uq <- lonlat %>% group_by(x, y) %>% unique() %>% 
  mutate(x = as.numeric(x), y = as.numeric(y))

leaflet(norway) %>% 
  addPolygons() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = stations %>% pull(Longitude), lat = stations %>% pull(Latitude), 
                   color = "red", radius = 5)

#### cellcounts with meta data ####

cellc_pivot <- tidyr::pivot_longer(cellcounts, cols = -Sample)

cellc_meta <- left_join(cellc_pivot, metadat2, by = "Sample")
cellc_meta_total <- cellc_meta %>% group_by(Sample) %>% summarise(Total =sum(value)) %>%
  left_join(., metadat2, by = "Sample") %>% mutate(logTotal = log(Total)) %>% 
  mutate(lat = Latitude.y, long = Longitude.y)

spec12 <- logcellc_meta %>% filter(name %in% c("Acanthoica_quattrospina", "Actiniscus_pentasterias"))

ts <- ggplot(spec12, aes(x = Tid_provetak, y = logVal, color = name))+
  geom_line()+
  facet_grid(rows = vars(Station_code))

tsly <- ggplotly(ts)
tsly

ts2 <- ggplot(spec12, aes(x = Tid_provetak, y = logVal, color = name, 
                         text = sprintf("Taxon: %s", name)))+
  geom_line()+
  facet_grid(rows = vars(Station_code))

tsly2 <- ggplotly(ts2, tooltip = "text")
tsly2

#### Taxonomy
?across()
newtax <- tax %>% mutate(across(.cols = Classification, list(~ifelse(Class == "Bacillariophyta", "Diatoms", ifelse(Classification == "NA", NA, ifelse(Division == "Haptophyta" | Class == "Prymnesiophyceae", "Haptophyta",.))))))

logcellc_meta_tax <- left_join(logcellc_meta, newtax, by = c("name")) %>% 
  dplyr::group_by(Classification_1, Day, Latitude.y, Longitude.y, Station, Tid_provetak, Station_code, Sample) %>% 
  summarise_at(vars(value), sum) %>% 
  mutate(logValuegroup = log(value+1)) %>%
  dplyr::select(-value) %>% 
  pivot_wider(., names_from = Classification_1, values_from = logValuegroup) %>% 
  left_join(., metadat2, by = "Sample")

# Testing grouping for time series
groupbytab <- logcellc_meta_tax %>% group_by(Division, Day, Year, Month, Latitude.y, Longitude.y, Sample, Station, Station_code) %>% summarise_at(vars(value), sum)
selectgroups <- groupbytab %>% filter(Division %in% c("Cercozoa", "Haptophyta")) %>% mutate(logVal = log(value +1))


