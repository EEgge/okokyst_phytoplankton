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

spec12 <- cellc_meta %>% filter(name %in% c("Acanthoica_quattrospina", "Actiniscus_pentasterias"))

ggplot(spec12, aes(x = Tid_provetak, y = value, color = name))+
  geom_line()+
  facet_grid(rows = vars(Station_code))
