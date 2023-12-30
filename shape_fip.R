require(spdep);require(sf);require(patchwork); require(tidyverse)
library(janitor)
or.shp <- st_read("~/gis/CountyToFips/or_shp_fips.shp") 
#MAP COLORS (not sure of the palette but I'm using 6 gradients here plus 'gray' for NA)
myPal <- c('#D9D9D9','#EEBCBC', '#E59696', '#DC7171', '#D24B4B', '#C92626', '#C00000')
# insert map file here
# this is a .csv from the output of the 
# ~\72_Cannabis_C1_F23\D_CleanData\72_Cannabis_C1_F23_Reporting_[date].xlsx file that
# comes from the output of the STATA files that Maryan produces

# you can always import thus from the import wizard

maps <- read_csv("~/Adhoc/qualtrix/maps.csv", 
                 col_types = cols(zip = col_character()))
maps <- maps %>% filter(state == "OR")
## county is included in maps, so just count the totals and then join to the
## or.shp shapefile

#zip <- read_csv("~/gis/CountyToFips/HUD_zip_to_fip_crosswalk_2023_ORonly.csv", 
#                col_types = cols(COUNTY = col_character(), 
#                                 ZIP = col_character()))
#getfips <- str_sub(zip$COUNTY,-2)
#names(getfips)<- zip$ZIP
#x <- maps %>% select(zip) %>% na.omit() %>% pull()
# x <- paste(x, collapse = ",")
# x <- (unlist(strsplit(x,",")))
# x <-str_trim(x,"both")
#xx <- tibble("fip"=unname(getfips[x]) )
xx <- maps %>% select(county)
xx <- xx %>% tabyl(county) %>% select(- percent)
or1 <- left_join(or.shp,xx, by="county")

#build colors for the text labels
or1$build <- or1 %>% select(county,n) %>% mutate(
    build = case_when(
    county  == 'Multnomah' & n>0 ~ "#C00000",
    county  != 'Multnomah' & n>0 ~ '#FFFFFF',
    .default = '#D9D9D9'
  )) %>% pull(build)

# get colors for counties
or1 <- or1 %>% mutate(quantile = ntile(n, 6))
nums <- ifelse(is.na(or1$quantile),1,or1$quantile+1)
or1 <- or1 %>%
  mutate(label_above = ifelse(build=="#C00000", n,"" ),
         label_below = ifelse(build=="#FFFFFF",n,""))

ggplot(or1)+
  # geom_sf(data=or1,aes(fill = ifelse(n>0,n,0)),lwd=.1,colour='black') +
  geom_sf(data=or1,aes(),fill=myPal[nums],lwd=.3,colour='white',
          show.legend = F) +    

  geom_sf_text(data=or1,
               aes(label=label_above, col=build),
               vjust= -.6,hjust=-.8,fontface = "bold",
               stat = "sf_coordinates",show.legend = F
  )+
  geom_sf_text(data=or1,
               aes(label=label_below, col=build),
               fontface = "bold",
               stat = "sf_coordinates",show.legend = F
  )+
  scale_color_identity()+
  coord_sf(expand = FALSE) +
  theme_void()

 ggsave("plot.png", dpi = 300)

