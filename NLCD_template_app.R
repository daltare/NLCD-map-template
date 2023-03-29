# Template for a Shiny application that contains a map showing 2019 Land Cover 
# from the USGS National Land Cover Database (NLCD)
#
# !! NOTE: The NLCD layer is off by default - it has to be turned on by clicking
# !! the box labeled "Land Cover (2019 NLCD)" in the layers selection pane (in 
# !! the upper right corner of the map)


## To deploy to shinyapps.io, include:
##      - this script 
##      - all of the files in the 'data_processed' folder 
## (no other files need to be published - e.g., don't need to publish the 
## 'data_raw' folder)


# load packages -----------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflegend)
library(leaflet)
library(glue)
library(janitor)
library(here)
library(FedData)

## conflicts ----
library(conflicted)
conflicts_prefer(dplyr::filter)



# setup -------------------------------------------------------------------

## coordinate systems for transformations
projected_crs <- 3310 # see: https://epsg.io/3310 
# other options: 26910 see: https://epsg.io/26910
# resources: 
# https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline
# 
geographic_crs <- 4269 # see: https://epsg.io/4269
# see: https://epsg.io/4326



# load data ----------------------------------------------------------------

## CA boundary ----
ca_boundary <- st_read(here('data_processed', 
                            'ca_boundary.gpkg'))

## NLCD ----
nlcd_legend <- nlcd_colors() %>% 
    filter(!str_detect(string = Description, 
                       pattern = 'Alaska only')) %>% 
    select(Class, Color)



# web services source ------------------------------------------------------
## NLCD (see: https://www.mrlc.gov/data-services-page 
##       AND more generally: https://www.mrlc.gov/data/nlcd-2019-land-cover-conus)
wms_nlcd <- 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?service=WMS&'



# define UI ---------------------------------------------------------------
ui <- fillPage(
    leafletOutput('leaflet_map_render', 
                  height = "100%") %>% 
        # withSpinner(color="#0dc5c1") %>% # not working
        addSpinner(color = '#0dc5c1', 
                   spin = 'double-bounce' # 'fading-circle' 'rotating-plane'
        )
)



# Define server logic -----------------------------------------------------
server <- function(input, output) {
    
    ## create leaflet map ----
    output$leaflet_map_render <- renderLeaflet({
        
        ### create empty map ----
        leaflet_map <- leaflet()
        
        ### set initial zoom ----
        leaflet_map <- leaflet_map %>% 
            setView(lng = -119.5, # CA centroid: -119.5266
                    lat = 37.5, # CA centroid: 37.15246
                    zoom = 6) 
        
        ### add basemap options ----
        basemap_options <- c( # NOTE: use 'providers$' to see more options
            #'Stamen.TonerLite',
            'CartoDB.Positron',
            'Esri.WorldTopoMap', 
            # 'Esri.WorldGrayCanvas',
            'Esri.WorldImagery'#,
            # 'Esri.WorldStreetMap'
        ) 
        
        for (provider in basemap_options) {
            leaflet_map <- leaflet_map %>% 
                addProviderTiles(provider, 
                                 group = provider, 
                                 options = providerTileOptions(noWrap = TRUE))
        }
        
        ### add panes ----
        #### (sets the order in which layers are drawn/stacked -- higher 
        #### numbers appear on top)
        leaflet_map <- leaflet_map %>% 
            addMapPane('nlcd_pane', zIndex = 480) %>% 
            addMapPane('nlcd_ca_pane', zIndex = 490) %>% 
            {.}
        
        ### add NLCD (land cover) ----
        #### (NOTE: the legend is added below via leafletProxy, so that the 
        #### legend doesn't show up when the map is first rendered)
        leaflet_map <- leaflet_map %>%
            addWMSTiles(
                wms_nlcd,
                layers = 'NLCD_2019_Land_Cover_L48',
                options = c(WMSTileOptions(format = 'image/png', transparent = TRUE),
                            pathOptions(pane = 'nlcd_pane')),
                attribution = 'National Land Cover Database 2019',
                group = 'Land Cover (2019 NLCD)') %>%
            # add the CA boundary on top of the NLCD layer
            addPolylines(data = ca_boundary %>% 
                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet)
                         options = pathOptions(pane = 'nlcd_ca_pane'),
                         color = 'black', 
                         weight = 1.0,
                         smoothFactor = 1.0,
                         opacity = 0.7,
                         group = 'Land Cover (2019 NLCD)',
                         label = 'CA Boundary') %>% 
            hideGroup('Land Cover (2019 NLCD)') 
        
        
        ### add layer controls ----
        leaflet_map <- leaflet_map %>%
            addLayersControl(baseGroups = basemap_options,
                             overlayGroups = c(
                                 'Land Cover (2019 NLCD)'
                             ),
                             options = layersControlOptions(collapsed = TRUE,
                                                            autoZIndex = TRUE))
    })
    
    ## add NLCD legend ----
    ### (have to do this with leafletProxy so that the legend doesn't show up 
    ### when the map is first rendered)
    observe({
        if ('Land Cover (2019 NLCD)' %in% input$leaflet_map_render_groups){
            leafletProxy('leaflet_map_render') %>%
                addLegend(position = 'bottomleft',
                          colors = nlcd_legend$Color,
                          labels = nlcd_legend$Class,
                          opacity = 1,
                          layerId = 'nlcd_legend',
                          group = 'Land Cover (2019 NLCD)',
                          title = 'Land Cover Classes (NLCD)')
        }
    })
    
}


# run application  --------------------------------------------------------
shinyApp(ui = ui, server = server)
