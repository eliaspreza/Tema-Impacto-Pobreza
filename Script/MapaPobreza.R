

#--------------------------------------------------------------------------
#=======================Impacto en pobreza por Remesas
#=======================Mapas
#---------------------------------------------------------------------------


#------------Librerias

library(leaflet)
library(sp)
library(cartography)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(RColorBrewer)
library(tmap)
library(rmapshaper)
library(maps)

setwd("F:/CONSULTORIAS/IdeaData/Temas Investigacion/Tema Impacto Pobreza/Shape")

mapaUsa <- readOGR("MapaUsa.shp",layer = "MapaUsa")

mapaUsa <-st_read("MapaUsa.shp")



mapaUsa<-mapaUsa%>%
  dplyr::filter(NAME_1!="Hawaii")%>%
  dplyr::group_by(NAME_1)%>%
  dplyr::summarise(sum(Base_2conf))



dplyr::glimpse(mapaUsa)
mapaUsa
df<-mapaUsa

tm_shape(mapaUsa)+tm_borders(col = NA, lwd = 1, lty = "solid")+
  tm_scale_bar(breaks = NULL, width = NA, size = 0.7,text.color = NA, color.dark = "black", color.light = "white",lwd = 1, position = "LEFT", just = NA)+
  tm_compass(north = 0, type = NA, fontsize = 0.8, size = NA,position=c("left", "top"))+
  tm_shape(mapaUsa)+tm_fill(col = "Base_2conf",palette = "BrBG", style = "pretty", title = "Clusters",id="NAME_2")+
  tm_shape(mapaUsa)+tm_fill(col = "CODDEP", lwd = 1, lty = "solid",palette = "#1F6EB3",legend.show = FALSE)


tm_shape(mapaUsa)+tm_borders(col = NA, lwd = 1, lty = "solid")+
  #tm_scale_bar(breaks = NULL, width = NA, size = 0.7,text.color = NA, color.dark = "black", color.light = "white",lwd = 1, position = "LEFT", just = NA)+
  #tm_compass(north = 0, type = NA, fontsize = 0.8, size = NA,position=c("left", "top"))+
  tm_shape(mapaUsa)+tm_fill(col = "Base_2conf",palette = "Spectral", style = "quantile", title = "Clusters",id="NAME_2")#+
#tm_shape(mapaUsa)+tm_fill(col = "CODDEP", lwd = 1, lty = "solid",palette = "#1F6EB3",legend.show = FALSE)

choroLayer(x = mapaUsa, var = "Base_2conf",
           method = "quantile", nclass = 8)


plot(st_geometry(mapaUsa))
propSymbolsLayer(x = mapaUsa, var = "Base_2conf",
                 legend.title.txt = "Population",
                 col = "#a7dfb4")

#-------------
#-------------------------------------------------------------






url<- url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-25-2020.csv")

baseCod<-read.csv(url)

View(corona)
colnames(baseCod)


corona<-baseCod%>%
  dplyr::rename(NAME=Admin2)%>%
  dplyr::group_by(NAME,Country_Region)%>%
  dplyr::summarise(Confirmados=sum(Confirmed))%>%
  dplyr::select(NAME,Confirmados,Country_Region)%>%
  dplyr::filter(Country_Region=="US")




#----ok

library(plotly)
df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-03-2020.csv')

# geo styling
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

fig <- plot_geo(df, lat = ~Lat, lon = ~Long_)
fig <- fig %>% add_markers(
  text = ~paste(Province_State, Country_Region, paste("Confirmados:", Confirmed), sep = "<br />"),
  color = ~Confirmed, symbol = I("scuare"), size = I(5), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Casos Confirmados Coronavirusts<br />Abril 2020")
fig <- fig %>% layout(
  title = 'Número de Casos de Covid 2019 <br />(Por Estado)', geo = g
)

fig



#----------------Ok grafica 2
library(rjson)
library(plotly)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- read.csv(url2, colClasses=c(fips="character"))
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=counties,
  locations=df$fips,
  z=df$unemp,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= -95.71, lat=37.09))
)
fig



url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
#url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
df <- mapaUsa
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=counties,
  locations=df$NAME_2,
  z=df$Base_2conf,
  colorscale="Viridis",
  zmin=0,
  zmax=12,
  marker=list(line=list(
    width=0),
    opacity=0.5
  )
)
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =2,
    center=list(lon= -95.71, lat=37.09))
)

fig

