library(sf)
library(ggplot2)
library(gganimate)
library(rnaturalearth)
library(readr)
library(randomcoloR)


# get basemap data
bg <- ne_countries(scale = "large", continent = 'north america', returnclass = "sf")

#read flight paths
input_files <- list.files(path = "paths",
              pattern = "\\.csv$",
              full.names = TRUE)

df <- readr::read_csv(input_files, id = "file_name")

#set up colors
colors <- c()
colors[input_files] <- distinctColorPalette(length(input_files))


#format dates as dates
df$`GPS_YYYY-MM-DD_HH:MM:SS` <- as.POSIXct(strptime(df$`GPS_YYYY-MM-DD_HH:MM:SS`, format="%m/%d/%y %H:%M"))


#remove uncollected data
#df[df$lon == 0 & df$lat == 0, c("lon", "lat")] <- NA #replace with NA
df <- df[df$lon != 0,] #remove rows with uncollected data

#plot
p = ggplot()+
  
  # basemap
  geom_sf(data = bg)+
  coord_sf(xlim = range(df$lon, na.rm = TRUE), 
           ylim = range(df$lat, na.rm = TRUE))+
  
  # lines and points
  geom_path(data = df, 
            aes(x=lon,y=lat,group=file_name), 
            alpha = 0.4, size=0.3, color = (function(x) colors[x])(df$file_name))+
  geom_point(data = df, 
             aes(x=lon,y=lat,group=file_name),
             alpha = 0.6, shape=20, size = 2, color = (function(x) colors[x])(df$file_name),
             na.rm = TRUE)+
  
  # formatting
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL)+
  theme_dark()+
  theme(panel.grid = element_blank())



# animate
anim = p + 
  transition_reveal(along = df$'GPS_YYYY-MM-DD_HH:MM:SS', keep_last=FALSE)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")



#1144 is the number of days between the start and the end date
animate(anim, nframes = 1144, fps = 60, width=1000, height=1000, res=280)
anim_save("gull_flight_paths.mp4")

