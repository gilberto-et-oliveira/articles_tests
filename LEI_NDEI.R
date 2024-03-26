
# Script para reprodução do Landscape Expansion Index (LEI) proposto poor Liu et al (2010)
# e o Normalized Differences Expansion Index (NDEI) proposto por Chakraborty et al (2022).

library(sf)

# previous urban areas of a region. Create id.
previous_urban <- st_read('dados/shapes/reproducao_crescimento_urb/area_urb_anterior.shp')
previous_urban$idPu <- seq.int(nrow(previous_urban)) 

# new urban area of a region. Create id.
new_urban <- st_read('dados/shapes/reproducao_crescimento_urb/crescimento.shp')
new_urban$idNu <- seq.int(nrow(new_urban))

# put the same projection for both shapefiles. Function is better with projections
# with meters units. Prefers to use UTM for small or Albers for large areas 
st_crs(previous_urban)
st_crs(new_urban)

previous_urban <- st_transform(previous_urban, st_crs(new_urban) )

plot(new_urban$geometry)
plot(previous_urban$geometry,add = T)


# calculating LEI 
# a0 = interseção entre a área de influência da nova área urbana e a área urbana pré-existente
# av = interseção entre a área de influência da nova área urbana com áreas não consideradas urbanas pré-existente
# LEI = 100 * a0 / av + a0

buffer <- 100 # the value is decided by who is using the script considering the data and local conditions
bf_new_urban <- st_buffer(new_urban, buffer)
head(bf_new_urban)

a0 <- st_intersection(bf_new_urban, previous_urban)
av <- st_difference(bf_new_urban, previous_urban)

a0$area <- st_area(a0)
av$area <- st_area(av)

head(a0)
head(av)

a0_grouped.temp <- tapply(a0$area, a0$idNu, FUN = 'sum') 
av_grouped.temp <- tapply(av$area, av$idNu, FUN = 'sum') 

a0_grouped <- as.data.frame(a0_grouped.temp)
av_grouped <- as.data.frame(av_grouped.temp)

colnames(a0_grouped) <- 'area_a0'
colnames(av_grouped) <- 'area_av'
dim(a0_grouped)
dim(av_grouped)

# Number of new rows to add
x <- 1

# Create new rows with NA values
new_rows <- data.frame(
  area_a0 = rep(0, x)
)


# Add new rows to the original dataframe
a0_grouped <- rbind(a0_grouped, new_rows)
a0_grouped

a0_grouped$id <- seq.int(nrow(a0_grouped)) 
av_grouped$id <- seq.int(nrow(av_grouped))

# with equalized data.frames containing the ids of new urban areas, 
# calculate the lei 

lei <- 100 * ( a0_grouped$area_a0 / (av_grouped$area_av + a0_grouped$area_a0) )
typeof(lei)
lei <- as.data.frame(lei)
lei$id <- seq.int(nrow(lei)) 



# Normalized Differences Expansion Index (NDEI) proposto por Chakraborty et al (2022).
# NDEI = (infill - (extension+leapfrog)) / (infill + (extension+leapfrog))

new_urban$area <- st_area(new_urban)
infill <- new_urban[new_urban$tipo == 'interno',]
extension <- new_urban[new_urban$tipo == 'extensao',]
leapfrog <- new_urban[new_urban$tipo == 'leap_frog',]


ndei <- ( ( sum(infill$area) ) - (  sum(extension$area) + sum(leapfrog$area)  )  )/
        ( ( sum(infill$area) ) + (  sum(extension$area) + sum(leapfrog$area)  ) )



