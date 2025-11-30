# 1. INSTALLATION ET CHARGEMENT DES PACKAGES
#-------------------------------------------
libs <- c("terra", "geodata", "sf", "tidyverse", "rayshader", "elevatr", "rayrender")

installed_libraries <- libs %in% rownames(installed.packages())

if(any(installed_libraries == FALSE)){
  install.packages(libs[!installed_libraries], dependencies = TRUE)
}

invisible(lapply(libs, library, character.only = TRUE))

# 2. TÉLÉCHARGEMENT DES FRONTIÈRES DE LA FRANCE (EPSG:2154)
#----------------------------------------------------------
country_sf <- geodata::gadm(
  country = "FRA",  
  level = 0,        
  path = getwd()
) |> sf::st_as_sf()

# Reprojection en Lambert-93 (EPSG:2154)
country_sf <- sf::st_transform(country_sf, crs = 2154)

# 3. RÉCUPÉRATION DU MNT
#-----------------------
elev <- elevatr::get_elev_raster(
  locations = country_sf,  
  z = 5,  
  clip = "locations"
)

# Projection du MNT en EPSG:2154
elev_lambert <- elev |> 
  terra::rast() |> 
  terra::project("EPSG:2154")

# Vérifier l’extension du MNT
print(st_bbox(elev_lambert))

# Conversion en matrice pour Rayshader
elmat <- elev_lambert |> rayshader::raster_to_matrix()

# Correction des valeurs négatives et NA
elmat[elmat < 0] <- 0  
elmat[is.na(elmat)] <- min(elmat, na.rm = TRUE)  

# 4. CHARGEMENT DES PHARES (Depuis GeoJSON)
#------------------------------------------
lighthouse_sf <- sf::st_read("/Users/dawa/Downloads/export.geojson")

# Reprojection en Lambert-93 (EPSG:2154)
lighthouse_sf <- sf::st_transform(lighthouse_sf, crs = 2154)

# 5. IDENTIFIER LES PHARES HORS DES LIMITES DU MNT
#-------------------------------------------------
bbox_mnt <- st_bbox(elev_lambert)
bbox_phares <- st_bbox(lighthouse_sf)

# Filtrer les phares hors des limites
phares_hors_limites <- lighthouse_sf %>%
  dplyr::filter(
    st_coordinates(.)[,1] < bbox_mnt$xmin | st_coordinates(.)[,1] > bbox_mnt$xmax |
      st_coordinates(.)[,2] < bbox_mnt$ymin | st_coordinates(.)[,2] > bbox_mnt$ymax
  )

# Afficher les phares hors limites
if (nrow(phares_hors_limites) > 0) {
  message("⚠️ Attention : ", nrow(phares_hors_limites), " phares sont hors des limites du MNT.")
  print(phares_hors_limites)
} else {
  message("✅ Tous les phares sont bien dans les limites du MNT.")
}

# 6. AFFICHAGE DES PHARES SUR UNE CARTE POUR VÉRIFICATION
#--------------------------------------------------------
plot(elev_lambert, main = "Vérification des phares")
points(st_coordinates(lighthouse_sf), col = "blue", pch = 20)  # Tous les phares
points(st_coordinates(phares_hors_limites), col = "red", pch = 20, cex = 2)  # Phares hors limites en rouge

# 7. SUPPRIMER LES PHARES HORS DES LIMITES AVANT AFFICHAGE 3D
#------------------------------------------------------------
lighthouse_sf <- lighthouse_sf %>%
  dplyr::filter(
    st_coordinates(.)[,1] >= bbox_mnt$xmin & st_coordinates(.)[,1] <= bbox_mnt$xmax &
      st_coordinates(.)[,2] >= bbox_mnt$ymin & st_coordinates(.)[,2] <= bbox_mnt$ymax
  )

# Vérifier après suppression
print(st_bbox(lighthouse_sf))

# Extraction des coordonnées des phares
coords <- sf::st_coordinates(lighthouse_sf)
long <- coords[, "X"]
lat <- coords[, "Y"]

# 🔄 Convertir les coordonnées des phares en indices de la matrice Rayshader
xy_matrix <- terra::xyFromCell(elev_lambert, terra::cellFromXY(elev_lambert, cbind(long, lat)))
long_adj <- xy_matrix[,1]
lat_adj <- xy_matrix[,2]

# 8. AFFICHAGE 3D AVEC RAYSHADER
#-------------------------------
h <- nrow(elmat)
w <- ncol(elmat)

elmat |> 
  rayshader::height_shade(
    texture = colorRampPalette(c("grey90", "grey60"))(128)
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 50,            
    solid = FALSE,            
    shadow = TRUE,            
    shadow_darkness = 1,      
    background = "white",     
    windowsize = c(800, 800), 
    zoom = .65, 
    phi = 85, 
    theta = 0
  )

# 9. AJOUT DES PHARES EN ORBES LUMINEUSES
#---------------------------------------
rayshader::render_points(
  lat = lat_adj,
  long = long_adj,
  extent = elev_lambert,
  heightmap = elmat,
  zscale = 1,
  size = 7,  
  color = "#F59F07"  # Couleur or
)

# 10. AJOUT D'UN EFFET DE LUMIÈRE DIFFUSE COMME LES POWER PLANTS
#---------------------------------------------------------------
rayshader::render_highquality(
  filename = "3d_france_lighthouses.png",  
  preview = TRUE,      
  light = FALSE,       
  point_radius = 3,  # Ajuster la taille des orbes
  point_material = rayrender::light,
  point_material_args = list(
    color = "#F59F07",        # Lumière principale dorée
    intensity = 30,           # Intensité lumineuse
    gradient_color = "#F5D014" # Dégradé vers un jaune plus clair
  ),
  clamp_value = 2, 
  min_variance = 0, 
  sample_method = "sobol",  
  interactive = FALSE, 
  parallel = TRUE
)
