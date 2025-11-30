# 1. INSTALLATION ET CHARGEMENT DES LIBRAIRIES
libs <- c("terra", "rayshader", "sf", "ggplot2", "png", "rayrender", "rgl")

installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == FALSE)) {
  install.packages(libs[!installed_libraries])
}
invisible(lapply(libs, library, character.only = TRUE))

# 2. CHARGEMENT DES DONNÉES LIDAR
lidar_path <- "~/Desktop/rstudio/lidar_crop_notriangle.tif"  # Remplace par ton chemin
lidar_rast <- terra::rast(lidar_path)

# Vérification et reprojection si nécessaire
if (terra::crs(lidar_rast) != "EPSG:2154") {
  lidar_rast <- terra::project(lidar_rast, "EPSG:2154")
}

# 3. REMPLISSAGE DES TROUS (NoData) DANS LE DSM
print("Correction des NoData en cours...")
lidar_rast_filled <- terra::focal(lidar_rast, w=matrix(1,5,5), fun=mean, na.rm=TRUE)
print("Correction terminée, raster en mémoire.")

# 3.1 AFFICHAGE DU RASTER CORRIGÉ
plot(lidar_rast_filled, main="Raster DSM corrigé (NoData remplis)")

# 4. CHARGEMENT DE L'ORTHOPHOTO (IMAGE AÉRIENNE)
ortho_path <- "~/Desktop/rstudio/ortho_crop.tif"
ortho_rast <- terra::rast(ortho_path)

# Vérification de la projection et reprojection si nécessaire
if (terra::crs(ortho_rast) != "EPSG:2154") {
  ortho_rast <- terra::project(ortho_rast, "EPSG:2154")
}

# 5. ALIGNEMENT ET RECADRAGE DE L'ORTHOPHOTO SUR LE LIDAR
ortho_crop <- terra::crop(ortho_rast, lidar_rast_filled)
ortho_resampled <- terra::resample(ortho_crop, lidar_rast_filled, method = "bilinear")

# Sauvegarde de l'image aérienne comme PNG
terra::writeRaster(ortho_resampled, "ortho_texture.png", overwrite = TRUE)

# Charger l'image pour rayshader
ortho_texture <- png::readPNG("ortho_texture.png")

# 6. CONVERSION DU RASTER LIDAR EN MATRICE POUR RAYSHADER
lidar_mat <- rayshader::raster_to_matrix(lidar_rast_filled)

# 7. CALCUL DE L'OMBRAGE
raymat <- rayshader::ray_shade(lidar_mat, sunangle = 45, zscale = 1)
ambmat <- rayshader::ambient_shade(lidar_mat, zscale = 1)

# Appliquer l’ombrage sur l'orthophoto
ortho_texture <- rayshader::add_shadow(ortho_texture, raymat, 0.5)
ortho_texture <- rayshader::add_shadow(ortho_texture, ambmat, 0.5)

# 8. VISUALISATION 3D AVEC L'IMAGE AÉRIENNE & EFFET CYLINDRE
clear3d()  # Nettoyer la scène RGL

lidar_mat |>
  rayshader::height_shade() |>
  rayshader::add_overlay(ortho_texture, alphalayer = 1) |>
  rayshader::plot_3d(
    heightmap  = lidar_mat,
    zscale     = 1,         
    baseshape  = "circle",  # Socle cylindrique
    solid      = TRUE, 
    solidcolor = "grey20",
    water      = TRUE,
    windowsize = c(1200, 1200), 
    phi        = 30,           
    theta      = 100,        
    zoom       = 0.7          
  )

# 9. AJOUT DU DÔME DE VERRE (RAYRENDER)
radius <- min(nrow(lidar_mat), ncol(lidar_mat)) / 2  # Rayon basé sur la taille du MNS

# Créer une sphère transparente (dôme)
scene_elements <- rayrender::sphere(
  x        = 0,
  y        = 0,
  z        = 0,
  radius   = radius,
  material = rayrender::dielectric()
)

# Ajouter une seconde sphère pour simuler l'épaisseur du verre
scene_elements <- dplyr::bind_rows(
  scene_elements,
  rayrender::sphere(
    x        = 0,
    y        = 0,
    z        = 0,
    radius   = radius * 0.995,  # Légèrement plus petit
    material = rayrender::dielectric(priority = 0, refraction = 1)
  )
)

# 10. RENDU HAUTE QUALITÉ (PATH-TRACING)
print("Lancement du rendu haute qualité...")

rayshader::render_highquality(
  lightdirection = c(315, 45),
  lightintensity = 500,
  clamp_value    = 10,
  sample_method = 'sobol',
  samples        = 2000,
  scene_elements = scene_elements  # Ajout du dôme de verre
)

# 11. EXPORT D'UNE IMAGE 3D
rayshader::render_snapshot("lidar_3d_with_ortho.png")

# 12. NETTOYAGE DE LA MÉMOIRE
print("Nettoyage de la mémoire...")
rm(lidar_rast_filled)
