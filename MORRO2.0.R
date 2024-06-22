## EL MORRO SOUNDSCAPE
library(raster) #para expotar las superficies
library(geoR) #para trabajar con datos espaciales

#Leer datos
data_maps <- read.csv("C:/Users/LENOVO/Documents/PAISAJE SONORO TESIS/GREEN SOUNDSCAPE/data/data.csv", sep = ",") # SUPONIENDO QUE TU ARCHIVO SE LLAME ASÍ, IGUAL DEBES CAMBIAR LA RUTA
head(data_maps)

#Leer coordenadas
coor <- read.csv("C:/Users/LENOVO/Documents/PAISAJE SONORO TESIS/GREEN SOUNDSCAPE/data/coor.csv", sep = ",") # DEBES AGREGAR LA RUTA DEL CSV CON LAS COORDENADAS DEL MORRO
head(coor)

# Convertir comas a puntos en las columnas $x y $y (SOLO SI ES NECESARIO, DADO QUE R SOLO RECONOCE VALORES NÚMERICOS SI ESTÁN SEPARADOS POR PUNTOS Y NO COMAS. APLICARÍA IGUAL PARA LAS COLUMNAS DE LOS DATOS, AUNQUE GENERALMENTE LOS VALORES SALEN SEPARADOS POR COMAS SOLO EN EL CSV DE LAS COORDENADAS AL SER EXPORTADOS DESDE GIS)
#coor$x <- as.numeric(gsub(",", ".", coor$x))
#coor$y <- as.numeric(gsub(",", ".", coor$y))
#head(coor)

# Cambiar el nombre de la columna "y" y "x" a "POINT_Y" y "POINT_X" /// SEGÚN CÓMO QUEDEN LOS NOMBRES DE TUS DOS COLUMNAS DE COORDENADAS EN TU CSV DE COORDENADAS, PERO EN CUALQUIER CASO DEBEN RENOMBRARSE A "POINT_Y" y "POINT_X" 
#names(coor)[names(coor) == "y"] <- "POINT_Y"
#names(coor)[names(coor) == "x"] <- "POINT_X"
#head(coor)

# Unir los dataframes por columnas
noisedata <- cbind(data_maps, coor)
head(noisedata)

# Espacializar los datos
noisetable=as.geodata(noisedata,coords.col=29:30,data.col=24) # AQUÍ DEBES INDICAR EN COORDS.COL EL NÚMERO DE LAS COLUMNAS DONDE ESTÁN TUS COORDENADAS Y EN DATA.COL EL NÚMERO DE LA COLUMNA DONDE ESTÁ LA VARIABLE QUE VAS A INTERPOLAR
plot(noisetable)

# Nube variograma
summary(dist(noisetable$coords))
nube <- variog(noisetable, option = "cloud")
plot(nube, main="Distancia de influencia por defecto")

# Semivariograma experimental
variograma=variog(noisetable,option = "bin",uvec=seq(0,100,30)) # LOS VALORES EN SEQ VARÍAN SEGÚN TUS DATOS
datos.env=variog.mc.env(noisetable,obj=variograma)
plot(variograma)

# Ajuste del semivariograma
ini.vals <- expand.grid(seq(1,5.0,l=10), seq(0,40.0,l=10))
model_mco_exp=variofit(variograma, ini=ini.vals, cov.model="exponential",fix.nug=TRUE, wei="npair", min="optim")
model_mco_gaus=variofit(variograma, ini=ini.vals, cov.model="gaussian",fix.nug=TRUE, wei="npair", min="optim")
model_mco_spe=variofit(variograma, ini=ini.vals, cov.model="spheric",fix.nug=TRUE, wei="npair", min="optim")
lines(model_mco_exp, lty = 1, lwd = 2, col="brown2") #exponencial
lines(model_mco_gaus, lty = 2, lwd = 2, col="darkgoldenrod2") #gauseano
lines(model_mco_spe, lty = 5, lwd = 2, col="aquamarine3") #esferico

# Predicción espacial Kriging
tabla_grid=expand.grid(Este=seq(1052713.0,1053606.6,l=500), Norte=seq(761822.5,762367.5,l=500)) #ESTA ES LA EXTENSIÓN GEOGRÁFICA SOBRE LA QUE SE CÁLCULA LA SUPERFICIE. EN PRINCIPIO, SE PUEDE OBTENER DESDE GIS.
tabla_ko=krige.conv(noisetable, loc=tabla_grid,
                    krige= krige.control(nugget=0.5,trend.d="cte", 
                                         trend.l="cte",cov.pars=c(sigmasq=1.44, phi=8.89)))
image(tabla_ko, main="kriging Predict", xlab="Oeste", ylab="Norte")
contour(tabla_ko,main="kriging Predict", add=TRUE, drawlabels=TRUE)

image(tabla_ko, main="kriging StDv Predicted",val=sqrt(tabla_ko$krige.var), xlab="West", ylab="North")
contour(tabla_ko,main="kriging StDv Predict",val=sqrt(tabla_ko$krige.var), add=TRUE, drawlabels=TRUE)

map_predict=rasterFromXYZ(cbind(tabla_grid,tabla_ko$predict))
map_error=rasterFromXYZ(cbind(tabla_grid,tabla_ko$krige.var))

writeRaster(map_predict,"C:/Users/LENOVO/Documents/PAISAJE SONORO TESIS/GREEN SOUNDSCAPE/results/arm_domi.tif") # AQUÍ LA RUTA DE SÁLIDA DEL TIFF CON LA SUPERFICIE DE INTERPOLACIÓN
