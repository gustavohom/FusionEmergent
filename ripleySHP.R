#---------------------------------------------
# Calculo de Ripley para area Baixio e Plato
#---------------------------------------------

# Mesclando pontos e shp

require(spatstat)
require(sp)
require(maptools)
require(magrittr)
require(dplyr)
require(tools)

diretorio ="E:\\Academico\\Mestrado\\Tese\\ws\\trans\\sistematicos"

inp <- file_path_sans_ext(dir(paste0(diretorio,"\\las\\"),pattern='.las'))  # Arquivos na pasta las 

input <- inp[6]

inpBPT = "t"

# Carreganfo dados

treeList1 <- read.table(paste0(diretorio,"\\csv\\",input,"tree_tile_0001_0001_treelist.csv"), header = T, sep = ",", dec = ".")
treeList2 <- read.table(paste0(diretorio,"\\csv\\",input,"tree_tile_0001_0002_treelist.csv"), header = T, sep = ",", dec = ".")
treeList3 <- read.table(paste0(diretorio,"\\csv\\",input,"tree_tile_0002_0001_treelist.csv"), header = T, sep = ",", dec = ".")
treeList4 <- read.table(paste0(diretorio,"\\csv\\",input,"tree_tile_0002_0002_treelist.csv"), header = T, sep = ",", dec = ".")

emergentTree <- rbind(treeList1, treeList2, treeList3, treeList4)

PPP <- emergentTree

# Filtrando percentil 95

PPP %<>% filter(Height > quantile(PPP$Height,.95))

# Criando poligono com extenções

coordinates(PPP) = ~X+Y

# criando windows apartir de shp

S <- readShapePoly(paste0(diretorio,"/qgis/finalizados/",input,"_",inpBPT,".shp" ))

SP <- as(S, "SpatialPolygons")

W <- as(SP, "owin")

# criando pontos a partir de dados shp

# criando ppp

p <- ppp(PPP$X, PPP$Y, window = W)

plot(p)

# K test e K test envelopado

ke <- envelope(p,Kest, nsim = 100)

beepr::beep(8)

plot(ke, main=" ")

plot(ke,xlim = c(0,50), main = " ")

# plot(ke, xlim = c(1400,1460), main=" ")

