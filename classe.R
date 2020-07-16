#-----------------------------------
# Objeto da Classe Tree
#-----------------------------------

# --- Carregand pacotes ---

require(R6)

require(sp)
require(rgdal)
require(rgeos)
require(grDevices)
require(maptools)

require(magrittr)
require(dplyr)
require(spatstat)
require(tools)
require(stringr)

require(beepr)

# --- Criando a Classe ---

las = R6Class("las",
              public= list(
                diretorio = NULL,
                input = NULL,
                
                initialize = function(
                  diretorio = NA,
                  input = NA
                  
                ){
                  self$diretorio = diretorio
                  self$input = input
                  
                },
                
                # Funcao criar pasta
                
                pasta = function(){
                  folder <- c('./fd','./gf','./dtm','./tiles',
                              './norm', './csv', './emergent')
                  
                  setwd(self$diretorio)
                  
                  for ( J in 1:length(folder)) {
                    
                    dir.create(folder[J])
                  }
                  
                  beep(1)
                  },
                
                # Funcao descompactar arquivos laz
                
                descompact = function(){
                  
                  setwd(diretorio)
                  if(file.exists(".\\las") == FALSE){
                    dir.create('./las')}
                  
                  shell(paste0("C:\\FUSION\\LasZip -i ",self$diretorio,"\\laz\\",
                               self$input,".laz -odir ", self$diretorio,"\\las"))
                  beep(1)
                  
                },
                
                # Funcao crair uma panilha com altura e coordenada das Arvores emergentes
                
                fusion = function(){ 
                  
                  shell(paste0("C:\\FUSION\\FilterData outlier 4.5 30 ",self$diretorio,"\\fd\\",
                               self$input,".las ",self$diretorio,"\\las\\",self$input,".las"))
                  beep(1)
                  shell(paste0("C:\\FUSION\\GroundFilter ",self$diretorio,"\\gf\\",
                               self$input,"_gnd.las 8 ", self$diretorio,"\\fd\\",
                               self$input,".las"))
                  beep(1)
                  shell(paste0("C:\\FUSION\\GridSurfaceCreate ",self$diretorio,"\\dtm\\",
                               self$input,"_dtm.dtm 1 m m 1 0 0 0 ", self$diretorio,"\\gf\\",
                               self$input,"_gnd.las"))
                  beep(1)
                  
                  shell(paste0("C:\\FUSION\\DTM2TIF ",self$diretorio,"\\dtm\\", self$input,"_dtm.dtm"))
                  
                  beep(1)
                  
                  shell(paste0("C:\\FUSION\\CanopyModel /ground:",self$diretorio,"\\dtm\\",
                               self$input,"_dtm.dtm /ascii ", self$diretorio,"\\dtm\\",
                               self$input,"chm.dtm 1 m m 1 0 0 0 ",self$diretorio,"\\fd\\",
                               self$input,".las"))
                  beep(1)
                  shell(paste0("C:\\FUSION\\SplitDTM ",self$diretorio,"\\dtm\\",
                               self$input,"chm.dtm ", self$diretorio,"\\tiles\\",
                               self$input,"chm_tile.dtm 2 4"))

                  for(M in 1:4){
                    
                    shell(paste0("call C:\\FUSION\\CanopyMaxima /threshold:30 /wse:20,0,0,0 /shape ", 
                                 self$diretorio,"\\tiles\\",
                                 self$input,"chm_tile_0001_000",M,".dtm ",self$diretorio,"\\csv\\",self$input,"tree_tile_0001_000",M,".csv"))
                    
                  }
                  
                  for(M in 1:4){
                    
                    shell(paste0("call C:\\FUSION\\CanopyMaxima /threshold:30 /wse:20,0,0,0 /shape ", 
                                 self$diretorio,"\\tiles\\",
                                 self$input,"chm_tile_0002_000",M,".dtm ",self$diretorio,"\\csv\\",self$input,"tree_tile_0002_000",M,".csv"))
                    
                  }
               
                  beep(8) },
                
                # Funcao obter planilha com arvores emergentes e salva-la em formato csv
                
                emergent = function(){
                  
                  treeList1 <<- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0001_0001_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList2 <<- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0001_0002_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList3 <<- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0002_0001_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList4 <<- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0002_0002_treelist.csv"), header = T, sep = ",", dec = ".")
                  emergentTree <<- rbind(treeList1, treeList2, treeList3, treeList4)
                  
                  emergentTree %<>% filter(Height > quantile(emergentTree$Height,.95))
                  
                  if(file.exists(paste0(self$diretorio,"\\emergent\\",self$input,".csv")) == TRUE){
                    file.remove(paste0(self$diretorio,"\\emergent\\",self$input,".csv"))
                  }
                  
                  write.table(emergentTree, file = paste0(self$diretorio,"\\emergent\\",self$input,".csv"), sep = ',', dec = '.')
                  
                  beep(1)
                },
                
                # Funcao processar K de Ripley
                
                ripley = function(){
                  
                  #---------------------------------------------
                  #         Importando dados
                  #---------------------------------------------
                  
                  treeList1 <- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0001_0001_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList2 <- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0001_0002_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList3 <- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0002_0001_treelist.csv"), header = T, sep = ",", dec = ".")
                  treeList4 <- read.table(paste0(self$diretorio,"\\csv\\",self$input,"tree_tile_0002_0002_treelist.csv"), header = T, sep = ",", dec = ".")
                  emergentTree <- rbind(treeList1, treeList2, treeList3, treeList4)
                  emergentTreePoly <<- emergentTree
                  emergentTree %<>% filter(Height > quantile(emergentTree$Height,.95))
              
                  #----------------------------------------------
                  #           Processando K de Ripley
                  #----------------------------------------------
                  
                  plot(data = emergentTree, X~Y)
                  
                  coordinates(emergentTree) = ~X+Y
                  coordinates(emergentTreePoly) = ~X+Y
                  
                  plot(emergentTreePoly)
                  plot(emergentTree)
                 
                  
                  # Click poly automatizado 
                  
                  dat = data.frame(X = emergentTreePoly$X, Y= emergentTreePoly$Y)
                  con.hull.pos <- chull(dat)
                  con.hull <- rbind(dat[con.hull.pos,],dat[con.hull.pos[1],])
                  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(con.hull)), ID=1)))
                  sp_poly = gBuffer(sp_poly,width = 4, byid = T)
                  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
                  
                  plot(sp_poly_df, add = T)
                  
                  if(file.exists(paste0(self$diretorio,'/retEnv/',self$input,'_retEnv.dbf')) == TRUE){
                    
                    file.remove(paste0(self$diretorio,'/retEnv/',self$input,'_retEnv.dbf'))
                    file.remove(paste0(self$diretorio,'/retEnv/',self$input,'_retEnv.shp'))
                    file.remove(paste0(self$diretorio,'/retEnv/',self$input,'_retEnv.shx'))}
                  
                  writeOGR(sp_poly_df, paste0(self$diretorio,"\\retEnv"), layer=paste0(self$input,"_retEnv"), driver="ESRI Shapefile")
                  S <- readShapePoly(paste0(self$diretorio,"\\retEnv\\", self$input,"_retEnv.shp"))
                  SP <- as(S, "SpatialPolygons")
                  W <- as(SP, "owin")
                  plot(dat)
                  plot(W, add = T)
                  
                  # criando objeto do tipo point patern
                  
                  p <<- ppp(emergentTree$X, emergentTree$Y, window = W)
                  plot(p)
                  
                  # Criando função k de ripley e envelope
                  
                  ke <<- envelope(p,Kest, nsim = 500)
                  
                  plot(ke, main = self$input)

                  # Plotando a funcao L
                  
                  le <<- envelope(p,Lest, nsim = 500)
                  
                  plot(le, main =  self$input)
                  
                  plot(le, . - r ~ r, xlab="d", ylab="L(d)", main =  self$input)
                  
                  beep(8)
                  
                }
                
                
              )
)

# Observacoes:
#   
#   Fusion deve ser instalado na pasta c:/
#   LasZip.exe deve estar na pasta c:/fusion
#   Deve-se apagar os arquivos da pasta retEnv pois sem isso nao vai
