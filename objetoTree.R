#-----------------------------------
# Objeto da Classe Tree
#-----------------------------------


#  --- Facilitando a importacao dos dados ---

diretorio ="E:\\Academico\\Mestrado\\Tese\\ws\\trans\\zf2"



# inp <- file_path_sans_ext(dir(paste0(diretorio,"\\laz\\"),pattern='.laz'))   #colocar arquivos na pasta laz
# Criar o lax (colocar na classe)
# input <- inp[6]
# inp015 = las$new(diretorio, input)
# inp015$descompact()


inp <- file_path_sans_ext(dir(paste0(diretorio,"\\las\\"),pattern='.las'))  # Arquivos na pasta las 

input <- inp[1] 


# --- Criando Objeto ---

inp015 = las$new(diretorio, input)

# --- Executando metodos ---


# inp015$pasta()
 
inp015$fusion()

# inp356$emergent()  ja tem nos outros

# inp356$ripley()  Já tem nos outros