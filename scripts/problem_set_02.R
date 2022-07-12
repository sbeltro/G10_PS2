################################################################################
# Problem Set 2: Predicting Poverty
# Autores: Natalia Capacho, Yurani Gonzalez, Sebastian Beltran
# Big Data - MECA 
################################################################################

# Limpiar el espacio de trabajo ----
rm(list=ls())

# Instalar Paquetes y cargar librerias ----
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(rio,
       tidyverse,
       skimr,
       dplyr,
       leaps,
       ROCR,
       glmnet,
       tableone,
       writexl,
       devtools,
       ggpubr,
       caret)

# Datos train ----
#  ** Personas ---- 

# Cargar la base de datos
train_personas <- readRDS("stores/dataPS2RDS/train_personas.rds")

#    *** Limpieza la base ----
train_personas <- train_personas %>%
  mutate(Impa = as.numeric(Impa),
         Impa = ifelse(test = is.na(Impa)==T | Impa==0,
                       yes  = Impaes,
                       no   = Impa))

recode_vars = c("Impa", "Oc", "Oficio", "P7040")

train_personas <- as.data.frame(train_personas)
for (i in recode_vars){
  train_personas[,i] = ifelse(test = is.na(train_personas[,i]) == T , yes = 0 , train_personas[,i])
}

train_personas <- as_tibble(train_personas)

train_personas <- train_personas %>% 
  mutate(mujer = case_when(P6020 == 1 ~ 0, 
                           P6020 == 2 ~ 1))

# Recodificar variables
variables_categoricas <- c("mujer", "Oc", "P7040")

train_personas <- as.data.frame(train_personas)
for (i in variables_categoricas){
  train_personas[,i] = as.numeric(train_personas[,i])
  train_personas[,i] = as.logical(train_personas[,i])
}

train_personas <- train_personas %>%
  mutate(P6050   = factor(P6050, levels = c(1, 2, 3, 4, 5, 6, 9)),
         P6090   = factor(P6090, levels = c(1, 2, 9)),
         P6100   = factor(P6100, levels = c(1, 2, 3, 9)),
         P6210   = factor(P6210, levels = c(1, 2, 3, 4, 5, 6, 9)),
         Oficio  = factor(Oficio, levels = c(1:99)),
         P6585s3 = factor(P6585s3, levels = c(1, 2, 9)),
         P6585s4 = factor(P6585s4, levels = c(1, 2, 9)),
         P7510s1 = factor(P7510s1, levels = c(1, 2, 9)),
         P7510s2 = factor(P7510s2, levels = c(1, 2, 9)),
         P7510s3 = factor(P7510s3, levels = c(1, 2, 9)),
         P6870   = factor(P6870, levels = c(1:9)))

train_personas <- train_personas %>%
  mutate(P6210 = ifelse(test = is.na(P6210)==T,
                        yes  = 9,
                        no   = P6210))

train_personas <- train_personas %>% 
  mutate(educ = case_when(P6210 == 1 | P6210 == 9 ~ 0, 
                          P6210 == 2 ~ 3, 
                          P6210 == 3 ~ 8, 
                          P6210 == 4 ~ 12,
                          P6210 == 5 ~ 16,
                          P6210 == 6 ~ 20))

train_personas <- train_personas %>%
  mutate(jefeHogar = case_when(P6050 == 1 ~ 1, 
                               P6050 == 2 | P6050 == 3 | P6050 == 4 | P6050 == 5 | 
                                 P6050 == 6 | P6050 == 9 | is.na(P6050) == T ~ 0))

train_personas <- train_personas %>%
  mutate(subsidiado = case_when(P6100 == 3 ~ 1, 
                                P6100 == 1 | P6100 == 2 | P6100 == 9 | is.na(P6100) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(formal = case_when(P6090 == 1 ~ 1, 
                            P6090 == 2 | P6090 == 9 | is.na(P6090) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(microEmpresa = case_when(P6870 == 1 | P6870 == 2 | P6870 == 3 | P6870 == 4 ~ 1,
                                  P6870 == 5 | P6870 == 6 | P6870 == 7 | P6870 == 8 | 
                                    P6870 == 9 | is.na(P6870) == T ~ 0))

train_personas <- train_personas %>% 
  mutate(ayudaHogares = ifelse(P7510s1 == 1 | P7510s2 == 1,
                               yes = 1,
                               no  = 0))

train_personas <- train_personas %>% 
  mutate(ayudaInstituciones = ifelse(P7510s3 == 1,
                                     yes = 1,
                                     no  = 0))

# Construir edad al cuadrado 
train_personas <- train_personas %>% 
  mutate(edad2 = P6040^2)

train_personas <- train_personas %>%
  mutate(educ = ifelse(test = is.na(educ) == T,
                       yes = 0,
                       no = educ))

# Construir variable experiencia laboral (experiencia potencial)
train_personas <- train_personas %>% 
  mutate(experiencia  = case_when(P6040 <= 18 ~ 0,
                                  educ == 0 & P6040 > 18 ~ P6040 - 18,
                                  educ > 0 & P6040 >= 18 & P6040 <= 22 ~ P6040 - 18, 
                                  educ > 0 & P6040 > 22 ~ P6040 - educ - 6),
         experiencia2 = experiencia^2)

train_personas <- train_personas %>%
  mutate(experiencia  = ifelse(test = experiencia < 0,
                               yes  = 0,
                               no   = experiencia),
         experiencia2 = ifelse(test = experiencia2 < 0,
                               yes  = 0,
                               no   = experiencia2))

# Renombrar variables 
colnames(train_personas)[which(colnames(train_personas)=="Impa")]    = "y_laboral"
colnames(train_personas)[which(colnames(train_personas)=="P6040")]   = "edad"
colnames(train_personas)[which(colnames(train_personas)=="P6585s3")] = "subFamiliar"
colnames(train_personas)[which(colnames(train_personas)=="P6585s4")] = "subEducativo"
colnames(train_personas)[which(colnames(train_personas)=="P7040")]   = "segundoTrabajo"
colnames(train_personas)[which(colnames(train_personas)=="Oc")]      = "Ocupado"

train_personas <- train_personas %>%
  mutate(mujer2           = ifelse(test = mujer == T, yes = 1, no  = 0),
         formal2          = ifelse(test = formal == T, yes = 1, no  = 0),
         jefeHogar2       = ifelse(test = jefeHogar == T, yes = 1, no  = 0),
         microEmpresa2    = ifelse(test = microEmpresa == T, yes = 1, no  = 0),
         Ocupado2         = ifelse(test = Ocupado == T, yes = 1, no  = 0),
         segundoTrabajo2  = ifelse(test = segundoTrabajo == T, yes = 1, no  = 0),
         jefeHogar_mujer2 = jefeHogar2 * mujer2)

recode_vars2 = c("educ", "ayudaHogares", "ayudaInstituciones", "segundoTrabajo2", "Oficio")

train_personas <- as.data.frame(train_personas)
for (i in recode_vars2){
  train_personas[,i] = ifelse(test = is.na(train_personas[,i]) == T,
                              yes = 0, 
                              train_personas[,i])
}

#  ** Hogares ----

# Cargar la base de datos
train_hogares <- readRDS("stores/dataPS2RDS/train_hogares.rds")

#  ** Base agregada ----
train <- merge(train_personas, train_hogares, by = "id", all = TRUE)

#    *** Limpieza de la base ----
train <- train %>% 
  mutate(subFamiliar    = case_when(subFamiliar == 1 ~ 1,
                                    subFamiliar == 2 | subFamiliar == 9 | is.na(subFamiliar) == T ~ 0),
         subEducativo   = case_when(subEducativo == 1 ~ 1, 
                                    subEducativo == 2 | subEducativo == 9 | is.na(subEducativo) == T ~ 0),
         P7510s1        = case_when(P7510s1 == 1 ~ 1,
                                    P7510s1 == 2 | P7510s1 == 9 | is.na(P7510s1) == T ~ 0),
         P7510s2        = case_when(P7510s2 == 1 ~ 1,
                                    P7510s2 == 2 | P7510s2 == 9 | is.na(P7510s2) == T ~ 0),
         P7510s3        = case_when(P7510s3 == 1 ~ 1,
                                    P7510s3 == 2 | P7510s3 == 9 | is.na(P7510s3) == T ~ 0),
         profesional = case_when(educ == 0|educ == 3|educ == 8 ~ 0, 
                                 educ == 12|educ == 16|educ == 20 ~ 1),
         personaxCuarto = Nper / P5010)

Subsidiado_hg <- train %>% 
  group_by(id) %>% 
  summarize(subsidiado = max(subsidiado))

subFamiliar_hg <- train %>%
  group_by(id) %>%
  summarize(subFamiliar = max(subFamiliar))

subEducativo_hg <- train %>% 
  group_by(id) %>%
  summarize(subEducativo = max(subEducativo))

ayudaHogaresnal_hg <- train %>%
  group_by(id) %>%
  summarize(P7510s1 = max(P7510s1))

ayudaHogaresext_hg <- train %>%
  group_by(id) %>%
  summarize(P7510s2 = max(P7510s2))

ayudaInstituciones_hg <- train %>%
  group_by(id) %>%
  summarize(P7510s3 = max(P7510s3))

profesional_hg <- train %>%
  group_by(id) %>%
  summarize(profesional = max(profesional))

personaxCuarto_hg <- train %>%
  group_by(id) %>% 
  summarize(personaxCuarto[1])

microempresa_hg <- train %>% 
  group_by(id) %>% 
  summarize(microempresa_hg = max(microEmpresa))

formal_hg <- train %>% 
  group_by(id) %>% 
  summarize(formal_hg = max(formal))

educ_hg <- train %>% 
  group_by(id) %>% 
  summarize(educ_hg = mean(educ))

train_hogares <- left_join(train_hogares, Subsidiado_hg, by = "id")
train_hogares <- left_join(train_hogares, subFamiliar_hg, by = "id")
train_hogares <- left_join(train_hogares, subEducativo_hg, by = "id")
train_hogares <- left_join(train_hogares, ayudaHogaresnal_hg, by = "id")
train_hogares <- left_join(train_hogares, ayudaHogaresext_hg, by = "id")
train_hogares <- left_join(train_hogares, ayudaInstituciones_hg, by = "id")
train_hogares <- left_join(train_hogares, profesional_hg, by = "id")
train_hogares <- left_join(train_hogares, personaxCuarto_hg, by = "id")
train_hogares <- left_join(train_hogares, microempresa_hg, by = "id")
train_hogares <- left_join(train_hogares, formal_hg, by = "id")
train_hogares <- left_join(train_hogares, educ_hg, by = "id")

colnames(train_hogares)[which(colnames(train_hogares)=="subsidiado")]        = "Subsidiado_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="subFamiliar")]       = "subFamiliar_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="subEducativo")]      = "subEducativo_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="P7510s1")]           = "ayudaHogaresnal_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="P7510s2")]           = "ayudaHogaresext_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="P7510s3")]           = "ayudaInstituciones_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="profesional")]       = "profesional_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="personaxCuarto[1]")] = "personaxCuarto_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="microempresa_hg")]   = "microempresa_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="formal_hg")]         = "formal_hg"
colnames(train_hogares)[which(colnames(train_hogares)=="educ_hg")]          = "educ_hg"

train_hogares_Ing <- train_hogares

# Seleccionar variables de interes
train_hogares <- train_hogares %>% 
  select(Pobre, Subsidiado_hg, subFamiliar_hg, subEducativo_hg, ayudaHogaresnal_hg, ayudaHogaresext_hg, 
         ayudaInstituciones_hg, profesional_hg, personaxCuarto_hg, microempresa_hg, formal_hg,
         educ_hg, Nper, id, Lp, Npersug)

# Modificar variables dummy a factor
train_hogares <- train_hogares %>%
  mutate(Subsidiado_hg         = factor(Subsidiado_hg, levels = c(1,0)),
         subFamiliar_hg        = factor(subFamiliar_hg, levels = c(1,0)),
         subEducativo_hg       = factor(subEducativo_hg, levels = c(1,0)),
         ayudaHogaresnal_hg    = factor(ayudaHogaresnal_hg, levels = c(1,0)),
         ayudaHogaresext_hg    = factor(ayudaHogaresext_hg, levels = c(1,0)),
         ayudaInstituciones_hg = factor(ayudaInstituciones_hg, levels = c(1,0)),
         profesional_hg        = factor(profesional_hg, levels = c(1,0)),
         microempresa_hg       = factor(microempresa_hg, levels = c(1,0)),
         formal_hg             = factor(formal_hg, levels = c(1,0)),
         Pobre                 = factor(Pobre, levels = c(1,0)))

# Renombrar variables tipo factor
train_hogares$Pobre <- factor((train_hogares$Pobre), 
                              levels = c(0, 1), 
                              labels = c("No", "Si"))

train_hogares$Subsidiado_hg <- factor((train_hogares$Subsidiado_hg), 
                                      levels = c(0, 1), 
                                      labels = c("No", "Si"))

train_hogares$subFamiliar_hg <- factor((train_hogares$subFamiliar_hg), 
                                       levels = c(0, 1), 
                                       labels = c("No", "Si"))

train_hogares$subEducativo_hg <- factor((train_hogares$subEducativo_hg), 
                                        levels = c(0, 1), 
                                        labels = c("No", "Si"))

train_hogares$ayudaHogaresnal_hg <- factor((train_hogares$ayudaHogaresnal_hg), 
                                           levels = c(0, 1), 
                                           labels = c("No", "Si"))

train_hogares$ayudaHogaresext_hg <- factor((train_hogares$ayudaHogaresext_hg), 
                                           levels = c(0, 1), 
                                           labels = c("No", "Si"))

train_hogares$ayudaInstituciones_hg <- factor((train_hogares$ayudaInstituciones_hg), 
                                              levels = c(0, 1), 
                                              labels = c("No", "Si"))

train_hogares$profesional_hg <- factor((train_hogares$profesional_hg), 
                                       levels = c(0, 1), 
                                       labels = c("No", "Si"))

train_hogares$microempresa_hg <- factor((train_hogares$microempresa_hg), 
                                        levels = c(0, 1), 
                                        labels = c("No", "Si"))

train_hogares$formal_hg <- factor((train_hogares$formal_hg), 
                                  levels = c(0, 1), 
                                  labels = c("No", "Si"))

# Datos test ----
#  ** Personas ---- 

# Cargar la base de datos
test_personas <- readRDS("stores/dataPS2RDS/test_personas.rds")

#    *** Limpieza de la base ----
recode_vars_per = c("Oc", "Oficio", "P7040")

test_personas <- as.data.frame(test_personas)
for (i in recode_vars_per){
  test_personas[,i] = ifelse(test = is.na(test_personas[,i]) == T , yes = 0 , test_personas[,i])
}

test_personas <- as_tibble(test_personas)

test_personas <- test_personas %>% 
  mutate(mujer = case_when(P6020 == 1 ~ 0, 
                           P6020 == 2 ~ 1))

# Recodificar variables
variables_categoricas_per <- c("mujer", "Oc", "P7040")

test_personas <- as.data.frame(test_personas)
for (i in variables_categoricas_per){
  test_personas[,i] = as.numeric(test_personas[,i])
  test_personas[,i] = as.logical(test_personas[,i])
}

test_personas <- test_personas %>%
  mutate(P6050   = factor(P6050, levels = c(1, 2, 3, 4, 5, 6, 9)),
         P6090   = factor(P6090, levels = c(1, 2, 9)),
         P6100   = factor(P6100, levels = c(1, 2, 3, 9)),
         P6210   = factor(P6210, levels = c(1, 2, 3, 4, 5, 6, 9)),
         Oficio  = factor(Oficio, levels = c(1:99)),
         P6585s3 = factor(P6585s3, levels = c(1, 2, 9)),
         P6585s4 = factor(P6585s4, levels = c(1, 2, 9)),
         P7510s1 = factor(P7510s1, levels = c(1, 2, 9)),
         P7510s2 = factor(P7510s2, levels = c(1, 2, 9)),
         P7510s3 = factor(P7510s3, levels = c(1, 2, 9)),
         P6870   = factor(P6870, levels = c(1:9)))

test_personas <- test_personas %>%
  mutate(P6210 = ifelse(test = is.na(P6210)==T,
                        yes  = 9,
                        no   = P6210))

test_personas <- test_personas %>% 
  mutate(educ = case_when(P6210 == 1 | P6210 == 9 ~ 0, 
                          P6210 == 2 ~ 3, 
                          P6210 == 3 ~ 8, 
                          P6210 == 4 ~ 12,
                          P6210 == 5 ~ 16,
                          P6210 == 6 ~ 20))

test_personas <- test_personas %>%
  mutate(jefeHogar = case_when(P6050 == 1 ~ 1, 
                               P6050 == 2 | P6050 == 3 | P6050 == 4 | P6050 == 5 | 
                                 P6050 == 6 | P6050 == 9 | is.na(P6050) == T ~ 0))

test_personas <- test_personas %>%
  mutate(subsidiado = case_when(P6100 == 3 ~ 1, 
                                P6100 == 1 | P6100 == 2 | P6100 == 9 | is.na(P6100) == T ~ 0))

test_personas <- test_personas %>% 
  mutate(formal = case_when(P6090 == 1 ~ 1, 
                            P6090 == 2 | P6090 == 9 | is.na(P6090) == T ~ 0))

test_personas <- test_personas %>% 
  mutate(microEmpresa = case_when(P6870 == 1 | P6870 == 2 | P6870 == 3 | P6870 == 4 ~ 1,
                                  P6870 == 5 | P6870 == 6 | P6870 == 7 | P6870 == 8 | 
                                    P6870 == 9 | is.na(P6870) == T ~ 0))

test_personas <- test_personas %>% 
  mutate(ayudaHogares = ifelse(P7510s1 == 1 | P7510s2 == 1,
                               yes = 1,
                               no  = 0))

test_personas <- test_personas %>% 
  mutate(ayudaInstituciones = ifelse(P7510s3 == 1,
                                     yes = 1,
                                     no  = 0))

# Construir edad al cuadrado 
test_personas <- test_personas %>% 
  mutate(edad2 = P6040^2)

test_personas <- test_personas %>%
  mutate(educ = ifelse(test = is.na(educ) == T,
                       yes  = 0,
                       no   = educ))

# Construir variable experiencia laboral (experiencia potencial)
test_personas <- test_personas %>% 
  mutate(experiencia  = case_when(P6040 <= 18 ~ 0,
                                  educ == 0 & P6040 > 18 ~ P6040 - 18,
                                  educ > 0 & P6040 >= 18 & P6040 <= 22 ~ P6040 - 18, 
                                  educ > 0 & P6040 > 22 ~ P6040 - educ - 6),
         experiencia2 = experiencia^2)

test_personas <- test_personas %>%
  mutate(experiencia  = ifelse(test = experiencia < 0,
                               yes  = 0,
                               no   = experiencia),
         experiencia2 = ifelse(test = experiencia2 < 0,
                               yes  = 0,
                               no   = experiencia2))

# Renombrar variables 
colnames(test_personas)[which(colnames(test_personas)=="Impa")]    = "y_laboral"
colnames(test_personas)[which(colnames(test_personas)=="P6040")]   = "edad"
colnames(test_personas)[which(colnames(test_personas)=="P6585s3")] = "subFamiliar"
colnames(test_personas)[which(colnames(test_personas)=="P6585s4")] = "subEducativo"
colnames(test_personas)[which(colnames(test_personas)=="P7040")]   = "segundoTrabajo"
colnames(test_personas)[which(colnames(test_personas)=="Oc")]      = "Ocupado"

test_personas <- test_personas %>%
  mutate(mujer2           = ifelse(test = mujer == T, yes = 1, no  = 0),
         formal2          = ifelse(test = formal == T, yes = 1, no  = 0),
         jefeHogar2       = ifelse(test = jefeHogar == T, yes = 1, no  = 0),
         microEmpresa2    = ifelse(test = microEmpresa == T, yes = 1, no  = 0),
         Ocupado2         = ifelse(test = Ocupado == T, yes = 1, no  = 0),
         segundoTrabajo2  = ifelse(test = segundoTrabajo == T, yes = 1, no  = 0),
         jefeHogar_mujer2 = jefeHogar2 * mujer2)

recode_vars2_per = c("educ", "ayudaHogares", "ayudaInstituciones", "segundoTrabajo2", "Oficio")

test_personas <- as.data.frame(test_personas)
for (i in recode_vars2_per){
  test_personas[,i] = ifelse(test = is.na(test_personas[,i]) == T, 
                             yes = 0, 
                             test_personas[,i])
}

#  ** Hogares ----

# Cargar la base de datos
test_hogares <- readRDS("stores/dataPS2RDS/test_hogares.rds")

#  ** Base agregada ----
test <- merge(test_personas, test_hogares, by = "id", all = TRUE)

#    *** Limpieza de la base ----
test <- test %>%
  mutate(subsidiado     = case_when(P6100 == 3 ~ 1, 
                                    P6100 == 1 | P6100 == 2 | P6100 == 9 | is.na(P6100) == T ~ 0),
         subFamiliar    = case_when(subFamiliar == 1 ~ 1,
                                    subFamiliar == 2 | subFamiliar == 9 | is.na(subFamiliar) == T ~ 0),
         subEducativo   = case_when(subEducativo == 1 ~ 1, 
                                    subEducativo == 2 | subEducativo == 9 | is.na(subEducativo) == T ~ 0),
         P7510s1        = case_when(P7510s1 == 1 ~ 1,
                                    P7510s1 == 2 | P7510s1 == 9 | is.na(P7510s1) == T ~ 0),
         P7510s2        = case_when(P7510s2 == 1 ~ 1,
                                    P7510s2 == 2 | P7510s2 == 9 | is.na(P7510s2) == T ~ 0),
         P7510s3        = case_when(P7510s3 == 1 ~ 1,
                                    P7510s3 == 2 | P7510s3 == 9 | is.na(P7510s3) == T ~ 0),
         profesional    = case_when(educ == 0|educ == 3|educ == 8 ~ 0, 
                                    educ == 12|educ == 16|educ == 20 ~ 1),
         personaxCuarto = Nper / P5010)

#Agrupar varibles por id a la base de test
Subsidiado_hgt <- test %>%
  group_by(id) %>%
  summarize(subsidiado = max(subsidiado))

subFamiliar_hgt <- test %>%
  group_by(id) %>%
  summarize(subFamiliar = max(subFamiliar))

subEducativo_hgt <- test %>%
  group_by(id) %>%
  summarize(subEducativo = max(subEducativo))

ayudaHogaresnal_hgt <- test %>%
  group_by(id) %>%
  summarize(P7510s1 = max(P7510s1))

ayudaHogaresext_hgt <- test %>%
  group_by(id) %>% 
  summarize(P7510s2 = max(P7510s2))

ayudaInstituciones_hgt <- test %>%
  group_by(id) %>%
  summarize(P7510s3 = max(P7510s3))

profesional_hgt <- test %>%
  group_by(id) %>%
  summarize(profesional = max(profesional))

personaxCuarto_hgt <- test %>%
  group_by(id) %>%
  summarize(personaxCuarto[1])

educ_hgt <- test %>% 
  group_by(id) %>% 
  summarize(educ_hg = mean(educ))

#Adicionar las variables recodificadas anteriormente a la base de test hogares por id 
test_hogares <- left_join(test_hogares,Subsidiado_hgt, by="id")
test_hogares <- left_join(test_hogares,subFamiliar_hgt, by="id")
test_hogares <- left_join(test_hogares,subEducativo_hgt, by="id")
test_hogares <- left_join(test_hogares,ayudaHogaresnal_hgt, by="id")
test_hogares <- left_join(test_hogares,ayudaHogaresext_hgt, by="id")
test_hogares <- left_join(test_hogares,ayudaInstituciones_hgt, by="id")
test_hogares <- left_join(test_hogares,profesional_hgt, by="id")
test_hogares <- left_join(test_hogares,personaxCuarto_hgt, by="id")
test_hogares <- left_join(test_hogares, educ_hgt, by = "id")

#Cambiar los nombres de las variables en la base de test hogares
colnames(test_hogares)[which(colnames(test_hogares)=="subsidiado")]        = "Subsidiado_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="subFamiliar")]       = "subFamiliar_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="subEducativo")]      = "subEducativo_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="P7510s1")]           = "ayudaHogaresnal_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="P7510s2")]           = "ayudaHogaresext_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="P7510s3")]           = "ayudaInstituciones_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="profesional")]       = "profesional_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="personaxCuarto[1]")] = "personaxCuarto_hg"
colnames(test_hogares)[which(colnames(test_hogares)=="educ_hg")]          = "educ_hg"

#Seleccionar las variables de interes y crear una nueva base
test_hogares_t <- test_hogares %>% 
  select(id, Subsidiado_hg, ayudaInstituciones_hg, personaxCuarto_hg, educ_hg)

#Modificar las variables dummy a factor
test_hogares_t <- test_hogares_t %>%
  mutate(Subsidiado_hg         = factor(Subsidiado_hg, levels = c(1,0)),
         ayudaInstituciones_hg = factor(ayudaInstituciones_hg, levels = c(1,0)))

#Renombrar la variables tipo factor
test_hogares_t$Subsidiado_hg <- factor((test_hogares_t$Subsidiado_hg), 
                                       levels = c(0, 1), 
                                       labels = c("No", "Si"))

test_hogares_t$ayudaInstituciones_hg <- factor((test_hogares_t$ayudaInstituciones_hg), 
                                               levels = c(0, 1), 
                                               labels = c("No", "Si"))

test_hogares <- test_hogares %>%
  mutate(Subsidiado_hg = factor(Subsidiado_hg),
         subFamiliar_hg = factor(subFamiliar_hg),
         subEducativo_hg = factor(subEducativo_hg),
         ayudaHogaresnal_hg = factor(ayudaHogaresnal_hg),
         ayudaHogaresext_hg = factor(ayudaHogaresext_hg),
         ayudaInstituciones_hg = factor(ayudaInstituciones_hg),
         profesional_hg = factor(profesional_hg))

# Estadisticas descriptivas ----
# Base Train ----
ed_train <- c("Subsidiado_hg", "subFamiliar_hg", "subEducativo_hg", "ayudaHogaresnal_hg", "ayudaHogaresext_hg",
              "ayudaInstituciones_hg", "profesional_hg", "personaxCuarto_hg")

ed_tab_train <- CreateTableOne(data = train_hogares,
                               strata = "Pobre",
                               vars = ed_train,
                               argsApprox = list(correct = TRUE))

ed_tab_train_ex <- print(ed_tab_train, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  as_tibble()

write_xlsx(ed_tab_train_ex, "views/ed_train.xlsx")

# Base Test ----
ed_test <- c("Subsidiado_hg", "subFamiliar_hg", "subEducativo_hg", "ayudaHogaresnal_hg", "ayudaHogaresext_hg",
             "ayudaInstituciones_hg", "profesional_hg", "personaxCuarto_hg")

ed_tab_test <- CreateTableOne(data = test_hogares,
                              vars = ed_test,
                              argsApprox = list(correct = TRUE))

ed_tab_test_ex <- print(ed_tab_test, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  as_tibble()

write_xlsx(ed_tab_test_ex, "views/ed_test.xlsx")

# Graficas de interes ----
test_hogares$dum <- "A"

# Base Train ----
gr_subs_tr <- ggplot(train_hogares, aes(Pobre, fill = Subsidiado_hg)) +
  geom_bar(position = "fill", width = 0.5) + 
  scale_fill_manual(values=c('00FFFFFF', '#63DEF1')) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title=element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Subsidio (Train)") +
  ylab("Proporcion")

gr_ayuda_tr <- ggplot(train_hogares, aes(Pobre, fill = ayudaInstituciones_hg)) +
  geom_bar(position="fill", width = 0.5) + 
  scale_fill_manual(values=c('00FFFFFF', '#E69F00')) +
  theme(legend.position="none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Ayuda (Train)") +
  ylab("Proporcion")

gr_personas_tr <- ggplot(data = train_hogares, mapping=aes(x = Pobre, y = personaxCuarto_hg)) +
  stat_summary(fun.data = mean_sdl, geom = "bar", width = 0.3, fill = 'pink') +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text( hjust = 0.5, vjust = 0.5)) +
  ggtitle("Personas por Cuarto (Train)") +
  ylab("Personas x Cuarto")

gr_educ_tr <- ggplot(data = train_hogares, mapping=aes(x = Pobre, y = educ_hg)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar", width = 0.3, fill = "purple") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Educacion (Train)") +
  ylab("Años de Educacion")

# Base Test ----
gr_subs_te <- ggplot(test_hogares, aes(dum, fill = as.factor(Subsidiado_hg))) +
  geom_bar(position = "fill", width = 0.3) + 
  scale_fill_manual(values = c('00FFFFFF', '#63DEF1')) +
  theme(axis.title.x = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.ticks.x = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Subsidio (Test)") +
  ylab("Proporcion")

gr_ayuda_te <- ggplot(test_hogares, aes(dum, fill = as.factor(ayudaInstituciones_hg))) +
  geom_bar(position = "fill", width = 0.3) + 
  scale_fill_manual(values = c('00FFFFFF','#E69F00')) +
  theme(axis.title.x = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.ticks.x = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Ayuda (Test)") +
  ylab("Proporcion")

gr_personas_te <- ggplot(data=test_hogares, mapping=aes(x=dum, y=personaxCuarto_hg)) + 
  stat_summary(fun.data = mean_sdl, geom="bar", width = 0.3, fill = 'pink') +
  theme(axis.title.x = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.ticks.x = element_blank(),
        legend.position ="none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Personas por Cuarto (Train)") +
  ylab("Personas x Cuarto")

gr_educ_te <- ggplot(data=test_hogares, mapping=aes(x=dum, y=educ_hg)) + 
  stat_summary(fun.data = mean_sdl, geom = "bar", width = 0.3, fill = "purple") +
  scale_fill_manual(values = c('00FFFFFF', '#E69F00')) +
  theme(axis.title.x = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.ticks.x = element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
  ggtitle("Educacion (Train)") + 
  ylab("Años de Educacion")

# Unir las graficas 
png("views/G1.png", width = 900, height = 450)
ggarrange(gr_subs_tr, 
          gr_ayuda_tr,
          gr_personas_tr,
          gr_educ_tr,
          gr_subs_te,
          gr_ayuda_te,
          gr_personas_te,
          gr_educ_te,
          ncol = 4, nrow = 2)
dev.off()

# Modelos de clasificacion  ----

# Crear regla y bases de prueba para entrenar los modelos
r <- 0.3

prueba1 <- train_hogares %>% 
  select(Pobre)
prueba1 <- prueba1 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba2 <- train_hogares %>%
  select(Pobre)
prueba2 <- prueba2 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba3 <- train_hogares %>%
  select(Pobre)
prueba3 <- prueba3 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba4 <- train_hogares %>%
  select(Pobre)
prueba4 <- prueba4 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba5 <- train_hogares %>%
  select(Pobre)
prueba5 <- prueba5 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba6 <- train_hogares %>%
  select(Pobre)
prueba6 <- prueba6 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

prueba7 <- train_hogares %>%
  select(Pobre)
prueba7 <- prueba7 %>% 
  mutate(Pobre = case_when(Pobre == "No" ~ 0,
                           Pobre == "Si" ~ 1))

# Best Subset Selection ----
best <- regsubsets(Pobre ~ ., 
                   method = "exhaustive", 
                   data = train_hogares)
# 1. Logit ----
logitn <- glm(Pobre ~ Subsidiado_hg + ayudaInstituciones_hg + personaxCuarto_hg + educ_hg, 
              data = train_hogares, 
              family = "binomial")

# Crear los predictores para cada observación y evaluar con la condicion de r
prueba1$logitp <- predict(logitn, newdata = train_hogares, type="response")
prueba1$logitp <- ifelse(prueba1$logitp > r, 
                         yes = 1, 
                         no  = 0)

# Evaluar desempeño del modelo
cm_prob1 <- confusionMatrix(data = factor(prueba1$logitp), 
                            reference = factor(prueba1$Pobre), 
                            mode = "sens_spec" , 
                            positive = "1")
cm_prob1

# Crear las variable de clase predict y performance del paquete ROCR
prelogitn <- prediction(predict(logitn), prueba1$Pobre)
perlogitn <- performance(prelogitn, "tpr", "fpr")

# AUC
auc_mod1 <- performance(prelogitn, measure = "auc")
auc_mod1 <- auc_mod1@y.values[[1]]

# Calcular otros indicadores
cm1 <- cm_prob1$table

# Ratio Falsos Positivos
fp_mod1 <- cm1[2,1] / sum(cm1[2,])

# Ratio Falsos Negativos
fn_mod1 <- cm1[1,2] / sum(cm1[1,])

# 2. Logit + Two-Class Summary + Validacion cruzada en K-conjuntos ----

# Crear el control de two class summary
ctrl_two <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)

# Establecer una semilla y estimar el modelo
set.seed(33)
logitTwoclass <- train(Pobre ~ ayudaInstituciones_hg + personaxCuarto_hg,
                       data = train_hogares,
                       method = "glm",
                       trControl = ctrl_two,
                       family = "binomial",
                       preProcess = c("center", "scale"))

# Crear los predictores para cada observación y testearlos con la condión de r
prueba2$locp <- predict(logitTwoclass, newdata = train_hogares, type = "prob")[,1]
prueba2$locp <- ifelse(prueba2$locp > r, 
                       yes = 1, 
                       no  = 0)

# Evaluar desempeño del modelo
cm_prob2 <- confusionMatrix(data = factor(prueba2$locp), 
                            reference = factor(prueba2$Pobre), 
                            mode = "sens_spec",
                            positive="1")
cm_prob2

#Crear las variable de clase predict y performance del paquete ROCR
prelog2 <- prediction(predict(logitTwoclass, type = "prob")[,2], prueba2$Pobre)
perlog2 <- performance(prelog2, "tpr", "fpr")

#AUC
auc_mod2 <- performance(prelog2, measure = "auc")
auc_mod2 <- auc_mod2@y.values[[1]]

# Calcular otros indicadores
cm2 <- cm_prob2$table

# Ratio Falsos Positivos
fp_mod2 <- cm2[2,1] / sum(cm2[2,])

# Ratio Falsos Negativos
fn_mod2 <- cm2[1,2] / sum(cm2[1,])

# 3. Logit + Five-Stats Summary + Validacion cruzada en K-conjuntos ----

#Crear la función que contenga five stats
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

#Crear un nuevo control con la función de 5 stats 
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     verbose=FALSE,
                     savePredictions = T)

#Establecer una semilla y estimar el modelo
set.seed(33)
logitFiveStats <- train(Pobre ~ microempresa_hg + educ_hg + ayudaInstituciones_hg,
                        data = train_hogares,
                        method = "glm",
                        trControl = ctrl,
                        family = "binomial",
                        preProcess = c("center", "scale"))

#Crear los predictores para cada observación y evaluar con la condicion de r
prueba3$loc5 <- predict(logitFiveStats, newdata = train_hogares, type = "prob")[,1]
prueba3$loc5 <- ifelse(prueba3$loc5 > r, 
                       yes = 1, 
                       no  = 0)

# Evaluar desempeño del modelo
cm_prob3 <- confusionMatrix(data = factor(prueba3$loc5) , 
                            reference = factor(prueba3$Pobre), 
                            mode = "sens_spec", 
                            positive="1")

cm_prob3

# Crear las variable de clase predict y performance del paquete ROCR
prelog5 <- prediction(predict(logitFiveStats, type = "prob")[,2], prueba3$Pobre)
perlog5 <- performance(prelog5, "tpr", "fpr")

# AUC
auc_mod3 <- performance(prelog5, measure = "auc")
auc_mod3 <- auc_mod3@y.values[[1]]

# Calcular otros indicadores
cm3 <- cm_prob3$table

# Ratio Falsos Positivos
fp_mod3 <- cm3[2,1] / sum(cm3[2,])

# Ratio Falsos Negativos
fn_mod3 <- cm3[1,2] / sum(cm3[1,])

# 4. Logit  + Lasso maximizado + Five-Stats Summary ----

#Crear una lista de lambdas de -3 a 10 con un salto de 0,05
lam <- 10 ^ seq(-3, 0.05, length = 10)

#Establecer una semmilla y estimar el modelo
set.seed(33)
loglas <- train(Pobre ~ ayudaInstituciones_hg + ayudaHogaresnal_hg + educ_hg,
                data = train_hogares,
                method = "glmnet",
                trControl = ctrl,
                family = "binomial",
                metric = "ROC",
                tuneGrid = expand.grid(alpha = 0,lambda = lam),
                preProcess = c("center", "scale"))

#Crear los predictores para cada observación y evaluar con la condión de r
prueba4$logl <- predict(loglas, newdata = train_hogares, type="prob")[,1]
prueba4$logl <- ifelse(prueba4$logl > r, 
                       yes = 1, 
                       no  = 0)

# Evaluar el desempeño del modelo
cm_prob4 <- confusionMatrix(data=factor(prueba4$logl), 
                            reference = factor(prueba4$Pobre), 
                            mode = "sens_spec", 
                            positive = "1")

cm_prob4

# Crear las variable de clase predict y performance del paquete ROCR
preloglas <- prediction(predict(loglas, type = "prob")[,2], prueba4$Pobre)
perloglas <- performance(preloglas, "tpr", "fpr")

# AUC
auc_mod4 <- performance(preloglas, measure = "auc")
auc_mod4 <- auc_mod4@y.values[[1]]

# Calcular otros indicadores
cm4 <- cm_prob4$table

# Ratio Falsos Positivos
fp_mod4 <- cm4[2,1] / sum(cm4[2,])

# Ratio Falsos Negativos
fn_mod4 <- cm4[1,2] / sum(cm4[1,])

# 5. Logit  + Lasso maximizado + Five-Stats Summary + Upsampling ----

# Establecer una semilla y crear una muestra de datos de train_hogares con el método de upsampling
set.seed(33)
upsam <- upSample(x = train_hogares,
                  y = train_hogares$Pobre,
                  yname = "Pobre")

# Explorar las dimensiones de la muestra de upsampling creada y revisar como quedo la distribución para la variable Pobre
dim(upsam)
table(upsam$Pobre)

# Establecer una semilla y estimar el modelo
set.seed(33)
logs <- train(Pobre ~ subEducativo_hg + ayudaInstituciones_hg + profesional_hg,
              data = upsam,
              method = "glmnet",
              trControl = ctrl,
              family = "binomial",
              metric = "Sens",
              tuneGrid = expand.grid(alpha = 0, lambda = lam),
              preProcess = c("center", "scale"))

#Crear los predictores para cada observación y evaluar con la condión de r
prueba5$lsam <- predict(logs, newdata = train_hogares, type = "prob")[,1]
prueba5$lsam <- ifelse(prueba5$lsam > r, 
                       yes = 1, 
                       no  = 0)

#Crear las variable de clase predict y performance del paquete ROCR
cm_prob5 <- confusionMatrix(data = factor(prueba5$lsam), 
                            reference = factor(prueba5$Pobre), 
                            mode = "sens_spec",
                            positive = "1")
cm_prob5

#Crear las variable de clase predict y performance del paquete ROCR con la muestra de upsampling
preupsam <- prediction(predict(logs, type = "prob")[,2], upsam$Pobre)
perupsam <- performance(preupsam, "tpr", "fpr")

# AUC
auc_mod5 <- performance(preupsam, measure = "auc")
auc_mod5 <- auc_mod5@y.values[[1]]

# Calcular otros indicadores
cm5 <- cm_prob5$table

# Ratio Falsos Positivos
fp_mod5 <- cm5[2,1] / sum(cm5[2,])

# Ratio Falsos Negativos
fn_mod5 <- cm5[1,2] / sum(cm5[1,])

# 6. Logit  + Lasso maximizado + Five-Stats Summary + Downsampling ----

#Establecer una semmilla y crear una muestra de datos de train_hogares con el método de downsampling
set.seed(33)
downsam <- downSample(x = train_hogares,
                      y = train_hogares$Pobre,
                      yname = "Pobre")

#Explorar las dimensiones de la muestra de downsampling creada y revisar como quedo la distribución para la variable Pobre
dim(downsam)
table(downsam$Pobre)

#Establecer una semilla y estimar el modelo
set.seed(33)
logd <- train(Pobre ~ subFamiliar_hg + ayudaInstituciones_hg + profesional_hg,
              data = downsam,
              method = "glmnet",
              trControl = ctrl,
              family = "binomial",
              metric = "Sens",
              tuneGrid = expand.grid(alpha = 0, lambda = lam),
              preProcess = c("center", "scale"))

#Crear los predictores para cada observación y testearlos con la condión de r
prueba6$ldown <- predict(logd, newdata = train_hogares, type = "prob")[,1]
prueba6$ldown <- ifelse(prueba6$ldown > r,
                        yes = 1,
                        no  = 0)

# Evaluar el desempeño del modelo
cm_prob6 <- confusionMatrix(data = factor(prueba6$ldown), 
                            reference = factor(prueba6$Pobre), 
                            mode = "sens_spec", 
                            positive = "1")
cm_prob6

#Crear las variable de clase predict y performance del paquete ROCR con la muestra de upsampling
predowsam <- prediction(predict(logd, type = "prob")[,2], downsam$Pobre)
perdowsam <- performance(predowsam, "tpr", "fpr")

# AUC
auc_mod6 <- performance(predowsam, measure = "auc")
auc_mod6 <- auc_mod6@y.values[[1]]

# Calcular otros indicadores
cm6 <- cm_prob6$table

# Ratio Falsos Positivos
fp_mod6 <- cm5[2,1] / sum(cm6[2,])

# Ratio Falsos Negativos
fn_mod6 <- cm5[1,2] / sum(cm6[1,])

# Comparar ROC, AUC, Falsos-positivos y Falsos-negativos de los modelos ----

#Crear un data frame para la tabla de comparación y agregarle sus respectivos valores para cada modelo
Class_models <- data.frame(matrix(NA, 6, 4))
colnames(Class_models) <- c("Modelo", "AUC", "Falsos-positivos", "Falsos-negativos")

Class_models[1,1] = "M.1"
Class_models[2,1] = "M.2"
Class_models[3,1] = "M.3"
Class_models[4,1] = "M.4"
Class_models[5,1] = "M.5"
Class_models[6,1] = "M.6"

Class_models[1,2] = auc_mod1
Class_models[2,2] = auc_mod2
Class_models[3,2] = auc_mod3
Class_models[4,2] = auc_mod4
Class_models[5,2] = auc_mod5
Class_models[6,2] = auc_mod6

Class_models[1,3] = fp_mod1
Class_models[2,3] = fp_mod2
Class_models[3,3] = fp_mod3
Class_models[4,3] = fp_mod4
Class_models[5,3] = fp_mod5
Class_models[6,3] = fp_mod6

Class_models[1,4] = fn_mod1
Class_models[2,4] = fn_mod2
Class_models[3,4] = fn_mod3
Class_models[4,4] = fn_mod4
Class_models[5,4] = fn_mod5
Class_models[6,4] = fn_mod6

#Exportar la tabla a un excel
write_xlsx(Class_modelos, "views/Class_models.xlsx")

#Crear una gráfica que contenga las curvas de ROC de los 6 modelos y exportar la imagen
png("views/G2.png", width = 564, height = 422)
plot(perlogitn, col = "red" )
par(new = TRUE)
plot(perlog2, col = "black" )
par(new = TRUE)
plot(perlog5, col = "blue")
par(new = TRUE)
plot(perloglas, col = "green")
par(new = TRUE)
plot(perupsam, col = "violet")
par(new = TRUE)
plot(perdowsam, col = "orange")
legend(x = "right", 
       legend = c("M.1", "M.2", "M.3", "M.4", "M.5", "M.6"), 
       fill = c("red", "black", "blue", "green", "violet", "orange"))
abline(a = 0, b = 1)
dev.off()

# Estimar fuera de muestra y determinar pobreza ----
test_hogares_t$logitn <- predict(logitn, newdata = test_hogares_t, type="response")

# Evaluar la variable dependiente calculada con la regla r para determinar si espobre o no
test_hogares_t <- test_hogares_t %>%
  mutate(Pobre_clasificacion = ifelse(logitn > r, 
                                      yes = 1, 
                                      no = 0))

#Obtener los ratios de los hogares pobres y no pobres
table(test_hogares_t$Pobre_clasificacion)

texreg::htmlreg(logitn, file = 'views/modelo_logitn.doc')








# Modelos de regresion del ingreso ----

#Se define la semilla para poder replicar los resultados
set.seed(101010)

# Seleccionar variables de interes
datos_train <- train_personas %>% 
  select(y_laboral, edad, edad2, experiencia, experiencia2, educ, mujer2, jefeHogar2, formal2, 
         microEmpresa2, segundoTrabajo2, Ocupado2, Oficio, jefeHogar_mujer2)

# 1. Best Subset Selection ----
bestIng_1 <- regsubsets(y_laboral ~ ., 
                        method = "exhaustive", 
                        data = datos_train)
summary(bestIng_1)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

pred_bestIng_1 <- predict.regsubsets(bestIng_1, newdata = datos_train, id = 4)
RMSE_bestIng_1 <- sqrt(mean((datos_train$y_laboral - pred_bestIng_1)^2))


# 2. Backward Stepwise Selection ----
backwardIng_1 <- train(y_laboral ~ .,
                       data = datos_train,
                       method="leapBackward",
                       trControl = trainControl(method = "cv", number=5))
backwardIng_1
summary(backwardIng_1$finalModel)

pred_backward <- predict(backwardIng_1, newdata = datos_train)
RMSE_backward <- sqrt(mean((datos_train$y_laboral - pred_backward)^2))

# 3. Elastic net y validacion cruzada en K-conjuntos (K-fold Cross-Validation) ---- 

#   ** 3.1 Modelo 3 ---- 
elasticIng_1 <- train(y_laboral ~ microEmpresa2 + Ocupado2 + educ + Oficio, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_1

elasticIng_1bl <- train(y_laboral ~ microEmpresa2 + Ocupado2 + educ + Oficio,
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 0.1, lambda = 1042.294),
                        preProcess = c("center", "scale"))
elasticIng_1bl

# Ajuste del modelo
RMSE_elastic1 <- elasticIng_1bl[["results"]][["RMSE"]]

#   ** 3.2 Modelo 4 ----
elasticIng_2 <- train(y_laboral ~ jefeHogar2 + mujer2 + jefeHogar_mujer2 + segundoTrabajo2, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_2

elasticIng_2bl <- train(y_laboral ~ jefeHogar2 + mujer2 + jefeHogar_mujer2 + segundoTrabajo2, 
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 0.55, lambda = 1042.294),
                        preProcess = c("center", "scale"))
elasticIng_2bl

# Ajuste del modelo
RMSE_elastic2 <- elasticIng_2bl[["results"]][["RMSE"]]

#   ** 3.3 Modelo 5 ----
elasticIng_3 <- train(y_laboral ~ microEmpresa2 + Oficio + experiencia + experiencia2, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_3

elasticIng_3bl <- train(y_laboral ~ microEmpresa2 + Oficio + experiencia + experiencia2, 
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 0.1, lambda = 566.7648),
                        preProcess = c("center", "scale"))
elasticIng_3bl

# Ajuste del modelo
RMSE_elastic3 <- elasticIng_3bl[["results"]][["RMSE"]]

#   ** 3.4 Modelo 6 ----
elasticIng_4 <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + edad + edad2, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_4

elasticIng_4bl <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + edad + edad2, 
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 0.1, lambda = 1042.294),
                        preProcess = c("center", "scale"))
elasticIng_4bl

# Ajuste del modelo
RMSE_elastic4 <- elasticIng_4bl[["results"]][["RMSE"]]

#   ** 3.5 Modelo 7 ----
elasticIng_5 <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + educ + Oficio, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_5

elasticIng_5bl <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + educ + Oficio, 
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 0.1, lambda = 1042.294),
                        preProcess = c("center", "scale"))
elasticIng_5bl

# Ajuste del modelo
RMSE_elastic5 <- elasticIng_5bl[["results"]][["RMSE"]]

#   ** 3.6 Modelo 8 ----
elasticIng_6 <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + educ, 
                      data       = train_personas, 
                      method     = "glmnet",
                      trControl  = trainControl("cv", number = 5),
                      preProcess = c("center", "scale"))
elasticIng_6

elasticIng_6bl <- train(y_laboral ~ microEmpresa2 + segundoTrabajo2 + educ, 
                        data       = train_personas, 
                        method     = "glmnet",
                        trControl  = trainControl("cv", number = 5),
                        tuneGrid   = expand.grid(alpha = 1, lambda = 1042.294),
                        preProcess = c("center", "scale"))
elasticIng_6bl

# Ajuste del modelo
RMSE_elastic6 <- elasticIng_6bl[["results"]][["RMSE"]]

# Comparar error de prediccion promedio de los modelos ----
MSE_modelos <- data.frame(matrix(NA, 8, 2))
colnames(MSE_modelos) <- c("Modelo", "MSE")

MSE_modelos[1,1] = "M.1"
MSE_modelos[2,1] = "M.2"
MSE_modelos[3,1] = "M.3"
MSE_modelos[4,1] = "M.4"
MSE_modelos[5,1] = "M.5"
MSE_modelos[6,1] = "M.6"
MSE_modelos[7,1] = "M.7"
MSE_modelos[8,1] = "M.8"

MSE_modelos[1,2] = RMSE_bestIng_1
MSE_modelos[2,2] = RMSE_backward
MSE_modelos[3,2] = RMSE_elastic1
MSE_modelos[4,2] = RMSE_elastic2
MSE_modelos[5,2] = RMSE_elastic3
MSE_modelos[6,2] = RMSE_elastic4
MSE_modelos[7,2] = RMSE_elastic5
MSE_modelos[8,2] = RMSE_elastic6

write_xlsx(MSE_modelos, "views/MSE_modelos.xlsx")

png("views/G3.png", width = 499, height = 290)
MSE_m_grafico1 <- ggplot(MSE_modelos, aes(x = Modelo, y = MSE, group = 1)) +
  geom_line(color="navyblue")                              +
  geom_point()                                             +
  theme(axis.text.x = element_text(angle = 90))            +
  labs(x = "Modelo", y = "RMSE")                           +
  theme_test()
dev.off()

# Prediccion modelo seleccionado ----
train_personas$elasticIng_1bl <- predict(elasticIng_1bl, newx = train_personas)

sum_ingresos <- train_personas %>% 
  group_by(id) %>% 
  summarize(Ingreso_predicho = sum(elasticIng_1bl, na.rm = TRUE)) 
summary(sum_ingresos)

train_hogares_Ing <- left_join(train_hogares_Ing, sum_ingresos)

# Determinar pobreza 
train_hogares_Ing <- train_hogares_Ing %>% 
  mutate(Pobre_construida = ifelse(Ingreso_predicho < Lp * Npersug,
                                   yes = 1,
                                   no = 0))

table(train_hogares_Ing$Pobre)
table(train_hogares_Ing$Pobre_construida)
with(train_hogares_Ing,prop.table(table(Pobre, Pobre_construida)))

