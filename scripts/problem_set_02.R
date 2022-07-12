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
