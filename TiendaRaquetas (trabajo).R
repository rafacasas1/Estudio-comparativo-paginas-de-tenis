library('rvest')
library('reprex')
library(stringr)
library(xml2)
library(readr)
library(gmodels)
library(ggplot2)
library(gmodels)
library(dplyr)


################################################################################################################
################################################################################################################
##################### TRABAJO   - ESTUDIO COMPARATIVO MERCADO DE RAQUETAS DE TENNIS EN CHILE  ##################
##################### RAFAEL HIGUERAS CASAS - JULIO 2021                                      ##################
################################################################################################################
################################################################################################################

###############################################################################################################
################ TIENDA 1 - TIENDATENNIS.CL ###################################################################
###############################################################################################################
Tienda_1<-read_html('https://www.tiendadetenis.cl/collections/raquetas-nuevas')

##### CSS_PRECIO #####
css_precio <- "span.price-item.price-item--regular"
css_precio

##### Eliminamos el Salto Linea y formateamos precio #####
Precio_Raqueta_1<-html_nodes(Tienda_1,css_precio)
Precio_Raqueta_1<-gsub("<span class='price-item price-item--regular' data-regular-price>","",html_text(Precio_Raqueta_1))
Precio_Raqueta_1<-gsub("[\r\n]","",html_text(Precio_Raqueta_1))
Precio_Raqueta_1<-gsub("\\$","",Precio_Raqueta_1)
Precio_Raqueta_1<-gsub("\\.","",Precio_Raqueta_1)
Precio_Raqueta_1<-gsub("\\n","",Precio_Raqueta_1)
Precio_Raqueta_1<-str_trim(Precio_Raqueta_1)
precio_numerico <- as.integer(Precio_Raqueta_1)

##### CSS_NOMBRE #####
css_Nombre <- "div.h4.grid-view-item__title.product-card__title"
css_Nombre

Nombre_Raqueta<-html_nodes(Tienda_1,css_Nombre)
Nombre_Raqueta<-gsub("<div class='h4 grid-view-item__title product-card__title' aria-hidden='true'>","",html_text(Nombre_Raqueta))

##### CSS_URL #####
css_url<-"a.grid-view-item__link.grid-view-item__image-container.full-width-link"
css_url

Url_Prod<-html_attr(html_nodes(Tienda_1, css_url), "href")
Url_Prod<-paste('https://www.tiendadetenis.cl',Url_Prod,sep = '')
print(Url_Prod)

#### GENERAMOS DATAFRAME PARA ALMACENAR LOS DATOS ########
df_t1<-data.frame("Nombre"=Nombre_Raqueta)
df_t1$Precio<-precio_numerico

n<-length(Nombre_Raqueta)

##### CREAMOS VECTOR VACIO PARA CARGAR MARCA DENTRO DEL CICLO FOR ####
vec<-c()

for (i in 1:n){
  print(paste("N° Producto :",i))
  print(Nombre_Raqueta[i])
  print(precio_numerico[i])
  url_detprod<-read_html(Url_Prod[i])
  marca_prod<-html_nodes(url_detprod,"h1.product-single__title")
  vec<-c(vec,gsub("Raqueta","",html_text(marca_prod)))
}

########## ASIGNA MARCA TIENDA  #########################
t1<-c()
for (i in Nombre_Raqueta){
  t1<-c(t1,'TIENDATENIS')
}

df_t1$Marca<-vec
df_t1$Tienda<-t1


###############################################################################################################
######### Tienda 2 - locosporeltenis   ########################################################################
###############################################################################################################

Tienda_2<-read_html('https://locosporeltenis.cl/78-raquetas-tenis?id_category=78&n=60')

##### CSS_PRECIO 2 #####
css_precio2 <- "span.price.product-price"
css_precio2

##### Eliminamos el Salto Linea y formateamos precio #####
Precio_Raqueta2<-html_nodes(Tienda_2,css_precio2)
Precio_Raqueta_2<-gsub("[\r\n]","",html_text(Precio_Raqueta2))
Precio_Raqueta_2<-gsub("\\$","",Precio_Raqueta_2)
Precio_Raqueta_2<-gsub("\\.","",Precio_Raqueta_2)
Precio_Raqueta_2<-gsub("\\n","",Precio_Raqueta_2)
precio2_numerico <- as.numeric(Precio_Raqueta_2)


##### CSS_NOMBRE #####
css_Nombre2 <- "a.product-name"
css_Nombre2

Nombre_Raqueta2<-html_nodes(Tienda_2,css_Nombre2)
Nombre_Raqueta2<-gsub("<div class='h4 grid-view-item__title product-card__title' aria-hidden='true'>","",html_text(Nombre_Raqueta2))

##### CSS_URL #####
css_url2<-"a.product-name"
css_url2

Url_Prod2<-html_attr(html_nodes(Tienda_2, css_url2), "href")

#### GENERAMOS DATAFRAME PARA ALMACENAR LOS DATOS ########
df_t2<-data.frame("Nombre"=Nombre_Raqueta2)
df_t2$Precio<-precio2_numerico

##### CREAMOS VECTOR VACIO PARA CARGAR MARCA DENTRO DEL CICLO FOR ####
vec2<-c()

n2<-length(Nombre_Raqueta2)
for (i in 1:n2){
  print(paste("N° Producto :",i))
  print(Nombre_Raqueta2[i])
  print(precio2_numerico[i])
  url_detprod2<-read_html(Url_Prod2[i])
  #marca_prod2<-html_nodes(url_detprod2,"span.editable.sm_lable")
  #marca_prod2<-gsub("<span class='editable.sm_lable.manufacturer_unic'>","",html_text(marca_prod2))
  print(marca_prod2)
  vec2<-c(vec2,html_text(marca_prod2))
}

##### CSS MARCA ####
css_marca2<-"span.editable.sm_lable"

marca_prod2<-html_nodes(Tienda_2,css_marca2)

########## ASIGNA MARCA TIENDA  #########################
t2<-c()
for (i in Nombre_Raqueta2){
  t2<-c(t2,'LOCOSPORELTENIS')
}

df_t2$Marca<-html_text(marca_prod2)
df_t2$Tienda<-t2

###############################################################################################################
################ Tienda 3 - larrytennis        ################################################################
###############################################################################################################

tienda_3<-read_html('https://larrytennis.com/collections/marcas-raquetas-tenis')

##### CSS_PRECIO 3 #####
css_precio3 <- "span.money"
css_precio3

Precio_Raqueta_3<-html_nodes(tienda_3,css_precio3)
Precio_Raqueta_3<-gsub("[\r\n]","",html_text(Precio_Raqueta_3))
Precio_Raqueta_3<-gsub("\\$","",Precio_Raqueta_3)
Precio_Raqueta_3<-gsub("\\.","",Precio_Raqueta_3)
Precio_Raqueta_3<-gsub("\\n","",Precio_Raqueta_3)
precio3_numerico <- as.integer(Precio_Raqueta_3)

##### CSS_NOMBRE #####
css_Nombre3 <- "h3.product__title.h4"
css_Nombre3

Nombre_Raqueta3<-html_nodes(tienda_3,css_Nombre3)
print(html_text(Nombre_Raqueta3))

##### CSS_URL #####
css_url3<-"a.product-link"
css_url3

Url_Prod3<-html_attr(html_nodes(tienda_3, css_url3), "href")
Url_Prod3<-paste('https://larrytennis.com',Url_Prod3,sep = '')

#### GENERAMOS DATAFRAME PARA ALMACENAR LOS DATOS ########
df_t3<-data.frame("Nombre"=html_text(Nombre_Raqueta3))
df_t3$Precio<-precio3_numerico

##### CREAMOS VECTOR VACIO PARA CARGAR MARCA DENTRO DEL CICLO FOR ####
vec3<-c()

n3<-length(Nombre_Raqueta3)
print(n3)
for (i in 1:n3){
  print(paste("N° Producto :",i))
  print(html_text(Nombre_Raqueta3[i]))
  print(precio3_numerico[i])
  url_detprod3<-read_html(Url_Prod3[i])
  marca_prod3<-html_nodes(url_detprod3,"h4.section__title-desc.product-single__title-desc")
  marca_prod3<-gsub("<h4 class='section__title-desc product-single__title-desc'>","",marca_prod3)
  marca_prod3<-gsub("<h4 class=\"section__title-desc product-single__title-desc\">","",marca_prod3)
  marca_prod3<-gsub("</a></h4>","",marca_prod3)
  marca_prod3<-gsub("collections","",marca_prod3)
  marca_prod3<-gsub("title=","",marca_prod3)
  marca_prod3<-gsub("<","",marca_prod3)
  marca_prod3<-gsub("//","",marca_prod3)
  marca_prod3<-gsub("q=","",marca_prod3)
  marca_prod3<-gsub("a href=","",marca_prod3)
  marca_prod3<-substr(marca_prod3,str_locate(marca_prod3,">")+1,str_locate(marca_prod3,">")+10)
  print(marca_prod3)
  vec3<-c(vec3,marca_prod3)
}


########## ASIGNA MARCA TIENDA  #########################
t3<-c()
for (i in Nombre_Raqueta3){
  t3<-c(t3,'LARRYTENNIS')
}


print(vec3)
df_t3$Marca<-vec3
df_t3$Tienda<-t3

###############################################################################################################
################ Tienda 4 - tenisgoat        ##################################################################
###############################################################################################################

tienda_4<-read_html('https://www.tenisgoat.cl/collections/raquetas')

##### CSS_PRECIO 4 #####
css_precio4 <- "span.price"
css_precio4

Precio_Raqueta_4<-html_nodes(tienda_4,css_precio4)
Precio_Raqueta_4<-gsub(" ","",html_text(Precio_Raqueta_4))
Precio_Raqueta_4<-gsub("\\$","",Precio_Raqueta_4)
Precio_Raqueta_4<-gsub("\\.","",Precio_Raqueta_4)
Precio_Raqueta_4<-gsub("\\n","",Precio_Raqueta_4)
Precio_Raqueta_4<-gsub("\\,","",Precio_Raqueta_4)
Precio_Raqueta_4<-str_trim(Precio_Raqueta_4)
precio4_numerico <- as.integer(Precio_Raqueta_4)

##### CSS_NOMBRE #####
css_Nombre4 <- "a.product-item__title.text--strong.link"
css_Nombre4

Nombre_Raqueta4<-html_nodes(tienda_4,css_Nombre4)

##### CSS_URL #####
css_url4<-"a.product-item__title.text--strong.link"
css_url4

Url_Prod4<-html_attr(html_nodes(tienda_4, css_url4), "href")
Url_Prod4<-paste("https://www.tenisgoat.cl",Url_Prod4,sep="")

#### GENERAMOS DATAFRAME PARA ALMACENAR LOS DATOS ########
df_t4<-data.frame("Nombre"=html_text(Nombre_Raqueta4))
df_t4$Precio<-precio4_numerico

##### CREAMOS VECTOR VACIO PARA CARGAR MARCA DENTRO DEL CICLO FOR ####
vec4<-c()

n4<-length(Nombre_Raqueta4)
for (i in 1:n4){
  print(paste("N° Producto :",i))
  print(html_text(Nombre_Raqueta4[i]))
  print(precio4_numerico[i])
  url_detprod4<-read_html(Url_Prod4[i])
  marca_prod4<-html_nodes(url_detprod4,"a.product-meta__vendor.link.link--accented")
  marca_prod4<-gsub("<a class='product-meta__vendor link link--accented'' href='/collections/head'>","",html_text(marca_prod4))
  vec4<-c(vec4,marca_prod4)
}

########## ASIGNA MARCA TIENDA  #########################
t4<-c()
for (i in Nombre_Raqueta4){
  t4<-c(t4,'TENISGOAT')
}

print(vec4)
df_t4$Marca<-vec4
df_t4$Tienda<-t4

print(df_t1)
print(df_t2)
print(df_t3)
print(df_t4)

### grafico precios tienda 1
barplot(precio_numerico,
        main = "Precio Raquetas Tienda 1",
        ylab = "Precio",
        names.arg = (Nombre_Raqueta),
        col = "darkred",
        las = 2,
        horiz = FALSE)

### grafico precios tienda 2
barplot(precio2_numerico,
        main = "Precio Raquetas Tienda 2",
        ylab = "Precio",
        names.arg = (Nombre_Raqueta2),
        col = "darkred",
        las = 2,
        horiz = FALSE)

### grafico precios tienda 3
barplot(precio3_numerico,
        main = "Precio Raquetas Tienda 3",
        ylab = "Precio",
        names.arg = (html_text(Nombre_Raqueta3)),
        col = "darkred",
        las = 2,
        horiz = FALSE)

### grafico precios tienda 4
barplot(precio4_numerico,
        main = "Precio Raquetas Tienda 4",
        ylab = "Precio",
        names.arg = (html_text(Nombre_Raqueta4)),
        col = "darkred",
        las = 2,
        horiz = FALSE)

###########RESUMEN ESTADISTICO POR TIENDA ############
summary(df_t1)
summary(df_t2)
summary(df_t3)
summary(df_t4)

##### ANALISIS ESTADÍSTICO ################
vtienda<-c('Tienda1','Tienda2','Tienda3','Tienda4')
promedios<-c(mean(precio_numerico),mean(precio2_numerico),mean(precio3_numerico),mean(precio4_numerico))
medianas<-c(median(precio_numerico),median(precio2_numerico),median(precio3_numerico),median(precio4_numerico))
minimos<-c(min(precio_numerico),min(precio2_numerico),min(precio3_numerico),min(precio4_numerico))
maximos<-c(max(precio_numerico),max(precio2_numerico),max(precio3_numerico),max(precio4_numerico))
desv_est<-c(sd(precio_numerico),sd(precio2_numerico),sd(precio3_numerico),sd(precio4_numerico))

df_estadistico<-data.frame(vtienda,promedios,medianas,minimos,maximos,desv_est)

####### RESUMEN ESTAD�STICO DE TODAS LAS TIENDAS ##############
print(df_estadistico)

##################### HIATOGRAMA CON DISRIBUCI�N NORMAL REFERENTE AL RESUMEN DE TIENDAS  ###########
hist(df_estadistico$desv_est, main = "Histograma de Precios", prob = TRUE,
     ylab = "Frecuencia")
x <- seq(min(df_estadistico$desv_est), max(df_estadistico$desv_est), length = 40)
f <- dnorm(x, mean = mean(df_estadistico$desv_est), sd = sd(df_estadistico$desv_est))
lines(x, f, col = "red", lwd = 2)



######### CONSOLIDAMOS TODO EN UN DATAFRAME y HOMOLOGAMOS EN UN DATAFRAME#####################
df_consolidado<-merge(x=df_t1,y=df_t2,all = TRUE)
df_consolidado2<-merge(x=df_t3,y=df_t4,all = TRUE)
df_consolidado$Nombre <- toupper(df_consolidado$Nombre)
df_consolidado$Marca<-  toupper(df_consolidado$Marca)
print(df_consolidado)
print(df_consolidado2)
df_consolidado_final<-merge(x=df_consolidado,y=df_consolidado2,all = TRUE)
print (df_consolidado_final)
##################### HISTOGRAMA REFERENTE A LOS PRECIOS  ###########

ggplot(data = df_consolidado_final,
       mapping = aes(x = df_consolidado_final$Precio,)) +
  geom_histogram(bins = 9)

print (df_consolidado_final$Marca.value_counts)
str(df_consolidado)

gdp_bycontinents <- df_consolidado_final %>%
  group_by(df_consolidado_final$Marca) %>%
  summarize(mean_gdpPercap=mean(df_consolidado_final$Precio))


str(df_consolidado)

n_marcas <- df_consolidado_final %>%
  count(df_consolidado_final$Marca,  sort = TRUE)

### CONTAMOS NUMEROS RAQUETAS VENDIDAS X MARCA #####
print(n_marcas)


n_tiendas <- df_consolidado_final %>%
  count(df_consolidado_final$Tienda,  sort = TRUE)

### CONTAMOS NUMEROS RAQUETAS DISPONIBLES PARA VENTA X TIENDA ###
print(n_tiendas)

hist(n_marcas$n)

