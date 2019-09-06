library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggridges)


# working directory -------------------------------------------------------

getwd()
path = "..."  
# o usar proyectos!

# read data ---------------------------------------------------------------

fdat = "data/raw/..."
dat = read.delim(fdat, header=T, sep="", dec="", stringsAsFactors=F)
# data.table::fread

# queries / dplyr ---------------------------------------------------------

dim(dat)
str(dat)
glimpse(dat)
head(dat)
View(dat)

# rows
dat %>% 
  dplyr::filter(... == "" & ... > "")
dat %>% 
  arrange(...,desc(...))

# columns
dat %>% select(...,...)
dat %>% select(-c(...,...))
dat %>% pull(...)

# nueva columna
dat %>% mutate(
  ... = ....
  ,... = ....
)
# rename
dat %>% rename(new = old)
dat %>% janitor::clean_names()

# ifelse
dat %>% 
  mutate(
    new = ifelse(x == "" ~ ..., a, b)
  )
# case_when
dat %>% 
  mutate(
    new = case_when(
      x == "" ~ ...
      ,x == "" ~ ...
      ,x == "" ~ ...
      ,TRUE ~ ...
    )
  )

# medidas resumen
dat %>% summarise(
  x = mean(x)
  ,d = max(x)
  ,f = min(x)
)
      # dat = iris
dat %>% 
  summarise_if(is.numeric, list(media=mean, max=max, min=min))

# group by
dat %>% 
  group_by(Species) %>% 
  summarise_if(is.numeric, list(media=mean, max=max, min=min))

# joins
tot = dat1 %>% inner_join(dat2, by=c("...","..."))

# reshape / tidyr ---------------------------------------------------------

# wide to long
dat %>% 
  gather(key="var", value="value", -id)
# long to wide
dat %>% 
  spread(key=var, value=value)

# ver si poner data.table:
# dat %>% 
  # data.table::dcast(nombre + apellido ~ parcial, value.vars=c("..."))

# y revisar si ya existe tidyr::pivot_longer y tidyr::pivot_wider !!!

# exploratorio ------------------------------------------------------------

# general
skimr::skim(dat)
exp = skimr::skim_to_list(dat)

d_num = dat %>% select_if(is.numeric)
d_cat = dat %>% select_if(function(x) !is.numeric(x))

# cuantiles
cuants = d_num %>% map(function(x) quantile(x, seq(0,1,0.05)))

# correlograma
ggcorrplot::ggcorrplot(cor(d_num), hc.order=T, type="lower"
                       ,lab=T, insig="blank")
GGally::ggcorr(dat, label=T)

# bivariados
GGally::ggpairs(dat, mapping=aes(color=Species))
GGally::ggpairs(dat, mapping=aes(color=Species), columns="Sepal.Length")
GGally::ggpairs(dat, mapping=aes(color=Species), columns=c("Sepal.Length","Petal.Width"))

# categoricas
tabs = d_cat %>% map(function(x) table(x, useNA="ifany"))
tabs = d_cat %>% map(function(x) janitor::tabyl(x))
janitor::tabyl(mpg, year)
janitor::tabyl(mpg, year, drv)


# plots -------------------------------------------------------------------

# scatter plot
ggplot(mpg, aes(x=cty, y=hwy, color=factor(cyl))) +
  geom_point() +
  NULL
ggplot(mpg, aes(x=cty, y=hwy)) +
  geom_point(aes(color=factor(cyl))) +
  geom_smooth(se=F, method="lm") +
  # facet_grid(~ class) +
  facet_wrap(~ factor(year)) +
  # theme_minimal() +
  NULL
ggscatterhist(
  iris, x="Sepal.Length", y="Sepal.Width", color="Species"
  ,size=3, alpha=0.6
  ,ggtheme=theme_bw()
)
ggscatterhist(
  iris, x="Sepal.Length", y="Sepal.Width", color="Species"
  ,size=3, alpha=0.6, margin.plot="boxplot"
  ,ggtheme=theme_bw()
)

# box plot
ggplot(mpg, aes(x=factor(cyl), y=cty, fill=factor(cyl))) +
  geom_boxplot()+
  facet_wrap(~ year) +
  NULL
ggplot(mpg, aes(x=factor(year), y=cty, fill=factor(year))) +
  geom_boxplot()+
  facet_wrap(~ factor(cyl)) +
  NULL

# histogram/density plot
ggplot(mpg, aes(x=cty, fill=factor(year))) +
  geom_histogram(alpha=0.5) + 
  # geom_density(alpha=0.5, adjust=2) + 
  # geom_density(alpha=0.5, adjust=0.1) + 
  NULL
ggplot(iris, aes(x=Sepal.Length, y=Species, fill =..x..)) +
  geom_density_ridges_gradient(scale=1) +
  viridis::scale_fill_viridis()
ggplot(iris, aes(x=Sepal.Length, y=Species, fill=Species)) +
  stat_density_ridges(quantile_lines=T, quantiles=c(.25,.5,.75), alpha=0.7)
ggplot(iris, aes(x=Sepal.Length, y=Species, fill=Species)) +
  geom_density_ridges(jittered_points=T)       

# interactive
g = ggplot(iris,aes(x=Sepal.Length, y=Petal.Length, color=Species
                     ,label=Species, label2=Sepal.Width)) +
  geom_point()
plotly::ggplotly(g)



# PONER EJERCICIOS!!!!


# lubridate solo si necesario
