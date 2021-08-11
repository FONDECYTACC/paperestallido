
load("C:/Users/CISS Fondecyt/Mi unidad/paperestallido/Definitive_models_2021_after_revision.RData")

#Visualmente es complicado hacer eso, y poder encontrar diferencias. En cambio, lo que presentamos el error acumulado del forecast 
#y tal como muestra la figura,no hay una diferencia importante. El detalle adicional que muestran. Si bien elegimos el modelo con el menor 
#error acumulado, las distintas especificaciones del modelo, no llevan a especificaciones relevantes.
#Ojalá en una tabla mostrar todo.



#Using rvest and jsonlite the following code will get you the data you are looking for. The data for the plot.ly diagrams is stored in <script> tags.

#First step is to identify the widget ID of the figure of interest, the code below shows you how to find the widget
#ID by looking for the caption text of the figure of interest. Then you can search for the correct node with html_nodes() and html_attrs(). 
#jsonlite::fromJSON() converts the JSON data to an R list object.

library(rvest)
library(jsonlite)
library(purrr)
library(stringr)
library(dplyr)
library(ggplot2)


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
url <-
  "https://fondecytacc.github.io/paperestallido/Causal_Impact2_cons_trauma.html"

raw_html <- read_html(url)

caption <-
  "Figure 9. Comparison of BSTS models (cum. error)"

figure_divs <- html_nodes(raw_html,".figure")


figure_21_div_lgl <- grepl("*", figure_divs)

widget_id <-
  figure_divs[figure_21_div_lgl] %>%
  html_nodes("div") %>%
  html_attr("id")

# find data for the correct widget_id

data_for <-
  html_nodes(raw_html, "script") %>%
  html_attr("data-for")

data_for_figure_21_lgl <-
  !is.na(data_for) & data_for == widget_id

data_for_figure_21 <-
  html_nodes(raw_html, "script") %>%
  .[data_for_figure_21_lgl] %>%
  html_text()

dff21_l <- fromJSON(data_for_figure_21)

fig_data_cons_trauma<-data.frame(unlist(dff21_l$x$data$text))

fig_data_cons_trauma_df<-
fig_data_cons_trauma %>% 
  tidyr::separate(unlist.dff21_l.x.data.text., c("CAFE", "Time","Model"), "<br />") %>% 
  dplyr::mutate_at(1:2,~readr::parse_number(.)) %>% 
  dplyr::mutate(type="Trauma Consultations")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
url <-
  "https://fondecytacc.github.io/paperestallido/Causal_Impact2_cons_resp.html"

raw_html <- read_html(url)

caption <-
  "Figure 9. Comparison of BSTS models (cum. error)"

figure_divs <- html_nodes(raw_html,".figure")


figure_21_div_lgl <- grepl("*", figure_divs)

widget_id <-
  figure_divs[figure_21_div_lgl] %>%
  html_nodes("div") %>%
  html_attr("id")

# find data for the correct widget_id

data_for <-
  html_nodes(raw_html, "script") %>%
  html_attr("data-for")

data_for_figure_21_lgl <-
  !is.na(data_for) & data_for == widget_id

data_for_figure_21 <-
  html_nodes(raw_html, "script") %>%
  .[data_for_figure_21_lgl] %>%
  html_text()

dff21_l <- fromJSON(data_for_figure_21)

fig_data_cons_resp<-data.frame(unlist(dff21_l$x$data$text))

fig_data_cons_resp_df<-
  fig_data_cons_resp %>% 
  tidyr::separate(unlist.dff21_l.x.data.text., c("CAFE", "Time","Model"), "<br />") %>% 
  dplyr::mutate_at(1:2,~readr::parse_number(.)) %>% 
  dplyr::mutate(type="Respiratory Consultations")

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
url <-
  "https://fondecytacc.github.io/paperestallido/Causal_Impact2_hosp_trauma.html"

raw_html <- read_html(url)

caption <-
  "Figure 9. Comparison of BSTS models (cum. error)"

figure_divs <- html_nodes(raw_html,".figure")


figure_21_div_lgl <- grepl("*", figure_divs)

widget_id <-
  figure_divs[figure_21_div_lgl] %>%
  html_nodes("div") %>%
  html_attr("id")

# find data for the correct widget_id

data_for <-
  html_nodes(raw_html, "script") %>%
  html_attr("data-for")

data_for_figure_21_lgl <-
  !is.na(data_for) & data_for == widget_id

data_for_figure_21 <-
  html_nodes(raw_html, "script") %>%
  .[data_for_figure_21_lgl] %>%
  html_text()

dff21_l <- fromJSON(data_for_figure_21)

fig_data_hosp_trauma<-data.frame(unlist(dff21_l$x$data$text))

fig_data_hosp_trauma_df<-
  fig_data_hosp_trauma %>% 
  tidyr::separate(unlist.dff21_l.x.data.text., c("CAFE", "Time","Model"), "<br />") %>% 
  dplyr::mutate_at(1:2,~readr::parse_number(.)) %>% 
  dplyr::mutate(type="Trauma Hospitalizations")


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
url <-
  "https://fondecytacc.github.io/paperestallido/Causal_Impact2_hosp_resp.html"

raw_html <- read_html(url)

caption <-
  "Figure 9. Comparison of BSTS models (cum. error)"

figure_divs <- html_nodes(raw_html,".figure")


figure_21_div_lgl <- grepl("*", figure_divs)

widget_id <-
  figure_divs[figure_21_div_lgl] %>%
  html_nodes("div") %>%
  html_attr("id")

# find data for the correct widget_id

data_for <-
  html_nodes(raw_html, "script") %>%
  html_attr("data-for")

data_for_figure_21_lgl <-
  !is.na(data_for) & data_for == widget_id

data_for_figure_21 <-
  html_nodes(raw_html, "script") %>%
  .[data_for_figure_21_lgl] %>%
  html_text()

dff21_l <- fromJSON(data_for_figure_21)

fig_data_hosp_resp<-data.frame(unlist(dff21_l$x$data$text))

fig_data_hosp_resp_df<-
  fig_data_hosp_resp %>% 
  tidyr::separate(unlist.dff21_l.x.data.text., c("CAFE", "Time","Model"), "<br />") %>% 
  dplyr::mutate_at(1:2,~readr::parse_number(.)) %>% 
  dplyr::mutate(type="Respiratory Hospitalizations")
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

library(ggplot2)
library(gghighlight)

Sys.setlocale(category = "LC_ALL", locale = "english")

rbind.data.frame(fig_data_cons_trauma_df,
              fig_data_cons_resp_df,
              fig_data_hosp_trauma_df,
              fig_data_hosp_resp_df) %>% 
  dplyr::filter(Time<=250) %>% 
  #dplyr::mutate(date=Time) %>% 
  dplyr::left_join(data15a64_rn[,c("rn","date")],by=c("Time"="rn")) %>% 
  dplyr::mutate(selected=ifelse(Model=="a, Student Dist, Prior sd=.1",1,0)) %>% 
ggplot(aes(x=date, y=CAFE,group=Model))+ 
  theme_minimal() +
  geom_line(data = . %>% filter(selected==0),color = "gray60")+
  geom_line(data = . %>% filter(selected==1),color = "coral", size=1)+
  #geom_line(data = . %>% filter(selected==0 & Time>=250),color = "gray60")+
  #geom_line(data = . %>% filter(selected==1 & Time>=250),color = "coral", size=1)+
  #gghighlight(selected==1)+
  facet_wrap(.~type, scales = "free_y")+
  scale_x_date(breaks="6 months", labels=scales::date_format("%Y %b"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5,size=11))
  
  