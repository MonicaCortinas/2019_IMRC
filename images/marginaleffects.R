# Reading
library(knitr)
library(kableExtra)
library(tidyverse)
library(tidylog)
library(summarytools)
library(here)
library(broom)
library(nnet)

data <- read_csv(here("data","omni_dataframe.csv"),
                 col_types = cols(respid = col_factor()))


# Recoding variables and computing new ones

data <- data %>%
  mutate(assurancetime_S_q28q30 = (q28_instore_assort_lasting+q30_instore_deliverytime)/2,
         assurancetime_W_q29q31 = (q29_online_assort_lasting+q31_online_deliverytime)/2,
         ambiance_S_q37q38q42 = (q37_instore_kindness_employees+q38_instore_tidyness+q42_instore_pleasant_socialinteraction)/3,
         ambience_W_q39q40 = (q39_online_attractivedesign+q40_online_clarityportfolio)/2,
         prices_q57q58 = (q57_instore_prices_to_otherbrands+q58_online_pricesto_otheronline)/2,
         imptime_q60q61 = (q60_instore_impreducingtime+q61_online_impreducingtime)/2,
         innovative = case_when(q56_newchannel_attitud == "Disfruto explorando canales alternativos" ~ 1,
                                q56_newchannel_attitud == "Investigar nuevos canales es una pérdida de tiempo" ~ 0,
                                q56_newchannel_attitud == "Me gusta comprar en el mismo canal" ~ 0,
                                q56_newchannel_attitud == "Me gusta probar nuevos canales por diversión" ~ 1,
                                q56_newchannel_attitud == "Soy cauteloso a la hora de probar nuevos canales" ~ 0),
         male = case_when(q64_Sex == "Male" ~ 1,
                          q64_Sex == "Female" ~ 0),
         monthlyincomerec = case_when(
           q74_monthlyincome == "Menos de 500 €" ~ 1,
           q74_monthlyincome == "De 500 a 999,99 €" ~ 2,
           q74_monthlyincome == "De 1.000 a 1.249,99 €" ~ 3,
           q74_monthlyincome == "De 1.250 a 1.499,99 €" ~ 4,
           q74_monthlyincome == "De 1.500 a 1.999,99 €" ~ 5,
           q74_monthlyincome == "De 2.000 a 2.499,99 €" ~ 6,
           q74_monthlyincome == "De 2.500 a 2.999,99 €" ~ 7,
           q74_monthlyincome == "De 3.000 a 3.499,99 €" ~ 8,
           q74_monthlyincome == "De 3.500 a 3.999,99 €" ~ 9,
           q74_monthlyincome == "4000 € ó más" ~ 10),
         omni = case_when(segments == "In-store customer_monochannel user" ~ 0,
                          segments == "In-store customer_omnichannel user" ~ 1,
                          segments == "Omnichannel customer and user" ~ 1,
                          segments == "Online customer_monochannel user" ~ 0,
                          segments ==  "Online customer_omnichannel user" ~ 1),
         threeseg = case_when( segments == "In-store customer_monochannel user" ~ 0,
                               segments == "In-store customer_omnichannel user" ~ 1,
                               segments == "Omnichannel customer and user" ~  2,
                               segments == "Online customer_monochannel user"~ 0,
                               segments == "Online customer_omnichannel user"~ 1)
  )


subset <- data %>%
  select(segments ,  q16_instore_access_product_anywhere ,  q17_online_access_product_anywhere ,
         q22_instore_infoaboutproducts ,  q23_online_infoaboutproducts ,
         q24_instore_assortment ,  q25_online_assortment ,
         q26_instore_productform_assurance  ,  q27_online_productform_assurance ,
         assurancetime_S_q28q30 ,  assurancetime_W_q29q31 ,   ambiance_S_q37q38q42 , 
         ambience_W_q39q40 ,  q14_storelocationaccesibility ,  q15_webaccesibility ,
         q33_instore_return_policy ,  q34_online_return_policy , 
         q35_instore_payment_methods , q36_online_payment_methods , 
         q41_online_peaceofmindinfo_privacysecurity , prices_q57q58 ,
         q59_online_sendingfees_toothers, innovative , 
         imptime_q60q61 ,  male ,q65_age, monthlyincomerec) %>%
  rename(S1_S = q16_instore_access_product_anywhere ,  
         S1_O = q17_online_access_product_anywhere , 
         S2_S = q22_instore_infoaboutproducts,
         S2_O = q23_online_infoaboutproducts,
         S3_S = q24_instore_assortment , 
         S3_O = q25_online_assortment,
         S4F_S  = q26_instore_productform_assurance   ,
         S4F_O = q27_online_productform_assurance ,
         S4T_S= assurancetime_S_q28q30   ,  
         S4T_O = assurancetime_W_q29q31   ,   
         S5_S = ambiance_S_q37q38q42 , 
         S5_O= ambience_W_q39q40,
         P1_S = q14_storelocationaccesibility  ,  
         P1_O= q15_webaccesibility ,
         P2_S = q33_instore_return_policy ,  
         P2_O=  q34_online_return_policy   , 
         P3_S= q35_instore_payment_methods, 
         P3_O= q36_online_payment_methods , 
         P4_O= q41_online_peaceofmindinfo_privacysecurity ,  
         P5= prices_q57q58  ,
         P6_O = q59_online_sendingfees_toothers,  
         A1= innovative, 
         A2= imptime_q60q61,  
         C1= male ,  
         C2= q65_age,
         C3= monthlyincomerec)


#create variable
subset <- subset %>% 
  mutate(type = case_when(segments == "In-store customer_monochannel user" ~ "Type I",
                          segments == "Online customer_monochannel user" ~ "Type I",
                          segments == "In-store customer_omnichannel user" ~ "Type II",
                          segments == "Online customer_omnichannel user" ~ "Type II",
                          segments == "Omnichannel customer and user" ~ "Type III")) %>%
  mutate(type = factor(type, levels = c("Type I", "Type II", "Type III")))


library(nnet)



# Model estimation

multinomial_est <- multinom(type  ~ S1_S + S1_O + S2_S + S2_O + S3_S + S3_O + S4F_S+   
                              S4F_O + S4T_S + S4T_O + S5_S + S5_O + P1_S + P1_O + P2_S + P2_O +    
                              P3_S + P3_O + P4_O + P5 + P6_O + A1 + A2 + C1 + C2 + C3 + 1, 
                            data=subset, trace=FALSE)

# Summary of the model

sum_multinomial<-summary(multinomial_est)

library(broom)

coeffmodel <- tidy(multinomial_est)
ajuste <- glance(multinomial_est)

library(margins)

# Efectos marginales para cada categoría

margins_mono<- as_tibble(margins(multinomial_est, category = "Type I"))
margins_user<- as_tibble(margins(multinomial_est, category = "Type II"))
margins_purchaser<- as_tibble(margins(multinomial_est, category = "Type III"))

# Añadimos un indicador antes de unir las bases

margins_mono <- margins_mono %>%
  mutate(reflevel="Type 1")

margins_user <- margins_user %>%
  mutate(reflevel="Type 2")

margins_purchaser <- margins_purchaser %>%
  mutate(reflevel="Type 3")

# Fundimos las tres y las ponemos como tibble

long_margins <- as_tibble(rbind(margins_mono, margins_user, margins_purchaser))

long_margins <-long_margins %>%
  mutate(reflevel = factor(reflevel, levels = c("Type 1", "Type 2", "Type 3")))

library(ggsci)
paletastartreck <- pal_startrek()(3)

g1 <- ggplot(data=long_margins, aes(x=S1_S, y= dydx_S1_S))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(S1_S)),
              linetype = 2, color="blue")+
  geom_text(aes(x=2, y= 0.019, label="T1 vs T2 no sig."),size=4,  color="grey40")+
  geom_text(aes(x=2, y= 0.012, label="T1 vs T3 sig. **"),size=4,  color="grey40")+  
  geom_text(aes(x=2, y= 0.004, label="T2 vs T3 sig. **"),size=4,  color="grey40")+
  ylab("Marginal effect: dydx")+
  xlab("S1S (median value)")+
  ggtitle("I. S1S. Access to Product. Store")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g2 <- ggplot(data=long_margins, aes(x=S4F_O, y= dydx_S4F_O))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(S5_S)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("S4FO (median value)")+
  ggtitle("II. S4FO  Ass. product desired form (O)")+
  geom_text(aes(x=2, y= -0.010, label="T1 vs T2 no sig."),size=4,  color="grey40")+
  geom_text(aes(x=2, y= -0.022, label="T1 vs T3  no sig."),size=4,  color="grey40")+
  geom_text(aes(x=2, y= -0.035, label="T2 vs T3 sig. ***"),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")


g3 <- ggplot(data=long_margins, aes(x=S5_S, y= dydx_S5_S))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(S5_S)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("S5S (median value)")+
  ggtitle("III.  S5S. Shopping Ambiance. Store")+
  geom_text(aes(x=4.5, y= 0.028, label="T1 vs T2 sig. **"),size=4,  color="grey40")+
  geom_text(aes(x=4.5, y= 0.018, label="T1 vs T3  sig.*"),size=4,  color="grey40")+
  geom_text(aes(x=4.5, y= 0.008, label="T2 vs T3 no sig."),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g4 <- ggplot(data=long_margins, aes(x=P1_O, y= dydx_P1_O))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(P1_O)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("P1O (median value)")+
  ggtitle("IV. P1O Ease of access. Online")+
  geom_text(aes(x=2.5, y= -0.010, label="T1 vs T2 sig. ***"),size=4,  color="grey40")+
  geom_text(aes(x=2.5, y= -0.025, label="T1 vs T3  sig.***"),size=4,  color="grey40")+
  geom_text(aes(x=2.5, y= -0.04, label="T2 vs T3 no sig."),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")



g5 <- ggplot(data=long_margins, aes(x=P2_O, y= dydx_P2_O))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(P2_O)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("P2O (median value)")+
  ggtitle("V. P2O Product return policy (O)")+
  geom_text(aes(x=5, y= 0.030, label="T1 vs T2 no sig."),size=4,  color="grey40")+
  geom_text(aes(x=5, y= 0.020, label="T1 vs T3  no sig."),size=4,  color="grey40")+
  geom_text(aes(x=5, y= 0.01, label="T2 vs T3 sig. **"),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g6 <- ggplot(data=long_margins, aes(x=P5, y= dydx_P5))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(P5)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("P5 (median value)")+
  ggtitle("VI. P5 Relative prices")+
  theme_classic(base_size = 7)+
  geom_text(aes(x=3, y= -0.004, label="T1 vs T2 sig. *"),size=4,  color="grey40")+
  geom_text(aes(x=3, y= -0.014, label="T1 vs T3  sig. **"),size=4,  color="grey40")+
  geom_text(aes(x=3, y= -0.024, label="T2 vs T3 no sig."),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g7 <- ggplot(data=long_margins, aes(x=P6_O, y= dydx_P6_O))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(P6_O)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("P6O (median value)")+
  ggtitle("VII. P6O Delivery fees")+
  geom_text(aes(x=4.5, y= 0.024, label="T1 vs T2 sig. *"),size=4,  color="grey40")+
  geom_text(aes(x=4.5, y= 0.016, label="T1 vs T3  sig. ***"),size=4,  color="grey40")+
  geom_text(aes(x=4.5, y= 0.008, label="T2 vs T3 no sig."),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g8 <- ggplot(data=long_margins, aes(x=A1, y= dydx_A1))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = 0.5),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("A1.")+
  scale_x_discrete("",c(0,1),c("Non-Innovator", "Innovator"),limits=c(0,1))+
  ggtitle("VIII. A1 Open to new channels")+
  geom_text(aes(x=0, y= 0.09, label="T1 vs T2 sig. *"),size=4,  color="grey40")+
  geom_text(aes(x=0, y= 0.06, label="T1 vs T3  sig. ***"),size=4,  color="grey40")+
  geom_text(aes(x=0, y= 0.03, label="T2 vs T3 sig. *"),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g9 <- ggplot(data=long_margins, aes(x=C1, y= dydx_C1))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = 0.5),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("A1")+
  scale_x_discrete("",c(0,1),c("Female", "Male"),limits=c(0,1))+
  ggtitle("IX. A1 Gender (male)")+
  geom_text(aes(x=1, y= 0.09, label="T1 vs T2  sig. **"),size=4,  color="grey40")+
  geom_text(aes(x=1, y= 0.06, label="T1 vs T3  sig. **"),size=4,  color="grey40")+
  geom_text(aes(x=1, y= 0.03, label="T2 vs T3 no sig."),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g10 <- ggplot(data=long_margins, aes(x=C2, y= dydx_C2))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(C2)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("C2 (median value)")+
  ggtitle("X. C2. Age")+
  geom_text(aes(x=30, y= -0.001, label="T1 vs T2  no sig."),size=4,  color="grey40")+
  geom_text(aes(x=30, y= -0.002, label="T1 vs T3  sig. **"),size=4,  color="grey40")+
  geom_text(aes(x=30, y= -0.003, label="T2 vs T3 sig. *"),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

g11 <- ggplot(data=long_margins, aes(x=C3, y= dydx_C3))+
  geom_smooth(aes(color=reflevel,linetype=reflevel, fill = reflevel), method = 'loess')+
  geom_hline( aes(yintercept = 0),
              linetype = 2, color="red") +
  geom_vline( aes(xintercept = median(C3)),
              linetype = 2, color="blue")+
  ylab("Marginal effect: dydx")+
  xlab("C2 (median value)")+
  ggtitle("XI. C3. Income level")+
  geom_text(aes(x=5, y= 0.03, label="T1 vs. T2  no sig."),size=4,  color="grey40")+
  geom_text(aes(x=5, y= 0.02, label="T1 vs. T3  sig. ***"),size=4,  color="grey40")+
  geom_text(aes(x=5, y= 0.01, label="T2 vs. T3 sig. ***"),size=4,  color="grey40")+
  theme_classic(base_size = 9)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Segment: ",  values=c( paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  scale_linetype_manual(name = "Segment: ", values=c("solid", "twodash", "dotted"))+
  scale_fill_manual(name = "Segment: ", values=c(paletastartreck[1], paletastartreck[2], paletastartreck[3])) +
  theme(legend.position="bottom")

library(cowplot)
legend <- get_legend(g1)

first_row <- plot_grid( g1+ theme(legend.position="none"), 
                        g2+ theme(legend.position="none"), 
                        g3 + theme(legend.position="none"))


png(here("images" , "marg_1.png"))

plot_grid(first_row, legend,nrow=2, rel_heights = c(1.2,0.1)) 

dev.off()


second_row <- plot_grid(g4 + theme(legend.position="none"),
  g5+ theme(legend.position="none"), 
                        g6+ theme(legend.position="none"), 
                        g7 + theme(legend.position="none"))


png(here("images" , "marg_2.png"))

plot_grid(second_row, legend,nrow=2, rel_heights = c(1.2,0.1)) 

dev.off()


third_row <- plot_grid( g8 + theme(legend.position="none"),
                         g9+ theme(legend.position="none"), 
                         g10+ theme(legend.position="none"), 
                         g11 + theme(legend.position="none"))


png(here("images" , "marg_3.png"))

plot_grid(third_row, legend,nrow=2, rel_heights = c(1.2,0.1)) 

dev.off()
