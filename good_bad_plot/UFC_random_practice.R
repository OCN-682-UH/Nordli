### UFC practice

# libs
library(ggridges)
library(tidyverse)



# Density plot of Significant Strikes Landed per Minute across Weightclasses 
ggplot(clean_ufc_ath, aes(x = sig_str_landed, y = weight_class))+
  geom_density_ridges(aes(fill = weight_class),
                      alpha = 0.5,
                      show.legend = FALSE)+
  scale_fill_viridis_d()+
  theme_minimal()+
  labs(y="Weight Classes", x="Significant Strikes Landed per Minute")