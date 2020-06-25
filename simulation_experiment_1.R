source("global.R")

######################################
# Analysis of simulation experiment #1 
#####################################

# 1. Baseline weather and management ##############################
scenarios_historical <- read.csv("data/simulation_experiment_1.csv") %>% 
  left_join(runs %>% select(SimID, ClimateScenario, ParmSetID)) %>% 
  filter(ClimateScenario == "Historical")

# 1.1 Soil C fluxes 
soil_C_flux <- filter(scenarios_historical, DOY == harvestDoy) %>%
  group_by(ParmSetID,Site) %>%
  mutate(Litter = AGB*(1-harvestFraction)*biomassCfraction, C_In  = diff(c(0,C_In)), C_Out = diff(c(0,C_Out)), Roots = C_In - Litter) %>%
  gather(variable,value,Litter,Roots,C_Out) %>%
  group_by(variable,ParmSetID) %>%
  summarise(value = mean(value)) %>% 
  summarise(y = max(value*(ParmSetID==1)), yupr = quantile(value,1 - (1 - distRange)/2), ylwr = quantile(value,(1 - distRange)/2))

soil_C_flux %>%
  mutate(xvar = ifelse(variable != "C_Out","In","Out"),
         variable = factor(variable,levels = c("Roots","Litter","C_Out"))) %>%
  group_by(xvar) %>%
  mutate_at(vars(yupr,ylwr), cumsum) %>%
  ggplot(aes(x=xvar,y,fill = variable)) + 
  geom_hline(yintercept = 0) +
  geom_col(colour = "black") + 
  geom_errorbar(aes(ymin = ylwr, ymax = yupr), width = 0.3) +
  coord_cartesian(ylim =  c(0,7), xlim = c(0.25,2.75), expand = F) +
  scale_fill_manual(values = c("#f7f7f7","#f1a340","#7570b3"), guide = guide_legend(direction = "vertical"), labels = parse(text = c("Dead~roots","Plant~Litter","'Resp.'~CO[2]"))) +
  labs(y=expression("Soil C flux (Mg C "~ha^-1~y^-1*")"), x ="", fill= "") + 
  theme(legend.position = "right", plot.margin = unit(c(2,2,2,10),"mm"))

ggsave("figures/1_1_soil_C_flux.png", width = 6, height = 6)

# 1.2 Variance in SOC changes explained by simulation factors
dSOC <- scenarios_historical %>%
  filter(DOY == initDOY) %>%
  group_by(ParmSetID,Site) %>%
  mutate(date = as.Date(paste(Year,DOY),format = "%Y %j"), dSOC = SOC - sum((date == min(date))*SOC)) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(ParmSetID = as.factor(ParmSetID))

(dSOC_vca <- anovaVCA(dSOC~Site + ParmSetID, Data = as.data.frame(dSOC))) # Variance componets analysis using ANOVA SS decompostion

dSOC_vca$aov.tab %>%
  as.data.frame() %>%
  mutate(term = row.names(.)) %>%
  as_tibble() %>%
  filter(term != "total") %>%
  transmute(vc = `%Total`, 
            term = factor(term,levels = rev(c("Site","ParmSetID","error")), labels = rev(c("Sites","SALUS Parameters","Interactions")))) %>%
  arrange(term) %>%
  ungroup() %>%
  mutate(z = (100  - cumsum(vc)) + vc*0.3) %>%
  ggplot(aes(x = 1, y=vc, fill = term)) +
  geom_segment(aes(y = z, x = 0.5, yend = z, xend = 1.7))  +
  geom_text(aes(y = z, x = 2, label = paste0(term,"\n(",round(vc),"%)")),size = 3) +   
  geom_col(colour = "black", width = 1) + 
  coord_polar("y",start = 0) +
  scale_fill_manual(values = c("white",'#377eb8','#e41a1c')) + 
  theme_void() +
  theme(legend.position = "none")

ggsave("figures/1_2_deltaSOC_shareOfVariance.png", width = 6, height = 6)

# 1.3 SOC change as time series 
dSOC_ts <- scenarios_historical %>%
  group_by(ParmSetID,Site) %>%
  mutate(date = as.Date(paste(Year,DOY),format = "%Y %j"),
         dSOC = (SOC - sum((date == min(date))*SOC))) %>%
  group_by(Site) %>%
  transmute(ParmSetID, 
            x = as.numeric(date - min(date))/365,
            dSOC,
            dSOC_opt = ifelse(ParmSetID == 1,dSOC,NA)) 

dSOC_ts %>%
  ggplot(aes(x,dSOC,group = factor(ParmSetID)))+
  geom_line(colour = '#377eb8', lwd = 0.5, alpha = 0.1) +
  geom_line(aes(y = dSOC_opt), colour = "#e41a1c" , lwd = 0.8, alpha = 1) +
  coord_cartesian(xlim = c(0,20), ylim = c(-5,45), expand = F) +
  labs(y = expression(Delta~"SOC (Mg C"~~ha^-1*"; 0-100 cm)"), 
       shape = "", fill = "",colour = "",
       x = "Years after establishment") +
  facet_wrap(~Site) + 
  scale_x_continuous(breaks = c(0,5,10,15)) + 
  theme(legend.position =  "none",
        axis.title = element_text(size = 11),
        plot.margin = unit(c(2,2,2,10),"mm"))

ggsave("figures/1_3_deltaSOC_timeSeries.png", width = 10, height = 6)

# 1.4 Site characteristics describing SOC change
dSOC_site <- scenarios_historical %>%
  filter(DOY == harvestDoy) %>%
  group_by(Site,ParmSetID) %>%
  mutate(date = as.Date(paste(Year,DOY),format = "%Y %j")) %>%
  filter(date %in% c(min(date),max(date))) %>%
  summarise_at(vars(date,SOC), diff) %>%
  mutate(x = as.numeric(date/365),
         dSOC = SOC/x) %>%
  summarise(yopt = sum(dSOC*(ParmSetID == 1)),
            yupr = quantile(dSOC,1 - (1 - distRange)/2),
            ylwr = quantile(dSOC,(1 - distRange)/2)) %>%
  left_join(sites)  

dm <- as.matrix(select(dSOC_site, lat,MAP,MAT,init_SOC,BD,texture))
cor(dm) # Correlation matrix

# Principal component analysis
pca <- prcomp(dm, scale = T)
pcs <- as.data.frame(pca$x)
plot(pcs$PC1,pcs$PC2)
pca.var <- pca$sdev^2
(pca.var.per <- round(pca.var/sum(pca.var)*100,1)) # Percent of the varince associated with each PC

# 1.4. PC loadings 
as.data.frame(pca$rotation) %>%
  mutate(variable = row.names(.)) %>%
  gather(PC,x,-variable) %>%
  arrange(variable)%>%
  group_by(variable) %>%
  mutate(load = x^2*pca.var.per,
         score = ifelse(abs(load) > 5,round(x,2),NA),
         variable2 = factor(variable, 
                            levels = c("lat","MAT","MAP","BD","texture","init_SOC"),
                            labels = c("Latitude","MAT","MAP","Bulk density","Clay content","Initial SOC"))) %>%
  arrange(PC,variable2) %>%
  group_by(PC) %>%
  mutate(z = (sum(load) - cumsum(load)) + load*0.5) %>%
  ggplot(aes(PC,load,fill = variable2)) + 
  geom_col(colour = "black") +
  geom_text(aes(y = z, label = score), size = 3) + 
  labs(x="",y="\nShare of the inter-site variability (%)",
       fill = "Descriptor:") + 
  coord_cartesian(expand = F, ylim = c(0,60), xlim = c(0,7)) + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(3,"Blues"),RColorBrewer::brewer.pal(3,"Oranges"))) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.7,0.7))

ggsave("figures/1_4_SitesPCAloadings.png", width = 10, height = 6)

# 1.5 Regression of deltaSOC against PC1 and PC2
dSOC_site %>%
  bind_cols(pcs) %>%
  gather(PC,x,PC1:PC6) %>%
  mutate(comp = factor(PC,labels = paste0(names(pcs)," (",pca.var.per,"%)"))) %>%
  group_by(PC) %>%
  mutate(R2 = paste0("R^2==",round(cor(x,yopt)^2,2)),
         Site = ifelse(Site == "WQFS","WEST LAFAYETTE", toupper(Site))) %>%
  filter(PC %in% paste0("PC",1:2)) %>%
  ggplot(aes(x,yopt)) +
  geom_smooth(method = "lm", se = F, colour = "red") + 
  geom_errorbar(aes(ymin = (ylwr), ymax = (yupr)), width  = 0.1) +
  geom_point(aes(shape = Site, fill = Site), size = 3) + 
  geom_label(aes(y = 2, x  = -3, label = R2), fill = "gray10", colour = "gray90",
             parse = T, hjust  = 0, vjust = 1) +
  guides(fill = guide_legend(ncol = 3), shape = guide_legend(ncol = 3)) + 
  scale_shape_manual(values = rep(c(21,23,24),times = 3)) + 
  scale_fill_brewer(type = "qual",palette = 3) + 
  facet_grid(~comp,switch = "x") + 
  labs(y = expression(Delta~"SOC (Mg C "~ha^-1~yr^-1*"; 0-100 cm)"), 
       shape = "", fill = "", colour = "", x = "") +
  theme(strip.placement = "outside",
        legend.position = "top",
        axis.title.x = element_blank()) 

ggsave("figures/1_5_deltaSOCvsPCAloadings.png", width = 10, height = 6)

# 2. Climate change effects on SOC change
scenarios_climate <- read.csv("data/simulation_experiment_1.csv") %>% 
  left_join(runs %>% select(SimID, ClimateScenario, ParmSetID)) %>%
  filter(DOY == harvestDoy) %>%
  group_by(ClimateScenario,ParmSetID,Site) %>%
  mutate(date = as.Date(paste(Year,DOY),format = "%Y %j")) %>%
  filter(date %in% c(min(date),max(date))) %>%
  summarise_at(vars(date,SOC,C_In,C_Out), diff) %>%
  mutate(x = as.numeric(date/365),
         dSOC = SOC/x,
         C_Out = C_Out/x,
         C_In = C_In/x) %>%
  ungroup() %>%
  gather(variable,y,C_In,C_Out,dSOC) %>%
  mutate(variable = factor(variable,
                           levels = c("C_In","C_Out","dSOC"),
                           labels = c("'Soil C inputs ('~Mg~C~ha^-1~yr^-1*')'",
                                      "Soil~CO[2]~'respiration ('~Mg~C~ha^-1~yr^-1*')'",
                                      "Delta~SOC~'('~Mg~C~ha^-1~yr^-1*')'"))) %>%
  group_by(variable,Site,ClimateScenario,ParmSetID) %>%
  summarise(y = median(y)) %>%
  group_by(variable,Site) %>%
  mutate(a = mean(y))

scenarios_climate %>%
  ggplot(aes(ClimateScenario,y, fill = ClimateScenario)) + 
  geom_violin(draw_quantiles = 0.5, lwd = 0.5) +
  geom_hline(aes(yintercept = ifelse(as.numeric(variable) == 3,0,NA))) + 
  geom_point(aes(y = ifelse(ParmSetID == 1,y,NA)), size = 2, colour = "black") + 
  scale_fill_viridis_d(begin = 0.3) +
  facet_wrap(~variable,ncol = 3,scales = "free",strip.position = "left", labeller = label_parsed) + 
  labs(fill = "", x = "Climate Scenario") +
  theme(strip.placement = "outside",
        legend.position = "none",
        strip.text = element_text(size = 12), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12)) 

ggsave("figures/2_1_deltaSOC_climateChange.png", width = 10, height = 5)
