source("global.R")

######################################
# Analysis of simulation experiment #2 
#####################################

# 2. Adaptation scenarios under climate change
scenarios_adapt <- read.csv("data/simulation_experiment_2.csv") %>% 
  left_join(runs)

# 2.1 Variance in SOC changes explained by simulation factors
ClimateScenario <-  filter(scenarios_adapt, ClimateScenario != "Historical")
ClimateScenario$ParmSetID <- factor(ClimateScenario$ParmSetID)
ClimateScenario_vca <- anovaVCA(deltaSOC~Site + ClimateScenario + BiomassRemoval + FertilizerN  + ParmSetID + Genotype, Data = as.data.frame(ClimateScenario))

ClimateScenario_vca$aov.tab %>%
  as.data.frame() %>%
  mutate(term = row.names(.)) %>%
  as_tibble() %>%
  filter(term != "total") %>%
  transmute(vc = `%Total`, 
            term = factor(term,
                          levels = rev(c("Site","ParmSetID","ClimateScenario","FertilizerN","Genotype","BiomassRemoval","error")),
                          labels = rev(c("Site","SALUS parameters","ClimateScenario scenario","Fertilizer use","Genotype","Harvest intensity","Interactions")))) %>%
  arrange(term) %>%
  ungroup() %>%
  mutate(z = (100  - cumsum(vc)) + vc*0.5,
         rep = factor(c(1,1,1,1,2,1,2)),
         x = c(1.7,1.6,1.5,1.4,1.3,1.2,1.1)) %>%
  ggplot(aes(x = 1, y=vc, fill = term)) +
  geom_segment(aes(y = z, x = x, yend = z, xend = 1))  +
  geom_point(aes(y = z, x = x), size = 3, shape = 21) + 
  geom_col(colour = "black", width = 0.1) + 
  geom_text(aes(y = z , x = x, label = term), hjust = 1, nudge_y = -2, size = 3) + 
  annotate("segment", y = 0, x = -Inf, xend = -Inf, yend = 100) + 
  #scale_y_reverse(breaks = c(0,25,50,75,100)) + 
  coord_flip() + 
  scale_fill_manual(values = (c("white",'#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00',"gray50"))) + 
  ylab("Share of the variance (%)") +
  #theme_void() + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length.x = unit(3, "mm"),
        #plot.margin = unit(c(0,0,0,75),"mm"),
        axis.ticks.y = element_blank()) +
  scale_colour_manual(values= c("gray10","gray90"))

ggsave("figures/2_1_deltaSOCAdapt_shareOfVariance.png", width = 10, height = 5)


# 2.2 Effect of adaptation on deltaSOC under climate change 

# Calculate mean SOC change under baseline and climate change conditions
scenarios_adapt %>%
  filter(ParmSetID == 1) %>%
  group_by(Site,ClimateScenario) %>%
  filter(BiomassRemoval == 65 & FertilizerN == 50 & Genotype == "Baseline") %>%
  group_by(ClimateScenario) %>%
  summarise(y = mean(deltaSOC)) 

# Recode variables to compare between scenarios
adapt <-  scenarios_adapt %>%
  group_by(ParmSetID,Site) %>%
  mutate(deltaSOC = deltaSOC - (sum(deltaSOC*(BiomassRemoval == 65 & FertilizerN == 50 & Genotype == "Baseline" & ClimateScenario == "Historical")))) %>%
  filter(ClimateScenario != "Historical") %>%
  ungroup() %>%
  mutate(ClimateScenario = factor(ClimateScenario, levels =  rev(c("SSP585","SSP245","SSP126"))), 
         HBPc = factor(BiomassRemoval, levels = c("65","55","45"), labels = c("","10R","20R")),
         ANFer = factor(FertilizerN,levels = c("50","75","100"), labels = c("","25N","50N")),
         Genotype = factor(Genotype,levels = c("Baseline","Adapted"), labels = c("","AGn")),
         x0 = factor(gsub("","",paste0(Genotype,BiomassRemoval,FertilizerN))),
         x1 = factor(paste(as.numeric(Genotype),as.numeric(BiomassRemoval),as.numeric(FertilizerN))),
         x = factor(as.numeric(x1),
                    levels = 18:1,
                    labels = rev(c("No adaptation",
                                   "25 kg/ha more N fertilizer (25N)",
                                   "50 kg/ha more N fertilizer (50N)",
                                   "10% less biomass removed at harvest (10R)",
                                   "10R + 25N",
                                   "10R + 50N",
                                   "20% less biomass removed at harvest (20R)",
                                   "20R + 25N",
                                   "20R + 50N",
                                   "Adapted genotype (AGn)",
                                   "AGn + 25N",
                                   "AGn + 50N",
                                   "AGn + 10R",
                                   "AGn + 10R + 25N",
                                   "AGn + 10R + 50N",
                                   "AGn + 20R",
                                   "AGn + 20R + 25N",
                                   "AGn + 20R + 50N"))))

adapt %>%
  filter(as.numeric(x) %in% (18 - c(1,2,3,4,7,9,10,12,16,18) + 1)) %>%
  mutate(deltaSOC1 =  ifelse(ParmSetID == 1, deltaSOC,NA)) %>% 
  ggplot(aes(x,deltaSOC, fill = ClimateScenario)) + 
  geom_violin() + 
  geom_hline(yintercept = 0) +
  geom_point(aes(y=deltaSOC1)) + 
  coord_flip(ylim = c(-1.4,0.8) ) +
  facet_grid(~ClimateScenario) +
  scale_fill_manual(values = viridis::viridis(3,begin = 0.3))  + 
  labs(x = "", y = expression(Delta~SOC[Adaptation] - Delta~SOC[Baseline]~"("*Mg~C~ha^-1~yr^-1*")")) +
  theme(legend.position = "none")

ggsave("figures/2_2_deltaSOCwithAdapt.png", width = 10, height = 5)
