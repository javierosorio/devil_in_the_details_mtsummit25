#####################################################################
# UN Multilingual Corpus
# Prediction analysis
# Javier Osorio 
# 9-12-2024
#####################################################################



# SETUP --------------------------------------------------

# Clear environment
rm(list = ls())


# Load all packages here

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, glue, openxlsx, tidyverse, readxl, dplyr, stringr, stringi, 
               ggplot2, grid, GGally , tidyverse, broom, stargazer,
               egg, gridExtra , reshape ) 






# GET THE DATA --------------------------------------------------

data.pred.merged <- read.csv('results/prediction_merged_Data_Master_v2.csv')





# REGRESSION ANALYSIS  -------------------------------------


# Create empty data frame to store results
model.results <- as.data.frame(matrix(data= '', nrow=0 , ncol= 5))
names(model.results)



## Loop ----------------


# Get unique lang
unique.lang <- unique(data.pred.merged$lang)

# Begin Loop lang 
for(x in unique.lang){
  
  # Subset lang
  subset1 <- data.pred.merged %>% filter(lang == x) 
  
  
  
  # Level 2 trans.lang
  
  # Get unique trans.lang
  unique.trans.lang <- unique(subset1$trans.lang)
  
  print(glue(x, " ##############################################"))
  
  # Begin Loop trans.lang 
  for(y in unique.trans.lang){
    
    # Subset trans.lang 
    subset2 <- subset1 %>% filter(trans.lang == y) 
    
    print(glue("- ", y, " ---------------------------"))
    
    # Get unique mt.tool
    unique.mt.tool <- unique(subset2$mt.tool)
    
    
    
    # IF ELSE language is Native or Not-Native
    
    if (y=="Native"){
      
      # Level 3 mt.tool
      
      # Begin Loop mt.tool
      for(z in unique.mt.tool){
        
        # Subset trans.lang 
        subset3 <- subset2 %>% filter(mt.tool == z) 
        
        print(glue("... ", z, " ..................."))
        
        
        # Start regression analysis
        
        # Run base model
        m.1 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std + ddm.std , data = subset3) 
        rmse.1 <- sqrt(mean(m.1$residuals^2))          # Extract rmse
        result.1 <- c("full",rmse.1)                   # Add variable

        # Exclude ddm.std
        m.2 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std  , data = subset3) 
        rmse.2 <- sqrt(mean(m.2$residuals^2))          # Extract rmse
        result.2 <- c("-DDM",rmse.2)                   # Add variable

        # Exclude verb.std
        m.3 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral    , data = subset3) 
        rmse.3 <- sqrt(mean(m.3$residuals^2))          # Extract rmse
        result.3 <- c("-Verbs",rmse.3)                 # Add variable

        # Exclude rarity.gral
        m.4 <- lm(pred ~ lemma.std +  noun.std + rarity.dom      , data = subset3) 
        rmse.4 <- sqrt(mean(m.4$residuals^2))          # Extract rmse
        result.4 <- c("-Gral. rarity",rmse.4)          # Add variable

        # Exclude rarity.dom
        m.5 <- lm(pred ~ lemma.std +  noun.std        , data = subset3) 
        rmse.5 <- sqrt(mean(m.5$residuals^2))          # Extract rmse
        result.5 <- c("-Dom. rarity",rmse.5)           # Add variable

        # Exclude rarity.dom
        m.6 <- lm(pred ~ lemma.std        , data = subset3) 
        rmse.6 <- sqrt(mean(m.6$residuals^2))          # Extract rmse
        result.6 <- c("-Nouns",rmse.6)                 # Add variable

        
        # Integrate all rmse results
        rmse.results <- rbind(result.1,result.2,result.3,result.4,result.5,result.6)
        
        # Check results
        rmse.results <- as.data.frame(rmse.results)
        names(rmse.results)<- c("var","rmse")
        rmse.results
        
        # Fix variables
        rmse.results <- rmse.results %>%
          mutate(var= as.factor(var)) %>%
          mutate(rmse= as.numeric(rmse)) %>%
          mutate(lang = x) %>%
          mutate(trans.lang = y) %>%
          mutate(mt.tool = z) 
        
        # Append results to the model.results df
        model.results <- rbind(model.results,rmse.results)

        # Plot results
        ggplot(rmse.results, aes(x=factor(var, level=c('full', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=rmse , group = 1)) +
          geom_point() +
          geom_line()+
          #ylim(0.0463,0.0466) +
          xlab("") + ylab("RMSE") + 
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        # Save plot 
        ggsave(glue("graphs/rmse/rmse_{x}_{y}_{z}_v1.pdf"))
        
      } # End Loop mt.tool
      
    } else {
      
      # Level 3 mt.tool
      
      # Begin Loop mt.tool
      for(z in unique.mt.tool){
        
        # Subset trans.lang 
        subset3 <- subset2 %>% filter(mt.tool == z) 
        
        print(glue("... ", z, " ..................."))
        
        
        # Start regression analysis
        
        # Run base model
        m.0 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std + ddm.std + ddm.diff.std , data = subset3) 
        rmse.0 <- sqrt(mean(m.0$residuals^2))          # Extract rmse
        result.0 <- c("full",rmse.0)                   # Add variable

        # Exclude 
        m.1 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std + ddm.std  , data = subset3) 
        rmse.1 <- sqrt(mean(m.1$residuals^2))          # Extract rmse
        result.1 <- c("-DDM diff.",rmse.1)                   # Add variable

        # Exclude ddm.std
        m.2 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std  , data = subset3) 
        rmse.2 <- sqrt(mean(m.2$residuals^2))          # Extract rmse
        result.2 <- c("-DDM",rmse.2)                   # Add variable

        # Exclude verb.std
        m.3 <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral    , data = subset3) 
        rmse.3 <- sqrt(mean(m.3$residuals^2))          # Extract rmse
        result.3 <- c("-Verbs",rmse.3)                 # Add variable

        # Exclude rarity.gral
        m.4 <- lm(pred ~ lemma.std +  noun.std + rarity.dom      , data = subset3) 
        rmse.4 <- sqrt(mean(m.4$residuals^2))          # Extract rmse
        result.4 <- c("-Gral. rarity",rmse.4)          # Add variable

        # Exclude rarity.dom
        m.5 <- lm(pred ~ lemma.std +  noun.std        , data = subset3) 
        rmse.5 <- sqrt(mean(m.5$residuals^2))          # Extract rmse
        result.5 <- c("-Dom. rarity",rmse.5)           # Add variable

        # Exclude rarity.dom
        m.6 <- lm(pred ~ lemma.std        , data = subset3) 
        rmse.6 <- sqrt(mean(m.6$residuals^2))          # Extract rmse
        result.6 <- c("-Nouns",rmse.6)                 # Add variable

        
        # Integrate all rmse results
        rmse.results <- rbind(result.0,result.1,result.2,result.3,result.4,result.5,result.6)

        # Check results
        rmse.results <- as.data.frame(rmse.results)
        names(rmse.results)<- c("var","rmse")
        rmse.results
        
        # Fix variables
        rmse.results <- rmse.results %>%
          mutate(var= as.factor(var)) %>%
          mutate(rmse= as.numeric(rmse)) %>%
          mutate(lang = x) %>%
          mutate(trans.lang = y) %>%
          mutate(mt.tool = z) 
        
        # Append results to the model.results df
        model.results <- rbind(model.results,rmse.results)
        
        # Plot results
        ggplot(rmse.results, aes(x=factor(var, level=c('full', '-DMM diff.', '-DDM diff.' , '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=rmse , group = 1)) +
          geom_point() +
          geom_line()+
          #ylim(0.0463,0.0466) +
          xlab("") + ylab("RMSE") + 
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        
        # Save plot 
        ggsave(glue("graphs/rmse/rmse_{x}_{y}_{z}_v2.pdf"))
        
      } # End Loop mt.tool
      
    } # End if else
    
  } # End Loop trans.lang
  
} # End Loop lang





## Full plot 

ggplot(model.results,aes(x=var, y=rmse, color=mt.tool, group=mt.tool)) +
  geom_point() +
  geom_line()+
  labs(x = NULL ,y = "RMSE") +
  theme_classic() + # theme_bw() +  
  facet_grid(rows=vars(trans.lang),scales="free") +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=3)) +
  theme(legend.text=element_text(size=rel(0.8))) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) 


# Save plot 
ggsave(glue("graphs/rmse/rmse_ddm_std_v1.pdf"), 
       width = 3.2, height = 10.5, units = "in")




## Two columns plot 


# Rename values
model.results2 <- model.results %>%
  mutate(trans.lang = case_when(
    trans.lang=="Native" ~ "(a) Native",
    trans.lang=="EN to AR" ~ "(b) Arabic \n [EN to AR]",
    trans.lang=="AR to EN" ~ "(c) English \n [AR to EN]",
    trans.lang=="EN to ES" ~ "(d) Spanish \n [EN to ES]",
    trans.lang=="ES to EN" ~ "(e) English \n [ES to EN]"))



# Plots
p.a <- ggplot(subset(model.results2, trans.lang=="(a) Native"),
              aes(x=factor(var, level=c('full', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=rmse, color=lang, group=lang)) +
  geom_point() +
  geom_line()+
  labs(x = NULL, y = "RMSE") +
  theme_classic() +   ylim(0.025,0.125) +
  facet_wrap(~trans.lang,scales="free",ncol=1) +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=3)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p.a)



p.b <- ggplot(subset(model.results2, trans.lang=="(b) Arabic \n [EN to AR]" | trans.lang=="(c) English \n [AR to EN]"),
              aes(x=factor(var, level=c('full', '-DDM diff.', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=rmse, color=mt.tool, group=mt.tool)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = "RMSE") +
  theme_classic() +   ylim(0.025,0.125) +
  #facet_wrap(~trans.lang,scales="free",ncol=2) +
  facet_grid(cols=vars(trans.lang),scales="free") +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=4)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p.b)



p.c <- ggplot(subset(model.results2, trans.lang=="(d) Spanish \n [EN to ES]" | trans.lang=="(e) English \n [ES to EN]" ),
              aes(x=factor(var, level=c('full', '-DDM diff.', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=rmse, color=mt.tool, group=mt.tool)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = "RMSE") +
  theme_classic() +   ylim(0.025,0.125) +  
  #facet_wrap(~trans.lang,scales="free",ncol=2) +
  facet_grid(cols=vars(trans.lang),scales="free") +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=4)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p.c)




pdf(glue("graphs/rmse/rmse_ddm_std_v2.pdf"), width = 3.8, height = 8.2) # Open a new pdf file
# Three rows grid
grid.arrange(grobs = lapply(
  list(p.a, p.b, p.c),
  set_panel_size,
  width = unit(3.2, "cm"),
  height = unit(3, "cm")))
dev.off() # Close the file





#  PERCENTAGE PLOTS ########################################33

# Transform model results into percentages 
# Percentage of model fit loss as increase in RMSE

# Generate data 
model.results.pct <- model.results2 %>%
  group_by(lang, trans.lang, mt.tool) %>%
  mutate(min.rmse=min(rmse)) %>%
  mutate(fit.loss=(rmse/min.rmse)-1)


# Plots
p.a.pct <- ggplot(subset(model.results.pct, trans.lang=="(a) Native"),
              aes(x=factor(var, level=c('full', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=fit.loss, color=lang, group=lang)) +
  geom_point() +
  geom_line()+
  labs(x = NULL, y = "Model Fit Loss") +
  theme_classic() +  ylim(0,0.02) +
  facet_wrap(~trans.lang,scales="free",ncol=1) +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=3)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p.a.pct)



p.b.pct <- ggplot(subset(model.results.pct, trans.lang=="(b) Arabic \n [EN to AR]" | trans.lang=="(c) English \n [AR to EN]"),
              aes(x=factor(var, level=c('full', '-DDM diff.', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=fit.loss, color=mt.tool, group=mt.tool)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = "Model Fit Loss") +
  theme_classic() +  ylim(0,0.02) +
  #facet_wrap(~trans.lang,scales="free",ncol=2) +
  facet_grid(cols=vars(trans.lang),scales="free") +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=4)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p.b.pct)



p.c.pct <- ggplot(subset(model.results.pct, trans.lang=="(d) Spanish \n [EN to ES]" | trans.lang=="(e) English \n [ES to EN]" ),
              aes(x=factor(var, level=c('full', '-DDM diff.', '-DDM' , '-Verbs', '-Gral. rarity', '-Dom. rarity', '-Nouns')), y=fit.loss, color=mt.tool, group=mt.tool)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = "Model Fit Loss") +
  theme_classic() + ylim(0,0.02) +  
  #facet_wrap(~trans.lang,scales="free",ncol=2) +
  facet_grid(cols=vars(trans.lang),scales="free") +
  theme(legend.position="bottom") + 
  guides(color=guide_legend(ncol=4)) +
  theme(legend.text=element_text(size=rel(0.8)),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-1,-1,-1,-1)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(colour="") + 
  scale_color_manual(values=c("#00cc00", "#FF9933", "#3399FF",
                              "#6600FF", "#003366", "#66cc99",
                              "#cc3333")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
print(p.c.pct)




pdf(glue("graphs/rmse/rmse_ddm_pct_v2.pdf"), width = 3.8, height = 8.2) # Open a new pdf file
# Three rows grid
grid.arrange(grobs = lapply(
  list(p.a.pct, p.b.pct, p.c.pct),
  set_panel_size,
  width = unit(3.2, "cm"),
  height = unit(2.5, "cm")))
dev.off() # Close the file



# Identify main contributors

main.contributors <- model.results.pct %>%
  mutate(fit.loss=round(fit.loss, 3))








# End script
