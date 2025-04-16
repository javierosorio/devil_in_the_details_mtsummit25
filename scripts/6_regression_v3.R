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
               egg, gridExtra , reshape) 






# GET THE DATA --------------------------------------------------

data.pred.merged <- read.csv('results/prediction_merged_Data_Master_v2.csv')



# check means
summary(data.pred.merged)





# REGRESSION ANALYSIS  -------------------------------------


# Create empty data frame to store results
model.results <- as.data.frame(matrix(data= '', nrow=0 , ncol= 8))
names(model.results)<- c("term","estimate","std.error","statistic","p.value","lang","trans.lang","mt.tool")
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
        
        # Regression Model
        #model <- lm(pred ~ lemma +  noun + rarity.dom + rarity.gral + verb , data = subset3) 
        model <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std + ddm.std , data = subset3) 
        summary(model)
        
        # Export results to latex
        stargazer(model, type = 'latex', out=glue("tables/reg_{x}_{y}_{z}_v2.tex"))
        
        # Tidy model
        model.tidy <- model %>% tidy()
        
        # Add variables
        model.tidy <- model.tidy %>%
          mutate(lang = x) %>%
          mutate(trans.lang = y) %>%
          mutate(mt.tool = z) 
        
        # Append results to the model.results df
        model.results <- rbind(model.results,model.tidy) %>%
          filter(term != "(Intercept)")
        
        # Plot coefficients
        model.tidy%>%
          filter(term != "(Intercept)") %>%
          ggplot(aes(estimate, term)) +
          geom_point() +
          geom_vline(xintercept = 0, lty = 2) +
          labs(x = "Estimate",
               y = NULL,
               title = "Coefficient plot") +
          theme_bw()
        
        # Save plot 
        ggsave(glue("graphs/reg/reg_{x}_{y}_{z}_v2.pdf"))
        
      } # End Loop mt.tool
      
    } else {
      
      # Level 3 mt.tool
      
      # Begin Loop mt.tool
      for(z in unique.mt.tool){
        
        # Subset trans.lang 
        subset3 <- subset2 %>% filter(mt.tool == z) 
        
        print(glue("... ", z, " ..................."))
        
        
        # Start regression analysis
        
        # Regression Model
        #model <- lm(pred ~ lemma +  noun + rarity.dom + rarity.gral + verb + ddm + ddm.diff , data = subset3) 
        model <- lm(pred ~ lemma.std +  noun.std + rarity.dom + rarity.gral + verb.std + ddm.std + ddm.diff.std  , data = subset3) 
        summary(model)
        
        # Export results to latex
        stargazer(model, type = 'latex', out=glue("tables/reg_{x}_{y}_{z}_v2.tex"))
        
        # Tidy model
        model.tidy <- model %>% tidy()
        
        # Add variables
        model.tidy <- model.tidy %>%
          mutate(lang = x) %>%
          mutate(trans.lang = y) %>%
          mutate(mt.tool = z) 
        
        # Append results to the model.results df
        model.results <- rbind(model.results,model.tidy) %>%
          filter(term != "(Intercept)")
        
        # Plot coefficients
        model.tidy%>%
          filter(term != "(Intercept)") %>%
          ggplot(aes(estimate, term)) +
          geom_point() +
          geom_vline(xintercept = 0, lty = 2) +
          labs(x = "Estimate",
               y = NULL,
               title = "Coefficient plot") +
          theme_bw()
        
        # Save plot 
        ggsave(glue("graphs/reg/reg_{x}_{y}_{z}_v2.pdf"))
        
      } # End Loop mt.tool
      
    } # End if else
    
  } # End Loop trans.lang
  
} # End Loop lang



# Rename values
model.results <- model.results %>%
  mutate(mt.tool=case_when(
    lang=="EN" & trans.lang=="Native" & mt.tool=="Native" ~ "NST EN",
    lang=="ES" & trans.lang=="Native" & mt.tool=="Native" ~ "NST ES",
    lang=="AR" & trans.lang=="Native" & mt.tool=="Native" ~ "NST AR",
    TRUE ~ mt.tool)) %>%
  mutate(term=case_when(
    term=="verb.std" ~ "Verbs", 
    term=="noun.std" ~ "Nouns", 
    term=="lemma.std" ~ "Lemmas", 
    term=="rarity.gral" ~ "Gral. rarity", 
    term=="rarity.dom" ~ "Domian rarity", 
    term=="ddm.std" ~ "DDM",
    term=="ddm.diff.std" ~ "DDM diff."
  )) %>%
  mutate(trans.lang = case_when(
    trans.lang=="Native" ~ "(a) NST",
    trans.lang=="EN to AR" ~ "(b) Arabic \n [EN to AR]",
    trans.lang=="AR to EN" ~ "(c) English \n [AR to EN]",
    trans.lang=="ES to EN" ~ "(d) English \n [ES to EN]",
    trans.lang=="EN to ES" ~ "(e) Spanish \n [EN to ES]"
  ))
  





## Two columns plot 


# Rename values
model.results2 <- model.results %>%
  mutate(trans.lang = case_when(
    trans.lang=="(e) Spanish \n [EN to ES]" ~ "(d) Spanish \n [EN to ES]",
    trans.lang=="(d) English \n [ES to EN]" ~ "(e) English \n [ES to EN]",
    TRUE~trans.lang))


# Plots
p.a <- ggplot(subset(model.results2, trans.lang=="(a) NST"),aes(x=estimate, y=term, color=mt.tool, group=mt.tool)) +
  geom_hline(yintercept = 0.4 , colour="black") +
  geom_hline(yintercept = 1.5 , colour="lightgray") +
  geom_hline(yintercept = 2.5 , colour="lightgray") +
  geom_hline(yintercept = 3.5 , colour="lightgray") +
  geom_hline(yintercept = 4.5 , colour="lightgray") +
  geom_hline(yintercept = 5.5 , colour="lightgray") +
  geom_hline(yintercept = 6.5 , colour="lightgray") +
  geom_hline(yintercept = 7.5 , colour="lightgray") +
  geom_hline(yintercept = 8.5 , colour="lightgray") +
  geom_hline(yintercept = 9.5 , colour="lightgray") +
  geom_hline(yintercept = 10.6 , colour="lightgray") +
  geom_vline(xintercept = 0, lty = 1) +
  geom_point(position=position_dodge(width = 1)) +
  geom_errorbarh(aes(xmin=(estimate-std.error), xmax=(estimate+std.error)),
                 position=position_dodge(width = 1)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +  xlim(-0.4,0.6) +  
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
                              "#cc3333")) 
print(p.a)



p.b <- ggplot(subset(model.results2, trans.lang=="(b) Arabic \n [EN to AR]" | trans.lang=="(c) English \n [AR to EN]"),aes(x=estimate, y=term, color=mt.tool, group=mt.tool)) +
  geom_hline(yintercept = 0.4 , colour="black") +
  geom_hline(yintercept = 1.5 , colour="lightgray") +
  geom_hline(yintercept = 2.5 , colour="lightgray") +
  geom_hline(yintercept = 3.5 , colour="lightgray") +
  geom_hline(yintercept = 4.5 , colour="lightgray") +
  geom_hline(yintercept = 5.5 , colour="lightgray") +
  geom_hline(yintercept = 6.5 , colour="lightgray") +
  geom_hline(yintercept = 7.5 , colour="lightgray") +
  geom_hline(yintercept = 8.5 , colour="lightgray") +
  geom_hline(yintercept = 9.5 , colour="lightgray") +
  geom_hline(yintercept = 10.6 , colour="lightgray") +
  geom_vline(xintercept = 0, lty = 1) +
  geom_point(position=position_dodge(width = 1)) +
  geom_errorbarh(aes(xmin=(estimate-std.error), xmax=(estimate+std.error)),
                 position=position_dodge(width = 1)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +  xlim(-0.4,0.6) +  
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
                              "#cc3333")) 
print(p.b)


p.c <- ggplot(subset(model.results2, trans.lang=="(d) Spanish \n [EN to ES]" | trans.lang=="(e) English \n [ES to EN]"),aes(x=estimate, y=term, color=mt.tool, group=mt.tool)) +
  geom_hline(yintercept = 0.4 , colour="black") +
  geom_hline(yintercept = 1.5 , colour="lightgray") +
  geom_hline(yintercept = 2.5 , colour="lightgray") +
  geom_hline(yintercept = 3.5 , colour="lightgray") +
  geom_hline(yintercept = 4.5 , colour="lightgray") +
  geom_hline(yintercept = 5.5 , colour="lightgray") +
  geom_hline(yintercept = 6.5 , colour="lightgray") +
  geom_hline(yintercept = 7.5 , colour="lightgray") +
  geom_hline(yintercept = 8.5 , colour="lightgray") +
  geom_hline(yintercept = 9.5 , colour="lightgray") +
  geom_hline(yintercept = 10.6 , colour="lightgray") +
  geom_vline(xintercept = 0, lty = 1) +
  geom_point(position=position_dodge(width = 1)) +
  geom_errorbarh(aes(xmin=(estimate-std.error), xmax=(estimate+std.error)),
                 position=position_dodge(width = 1)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +  xlim(-0.4,0.6) +  
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
                              "#cc3333")) 
print(p.c)




pdf(glue("graphs/reg/reg_ddm_std_2cols_fv.pdf"), width = 3.8, height = 8.7) # Open a new pdf file
# Three rows grid
grid.arrange(grobs = lapply(
  list(p.a, p.b, p.c),
  set_panel_size,
  width = unit(3.2, "cm"),
  height = unit(5, "cm")))
dev.off() # Close the file



# End script