library(tidyverse)
library(timetk)
library(ggpmisc)
library(ggpubr)
library(ggsignif)
library(Cairo)
library(patchwork)
library(performance)
library(emmeans)
library(broom) 
 


JR_facet_theme2 <- function(){
  theme_light()+
    theme(
      strip.background = element_rect(fill = "grey40", color = "grey80", size = .8),
      strip.text = element_text(colour = "white"),
      axis.title = element_text(face = "bold", size = 13),
      
    )
}



# read data ---------------------------------------------------------------
group_tbl <- read_rds("synthetic_data.rds")



median_carbs <- group_tbl %>% 
  group_by(subject_id) %>%
  summarise(median_CHO = median(diet_carb_g_kg, na.rm = T)) 

high_carb_quantile <- quantile(median_carbs$median_CHO, 0.8, names = FALSE)
low_carb_quantile <- quantile(median_carbs$median_CHO, 0.2, names = FALSE)

group_tbl_diet <- group_tbl %>% 
  mutate(subject_id = factor(subject_id)) %>% 
  group_by(subject_id) %>% 
  mutate(
    habitual_diet = case_when(
      median(diet_carb_g_kg, na.rm = T) < low_carb_quantile ~ "Low-CHO",
      median(diet_carb_g_kg, na.rm = T) > high_carb_quantile ~ "High-CHO",
      TRUE ~ "Mod-CHO"),
    habitual_diet = factor(habitual_diet, levels = c("Low-CHO", "Mod-CHO", "High-CHO"))
  ) %>% 
  ungroup()




# nest tbl ----------------------------------------------------------------

pci_list_tbl <- group_tbl_diet %>%
  mutate(subject_level = ifelse(subject_level == "Professional", "Elite non-professional", subject_level),
         subject_level = factor(subject_level, levels = c("Amateur", "High-level amateur", "Elite non-professional")),
         subject_id = factor(subject_id)
  ) %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  mutate(
    cor = map(data, ~ cor.test(.x$diet_carb_g_kg, .x$exercise_load, , method="pearson", use = "pairwise.complete.obs")),
    cor = map(cor, broom::tidy),
  ) %>% 
  unnest(cor) %>% select(-c(method, alternative, statistic, parameter)) %>%
  rename(cor_val = "estimate",
         cor_p = "p.value"
  ) %>% 
  mutate(
    mean_kcal= map_dbl(data, ~mean(.x$diet_kcal_kg, na.rm = T)),
    mean_protein = map_dbl(data, ~mean(.x$diet_protein_g_kg, na.rm = T)),
    mean_fat = map_dbl(data, ~mean(.x$diet_fat_g_kg, na.rm = T)),
    mean_carb = map_dbl(data, ~mean(.x$diet_carb_g_kg, na.rm = T)),
    sd_carb = map_dbl(data, ~sd(.x$diet_carb_g_kg, na.rm = T)),
    monotony = map_dbl(data, ~ mean_carb/sd_carb),
    carb_range = map_dbl(data, ~ max(.x$diet_carb_g_kg, na.rm = T) - min(.x$diet_carb_g_kg, na.rm = T)),
    carb_index = carb_range * cor_val/monotony,
  ) 


subject_diet_level_tbl <- pci_list_tbl %>% 
  unnest(data) %>% 
  select(subject_level, habitual_diet, subject_id, subject_sex, subject_weekly_training_h, subject_missing_diet, subject_primary_sport) %>% 
  summarise(habitual_diet = first(habitual_diet),
            subject_level = first(subject_level),
            sex = factor(first(subject_sex)),
            training_volume = first(subject_weekly_training_h),
            missing_diet = first(subject_missing_diet),
            subject_primary_sport = first(subject_primary_sport)
  )



fsted_pct_tbl <- group_tbl_diet %>% 
  select(subject_id, exercise_duration_min,exercise_fasted ) %>% 
  filter(exercise_duration_min >0) %>% 
  mutate(exercise_fasted = as.numeric(exercise_fasted)-1) %>% 
  group_by(subject_id) %>% 
  summarise(fasted_training = sum(exercise_fasted, na.rm = T),
            nobs = length(exercise_fasted),
            fasted_training_pct = fasted_training/nobs*100) %>% 
  select(subject_id, fasted_training_pct)



df_for_comparison <- pci_list_tbl %>% 
  left_join(subject_diet_level_tbl, by = "subject_id") %>% 
  left_join(fsted_pct_tbl, by = "subject_id") %>% 
  ungroup()


exercise_rpe_tbl <- group_tbl_diet %>% 
  filter(exercise_duration_min >0) %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    mean_rpe = map_dbl(data, ~mean(.x$exercise_RPE_weighted, na.rm = T)),
    mean_duration = map_dbl(data, ~mean(.x$exercise_duration_min, na.rm = T)),
    max_duration = map_dbl(data, ~max(.x$exercise_duration_min, na.rm = T))
  )


mean_rpe_overall <-  format(round(mean(exercise_rpe_tbl$mean_rpe),1), nsmall = 1)
sd_rpe_overall <- round(sd(exercise_rpe_tbl$mean_rpe),1)
mean_rpe_low <-  format(round(min(exercise_rpe_tbl$mean_rpe),1), nsmall = 1)
mean_rpe_high <- format(round(max(exercise_rpe_tbl$mean_rpe),1), nsmall = 1)

mean_max_duration_overall <-  format(round(mean(exercise_rpe_tbl$max_duration)/60,1), nsmall = 1)
sd_max_duration_overall <- round(sd(exercise_rpe_tbl$max_duration)/60,1)
mean_max_duration_low <-  format(round(min(exercise_rpe_tbl$max_duration)/60,1), nsmall = 1)
mean_max_duration_high <- format(round(max(exercise_rpe_tbl$max_duration)/60,1), nsmall = 1)

mean_mean_duration_overall <-  format(round(mean(exercise_rpe_tbl$mean_duration)/60,1), nsmall = 1)
sd_mean_duration_overall <- round(sd(exercise_rpe_tbl$mean_duration)/60,1)
mean_mean_duration_low <-  format(round(min(exercise_rpe_tbl$mean_duration)/60,1), nsmall = 1)
mean_mean_duration_high <- format(round(max(exercise_rpe_tbl$mean_duration)/60,1), nsmall = 1)



# colors ------------------------------------------------------------------

colors2 <- c("midnightblue", "#6CC458")    #limegreen
colors2green <- c("#6CC458", "#6CC458")    #limegreen
colors2red <- c("midnightblue" , "#B33951") 
colors3 <- c("#6CC458", "#6CC458", "midnightblue")
colors33 <- c("#B33951", "midnightblue","#6CC458")
colors33rev <- c("midnightblue", "#B33951", "#6CC458")
colors331 <- c("#B33951")
colors332 <- c("midnightblue")
colors333 <- c("#6CC458")
colors4 <- c("midnightblue", "#6CC458", "midnightblue", "#6CC458")
colors5 <- c("#6CC458", "midnightblue",   "#6CC458", "midnightblue", "grey60")
colors7 = c( "#6CC458", "grey70", "red")


# Methods -----------------------------------------------------------------
# * 2.2 participants ----------------------------------------------------------
number_subjects_included <- length(unique(group_tbl_diet$subject_id))


# * 2.4 incomplete days ---------------------------------------------------------------

missing_days_tbl <- df_for_comparison %>% select(subject_id, data, missing_diet) %>% 
  mutate(nrow = map_dbl(data, nrow),
         days_missing = round(missing_diet/100*nrow)
  ) 


mean_missing_diet_days_pct <- format(round(mean(df_for_comparison$missing_diet, na.rm = T),1), nsmall = 1)
sd_missing_diet_days_pct <- format(round(sd(df_for_comparison$missing_diet, na.rm = T),1), nsmall = 1)
mean_missing_days <- round(mean(missing_days_tbl$days_missing),1)
sd_missing_days <- round(sd(missing_days_tbl$days_missing),1)
max_missing_days <- round(max(missing_days_tbl$days_missing),1)
min_missing_days <- round(min(missing_days_tbl$days_missing),1)


paste0(
  "Incomplete days of tracking (", mean_missing_days, " ± ", 
  sd_missing_days ," per participant, range ", min_missing_days, " to ", max_missing_days, ") were removed."
)



# Sub-group analysis ----------------------------------------------------------

# * CPI Emmeans -----------------------------------------------------------------

df_for_mod <- df_for_comparison %>% select(carb_index, subject_level, mean_carb, habitual_diet, sex, subject_id, fasted_training_pct, training_volume) 

min(df_for_mod$carb_index)
cpi_mod <- lm(log(carb_index + 2.225073) ~ subject_level + sex , df_for_mod) 

summary(cpi_mod)
check_collinearity(cpi_mod)
check_heteroscedasticity(cpi_mod)
check_model(cpi_mod)

emmeans(cpi_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()  
emmeans(cpi_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()  


# * training Emmeans -----------------------------------------------------------------

training_mod <- lm(training_volume ~ subject_level + sex, df_for_mod)  

check_model(training_mod)
check_heteroscedasticity(training_mod)

emmeans(training_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()
emmeans(training_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()


# * fasting Emmeans -----------------------------------------------------------------

fasting_mod <- lm(fasted_training_pct ~ subject_level +  sex , df_for_mod)  

summary(fasting_mod)
check_model(fasting_mod)
check_heteroscedasticity(fasting_mod)


emmeans(fasting_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()
emmeans(fasting_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()


# Number days needed ------------------------------------------------------

intake_days_fn_individ <- function(x){
  mean <- mean(x, na.rm = T)
  sd <- sd(x, na.rm = T)
  
  ((1.96^2) * (sd^2))/ ((.1^2)*(mean^2))
  
}

number_days_individ_tbl <- group_tbl_diet %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    number_days_kcal = map_dbl(data, ~intake_days_fn_individ(.x$diet_kcal_kg)),
    number_days_carb = map_dbl(data, ~intake_days_fn_individ(.x$diet_carb_g_kg)),
    number_days_protein = map_dbl(data, ~intake_days_fn_individ(.x$diet_protein_g_kg)),
    number_days_fat = map_dbl(data, ~intake_days_fn_individ(.x$diet_fat_g_kg))
  )

joined_number_days_tbl <- pci_list_tbl %>% select(subject_id, carb_index, monotony) %>% 
  left_join(number_days_individ_tbl, by = "subject_id") %>% 
  ungroup()  



index_days_cor <- cor.test(joined_number_days_tbl$carb_index, joined_number_days_tbl$number_days_carb, method = "spearman") 
index_days_rho <- round(index_days_cor$estimate,2)
index_days_rho_p <- round(index_days_cor$p.value,3)



intake_days_fn_group <- function(df, x){
  
  y <- df %>% select({{x}}, subject_id) %>% drop_na()
  mean <- mean(y[[1]], na.rm = T)
  sd <- sd(y[[1]], na.rm = T)
  
  ((1.96^2) * (sd^2))/ ((.1^2)*(mean^2)) * (1/length(unique(y[[2]])))
  
}

intake_days_fn_group(group_tbl_diet, diet_kcal_kg)



# FIGS --------------------------------------------------------------------

# *habitual diet fig carb-------------------------------------------------------

group_tbl_diet %>% 
  ggplot(aes(fct_reorder(subject_id, diet_carb_g_kg, na.rm = T), diet_carb_g_kg, 
             color = factor(subject_sex, labels = c("Female", "Male"))))+
  geom_hline(yintercept = low_carb_quantile, lty = 3, color = "grey40")+    #maybe make darker?
  geom_hline(yintercept = high_carb_quantile, lty = 3, color = "grey40")+
  geom_boxplot()+
  scale_color_manual(values = colors2red, breaks = c("Male", "Female"))+
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12, 15)) +
  labs(x = "Individual participants", y = "Dietary carbohydrate (g/kg)", color = NULL) +
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = c(0.15, 0.95),
        legend.text = element_text(size = 12),
        axis.text.x = element_blank()
  )



# *habitual diet carb protein fat ---------------------------------------------------

carb_sex <- group_tbl_diet %>% 
  ggplot(aes(fct_reorder(subject_id, diet_carb_g_kg, na.rm = T), diet_carb_g_kg, 
             color = factor(subject_sex, labels = c("Female", "Male"))))+
  geom_hline(yintercept = low_carb_quantile, lty = 3, color = "grey40")+    #maybe make darker?
  geom_hline(yintercept = high_carb_quantile, lty = 3, color = "grey40")+
  geom_boxplot()+
  scale_color_manual(values = colors2red, breaks = c("Male", "Female"))+
  scale_y_continuous(breaks = c(0,3,6,9,12,15))+
  labs(x = NULL, y = "Dietary CHO (g/kg)", color = NULL)+
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = c(0.15, 0.95),
        legend.text = element_text(size = 12),
  )


mean_mean_protein <- mean(pci_list_tbl$mean_protein)

protein_sex <- group_tbl_diet %>% 
  ggplot(aes(fct_reorder(subject_id, diet_protein_g_kg, na.rm = T), diet_protein_g_kg, 
             color = factor(subject_sex, labels = c("Female", "Male"))))+
  geom_hline(yintercept = mean_mean_protein, lty = 3, color = "grey40")+
  geom_boxplot() +
  scale_color_manual(values = colors2red, breaks = c("Male", "Female"))+
  labs(x = NULL, y = "Dietary protein (g/kg)", color = NULL)+
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none",
  )



mean_mean_fat <- mean(pci_list_tbl$mean_fat)

fat_sex <- group_tbl_diet %>% 
  ggplot(aes(fct_reorder(subject_id, diet_fat_g_kg, na.rm = T), diet_fat_g_kg, 
             color = factor(subject_sex, labels = c("Female", "Male"))))+
  geom_hline(yintercept = mean_mean_fat, lty = 3, color = "grey40")+
  geom_boxplot()+
  scale_color_manual(values = colors2red, breaks = c("Male", "Female"))+
  labs(x = NULL, y = "Dietary fat (g/kg)", color = NULL)+
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none",
  )


mean_mean_kcal <- mean(pci_list_tbl$mean_kcal)

kcal_sex <- group_tbl_diet %>% 
  ggplot(aes(fct_reorder(subject_id, diet_kcal_kg, na.rm = T), diet_kcal_kg, 
             color = factor(subject_sex, labels = c("Female", "Male"))))+
  geom_hline(yintercept = mean_mean_kcal, lty = 3, color = "grey40")+
  geom_boxplot()+
  scale_color_manual(values = colors2red, breaks = c("Male", "Female"))+
  labs(x = "Participant ID", y = "Dietary kcal/kg", color = NULL)+
  guides(color = guide_legend(nrow = 1))+
  theme(legend.position = "none",
  )


carb_sex + protein_sex + fat_sex + kcal_sex +
  plot_layout(nrow = 4) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))



# *monotony fig -----------------------------------------------------------
max_monotony_subject <- pci_list_tbl  %>% select(subject_id, data, monotony)%>% arrange(desc(monotony)) %>% 
  mutate(subject_id = as.character(subject_id),
         subject_id = as.numeric(subject_id)) %>%
  ungroup() %>%
  slice(2) %>%
  pull("subject_id")

min_monotony_subject <- pci_list_tbl  %>% select(subject_id, data, monotony) %>% arrange(monotony) %>% 
  mutate(subject_id = as.character(subject_id),
         subject_id = as.numeric(subject_id)) %>% 
  ungroup() %>% 
  slice(1) %>% 
  pull("subject_id")


monotony_tbl<- pci_list_tbl %>% select(subject_id, monotony) %>% 
  ungroup() %>% 
  mutate(
    code = case_when(
      subject_id == max_monotony_subject ~ "High",
      subject_id == min_monotony_subject ~ "Low",
      TRUE ~ "other"),
    code = factor(code)
  ) 

max_monotony <- format(round(max(monotony_tbl$monotony, na.rm = T),1), nsmall = 1)
min_monotony <- format(round(min(monotony_tbl$monotony, na.rm = T),1), nsmall = 1)
mean_monotony <- format(round(mean(monotony_tbl$monotony, na.rm = T),1), nsmall = 1)
sd_monotony <- format(round(sd(monotony_tbl$monotony, na.rm = T),1), nsmall = 1)


monotony_bars <- monotony_tbl %>% 
  ggplot(aes(fct_reorder(subject_id, monotony), monotony))+
  geom_col(aes(fill = code), show.legend = F)+
  scale_fill_manual(values = colors3)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = NULL, y = "Carbohydrate monotony\n(mean/SD))", title = "Carbohydrate Monotony")+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17))

monot_examples <- pci_list_tbl %>% 
  filter(subject_id == min_monotony_subject | subject_id == max_monotony_subject) %>% 
  mutate(
    subject_id = case_when(
      subject_id == max_monotony_subject ~ "High monotony",
      subject_id == min_monotony_subject ~ "Low monotony"),
    subject_id = factor(subject_id, levels = c("Low monotony", "High monotony"))
  ) %>% 
  unnest(data) %>% 
  ggplot(aes(study_day, diet_carb_g_kg, color = subject_id))+
  geom_point(alpha = .8, size = 2.5, show.legend = F, shape = 21, fill = "#6CC458", color= "grey30") +
  # scale_color_manual(values = colors2green)+
  scale_y_continuous(limits = c(0,10))+
  labs(x = "Study day", y = "Dietary carbohydrate\n(g/kg)", color = NULL)+
  facet_wrap(~ subject_id)+
  # facet_wrap(~ subject_id, labeller = as_labeller(monotony_facet_names))+
  JR_facet_theme2()+
  theme(
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "midnightblue"),
    axis.title.y = element_text(size = 13)
  )


monotony_bars + monot_examples  +
  plot_layout(ncol = 1) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))



# *correlation fig --------------------------------------------------------

ex_high_cor_high_range <- 33
ex_high_cor_low_range <- 25
ex_low_cor_high_range <- 16
ex_low_cor_low_range <- 51


diet_train_corr_fig  <- pci_list_tbl %>% 
  mutate(
    code = case_when(
      subject_id == ex_high_cor_high_range ~ "High correlation, high range",
      subject_id == ex_high_cor_low_range ~ "High correlation, low range",
      subject_id == ex_low_cor_high_range ~ "Low correlation, high range",
      subject_id == ex_low_cor_low_range ~ "Low correlation, low range",
      TRUE ~ "other" )
  ) %>% 
  ggplot(aes(cor_val, fct_reorder(subject_id, cor_val), color = code))+
  geom_vline(xintercept=0, linetype = "dotted") +
  geom_point(size = 2) +
  scale_colour_manual(values = colors5) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high,height = 0), linewidth = 0.6) +
  theme(legend.position = "none") +
  labs(x = "Correlation between daily CHO\nintake and training load", y = "Participants") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

diet_train_corr_fig


diet_train_corr_facet <- pci_list_tbl %>% 
  filter(subject_id == ex_high_cor_high_range | subject_id == ex_high_cor_low_range |  
           subject_id == ex_low_cor_high_range | subject_id == ex_low_cor_low_range) %>% 
  mutate(
    subject_id = case_when(
      subject_id == ex_high_cor_high_range ~ "High correlation, high range",
      subject_id == ex_high_cor_low_range ~ "High correlation, low range",
      subject_id == ex_low_cor_high_range ~ "Low correlation, high range",
      subject_id == ex_low_cor_low_range ~ "Low correlation, low range"),
    subject_id = factor(subject_id, levels = c("High correlation, low range", "High correlation, high range", "Low correlation, low range", "Low correlation, high range"))
  ) %>% 
  unnest(data) %>% 
  ggplot(aes(diet_carb_g_kg, exercise_load, fill= subject_id, color = subject_id))+
  geom_point(alpha = .85, size = 2.5, show.legend = F, shape = 21, color= "grey30") +
  geom_smooth(se = F, method = "lm", show.legend = F)+
  scale_color_manual(values = colors4)+
  scale_fill_manual(values = colors4)+
  labs(x = "Dietary carbohydrate\n(g/kg)", y = "Training load (AU)", fill = NULL)+
  facet_wrap(~ subject_id, scales = "free_y")+
  ggpubr::stat_cor(r.accuracy = 0.01, cor.coef.name = "r", aes(x = diet_carb_g_kg, y = exercise_load, label = paste(..r.label..)), color = "black",
                   label.y.npc="top", label.x.npc = "left", inherit.aes = FALSE) +
  #JR_facet_theme()+
  theme(
    strip.text = element_text(face = "bold", size = 10, color = "white"),
    strip.background = element_rect(fill = "midnightblue"),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "none"
  )


diet_train_corr_facet

diet_train_corr_fig + diet_train_corr_facet +
  plot_layout(ncol = 2,
              widths = c(1, 2)) +
  plot_annotation(title = "Diet-training correlations", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 19))



# *CTI fig -----------------------------------------------------------
highest_cpi <- round(max(pci_list_tbl$carb_index),1)
lowest_cpi <- round(min(pci_list_tbl$carb_index),1)

numb_subjects_over_1 <- pci_list_tbl %>% filter(carb_index >1) %>% nrow(.)
pct_subjects_over_1 <- round(numb_subjects_over_1/nrow(pci_list_tbl)*100,1)
numb_subjects_over_2 <- pci_list_tbl %>% filter(carb_index >2) %>% nrow(.)
pct_subjects_over_2 <- round(numb_subjects_over_2/nrow(pci_list_tbl)*100,1)
mean_cpi <- format(round(mean(pci_list_tbl$carb_index),1), nsmall = 1)
median_cpi <- format(round(median(pci_list_tbl$carb_index),1), nsmall = 1)
sd_cpi <- round(sd(pci_list_tbl$carb_index),1)


ex_high_CPI <- pci_list_tbl %>%  select(1,2,carb_index) %>% 
  ungroup() %>% 
  arrange(desc(carb_index)) %>% 
  mutate(subject_id = as.character(subject_id),
         subject_id = as.numeric(subject_id)) %>% 
  slice(1) %>% 
  pull(subject_id)


ex_low_CPI <- pci_list_tbl %>%  select(1,2,carb_index) %>% 
  ungroup() %>% 
  arrange(abs(carb_index)) %>% 
  mutate(subject_id = as.character(subject_id),
         subject_id = as.numeric(subject_id)) %>% 
  slice(3) %>% 
  pull(subject_id)

CPI_bars <- pci_list_tbl  %>% 
  mutate(
    code_CPI = case_when(
      subject_id == ex_high_CPI ~ "High",
      subject_id == ex_low_CPI ~ "Low",
      TRUE ~ "other"),
    code_CPI = factor(code_CPI, levels = c("High", "Low", "other"))) %>% 
  ggplot(aes(fct_reorder(subject_id, carb_index), carb_index))+
  geom_col(aes(fill = code_CPI), show.legend = F)+
  scale_fill_manual(values = colors3)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = NULL, y = "CTI (AU)", title = "Carbohydrate Training Index")+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17))

CPI_bars

ex1_CPI <- pci_list_tbl %>%   
  filter(subject_id == ex_low_CPI | subject_id == ex_high_CPI) %>% 
  mutate(
    subject_id = case_when(
      subject_id == ex_high_CPI ~ "High CTI",
      subject_id == ex_low_CPI ~ "Low CTI"),
    subject_id = factor(subject_id, levels = c("Low CTI", "High CTI")),
    across(c(monotony, carb_index, carb_range), ~ round(., 1)),
    cor_val = round(cor_val, 2)
  )

dat_text <- data.frame(
  label = c(paste0("Correlation: ", ex1_CPI$cor_val[1],"\n", "CHO range: ", ex1_CPI$carb_range[1], "\n", "Monotony: ", ex1_CPI$monotony[1] ), 
            paste0("Correlation: ",  ex1_CPI$cor_val[2],"\n", "CHO range: ", ex1_CPI$carb_range[2], "\n","Monotony: ", ex1_CPI$monotony[2] ) 
  ),
  subject_id   = c("Low CTI", "High CTI")
)


cpi_text <- data.frame(
  label = c(
    paste0("CTI: ",  ex1_CPI$carb_index[1]), 
    paste0("CTI: ",  ex1_CPI$carb_index[2])
  ),
  subject_id   = c("Low CTI", "High CTI")
)
cpi_text


CPI_examples <- pci_list_tbl %>% 
  filter(subject_id == ex_low_CPI | subject_id == ex_high_CPI) %>% 
  mutate(
    subject_id = case_when(
      subject_id == ex_high_CPI ~ "High CTI",
      subject_id == ex_low_CPI ~ "Low CTI"),
    subject_id = factor(subject_id, levels = c("Low CTI", "High CTI"))
  ) %>% 
  unnest(data) %>% 
  ggplot(aes(diet_carb_g_kg, exercise_load, color = subject_id, fill = subject_id))+
  geom_point(alpha = .85, size = 2.5, show.legend = F, shape = 21, color= "grey30") +
  geom_smooth(se = F, method = "lm", show.legend = F)+
  geom_text(data = dat_text, aes(x = 1.5, y = 2400, label = label), hjust   = -0.1,color = "black")+
  geom_text(data =  cpi_text, aes(x = 1.5, y = 1800, label = label), hjust   = -0.3,color = "black" , size = 5)+
  geom_text(data =  cpi_text, aes(x = 1.5, y = 1800, label = label), hjust   = -0.3,color = "black" , size = 5.05)+
  scale_color_manual(values = colors2green) +
  scale_fill_manual(values = colors2green) +
  scale_x_continuous(limits = c(1.5,10))+
  scale_y_continuous(limits = c(0, 2800))+
  labs(y = "Training load (AU)", x = "Dietary carbohydrate (g/kg)", color = NULL)+
  facet_wrap(~ factor(subject_id, levels = c("Low CTI", "High CTI")))+
  theme(
    strip.text = element_text(face = "bold", size = 11, color = "white"),
    strip.background = element_rect(fill = "midnightblue"),
    axis.title.y = element_text(size = 13),
    legend.position = "none"
  )

CPI_examples

CPI_bars + CPI_examples  +
  plot_layout(ncol = 1) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))


# * fasting -------------------------------------------------------

min_y_fasting<- min(df_for_comparison$fasted_training_pct, na.rm = T) 
max_y_fasting <- max(df_for_comparison$fasted_training_pct, na.rm = T)+20 


emmeans(fasting_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()  


fasting_by_level <- 
  df_for_comparison %>%  
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other")) %>% 
  ggplot(aes(subject_level, fasted_training_pct))+
  geom_boxplot(outlier.shape = NA, fill = "#B33951", alpha = 0.05)+
  geom_point(aes(fill = sex, shape = sex), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33)+
  scale_y_continuous(limits = c(min_y_fasting, max_y_fasting*1.15), breaks = c(0, 25, 50, 75, 100), labels = c(0, 25, 50, 75, 100)) +
  guides(shape = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))+
  labs( x =  NULL, y = "% Training days fasted", color = NULL, shape = NULL, fill = NULL)+
  theme(
    legend.position =  c(.2, .98),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17)
  )



emmeans(fasting_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()  
sex_fast_pvals <-emmeans(fasting_mod, ~sex) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble() %>% 
  mutate(p.value = round(p.value, 3)) %>% pull(p.value)

fasting_by_sex <- df_for_comparison %>%  
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other"))  %>% 
  ggplot(aes(sex, fasted_training_pct))+
  geom_boxplot(outlier.shape = NA, fill = "#B33951", alpha = 0.05)+
  geom_point(aes(fill = subject_level, shape = subject_level), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  geom_bracket(xmin = 1 , xmax = 2 , y.position = 95, vjust = -.5, inherit.aes = F, label = paste0("p = ", sex_fast_pvals)) +
  scale_y_continuous(limits = c(min_y_fasting, max_y_fasting*1.15), breaks = c(0, 25, 50, 75, 100), labels = c(0, 25, 50, 75, 100)) +
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33) +
  labs(x = NULL, y = "% Training days fasted", color = NULL, shape = NULL, fill = NULL)+
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))+
  theme(
    legend.position = c(.5, .98),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17))


fasting_by_sex


# *CPI subgroup  -----------------------------------------------------------------

min_y_cpi <- min(pci_list_tbl$carb_index) -.2
max_y_cpi <- max(pci_list_tbl$carb_index) + 2


emmeans(cpi_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()  
level_cti_pvals <- emmeans(cpi_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble(.) %>% 
  mutate(p.value = round(p.value, 3)) %>% pull(p.value)


cpi_by_level <-   df_for_comparison %>% 
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other")) %>% 
  ggplot(aes(subject_level, carb_index))+
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.15)+
  geom_point(aes(fill = sex, shape = sex), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  geom_bracket(xmin = 1 , xmax = 3 , y.position = 7, vjust = -.5, inherit.aes = F, label = paste0("p = ", level_cti_pvals[2])) +
  geom_bracket(xmin = 2 , xmax = 3 , y.position = 6, vjust = -.5, inherit.aes = F, label = paste0("p = ", level_cti_pvals[3])) +
  scale_y_continuous(limits = c(min_y_cpi, max_y_cpi))+
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33)+
  # guides(fill = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))+
  labs( x =  "Competitive level", y = "CTI (AU)", color = NULL, shape = NULL, fill = NULL)+
  theme(
    legend.position = "none", # c(.15, .97),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17)
  )

cpi_by_level

emmeans(cpi_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()  

cpi_by_sex  <- df_for_comparison %>% 
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other")) %>% 
  ggplot(aes(sex, carb_index))+
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.15)+
  geom_point(aes(fill = subject_level, shape = subject_level), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  scale_y_continuous(limits = c(min_y_cpi, max_y_cpi))+
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33) +
  # guides(fill = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))+
  labs(x = "Sex", y = "CTI (AU)", color = NULL, shape = NULL, fill = NULL)+
  theme(
    legend.position = "none", # c(.25, .97),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17))




fasting_by_level + fasting_by_sex + cpi_by_level + cpi_by_sex +  # ES_plot_cpi +
  plot_layout(ncol = 2)+
  plot_annotation(title = "", tag_levels = list(c('a', 'b', 'c', 'd'))) &
  theme(plot.tag = element_text(size = 17, face="bold"))


# * diet vs training --------------------------------------------------------------


plot_by_diet_fn_3 <- function(df, dv, iv){  
  
  my.formula1 <- y ~ poly(x, 1, raw = TRUE)
  my.formula2 <- y ~ poly(x, 2, raw = TRUE)

  remove_na <- df %>% filter(!is.na({{iv}}) & !is.na({{dv}})) %>% select({{dv}}, {{iv}}, subject_id, habitual_diet) %>% drop_na()
  
  frmla1 <- formula(paste(colnames(remove_na)[1], " ~ ", colnames(remove_na)[2], " +  (1| subject_id)", collapse = " "))
  frmla2 <- formula(paste(colnames(remove_na)[1], " ~ poly(", colnames(remove_na)[2], ",2) +  (1| subject_id)", collapse = " "))
  
  full_mod1 <- lme4::lmer(frmla1, remove_na, REML = F)
  full_mod2 <- lme4::lmer(frmla2, remove_na, REML = F)
  
  lik_test_tbl <- anova(full_mod1, full_mod2) %>% as_tibble()  %>% 
    rename(p = 'Pr(>Chisq)' ) %>% 
    mutate(p = ifelse(is.na(p), 0, p),
           best_mod_load= 1:2) %>%   #number the models
    filter(p < 0.08) %>% 
    select(-p) %>% 
    slice_tail(n = 1) 
  
  
  full_r2_1 <- format(round(MuMIn::r.squaredGLMM(full_mod1)[1], 2), nsmall = 2)
  full_r2_2 <- format(round(MuMIn::r.squaredGLMM(full_mod2)[1], 2), nsmall = 2)
  full_r2 <- ifelse(lik_test_tbl$best_mod_load == 1, full_r2_1, full_r2_2)
  
  
  diet_list_tbl <- remove_na %>%
    group_by(habitual_diet) %>% 
    nest()  %>% 
    mutate(
      load_model_1 = purrr::map(data, ~  lmerTest::lmer(frmla1, data= .x, REML = F)),
      load_model_2 = purrr::map(data, ~  lmerTest::lmer(frmla2, data= .x, REML = F)),
      anova = pmap(list(load_model_1, load_model_2), ~ lme4:::anovaLmer(..1,..2))) %>% 
    unnest(anova) %>% select(-c(npar:Df)) %>% 
    rename(p = 'Pr(>Chisq)' ) %>% 
    mutate(p = ifelse(is.na(p), 0, p),
           best_mod_load= 1:2) %>%   #number the models
    filter(p < 0.08) %>% 
    select(-p) %>% 
    slice_tail(n = 1)  %>% 
    mutate(
      RMSE_1 = map_dbl(load_model_1, sjstats::rmse),
      RMSE_2 = map_dbl(load_model_2, sjstats::rmse),
      R2mar_1 = map(load_model_1, MuMIn::r.squaredGLMM),
      R2mar_2 = map(load_model_2, MuMIn::r.squaredGLMM),
    ) %>% 
    unnest(R2mar_1:R2mar_2) 
  
  
  LC_r2 <- round(diet_list_tbl[paste0("R2mar_", diet_list_tbl$best_mod_load[1])] %>% slice(1),2)  %>% 
    as.matrix() %>% as_tibble() %>% mutate(
      across(1:2, ~round(., 2)),
      across(1, ~format(., nsmall = 2))
    ) %>% 
    pull(1)
  
  
  MC_r2 <- round(diet_list_tbl[paste0("R2mar_", diet_list_tbl$best_mod_load[2])] %>% slice(2),2)%>% 
    as.matrix() %>% as_tibble() %>% mutate(
      across(1:2, ~round(., 2)),
      across(1, ~format(., nsmall = 2))
    ) %>% 
    pull(1)
  
  HC_r2 <- round(diet_list_tbl[paste0("R2mar_", diet_list_tbl$best_mod_load[3])] %>% slice(3),2)%>% 
    as.matrix() %>% as_tibble() %>% mutate(
      across(1:2, ~round(., 2)),
      across(1, ~format(., nsmall = 2))
    ) %>% 
    pull(1)
  
  
  diet_list_tbl %>%
    unnest(data) %>%
    ggplot(aes())+
    geom_point(data = . %>% filter(habitual_diet == "Mod-CHO"), aes({{iv}}, {{dv}}), alpha=.1, color = colors332) +
    geom_smooth(data = . %>% filter(habitual_diet == "Mod-CHO"), aes({{iv}}, {{dv}}), method = "lm", color = colors332,
                se = F, formula = get(paste0("my.formula", diet_list_tbl$best_mod_load[2])))+
    geom_point(data = . %>% filter(habitual_diet == "Low-CHO"), aes({{iv}}, {{dv}}), alpha=.15, color = colors331)+
    geom_smooth(data = . %>% filter(habitual_diet == "Low-CHO"), aes({{iv}}, {{dv}}), method = "lm", color = colors331,
                se = F, formula = get(paste0("my.formula", diet_list_tbl$best_mod_load[1])))+
    geom_point(data = . %>% filter(habitual_diet == "High-CHO"), aes({{iv}}, {{dv}}), alpha=.15, color = colors333)+
    geom_smooth(data = . %>% filter(habitual_diet == "High-CHO"), aes({{iv}}, {{dv}}), method = "lm", color = colors333,
                se = F, formula = get(paste0("my.formula", diet_list_tbl$best_mod_load[3])))+
    annotate("text", x = max(remove_na[2]) *.05, y = max(remove_na[1]) *.82, hjust = 0,
             label = paste0("Low-CHO~R^2==", deparse(LC_r2)), parse=TRUE, size = 4.5,  color = colors331) +
    annotate("text", x = max(remove_na[2]) *.05, y = max(remove_na[1]) *.9, hjust = 0,
             label = paste0("Mod-CHO~R^2==", deparse(MC_r2)), parse=TRUE, size = 4.5,  color = colors332) +
    annotate("text", x = max(remove_na[2])*.05, y = max(remove_na[1])*.98, hjust = 0,
             label = paste0("High-CHO~R^2==", deparse(HC_r2)) , parse=TRUE, size = 4.5,  color = colors333) + 
    annotate("text", x = max(remove_na[2])*.05, y = max(remove_na[1])*1.07, hjust = 0,
             label = paste0("Group-level~R^2==", deparse(full_r2)) , parse=TRUE, size = 4.5,  color = "black") 
}   

# **carb vs rpe load ------------------------------------------------------------

carb_v_load <- plot_by_diet_fn_3(group_tbl_diet  %>% filter(exercise_load<5000), diet_carb_g_kg, exercise_load) +
  labs(x = "Training load (AU)", y = "Daily CHO (g/kg)")


carb_v_rpe <- plot_by_diet_fn_3(group_tbl_diet  %>% filter(exercise_load<5000), diet_carb_g_kg, exercise_RPE_weighted) +
  labs(x = "Session RPE", y = "Daily CHO (g/kg)" )

carb_v_duration <- plot_by_diet_fn_3(group_tbl_diet  %>% filter(exercise_load<5000), diet_carb_g_kg, exercise_duration_min) +
  labs(x = "Exercise duration (min)", y = "Daily CHO (g/kg)")


# **carb before duration load intensity ---------------------------------------------


pre_carb_v_load <- plot_by_diet_fn_3(group_tbl_diet %>% filter(exercise_load<5000), carb_before_g_kg, exercise_load)+
  labs(y = "Pre-exercise CHO (g/kg)", x = "Training load (AU)") 

pre_carb_v_duration <- plot_by_diet_fn_3(group_tbl_diet %>% filter(exercise_load<5000), carb_before_g_kg, exercise_duration_min)+
  labs(y = "Pre-exercise CHO (g/kg)", x = "Exercise duration (min)") 

pre_carb_v_rpe <- plot_by_diet_fn_3(group_tbl_diet  %>% filter(exercise_load<5000), carb_before_g_kg, exercise_RPE_weighted )+
  labs(y = "Pre-exercise CHO (g/kg)", x = "Session RPE") 


# ***combined diet ----------------------------------------------------------

carb_v_load + carb_v_duration + carb_v_rpe + 
  pre_carb_v_load + pre_carb_v_duration + pre_carb_v_rpe +
  plot_layout(nrow = 2) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))



# results -----------------------------------------------------------------
#added 1 row per subject to account for what had been removed during feature engineering
numb_diet_days_total <- prettyNum(group_tbl_diet %>%  filter(!is.na(diet_carb_g_kg)) %>% nrow() + length(unique(group_tbl_diet$subject_id)), big.mark = ",")
numb_diet_days_included <- prettyNum(group_tbl_diet %>%  filter(!is.na(diet_carb_g_kg)) %>% nrow() + length(unique(group_tbl_diet$subject_id)) , big.mark = ",")
numb_training_days_included <- prettyNum(group_tbl_diet %>%  filter(exercise_duration_min >0) %>% nrow() + length(unique(group_tbl_diet$subject_id)) , big.mark = ",")

days_tracked_summary <- group_tbl_diet %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  mutate(
    days_tracked = map_dbl(data, nrow),
    days_tracked = days_tracked+1
  ) %>% 
  ungroup() %>% 
  summarise(
    avg_days = mean(days_tracked),
    sd_days = sd(days_tracked)
  )

avg_days <- round(days_tracked_summary$avg_days,0)
sd_days <- round(days_tracked_summary$sd_days,0)

days_trained_summary <- group_tbl_diet %>% 
  filter(exercise_duration_min > 0) %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  mutate(
    days_trained = map_dbl(data, nrow),
    days_trained = days_trained+1
  ) %>% 
  ungroup() %>% 
  summarise(
    avg_days_trained = mean(days_trained),
    sd_days_trained = sd(days_trained)
  )

avg_days_trained <- round(days_trained_summary$avg_days_trained,0)
sd_days_trained <- round(days_trained_summary$sd_days_trained,0)



pct_male_included <- df_for_comparison %>% group_by(sex) %>% count() %>% ungroup() %>% 
  mutate(perc = round(n / sum(n) * 100,1)) %>% filter(sex == "M") %>% pull(perc)


mean_carb_low <- group_tbl_diet %>% group_by(subject_id) %>% summarise(mean_carb = round(mean(diet_carb_g_kg, na.rm = T),1)) %>% 
  arrange(mean_carb) %>% slice(1) %>% pull(mean_carb)
mean_carb_high <- group_tbl_diet %>% group_by(subject_id) %>% summarise(mean_carb = round(mean(diet_carb_g_kg, na.rm = T),1)) %>% 
  arrange(desc(mean_carb)) %>% slice(1) %>% pull(mean_carb)
max_daily_carb <- format(round(max(group_tbl_diet$diet_carb_g_kg, na.rm = T),1), nsmall = 1)
mean_range <- format(round(mean(pci_list_tbl$carb_range),1), nsmall = 1)
mean_carb_overall <-  format(round(mean(pci_list_tbl$mean_carb),1), nsmall = 1)
sd_carb_overall <- round(sd(pci_list_tbl$mean_carb),1)
mean_range_low <- format(round(min(pci_list_tbl$carb_range),1), nsmall = 1)
mean_range_high <- format(round(max(pci_list_tbl$carb_range),1), nsmall = 1)
mean_range_sd <- round(sd(pci_list_tbl$carb_range),1)

mean_protein_overall <-  format(round(mean(pci_list_tbl$mean_protein),1), nsmall = 1)
sd_protein_overall <- round(sd(pci_list_tbl$mean_protein),1)
mean_protein_low <-  format(round(min(pci_list_tbl$mean_protein),1), nsmall = 1)
mean_protein_high <- format(round(max(pci_list_tbl$mean_protein),1), nsmall = 1)
max_daily_protein <- format(round(max(group_tbl_diet$diet_protein_g_kg, na.rm = T),1), nsmall = 1)

mean_fat_overall <-  format(round(mean(pci_list_tbl$mean_fat),1), nsmall = 1)
sd_fat_overall <- round(sd(pci_list_tbl$mean_fat),1)
mean_fat_low <-  format(round(min(pci_list_tbl$mean_fat),1), nsmall = 1)
mean_fat_high <- format(round(max(pci_list_tbl$mean_fat),1), nsmall = 1)
max_daily_fat <- format(round(max(group_tbl_diet$diet_fat_g_kg, na.rm = T),1), nsmall = 1)

mean_kcal_overall <-  format(round(mean(pci_list_tbl$mean_kcal),1), nsmall = 1)
sd_kcal_overall <- round(sd(pci_list_tbl$mean_kcal),1)
mean_kcal_low <-  format(round(min(pci_list_tbl$mean_kcal),1), nsmall = 1)
mean_kcal_high <- format(round(max(pci_list_tbl$mean_kcal),1), nsmall = 1)
max_daily_kcal <- format(round(max(group_tbl_diet$diet_kcal_kg, na.rm = T),1), nsmall = 1)

min_cor <-  format(round(min(pci_list_tbl$cor_val),2), nsmall = 2)
max_cor <-  format(round(max(pci_list_tbl$cor_val),2), nsmall = 2)

fasted_mean <- format(round(mean(fsted_pct_tbl$fasted_training_pct),1), nsmall = 1)
fasted_sd <- format(round(sd(fsted_pct_tbl$fasted_training_pct),1), nsmall = 1)
fasted_max <- format(round(max(fsted_pct_tbl$fasted_training_pct),1), nsmall = 0)
fasted_min <- format(round(min(fsted_pct_tbl$fasted_training_pct),1), nsmall = 0)


training_summary <- group_tbl_diet %>% group_by(subject_id) %>% 
  summarise(traing_h = mean(subject_weekly_training_h, na.rm = T)) %>% 
  summarise(training_mean = round(mean(traing_h, na.rm = T),1),
            training_sd = round(sd(traing_h, na.rm = T),1),
            training_low = round(min(traing_h),1),
            training_high = round(max(traing_h),1)
  ) 


volume_by_level <- df_for_mod %>% 
  group_by(subject_level) %>% 
  summarise(mean = round(mean(training_volume),1), sd = round(sd(training_volume),1)) %>% 
  mutate(
    across(mean:sd, ~format(., nsmall = 1)),
    concat = paste0(mean, " ± ", sd),
    concat = str_trim(concat, "both")
  )

number_days_summary_tbl <- number_days_individ_tbl %>% 
  summarise(across(contains("number_days"), 
                   list(mean = mean, sd = sd, min = min, max = max)
  )) %>% 
  rename_with(., ~str_remove(., "number_"), everything()) %>% 
  mutate(across(everything(), round))


lmer_rm_corr_fn <- function(df, dv, iv){
  
  remove_na <- df %>% filter(!is.na({{iv}}) & !is.na({{dv}})) %>% select({{dv}}, {{iv}}, subject_id, habitual_diet) %>% drop_na()
  
  frmla1 <- formula(paste(colnames(remove_na)[1], " ~ ", colnames(remove_na)[2], " +  (1| subject_id)", collapse = " "))
  frmla2 <- formula(paste(colnames(remove_na)[1], " ~ poly(", colnames(remove_na)[2], ",2) +  (1| subject_id)", collapse = " "))
  
  full_mod1 <- lme4::lmer(frmla1, remove_na, REML = F)
  full_mod2 <- lme4::lmer(frmla2, remove_na, REML = F)
  
  lik_test_tbl <- anova(full_mod1, full_mod2) %>% as_tibble()  %>% 
    rename(p = 'Pr(>Chisq)' ) %>% 
    mutate(p = ifelse(is.na(p), 0, p),
           best_mod_load= 1:2) %>%   #number the models
    filter(p < 0.08) %>% 
    select(-p) %>% 
    slice_tail(n = 1) 
  
  
  full_r2_1 <- format(round(MuMIn::r.squaredGLMM(full_mod1)[1], 2), nsmall = 2)
  full_r2_2 <- format(round(MuMIn::r.squaredGLMM(full_mod2)[1], 2), nsmall = 2)
  full_r2 <- ifelse(lik_test_tbl$best_mod_load == 1, full_r2_1, full_r2_2)
  full_r2
}
group_carb_duration_r2 <- lmer_rm_corr_fn(group_tbl_diet, diet_carb_g_kg, exercise_duration_min)
group_carb_rpe_r2 <- lmer_rm_corr_fn(group_tbl_diet, diet_carb_g_kg, exercise_RPE_weighted)
group_carb_load_r2 <- lmer_rm_corr_fn(group_tbl_diet, diet_carb_g_kg, exercise_load)


# table ------------------------------------------------------------

library(gt)

tibble(
  value = c("Mean", "SD", "Low", "High"),
  'Kcal\n(kcal/kg)' = c(mean_kcal_overall, sd_kcal_overall, mean_kcal_low, mean_kcal_high),
  'CHO\n(g/kg)' = c(mean_carb_overall, sd_carb_overall, mean_carb_low, mean_carb_high),
  'Protein\n(g/kg)' = c(mean_protein_overall, sd_protein_overall, mean_protein_low, mean_protein_high),
  'Fat\n(g/kg)' = c(mean_fat_overall, sd_fat_overall, mean_fat_low, mean_fat_high),
  'Weekly training\n(h)' = c(training_summary$training_mean[1], training_summary$training_sd[1], training_summary$training_low[1], training_summary$training_high[1]),
  'Average session\nduration (h)' = c(mean_mean_duration_overall, sd_mean_duration_overall, mean_mean_duration_low, mean_mean_duration_high),
  'Longest session\nduration (h)' = c(mean_max_duration_overall, sd_max_duration_overall, mean_max_duration_low, mean_max_duration_high),
  'Average session RPE' = c(mean_rpe_overall, sd_rpe_overall, mean_rpe_low, mean_rpe_high),
  '% Training days fasted' = c(fasted_mean, fasted_sd, fasted_min, fasted_max)
)  %>% 
gt() 


# **3.2 -------------------------------------------------------------------

paste0(
  "The mean range of daily carbohydrate intake was ",
  mean_range, " ± ", mean_range_sd, " (range ", mean_range_low, " to ", mean_range_high, 
  ") g/kg. Carbohydrate monotony values were ",  mean_monotony, " ± ", sd_monotony, " (range ",
  min_monotony , " to ", max_monotony  ,") AU and are shown in Supplemental Figure 4 along with example participant data of low- and high-monotony diets. ", 
  "Pearson correlations between training load and daily carbohydrate intake ranged from ", min_cor, " to ", max_cor, 
  ", and are shown in Figure 2, along with examples of participants with high and low correlation values and carbohydrate ranges. ", 
  "Correlation plots for all participants are shown in Supplemental Figure 5. ",
  "The CTI scores for each participant are shown in Figure 3, along with examples of participants with high and low scores. The median CTI score was ", 
  median_cpi,  " (range ", lowest_cpi, " to ",  highest_cpi, "), with ", pct_subjects_over_1,
  "% of participants attaining a CTI of at least 1.0 and just ",  pct_subjects_over_2,  
  "% of participants attaining a CTI of at least 2.0."
)

# **3.3 -------------------------------------------------------------------

fasting_by_sex <- df_for_mod %>% group_by(sex) %>% 
  summarise(Mean=mean(fasted_training_pct), SD=sd(fasted_training_pct)) %>% 
  mutate(
    across(2:3, ~format(round(.,1)),nsmall = 1),
    concat = str_c(Mean, " ± ", SD)
  )

male_fasting <- fasting_by_sex %>% filter(sex == "M") %>% pull("concat")
female_fasting <- fasting_by_sex %>% filter(sex == "F") %>% pull("concat")

paste0(
  "The percentage of training days an athlete performed fasted training was higher for Low-CHO compared with Mod-CHO and High-CHO athletes ",
  "and for males compared with females, ", 
  "but there were no differences based on competitive level (Figure 4). ",
  
  "CTI values were higher among the highest-level compared with lower-level athletes, ",
  "but not different based on diet or sex (Figure 5). ",
  
  "Weekly training volume was higher for the highest-level (", volume_by_level$concat[3], 
  " h week) compared with the lowest-level athletes (", volume_by_level$concat[1], 
  " h week), but not different based on diet or sex (Supplemental Figure 6)."
)


# **4.2 cpi max -------------------------------------------------------------

theortical_cpi_max <- format(round(as.numeric(mean_range_high) * as.numeric(max_cor) /as.numeric(min_monotony),1), nsmall = 1)



paste0(
  "While there is no theoretical ceiling, a combination of the most extreme values observed in our data for carbohydrate range (",
  mean_range_high, " g/kg) and correlation (",  max_cor, 
  ") and monotony (", min_monotony, "), would yield a CTI of ", theortical_cpi_max, 
  ". However, these values seem unlikely to be attained in any practical scenario."
)


# carb v cti, fasting -----------------------------------------------------

carb_fasted <- df_for_comparison %>% 
  ggplot(aes(mean_carb, fasted_training_pct)) + 
  geom_smooth(method = "lm", se = T, color = colors33[3]) +
  geom_point(shape = 21, size = 2.5, fill = colors33[3])+
  ggpubr:: stat_cor(cor.coef.name = "r", p.accuracy = 0.001,
                    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Mean daily CHO intake (g/kg)", y = "% Training sessions fasted")


carb_index <- df_for_comparison %>% 
  ggplot(aes(mean_carb, carb_index)) + 
  geom_smooth(method = "lm", se = T, color = colors33[2]) +
  geom_point(shape = 21, size = 2.5, fill = colors33[2])+
  ggpubr:: stat_cor(cor.coef.name = "r", p.accuracy = 0.001,
                    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+ 
  labs(x = "Mean daily CHO intake (g/kg)", y = "CTI (AU)")


carb_volume <- df_for_comparison %>% 
  ggplot(aes(mean_carb, training_volume)) + 
  geom_smooth(method = "lm", se = T, color = colors33[1]) +
  geom_point(shape = 21, size = 2.5, fill = colors33[1])+
  ggpubr:: stat_cor(cor.coef.name = "r", p.accuracy = 0.001,
                    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Mean daily CHO intake (g/kg)", y = "Mean weekly\ntraining volume (h)")


carb_fasted + carb_index + carb_volume +
  plot_layout(nrow = 1) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))




# supplemental figs -------------------------------------------------------

# *carb v study day -------------------------------------------------------

facet_tbl <- group_tbl_diet %>% 
  group_by(subject_id) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(subject_id = as.character(subject_id),
         new_id = 1:nrow(.),
         new_id = paste0("ID ", new_id)
  ) 

facet_tbl %>% 
  unnest(data) %>% 
  mutate(
    exercise_fasted = factor(exercise_fasted, labels = c("Fed", "Fasted"))
  ) %>% 
  ggplot(aes(study_day, diet_carb_g_kg))+
  geom_point(aes(color = exercise_fasted))+
  scale_color_manual(values = colors2)+
  facet_wrap(~ fct_inorder(new_id) , ncol = 6)+
  labs(x = "Day of study", y = "Dietary carbohydrate (g/kg)", color = "Pre-exercise")+
  JR_facet_theme2()+
  theme(legend.position = "top",
        axis.title = element_text(face = "bold"),
        legend.text =  element_text(size = 12),
        legend.title =  element_text(face = "bold", size = 12)
  )




# *carb v load ------------------------------------------------------------

facet_tbl %>% 
  unnest(data) %>% 
  ggplot(aes(diet_carb_g_kg, exercise_load))+
  geom_point(aes(), alpha = .5, color = colors333)+
  ggpubr::stat_cor(r.accuracy = 0.01, cor.coef.name = 'r', aes(label = paste(..r.label..)))+
  geom_smooth(se = F, method = "lm")+
  facet_wrap(~ fct_inorder(new_id) , ncol = 6)+
  JR_facet_theme2()+
  labs(x = "Dietary carbohydrate (g/kg)", y = "Training load (AU)", color = "Pre-exercise")+
  theme(
    axis.title = element_text(face = "bold"),
  )



# * training volume -------------------------------------------------------

min_y_hours <- min(group_tbl_diet$subject_weekly_training_h, na.rm = T) *.9
max_y_hours <- max(group_tbl_diet$subject_weekly_training_h, na.rm = T) *1.1


emmeans(training_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble()  
level_volume_pvals <-emmeans(training_mod, ~subject_level) %>% pairs(adjust = "Holm", infer = T) %>% as_tibble() %>% 
  mutate(p.value = round(p.value, 3)) %>% pull(p.value)

hours_by_level <- 
  df_for_comparison %>%  
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other")) %>% 
  ggplot(aes(subject_level, training_volume))+
  geom_boxplot(outlier.shape = NA)+
  # geom_point(aes(color = sex, shape = sex), size = 1.8, alpha = .95, position = position_jitter(height = 0, width = 0.1, seed = 1))+
  geom_bracket(xmin = 1 , xmax = 3 , y.position = 21, vjust = -.5, inherit.aes = F, label = paste0("p = ", level_volume_pvals[2])) +
  # scale_color_manual(values = colors33)+
  geom_point(aes(fill = subject_level, shape = subject_level), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33) +
  scale_y_continuous(limits = c(min_y_hours, max_y_hours)) +
  guides(color = guide_legend(nrow = 1))+
  labs( x =  "Competitive level", y = "Weekly training (h)", color = NULL, shape = NULL, fill = NULL)+
  theme(
    legend.position = "none", #c(.2, .92),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 17)
  )


emmeans(training_mod, ~sex) %>% pairs(adjust = "Holm", infer = T)  %>% as_tibble()  

hours_by_sex <- df_for_comparison %>%  
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    TRUE ~ "other"))  %>% 
  ggplot(aes(sex, training_volume))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(fill = subject_level, shape = subject_level), color = "black", size = 2.5, alpha = .8, position = position_jitter(height = 0, width = 0.2, seed = 1))+
  scale_shape_manual(values = c(21, 22,24)) +
  scale_fill_manual(values = colors33) +
  # geom_point(aes(color = sex, shape = sex), size = 1.8, alpha = .95, position = position_jitter(height = 0, width = 0.1, seed = 1))+
  scale_y_continuous(limits = c(min_y_hours, max_y_hours))+
  # scale_color_manual(values = colors33)+
  labs(x = "Sex", y = "Weekly training (h)", color = NULL, shape = NULL)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 17))



hours_by_level + hours_by_sex + 
  plot_layout(ncol = 3)+
  plot_annotation(title = "", tag_levels = list(c('a', 'b', 'c', 'd'))) &
  theme(plot.tag = element_text(size = 17, face="bold"))



# * fasting count -----------------------------------------------
#at least 1x/wk

fasting_comparison <- df_for_comparison %>% 
  select(subject_id, habitual_diet:fasted_training_pct) %>% 
  mutate(
    regular_fasting = ifelse(fasted_training_pct > 15, "yes", "no")
  ) 


fasting_count_tbl <- fasting_comparison %>% 
  count(regular_fasting) %>% 
  mutate(pct = n/sum(n)*100)

overall_fasting_pct <- round(fasting_count_tbl$pct[2],0)


# *day before after --------------------------------------------------------

yesterday_carb_v_load <- plot_by_diet_fn_3(group_tbl_diet, lag1_diet_carb_g_kg, exercise_load)+
  labs(x = "Training load (AU)", y = "Prior day CHO (g/kg)")

carb_v_load <- plot_by_diet_fn_3(group_tbl_diet, diet_carb_g_kg, exercise_load) +
  labs(x = "Training load (AU)", y = "Training day CHO (g/kg)")

tomorrow_carb_v_load <- plot_by_diet_fn_3(group_tbl_diet, diet_carb_g_kg, lag1_exercise_load)+
  labs(x = "Training load (AU)",  y = "Next day CHO (g/kg)")

yesterday_carb_v_load + carb_v_load + tomorrow_carb_v_load +
  plot_layout(nrow = 1) +
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))



