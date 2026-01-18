## Figures made on  R version 3.5.1 ##

##### Install packages #####
# Skip this if you already have installed them
install.packages(c("ggrepel", "ggplot2", "tidyverse", "ggalluvial", "RColorBrewer"))

##### Load libraries ####
library(ggrepel)
library(ggplot2)
library(tidyverse)
library(ggalluvial)
library(RColorBrewer)


#### Load the datasets ####
banff_discrepancy_adult <- read.csv("banff_discrepancy_adult.csv")
banff_discrepancy_pediatric <- read.csv("banff_discrepancy_pediatric.csv")


banff_discrepancy_adult <- banff_discrepancy_adult %>% as_tibble() %>% select(-1)
banff_discrepancy_pediatric <- banff_discrepancy_pediatric %>% as_tibble() %>% select(-1)


##### Figure 3A - Sankey diagram for adult cohorts #####
# Transform the dataset
banff_discrepancy_adult %>% 
    rowid_to_column() %>% 
    rename(biopsy_id = rowid) %>% 
    gather(key = type, value = diagnosis, 
           pathologist_diagnosis:application_diagnosis) -> banff_discrepancy_sankey_adult

# Remove No specific diagnosis by Pathologist
banff_discrepancy_sankey_adult %>% 
    filter(type %in% "pathologist_diagnosis" & diagnosis %in% "No specific diagnosis") %>% pull(biopsy_id) -> normal_pathologist_biopsies_adult
banff_discrepancy_sankey_adult %>% 
    filter(!biopsy_id %in% normal_pathologist_biopsies_adult) -> rmnormal_banff_discrepancy_adult

# Calculate the percentage - this value will be added up by the count of the diagnosis group
rmnormal_banff_discrepancy_adult %>% 
    group_by(type, diagnosis) %>% 
    count() %>% 
    group_by(type) %>% 
    mutate(pct = n / sum(n),
           pct = pct / n) %>% 
    select(-n) -> pct_result_adult


# Merge the percentage and order the category of diagnosis in order from Active AMR to Other diagnoses, for clean visualization.
rmnormal_banff_discrepancy_adult %>% 
    left_join(pct_result_adult, by = c("type" = "type", "diagnosis" = "diagnosis")) %>%
    mutate(type = factor(type, levels = c("pathologist_diagnosis", "application_diagnosis"),
                         labels = c("Pathologist", "Banff Automation System")),
           diagnosis = factor(diagnosis, 
                              levels = c("aAMR", "caAMR", "ciAMR", 
                                         "aTCMR", "caTCMR", "Mixed",
                                         "BL", "SAMR", 
                                         "No specific diagnosis"),
                              labels = c("Active AMR", "Chronic active AMR", "Chronic inactive AMR",
                                         "Acute TCMR", "Chronic active TCMR", "Mixed",
                                         "Borderline", "Equivocal AMR",
                                         "Other diagnoses"))) -> banff_discrepancy_sankey_rm_adult


# Color scheme
AMR_colors <- brewer.pal(n = 8, name = "Reds")[6:8]
TCMR_colors <- brewer.pal(n = 8, name = "Blues")[7:8]
equivocal_colors <- brewer.pal(n = 8, name = "Greens")[6:8]
mixed_color <- brewer.pal(n = 8, name = "Purples")[8]
other_colors <- brewer.pal(n = 8, name = "Greys")[7:8]

diagnosis_names <- banff_discrepancy_sankey_rm_adult$diagnosis %>% levels()

names(AMR_colors) <- diagnosis_names[c(1:3)]
names(TCMR_colors) <- diagnosis_names[4:5]
names(mixed_color) <- diagnosis_names[6]
names(equivocal_colors) <- diagnosis_names[c(7, 8, 9)]
names(other_colors) <- c(diagnosis_names[9], "No rejection")
color_whole <- c(AMR_colors, TCMR_colors, mixed_color, equivocal_colors, other_colors)



# Graph
ggplot(banff_discrepancy_sankey_rm_adult,
       aes(x = type, stratum = diagnosis, alluvium = biopsy_id,
           y = pct,
           fill = diagnosis,
           label = diagnosis)) +
    scale_x_discrete(expand = c(0.4, 0.4)) +
    scale_y_continuous(label = scales::percent_format()) + 
    xlab("Diagnosis") + 
    ylab("Percentage") +
    geom_flow(alpha = 0.7, width = 1/2 - 0.1) +
    geom_stratum(alpha = 0.7, width = 1/2 - 0.1) +
    geom_text_repel(data = banff_discrepancy_sankey_rm_adult %>% filter(type %in% "Pathologist"),
                    aes(label = paste0(..stratum.., "\n(n=", n, ", ", scales::percent(..count.., accuracy = 0.1), ")")), 
                    stat = "stratum", size = 4,
                    direction = "both", nudge_x = -0.5) +
    geom_text_repel(data = banff_discrepancy_sankey_rm_adult %>% filter(type %in% "Banff Automation System"), 
                    aes(label = paste0(..stratum.., "\n(n=", n, ", ", scales::percent(..count.., accuracy = 0.1), ")")), 
                    stat = "stratum", size = 4,
                    direction = "both", nudge_x = 0.5) +
    theme_minimal() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25)) +
    scale_fill_manual(values = color_whole) -> p_adult

p_adult


##### Figure 3B - Sankey diagram for pediatric cohorts #####
# Transform the dataset
banff_discrepancy_pediatric %>% 
    rowid_to_column() %>% 
    rename(biopsy_id = rowid) %>% 
    gather(key = type, value = diagnosis, 
           pathologist_diagnosis:application_diagnosis) -> banff_discrepancy_sankey_pediatric

# Remove No specific diagnosis by Pathologist
banff_discrepancy_sankey_pediatric %>% 
    filter(type %in% "pathologist_diagnosis" & diagnosis %in% "No specific diagnosis") %>% pull(biopsy_id) -> normal_pathologist_biopsies_pediatric
banff_discrepancy_sankey_pediatric %>% 
    filter(!biopsy_id %in% normal_pathologist_biopsies_pediatric) -> rmnormal_banff_discrepancy_pediatric

# Calculate the percentage - this value will be added up by the count of the diagnosis group
rmnormal_banff_discrepancy_pediatric %>% 
    group_by(type, diagnosis) %>% 
    count() %>% 
    group_by(type) %>% 
    mutate(pct = n / sum(n),
           pct = pct / n) %>% 
    select(-n) -> pct_result_pediatric


# Merge the percentage and order the category of diagnosis in order from Active AMR to Other diagnoses, for clean visualization.
rmnormal_banff_discrepancy_pediatric %>% 
    left_join(pct_result_pediatric, by = c("type" = "type", "diagnosis" = "diagnosis")) %>%
    mutate(type = factor(type, levels = c("pathologist_diagnosis", "application_diagnosis"),
                         labels = c("Pathologist", "Banff Automation System")),
           diagnosis = factor(diagnosis, 
                              levels = c("aAMR", "caAMR", "ciAMR", 
                                         "aTCMR", "caTCMR", "Mixed",
                                         "BL", "SAMR", 
                                         "No specific diagnosis"),
                              labels = c("Active AMR", "Chronic active AMR", "Chronic inactive AMR",
                                         "Acute TCMR", "Chronic active TCMR", "Mixed",
                                         "Borderline", "Equivocal AMR",
                                         "Other diagnoses"))) -> banff_discrepancy_sankey_rm_pediatric


# Color scheme
AMR_colors <- brewer.pal(n = 8, name = "Reds")[6:8]
TCMR_colors <- brewer.pal(n = 8, name = "Blues")[7:8]
equivocal_colors <- brewer.pal(n = 8, name = "Greens")[6:8]
mixed_color <- brewer.pal(n = 8, name = "Purples")[8]
other_colors <- brewer.pal(n = 8, name = "Greys")[7:8]

diagnosis_names <- banff_discrepancy_sankey_rm_pediatric$diagnosis %>% levels()

names(AMR_colors) <- diagnosis_names[c(1:3)]
names(TCMR_colors) <- diagnosis_names[4:5]
names(mixed_color) <- diagnosis_names[6]
names(equivocal_colors) <- diagnosis_names[c(7, 8, 9)]
names(other_colors) <- c(diagnosis_names[9], "No rejection")
color_whole <- c(AMR_colors, TCMR_colors, mixed_color, equivocal_colors, other_colors)


# Graph
ggplot(banff_discrepancy_sankey_rm_pediatric,
       aes(x = type, stratum = diagnosis, alluvium = biopsy_id,
           y = pct,
           fill = diagnosis,
           label = diagnosis)) +
    scale_x_discrete(expand = c(0.4, 0.4)) +
    scale_y_continuous(label = scales::percent_format()) + 
    xlab("Diagnosis") + 
    ylab("Percentage") +
    geom_flow(alpha = 0.7, width = 1/2 - 0.1) +
    geom_stratum(alpha = 0.7, width = 1/2 - 0.1) +
    geom_text_repel(data = banff_discrepancy_sankey_rm_pediatric %>% filter(type %in% "Pathologist"),
                    aes(label = paste0(..stratum.., "\n(n=", n, ", ", scales::percent(..count.., accuracy = 0.1), ")")), 
                    stat = "stratum", size = 4,
                    direction = "both", nudge_x = -0.5) +
    geom_text_repel(data = banff_discrepancy_sankey_rm_pediatric %>% filter(type %in% "Banff Automation System"), 
                    aes(label = paste0(..stratum.., "\n(n=", n, ", ", scales::percent(..count.., accuracy = 0.1), ")")), 
                    stat = "stratum", size = 4,
                    direction = "both", nudge_x = 0.5) +
    theme_minimal() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 20),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25)) +
    scale_fill_manual(values = color_whole) -> p_pediatric

p_pediatric


