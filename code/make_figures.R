
library(tidyverse)
library(viridis)
## Table 3 analysis
table3 <- read_csv("~/repos/aea_survey/data/table3-experienced-discrimination-by-type.csv")
table3_question <- colnames(table3)[1]
colnames(table3)[1] <- "Question"
table3 <- table3 %>% gather(group, value, -Question) 

#Gender discrimination
female <- table3 %>% filter(group == "Female") 
table3$Question <- factor(table3$Question, levels = levels(reorder(female$Question, female$value)))
ggplot(data = table3 %>% filter(group == "Male" | group == "Female")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name="Gender") + 
  labs(x = "", y= "", title = table3_question) +
  theme_minimal() + 
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/gender_discrimination.pdf")


#Race discrimination
nonwhite <- table3 %>% filter(group == "Non-White") 
table3$Question <- factor(table3$Question, levels = levels(reorder(nonwhite$Question, nonwhite$value)))
ggplot(data = table3 %>% filter(group == "Non-White" | group == "White")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Race/Ethnicity") + 
  labs(x = "", y= "", title = table3_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/race_discrimination.pdf")


#Sexuality discrimination
nonhetero <- table3 %>% filter(group == "Other sexual orientation") 
table3$Question <- factor(table3$Question, levels = levels(reorder(nonhetero$Question, nonhetero$value)))
ggplot(data = table3 %>% filter(group == "Other sexual orientation" | group == "Heterosexual") %>%
         mutate(group = factor(group, levels = c("Other sexual orientation","Heterosexual")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Sexual Orientation") + 
  labs(x = "", y= "", title = table3_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/sexuality_discrimination.pdf")


#Disability discrimination
disability <- table3 %>% filter(group == "With disability") 
table3$Question <- factor(table3$Question, levels = levels(reorder(disability$Question, disability$value)))
ggplot(data = table3 %>% filter(group == "With disability" | group == "No disability") %>%
         mutate(group = factor(group, levels = c("With disability","No disability")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Disability") + 
  labs(x = "", y= "", title = table3_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/disability_discrimination.pdf")


## Table 4 analysis
table4 <- read_csv("~/repos/aea_survey/data/table4-discrimination-as-student.csv")
table4_question <- colnames(table4)[1]
colnames(table4)[1] <- "Question"
table4 <- table4 %>% gather(group, value, -Question) 
table4 <- table4 %>% mutate(color_group = case_when(group == "All" ~ "black",
                                                    group == "Male" | group == "White" | group == "No disability" | group == "Heterosexual" ~ viridis(2)[1],
                                                    TRUE ~ viridis(2)[2]))
all <- table4 %>% filter(group == "All") 
table4$Question <- factor(table4$Question, levels = levels(reorder(all$Question, all$value)))
table4$group <- factor(table4$group, levels = c("All","Male","Female","White","Non-White","No disability",
                                                "With disability","Heterosexual","Other sexual orientation"))
ggplot(data = table4) +
  geom_col(aes(y = value, x = group, fill = color_group), position= position_dodge2()) +
  facet_wrap(~Question, ncol=1) +
  coord_flip() + 
  scale_fill_viridis_d(guide = FALSE) + 
  labs(x = "", y= "", title = "During your time as a student studying economics, \nhave you personally experienced discrimination or unfair treatment with regard to:") +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/student_discrimination.pdf")

# Table 5 Analysis
table5 <- read_csv("~/repos/aea_survey/data/table5-experiences-of-discrimination-in-academia.csv")
table5_question <- colnames(table5)[1]
colnames(table5)[1] <- "Question"
table6 <- read_csv("~/repos/aea_survey/data/table6-discrimination-outside-academia.csv")
table6_question <- colnames(table6)[1]
colnames(table6)[1] <- "Question"
table6 <- table6 %>% gather(group, value, -Question) 
table5 <- table5 %>% gather(group, value, -Question)
table6 <- table6 %>% mutate(Academia = "Outside Academia")
table5 <- table5 %>% mutate(Academia = "In Academia")
table56 <- table5  %>% bind_rows(table6)

#Gender discrimination
female <- table5 %>% filter(group == "Female") 
table5$Question <- factor(table5$Question, levels = levels(reorder(female$Question, female$value)))
ggplot(data = table5 %>% filter(group == "Male" | group == "Female")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name="Gender") + 
  labs(x = "", y= "", title = "Have you personally experienced discrimination \nor unfair treatment with regard to:") +
  theme_minimal() + 
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/gender_discrimination_table5.pdf")



#Race discrimination
nonwhite <- table5 %>% filter(group == "Non-White") 
table5$Question <- factor(table5$Question, levels = levels(reorder(nonwhite$Question, nonwhite$value)))
ggplot(data = table5 %>% filter(group == "Non-White" | group == "White")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Race/Ethnicity") + 
  labs(x = "", y= "", title = "Have you personally experienced discrimination \nor unfair treatment with regard to:") +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/race_discrimination_table5.pdf")


#Sexuality discrimination
nonhetero <- table5 %>% filter(group == "Other sexual orientation") 
table5$Question <- factor(table5$Question, levels = levels(reorder(nonhetero$Question, nonhetero$value)))
ggplot(data = table5 %>% filter(group == "Other sexual orientation" | group == "Heterosexual") %>%
         mutate(group = factor(group, levels = c("Other sexual orientation","Heterosexual")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Sexual Orientation") + 
  labs(x = "", y= "", title = "Have you personally experienced discrimination \nor unfair treatment with regard to:") +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom") 
ggsave("~/repos/aea_survey/graphs/sexuality_discrimination_table5.pdf")


#Disability discrimination
disability <- table5 %>% filter(group == "With disability") 
table5$Question <- factor(table5$Question, levels = levels(reorder(disability$Question, disability$value)))
ggplot(data = table5 %>% filter(group == "With disability" | group == "No disability") %>%
         mutate(group = factor(group, levels = c("With disability","No disability")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Disability") + 
  labs(x = "", y= "", title = "Have you personally experienced discrimination \nor unfair treatment with regard to:") +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/disability_discrimination_table5.pdf")

ggplot(data = table56 %>% filter(Question == "Promotion decisions" | Question == "Compensation" | Question == "Publishing decisions")) +
    geom_point(aes(y = value, x = group, color = Academia)) +
    coord_flip() +
    facet_wrap(~Question, ncol = 1) +
    scale_color_viridis_d() + 
    labs(x = "", y= "", title = "Experiences of Discrimination in...") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(0,0.4)
ggsave("~/repos/aea_survey/graphs/academia_vs_nonacademia.pdf")

## Table 7 analysis
table7 <- read_csv("~/repos/aea_survey/data/table7-avoiding-harrassment.csv")
table7_question <- colnames(table7)[1]
table7_question <- "Have you ever done any of the following \n to avoid possible harassment, discrimination, \n or unfair or disrespectful treatment:"
colnames(table7)[1] <- "Question"
table7 <- table7 %>% gather(group, value, -Question) 


#Gender
female <- table7 %>% filter(group == "Female") 
table7$Question <- factor(table7$Question, levels = levels(reorder(female$Question, female$value)))
ggplot(data = table7 %>% filter(group == "Male" | group == "Female")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name="Gender") + 
  labs(x = "", y= "", title = table7_question) +
  theme_minimal() + 
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/gender_discrimination_table7.pdf")



#Race discrimination
nonwhite <- table7 %>% filter(group == "Non-White") 
table7$Question <- factor(table7$Question, levels = levels(reorder(nonwhite$Question, nonwhite$value)))
ggplot(data = table7 %>% filter(group == "Non-White" | group == "White")) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Race/Ethnicity") + 
  labs(x = "", y= "", title = table7_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/race_discrimination_table7.pdf")


#Sexuality discrimination
nonhetero <- table7 %>% filter(group == "Other sexual orientation") 
table7$Question <- factor(table7$Question, levels = levels(reorder(nonhetero$Question, nonhetero$value)))
ggplot(data = table7 %>% filter(group == "Other sexual orientation" | group == "Heterosexual") %>%
         mutate(group = factor(group, levels = c("Other sexual orientation","Heterosexual")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Sexual Orientation") + 
  labs(x = "", y= "", title =  table7_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom") 
ggsave("~/repos/aea_survey/graphs/sexuality_discrimination_table7.pdf")


#Disability discrimination
disability <- table7 %>% filter(group == "With disability") 
table7$Question <- factor(table7$Question, levels = levels(reorder(disability$Question, disability$value)))
ggplot(data = table7 %>% filter(group == "With disability" | group == "No disability") %>%
         mutate(group = factor(group, levels = c("With disability","No disability")))) +
  geom_col(aes(y = value, x = Question, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Disability") + 
  labs(x = "", y= "", title =  table7_question) +
  theme_minimal() +
  ylim(0,0.5) +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/disability_discrimination_table7.pdf")


#Table 12 -- By Field
table12 <- read_csv("~/repos/aea_survey/data/table12-experience-harrassment-by-field.csv")
colnames(table12)[1] <- "Field"
table12 <- table12 %>% gather(group, value, -Field) %>%
  separate(group, into=c("group", "experience"), sep=":") %>%
  mutate(experience = str_trim(experience))

### Social Exclusion
excluded <- table12 %>% filter(experience == "Socially excluded" & group == "Women only") 
table12$Field <- factor(table12$Field, levels = levels(reorder(excluded$Field, excluded$value)))
ggplot(data = table12 %>% filter(experience == "Socially excluded"))+
  geom_col(aes(y = value, x = Field, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Gender") + 
  labs(x = "", y= "", title =  "Social Exclusion By Field") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/harassment_by_field_socialexclusion.pdf")


### Disrespected
excluded <- table12 %>% filter(experience == "Disrespected" & group == "Women only") 
table12$Field <- factor(table12$Field, levels = levels(reorder(excluded$Field, excluded$value)))
ggplot(data = table12 %>% filter(experience == "Disrespected"))+
  geom_col(aes(y = value, x = Field, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Gender") + 
  labs(x = "", y= "", title =  "Disrespected By Field") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/harassment_by_field_disrespected.pdf")


### Work not taken seriously
excluded <- table12 %>% filter(experience == "Work not taken seriously" & group == "Women only") 
table12$Field <- factor(table12$Field, levels = levels(reorder(excluded$Field, excluded$value)))
ggplot(data = table12 %>% filter(experience == "Work not taken seriously"))+
  geom_col(aes(y = value, x = Field, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Gender") + 
  labs(x = "", y= "", title =  "Work not taken seriously By Field") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/harassment_by_field_notseriously.pdf")



### Subject/methods not taken seriously
excluded <- table12 %>% filter(experience == "Subject/methods not taken seriously" & group == "Women only") 
table12$Field <- factor(table12$Field, levels = levels(reorder(excluded$Field, excluded$value)))
ggplot(data = table12 %>% filter(experience == "Subject/methods not taken seriously"))+
  geom_col(aes(y = value, x = Field, fill = group), position= position_dodge2()) +
  coord_flip() + 
  scale_fill_viridis_d(name = "Gender") + 
  labs(x = "", y= "", title =  "Subject/methods not taken seriously not taken seriously by field") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("~/repos/aea_survey/graphs/harassment_by_field_subjectnotseriously.pdf")


table12_explicit <- table12 %>% filter(group == "Women only" &
                                         experience != "Subject/methods not taken seriously" &
                                         experience != "Work not taken seriously" &
                                         experience != "Disrespected" &
                                         experience != "Socially excluded")
### Explicit Harassment
excluded <- table12_explicit %>% filter(experience == "Attempted Assault") 
table12_explicit$experience <- factor(table12_explicit$experience, 
                                      levels = c("Inappropriate material/ language/gestures",
                                                 "Unwanted advances",
                                                 "Threatened with retaliation",
                                                 "Attempted Assault", "Assaulted", "Other touching",
                                                 "Stalked"))
table12_explicit$Field <- factor(table12_explicit$Field, 
                                 levels = levels(reorder(excluded$Field, excluded$value)))
ggplot(data = table12_explicit)+
  geom_col(aes(y = value, x = Field), position= position_dodge2()) +
  facet_wrap(~experience, scales="free_x") +
  coord_flip() + 
  scale_fill_viridis_d(name = "Experience") + 
  labs(x = "", y= "", title =  "Explicit harassment experienced by women  by field") +
  theme_minimal() +
  theme(strip.text.x = element_text(size=8))

ggsave("~/repos/aea_survey/graphs/harassment_by_field_explicitharassment.pdf", 
       width=11, height=8, unit = "in")
