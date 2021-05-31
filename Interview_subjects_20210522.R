library(tidyverse)
library(readxl)
library(ggsci) # Nature  Reviews color scale
# library(lettercase)
# library(gridExtra)

## Read interview subject data into R and 
interview_subjects <- data.frame(read_excel("C:/Users/u0078315/Dropbox (Personal)/Successful Life Scientist/Interviews/PowerBI analysis/106_permanent.xlsx"))
## Tidy up column names
names(interview_subjects) <- names(interview_subjects) %>% 
                              str_replace_all("\\.", "_") %>%
                              str_replace_all("\\_$", "") %>%
                              str_replace_all("\\__", "_")

# View(interview_subjects)
# class(interview_subjects)
# names(interview_subjects)

## keep only relevant variables, filter for completed reviews 
## of tenured or tenure-track subjects
## change "," to "." in PI_years, so it can be used at numeric later
tenured_interviewed <- interview_subjects %>% 
  select("No", "Academic_rank", "Institute",
   "Country", "Continent", "PI_years", "Postdoc_years",
   "h_index_Scopus","Documents_Scopus",
   "Citations_Scopus_self_citations_excluded",
   "Citing_documents_Scopus", "Co_authors",
   "MD", "PhD", "X2nd_PhD", "PharmD",
   "Ir_D", "DSc", "Gender",
   "Completed") %>%
    filter(!is.na(Completed) & 
             !Completed == "no" &
             Academic_rank != "not tenured") %>%
    mutate(PI_years = str_replace_all(PI_years, ",", ".")) %>%
    mutate(PI_years = as.numeric(PI_years)) %>%
    mutate(Postdoc_years = str_replace_all(Postdoc_years, ",", ".")) %>%
    mutate(Postdoc_years = as.numeric(Postdoc_years)) %>%
    mutate(h_index_Scopus = as.numeric(h_index_Scopus)) %>%
    mutate(Documents_Scopus = as.numeric(Documents_Scopus)) %>%
    mutate(Citations_Scopus_self_citations_excluded = as.numeric(Citations_Scopus_self_citations_excluded)) %>%
    mutate(Citing_documents_Scopus = as.numeric(Citing_documents_Scopus)) %>%
    mutate(Co_authors = as.numeric(Co_authors))

# View(tenured_interviewed)

#SUMMARIES
tenured_interviewed %>%
  summarise(median_year_postdoc = median(Postdoc_years, na.rm = TRUE),
            min = min(Postdoc_years, na.rm = TRUE),
            max = max(Postdoc_years, na.rm = TRUE))

tenured_interviewed %>%
  summarise(median_year_pi = median(PI_years, na.rm = TRUE),
            min = min(PI_years, na.rm = TRUE),
            max = max(PI_years, na.rm = TRUE))

tenured_interviewed %>%
  summarise(median_h_index = median(h_index_Scopus, na.rm = TRUE),
            min = min(h_index_Scopus, na.rm = TRUE),
            max = max(h_index_Scopus, na.rm = TRUE))

tenured_interviewed %>%
  summarise(median_documents = median(Documents_Scopus, na.rm = TRUE),
            min = min(Documents_Scopus, na.rm = TRUE),
            max = max(Documents_Scopus, na.rm = TRUE))

tenured_interviewed %>%
  summarise(median_citations = median(Citations_Scopus_self_citations_excluded, na.rm = TRUE),
            min = min(Citations_Scopus_self_citations_excluded, na.rm = TRUE),
            max = max(Citations_Scopus_self_citations_excluded, na.rm = TRUE))

tenured_interviewed %>%
  summarise(median_citing_documents = median(Citing_documents_Scopus, na.rm = TRUE),
            min = min(Citing_documents_Scopus, na.rm = TRUE),
            max = max(Citing_documents_Scopus, na.rm = TRUE))

tenured_interviewed %>% group_by(Gender) %>% 
  summarise(median_Postdoc_years = median(Postdoc_years, na.rm = TRUE),
            mean_Postdoc_years = mean(Postdoc_years, na.rm = TRUE),
            min = min(Postdoc_years, na.rm = TRUE),
            max = max(Postdoc_years, na.rm = TRUE))

# Country count
length(unique(tenured_interviewed$Country))
sort(unique(tenured_interviewed$Country))

# Continent count
length(unique(tenured_interviewed$Continent))
sort(unique(tenured_interviewed$Continent))

# Research Institution count
length(unique(tenured_interviewed$Institute))
sort(unique(tenured_interviewed$Institute))


## Set theme variables to make titles, labels, etc. 
## all size 15 for all subsequent plots 
theme_update(axis.title.y = element_text(size = 15),
             axis.title.x = element_text(size = 15),
                  axis.text.x  = element_text(size = 15),
                  axis.text.y  = element_text(size = 15),
                  legend.title = element_text(size = 15),
                  legend.text = element_text(size = 15))

## COUNT BAR PLOTS
# theme_update(axis.text.x = element_text(size = 11))
tenured_interviewed %>% ggplot(aes(x = Academic_rank, fill = Gender)) + 
  geom_bar() +
  scale_fill_npg() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))
  
tenured_interviewed %>% ggplot(aes(x = Gender, fill = Gender)) + 
  geom_bar(show.legend = FALSE) +
  scale_fill_npg() +
  xlab("")

# Boxplot of years spent as PI
tenured_interviewed %>% 
  ggplot(aes(Academic_rank, as.numeric(PI_years), fill = Academic_rank)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "Years as PI") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

# DENSITY PLOT h-index faceted BY Gender
tenured_interviewed %>% 
  mutate(Gender = factor(Gender, levels = c("male", "female"))) %>%
  ggplot() + 
  geom_density(aes(x = h_index_Scopus, y = ..count..,
                   fill = Gender), alpha = 0.7, bw = 6) +
  expand_limits(x = 0) +
  labs(x = "h-index (Scopus)", y = "Density") +
  scale_fill_manual(values=c("#00CDCD", "#EE3B3B"))

# BOX PLOT h-index faceted BY academic rank
tenured_interviewed %>% 
  ggplot(aes(x = Academic_rank, y= h_index_Scopus, fill = Academic_rank)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  xlab("") +
  labs(x = "", y = "h-index (Scopus)") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

# HISTOGRAMS of h-index, documents, citations, citing documents
tenured_interviewed %>% ggplot(aes(h_index_Scopus, fill = "#00CDCD")) + 
  geom_histogram(binwidth = 7, show.legend = FALSE) +
  expand_limits(x = 0, y = c(0, 16)) +
  scale_fill_npg() +
  labs(x = "h-index (Scopus)")

tenured_interviewed %>% 
  ggplot(aes(Documents_Scopus, fill = "#00CDCD")) + 
  geom_histogram(binwidth = 1, show.legend = FALSE) +
  expand_limits(x = 3, y = c(0, 16)) +
  scale_fill_npg() +
  labs(x = "Documents")+
  scale_x_continuous(trans = "log2")

tenured_interviewed %>% 
  ggplot(aes(Citations_Scopus_self_citations_excluded, fill = "#00CDCD")) + 
  geom_histogram(binwidth = 1, show.legend = FALSE) +
  expand_limits(x = 512, y = c(0, 16)) +
  scale_fill_npg() +
  labs(x = "Citations (self-citations excluded)") +
  scale_x_continuous(trans = "log2")

tenured_interviewed %>% 
  ggplot(aes(x = Citing_documents_Scopus, fill = "#00CDCD")) + 
  geom_histogram(binwidth = 1, show.legend = FALSE) +
  expand_limits(x = 512, y = c(0, 16)) +
  scale_fill_npg() +
  labs(x = "Citing documents") +
  scale_x_continuous(trans = "log2")

# COLUMN PLOT of institutes alphabetically 
tenured_interviewed %>% group_by(Institute) %>%
  summarise(n = n()) %>%
  mutate(Institute = reorder(Institute, desc(n))) %>%
  ggplot(aes(Institute, n, fill = "#00CDCD")) +
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "Interview subjects") +
  theme(axis.text.y  = element_text(size = 6)) +
  scale_fill_npg() +
  coord_flip()

#COLUMN PLOT of ordered tibble of distribution by country piped directly in ggplot
tenured_interviewed %>% group_by(Country) %>%
  summarise(n = n()) %>%
  mutate(Country = reorder(Country, desc(n))) %>%
  ggplot(aes(Country, n, fill = "#00CDCD")) + 
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "Interview subjects") +
  scale_fill_npg() +
  coord_flip() +
  theme(axis.text.y  = element_text(size = 14)) 

#COLUMN PLOT of ordered tibble of distribution by continent piped directly in ggplot
tenured_interviewed %>% group_by(Continent) %>%
  summarise(n = n()) %>%
  mutate(Continent = reorder(Continent, desc(n))) %>%
  ggplot(aes(Continent, n, fill = "#00CDCD")) + 
  geom_col(show.legend = FALSE) +
  labs(x = "", y = "Interview subjects") +
  scale_fill_npg() +
  coord_flip() +
  theme(axis.text.y  = element_text(size = 14)) 

#################################################################################
# DENSITY PLOT Postdoc_years faceted BY Gender
tenured_interviewed %>% 
  mutate(Gender = factor(Gender, levels = c("male", "female"))) %>%
  ggplot() + 
  geom_density(aes(x = Postdoc_years, y = ..count..,
                   fill = Gender), alpha = 0.7, bw = 1) +
  expand_limits(x = 0) +
  labs(x = "Years postdoc", y = "Density") +
  scale_fill_manual(values=c("#00CDCD", "#EE3B3B"))

tenured_interviewed %>% 
  mutate(Gender = factor(Gender, levels = c("male", "female"))) %>%
  ggplot() + 
  geom_density(aes(x = Postdoc_years, fill = Gender), alpha = 0.7, bw = 1) +
  expand_limits(x = 0) +
  labs(x = "Years postdoc", y = "Density") +
  scale_fill_manual(values=c("#00CDCD", "#EE3B3B"))

## HISTOGRAMS FACETED BY Gender
tenured_interviewed %>% ggplot(aes(h_index_Scopus, fill = Gender)) + 
  geom_histogram(binwidth = 5) +
  scale_fill_npg() +
  labs(x = "h-index (Scopus)") +
  facet_grid(Gender ~ .)

tenured_interviewed %>% ggplot(aes(Documents_Scopus, fill = Gender)) + 
  geom_histogram(binwidth = 50) +
  scale_fill_npg() +
  labs(x = "Documents (Scopus)") +
  facet_grid(Gender ~ .)

tenured_interviewed %>% ggplot(aes(Citations_Scopus_self_citations_excluded, fill= Gender)) + 
  geom_histogram(binwidth = 0.5) +
  scale_fill_npg() +
  labs(x = "Citations (self-citations excluded, Scopus)") +
  scale_x_continuous(trans = "log2") +
  facet_grid(Gender ~ .)

tenured_interviewed %>% ggplot(aes(Citing_documents_Scopus, fill = Gender)) + 
  geom_histogram(binwidth = 0.5) +
  scale_fill_npg() +
  labs(x = "Citing document (Scopus)") +
  scale_x_continuous(trans = "log2") +
  facet_grid(Gender ~ .)

tenured_interviewed %>% ggplot(aes(Gender, h_index_Scopus, fill = Academic_rank)) + 
  geom_boxplot() +
  scale_fill_npg() +
  labs(x = "", y = "h-index (Scopus)") +
  scale_y_continuous(trans = "log2")

## BOXPLOT OF H-INDEX BY Gender
tenured_interviewed %>% ggplot(aes(x = Gender, y = h_index_Scopus, fill = Gender)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "h-index (Scopus)") +
  scale_y_continuous(trans = "log2")

tenured_interviewed %>% ggplot(aes(Gender, Documents_Scopus, fill = Gender)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "Documents") +
  scale_y_continuous(trans = "log2")

tenured_interviewed %>% ggplot(aes(Gender, Citations_Scopus_self_citations_excluded, fill = Gender)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "Citations (self-citations excluded)") +
  scale_y_continuous(trans = "log2")

tenured_interviewed %>% ggplot(aes(Gender, Citing_documents_Scopus, fill = Gender)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "Citing documents (Scopus)") +
  scale_y_continuous(trans = "log2")

# library(ggpubr)
# ggarrange(g1, g2, g3, g4 + rremove("x.text"), 
#          labels = c("A", "B", "C", "D"),
#         ncol = 2, nrow = 2)

tenured_interviewed %>% ggplot(aes(x = Academic_rank, 
                               y = h_index_Scopus, fill = Gender)) + 
  geom_boxplot() +
  scale_fill_npg() +
  labs(x = "", y = "h-index (Scopus)") +
  scale_y_continuous(trans = "log2")

tenured_interviewed %>% ggplot(aes(Academic_rank,
                               Documents_Scopus, fill = Gender)) + 
  geom_boxplot() +
  scale_fill_npg() +
  labs(x = "", y = "Documents") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

tenured_interviewed %>% ggplot(aes(Academic_rank, 
                               Citations_Scopus_self_citations_excluded,
                               fill = Gender)) + 
  geom_boxplot() +
  scale_fill_npg() +
  labs(x = "", y = "Citations (self-citations excluded)") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1)) +
  theme(axis.title.y= element_text(hjust = 1))

tenured_interviewed %>% ggplot(aes(Academic_rank, 
                               Citing_documents_Scopus,
                               fill = Gender)) + 
  geom_boxplot() +
  scale_fill_npg() +
  labs(x = "", y = "Citing documents") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1)) +
  theme(axis.title.y= element_text(hjust = 1))


# distribution by academic rank
tenured_interviewed %>% ggplot(aes(Academic_rank, fill = Gender)) +
  geom_bar() +
  labs(x ="") +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

# Years as PI histogram, boxplots
tenured_interviewed %>%
  summarise(median_years = median(as.numeric(PI_years)),
            min = min(as.numeric(PI_years)),
            max = max(as.numeric(PI_years)))

tenured_interviewed %>% 
  ggplot(aes(as.numeric(PI_years), fill = "#00CDCD")) +
  geom_histogram(binwidth = 5, show.legend = FALSE) +
  scale_fill_npg() +
  labs(x = "", y = "Years as PI")

tenured_interviewed %>% group_by(Academic_rank) %>%
  summarise(median_years = median(as.numeric(PI_years)))


# distribution by country
# NOT ordered bargraph
tenured_interviewed %>% ggplot(aes(Country, fill = "#00CDCD")) +
  geom_bar(show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90))

# list of countries
sort(unique(tenured_interviewed$Country))

# ordered tibble of distribution by country with intermediate tibble
countries <- tenured_interviewed %>%
  group_by(Country) %>%
  summarise(n = n())

countries <- arrange(countries, n)
countries
class(countries)

# ordered bar graph of distribution by country increasing
countries %>%
  ggplot(aes(x = reorder(Country, n), y = n, fill = "#00CDCD")) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90))+
  xlab("") + ylab("Interview subjects")

# ordered bar graph of distribution by country decreasing
countries %>%
  ggplot(aes(x = reorder(Country, -n), y = n, fill = "#00CDCD")) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") + ylab("Interview subjects")

# distribution by continent
# NOT ordered
tenured_interviewed %>% ggplot(aes(Continent, fill = "#00CDCD")) +
  geom_bar(show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90))

sort(unique(tenured_interviewed$Continent))

# ordered tibble of distribution by Continent with intermediate tibble
continents <- tenured_interviewed %>%
  group_by(Continent) %>%
  summarise(n = n())

continents <- arrange(continents, n)
continents
# class(continents)

# ordered bar graph of distribution by continent increasing
continents %>%
  ggplot(aes(x = reorder(Continent, n), y = n, fill = "#00CDCD")) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90))+
  xlab("") + ylab("Interview subjects")

# ordered bar graph of distribution by continent decreasing
continents %>%
  ggplot(aes(x = reorder(Continent, -n), y = n, fill = "#00CDCD")) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_npg() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") + ylab("Interview subjects")

##################################################################
library(tidyverse)
library(readxl)
library(ggsci) # Nature  Reviews color scale
# library(lettercase)
# library(gridExtra)

## Read children subject data into R
Children_table <- read_excel("C:/Users/u0078315/Dropbox (Personal)/Successful Life Scientist/Seminars/Diverging Career Pathways Singapore/Figure data/Children.xlsx")
Children_table <- as.data.frame(Children_table)
Children_table <- filter(Children_table, !is.na(Children_table$Children))

#SUMMARY
Children_table %>%
  summarise(median_children = median(Children, na.rm = TRUE),
            min = min(Children, na.rm = TRUE),
            max = max(Children, na.rm = TRUE))

#summarise by gender
Children_table %>% filter(Gender == c("male")) %>% 
  summarise(median_children_males = median(Children, na.rm = TRUE),
            mean_children_males = mean(Children, na.rm = TRUE),
            min = min(Children, na.rm = TRUE),
            max = max(Children, na.rm = TRUE))

Children_table %>% filter(Gender == c("female")) %>% 
  summarise(median_children_females = median(Children, na.rm = TRUE),
            mean_children_females = mean(Children, na.rm = TRUE),
            min = min(Children, na.rm = TRUE),
            max = max(Children, na.rm = TRUE))

Children_table %>% group_by(Gender) %>% 
  summarise(median_children = median(Children, na.rm = TRUE),
            mean_children = mean(Children, na.rm = TRUE),
            min = min(Children, na.rm = TRUE),
            max = max(Children, na.rm = TRUE))

# DENSITY PLOT Children faceted BY Gender
Children_table %>% 
  mutate(Gender = factor(Gender, levels = c("male", "female"))) %>%
  ggplot() + 
  geom_density(aes(x = Children, y = ..count..,
                   fill = Gender), alpha = 0.7, bw = .5) +
  expand_limits(x = 0) +
  labs(x = "Number of children", y = "Density") +
  scale_fill_manual(values=c("#00CDCD", "#EE3B3B"))

# BOXPLOT Children faceted by Gender
Children_table %>% 
  ggplot(aes(x = Gender, y= Children, fill = Gender)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_fill_npg() +
  xlab("") +
  labs(x = "", y = "Number of children") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

#########################################################################
arrange(tenured_interviewed, Institute)

tenured_interviewed %>% group_by(Academic_rank, Gender) %>%
  summarise(n = n())

tenured_interviewed %>% group_by(Gender, Academic_rank) %>%
  summarise(n = n())

