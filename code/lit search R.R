library(tidyverse)
library(rentrez)
library(glue)

read_csv("Desktop/==Virus-particles/Manuscript/PubMed_Timeline_Results_by_Year.csv",
         skip=1) %>%
  ggplot(aes(x=Year, y=Count)) +
  geom_line()


entrez_dbs()

entrez_db_summary(db = "pubmed")
entrez_db_searchable(db = "pubmed")

s<- entrez_search(db = "pubmed", term = "virus")
glimpse(s)
s$count


entrez_search(db = "pubmed", term = "water AND virus")
entrez_search(db = "pubmed", term = "wastewater AND virus")
entrez_search(db = "pubmed", term = "stormwater AND virus")
entrez_search(db = "pubmed", term = "Sobsey MD [AUTH] OR Gerba CP [AUTH] AND virus")
yes_virus<-entrez_search(db = "pubmed", term = "(Sobsey MD [AUTH] OR Gerba CP [AUTH]) AND virus")$ids

entrez_fetch(db="pubmed", id=yes_virus, rettype="abstract")

entrez_search(db = "pubmed", term = "Sobsey MD [AUTH] AND Gerba CP [AUTH]")
non_virus <- entrez_search(db = "pubmed", term = "(Sobsey MD [AUTH] AND Gerba CP [AUTH]")$ids
entrez_fetch(db="pubmed", id=non_virus, rettype="abstract")

entrez_search(db = "pubmed", term = "virus AND 2020[PDAT]")$count

year <- 1950:2022
ubiome_search <- glue("virus wastewater AND {year}[PDAT]")
absorption_search <- glue("absorption AND {year}[PDAT]")
all_search <- glue("{year}[PDAT]")

search_counts<- tibble(year = year,
       ubiome_search = ubiome_search,
       absorption_search= absorption_search) %>% 
  mutate(ubiome = map_dbl(ubiome_search, ~entrez_search(db="pubmed", term=.x)$count),
        absorption_search = map_dbl(absorption_search, ~entrez_search(db="pubmed", term=.x)$count)
          )
        


search_counts <- tibble(year = year,
                        ubiome_search = ubiome_search,
                        cancer_search = cancer_search,
                        all_search = all_search) %>%
  mutate(ubiome = map_dbl(ubiome_search, ~entrez_search(db="pubmed", term=.x)$count),
         cancer = map_dbl(cancer_search, ~entrez_search(db="pubmed", term=.x)$count),
         all = map_dbl(all_search, ~entrez_search(db="pubmed", term=.x)$count)
  )

search_counts %>%
  select(year, ubiome, absorption) %>%
  filter(year != 2022) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x=year, y= value, group=name, color=name)) +
  geom_line()


search_counts %>%
  select(year, ubiome, cancer, all) %>%
  filter(year != 2022) %>%
  mutate(rel_ubiome = 100 * ubiome / all) %>%
  ggplot(aes(x=year,  y=rel_ubiome)) +
  geom_line() +
  scale_y_log1


# thumbnail
search_counts %>%
  select(year, ubiome, cancer, all) %>%
  filter(year != 2022) %>%
  mutate(rel_ubiome = 100 * ubiome / all,
         rel_cancer = 100 * cancer / all) %>%
  select(year, starts_with("rel")) %>%
  pivot_longer(-year) %>%
  ggplot(aes(x=year, y=value, group=name, color=name)) +
  geom_line(size=1) +
  scale_y_log10(limits=c(NA, 100),
                breaks=c(0.01, 0.1, 1, 10, 100),
                labels=c("0.01", "0.1", "1", "10", "100")) +
  labs(x="Year", y="Percentage of all papers in PubMed") +
  scale_color_manual(name=NULL,
                     label=c("Cancer", "Microbiome"),
                     values=c("gray", "red"),
                     breaks=c("rel_cancer", "rel_ubiome")) +
  theme_classic() +
  theme(legend.position=c(0.8, 0.2))

ggsave("figures/microbiome_vs_cancer.png", width=6, height=4)