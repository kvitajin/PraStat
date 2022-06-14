library(readxl)
library(dplyr)
library(rstatix)
library(lawstat)
library(FSA)

# načtení dat
file_path = "./data/data_ver220530B.xlsx"
data_raw = read_excel(file_path)

# nalezení odlehlých pozorování
data_m1_op =
  data_raw %>%
    group_by(skupina) %>%
    identify_outliers(m1)


# odfiltrování odlehlých hodnot
data_bez_op <- data %>%
  filter(!(ID %in% data_m1_op$ID))

# test normálního rozdělení
tapply(data_bez_op$m1, data_bez_op$skupina, shapiro.test)
# VÝSLEDEK: LZE modelovat normálním rozdělením

# test shody rozptylů
bartlett.test(data_bez_op$m1 ~ data_bez_op$skupina)
# VÝSLEDEK: homoskedasticita zamítnuta

# normální rozdělení: ANO
# symetrie: ANO (lze říct automaticky podle normality)
# shoda rozptylů: NE ----> Kruska-Wallis
kruskal.test(data_bez_op$m1 ~ data_bez_op$skupina)
# VÝSLEDEK: zamítáme hypotézu o shodě MEDIÁNŮ

# seřazení podle mediánu
data_bez_op %>%
  group_by(skupina) %>%
  summarize(median = median(m1)) %>%
  arrange(desc(median))
# VÝSLEDEK:
# 1. KONTROL
# 2. HIIT
# 3. KETO
# 4. KETO+HIIT

# analýza toho, které se vzájemně liší
dunnTest(m1 ~ skupina, data = data_bez_op, method = "bonferroni")
# VÝSLEDEK:
# KETO - KETO+HIIT stejné, všechny ostatní nehomogenní

# CELKOVÝ VÝSLEDEK:
# VÝSLEDEK:
# 1. KONTROL
# 2. HIIT
# 3. KETO/KETO+HIIT (homogenní)
