library(ggplot2)

df <- read.csv("dane/german_credit_data.csv")

ggplot(data = df, aes(x = Sex)) +
  geom_bar()

ggplot(data = df, aes(x = Sex, fill = Risk)) +
  geom_bar()

ggplot(data = df, aes(x = Sex, fill = Risk)) +
  geom_bar(position = "dodge")

ggplot(data = df, aes(x = Age)) +
  geom_bar()

ggplot(data = df, aes(x = Age)) +
  geom_density()

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density()

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.3)

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Risk)

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Risk, labeller = label_both)

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Purpose, labeller = label_both)

ggplot(data = df, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.3) +
  facet_grid(Risk ~ Purpose, labeller = label_both)

# 1. Przypisanie cech do atrybutów wizualnych
# 2. Geometria - funkcja która przerysowuje dane na wykres
# 3. Facetowanie - dzielenie danych na podzbiory

ggplot(data = df, aes(x = Sex, fill = Risk)) +
  geom_bar() +
  coord_polar()

ggplot(data = df, aes(x = Sex, fill = Risk)) +
  geom_bar(position = "dodge")

ggplot(data = df, aes(x = Sex, fill = Risk, label = ..count..)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", size = 18)

ggplot(data = df, aes(x = Sex, fill = Risk, label = ..count..)) +
  geom_bar(position = position_dodge(width = 1)) +
  geom_text(stat = "count", size = 11, 
            position = position_dodge(width = 1), vjust = 1)

library(dplyr)

df_count <- group_by(df, Sex, Risk) %>% 
  summarise(count = n())

ggplot(df_count, aes(x = Sex, fill = Risk, y = count)) +
  geom_bar(position = "dodge", stat = "identity")

ggplot(df_count, aes(x = Sex, fill = Risk, y = count, label = count)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(mapping = aes(y = count/2), 
            size = 12, position = position_dodge(width = 1))

df_count <- group_by(df, Sex, Risk, Purpose) %>% 
  summarise(count = n())

ggplot(df_count, aes(x = Sex, fill = Risk, y = count, label = count)) +
  geom_col(position = position_dodge(width = 1)) +
  geom_text(mapping = aes(y = count/2), 
            size = 12, position = position_dodge(width = 1)) +
  facet_wrap(~ Purpose)
