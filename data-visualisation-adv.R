library(ggplot2)

job_labels <- c('unskilled and non-resident',
                 'unskilled and resident',
                 'skilled',
                 'highly skilled')

df <- read.csv("dane/german_credit_data.csv") %>% 
  mutate(Job = factor(Job, labels = job_labels))

ggplot(df, aes(x = Job, fill = Purpose)) +
  geom_bar(position = "dodge")

p <- group_by(df, Job, Purpose) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = Job, y = Purpose, fill = count)) +
  geom_tile()

p + 
  scale_fill_gradient(low = "pink", 
                      high = "orange")

p + 
  scale_fill_gradient(low = "#e7e1ef", 
                      high = "#dd1c77") +
  geom_tile(color = "black")

pp <- p + 
  scale_fill_gradient("Number of clients", 
                      low = "#e7e1ef", 
                      high = "#dd1c77") +
  geom_tile(color = "black") + 
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5))

pp + geom_text(aes(label = count))

ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_point()

set.seed(1410)
ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_point(position = "jitter")

ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_boxplot()

ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_violin()

# install.packages("ggbeeswarm")
library(ggbeeswarm)

ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_quasirandom(method = "smiley")

set.seed(1410)
ggplot(df, aes(x = Purpose, 
               y = log(Credit.amount),
               color = Risk,
               shape = Sex)) +
  geom_point(position = "jitter") +
  scale_color_manual(values = c("red", "navyblue")) +
  scale_y_continuous("Credit amount\n(log scale)") 

set.seed(1410)
ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_point(position = "jitter") +
  scale_color_manual(values = c("red", "navyblue")) +
  scale_y_continuous("Credit amount") +
  facet_wrap(~ Sex, scales = "free_y")
    
moj_temat <- theme_bw(base_size = 18) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5))
ggplot(df, aes(x = Purpose, 
               y = Credit.amount,
               color = Risk)) +
  geom_point(position = "jitter") +
  scale_color_manual(values = c("red", "navyblue")) +
  scale_y_continuous("Credit amount") +
  facet_wrap(~ Sex, scales = "free_y") + moj_temat
