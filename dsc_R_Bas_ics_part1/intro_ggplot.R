library(readr)
library(ggplot2)
tips <- read_csv("tips.csv")
tips

ggplot(tips)

ggplot(tips, aes(x = total_bill, y = tip))

ggplot(tips, aes(total_bill, y = tip)) +
  geom_point()

ggplot(tips, aes(x = total_bill, y = tip, color = time)) +
  geom_point()

ggplot(tips, aes(x = total_bill, y = tip, color = time, shape = smoker)) +
  geom_point()

ggplot(tips, aes(x = total_bill, y = tip, color = time, shape = smoker)) +
  geom_point() +
  facet_wrap(~day)

ggplot(tips, aes(x = day)) +
  geom_bar()

ggplot(tips, aes(x = day, fill = smoker)) +
  geom_bar()

ggplot(tips, aes(x = day, fill = smoker)) + 
  geom_bar(position = "dodge")

ggplot(tips, aes(x = day, fill = smoker)) +
  geom_bar(position = "fill")

ggplot(tips, aes(x = day, fill = smoker)) +
  geom_bar(position = "dodge") +
  facet_wrap(~time)

ggplot(tips, aes(x = day, fill = smoker)) +
  geom_bar(position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Number of Patrons By Day and Meal", 
       subtitle = "Position by Smoker", 
       x = "Day", y = "Count")

tips %>%
  select(smoker, tip, day, time) %>%
  mutate(gbp_tip = tip * 0.82) %>%
  filter(gbp_tip >= 5) %>%
  ggplot(aes(x = day, fill = smoker)) +
  geom_bar(position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Number of Patrons by Day and Meal", 
       subtitle = "Position by Smoker",
       x = "Day", y = "Count")
