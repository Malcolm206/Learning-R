library(dplyr)
library(readr)
library(ggplot2)
tips <- read_csv("tips.csv")

select(tips, total_bill)

select(tips, total_bill, tip, sex, smoker, day, time, size)
select(tips, total_bill:size)

select(tips,  -X1)

select(tips, ,starts_with("s"))

filter(tips, day == 'Sun')
filter(tips, tip > 5)
filter(tips, sex == "Male" & smoker == "Yes")
filter(tips, sex == "Male" | smoker == "Yes")

mutate(tips, gbp_total = total_bill*0.81)
mutate(tips, 
       gbp_total = total_bill * 0.81, 
       gbp_tip = tip * 0.81)

tips %>%
  select(smoker, tip) %>%
  mutate(gbp_tip = tip * 0.82) %>%
  filter(gbp_tip >= 5)

tips %>%
  select(total_bill, tip, sex, smoker) %>%
  filter(sex == "Male" & smoker == "Yes") %>%
  mutate(gbp_total_bill = total_bill * 0.82,
         gbo_tip = tip * 0.82)

tips
tips %>%
  group_by(smoker)
tips %>%
  group_by(size)

tips %>%
  group_by(smoker) %>%
  summarise(mean_tip = mean(tip),
            count = n())

tips %>%
  group_by(smoker) %>%
  summarize(mean_tip = mean(tip),
            count = n()) %>%
  arrange(desc(mean_tip))
