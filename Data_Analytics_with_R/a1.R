library(tidyverse)
data(msleep)
?msleep

# 1. Convert into factors
msleep$genus <- as.factor(msleep$genus)
msleep$vore <- as.factor(msleep$vore)
msleep$order <- as.factor(msleep$order)
msleep$conservation <- as.factor(msleep$conservation)

# 2. Shortest sleep time
shortest_sleep <- min(msleep$sleep_total, na.rm=TRUE)
shortest_sleep_mammal <-as.character(msleep[which(msleep$sleep_total == shortest_sleep),1])

# 3. Most missing
most_missing <- colnames(msleep[which.max(colSums(is.na(msleep)))])
missing_values <- as.integer(sum(is.na(msleep[[most_missing]])))

# 4. Correlations
correlations <- msleep |> select(where(is.numeric)) |> cor(use="complete.obs")

# 5. Highest correlation
highest_corr <- max(correlations[correlations!=max(correlations)])

# 6. Sleep time distribution
sleep_histogram <- ggplot(msleep, aes(x=sleep_total)) + 
  geom_histogram(binwidth=1)

# 7. Bar chart for food categories
food_barchart <- ggplot(msleep, aes(x=vore)) +
  geom_bar()

# 8. Grouped box plot for sleep time
sleep_boxplot <- ggplot(msleep,aes(x=vore,y=sleep_total)) + geom_boxplot()

# 9. Longest average sleep time
highest_average <-msleep|> group_by(vore) |> summarise(mean_sleeptime=mean(sleep_total)) |> arrange(desc(mean_sleeptime)) |> slice(1) |> pull(mean_sleeptime) |> as.double()

# 10. REM sleep vs. total sleep, colored by order
sleep_scatterplot <- ggplot(msleep,aes(x=sleep_total,y=sleep_rem)) +geom_point(aes(color=order))

# 11. REM sleep vs. total sleep for the order most common in the data
most_common_order_name <-msleep |> group_by(order) |> summarise(count=n()) |> arrange(desc(count)) |> slice(1)  |> pull(order) |> as.character()
common_order <- msleep |> filter(order==most_common_order_name)
sleep_scatterplot2 <- ggplot(common_order,aes(x=sleep_total,y=sleep_rem)) + geom_point(aes(color=genus))
