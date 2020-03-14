
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 65), rnorm(200, 70))
)
filter(wdata,sex=="M")


rep(c("A","B"),each=4)
a <- ggplot(wdata, aes(x = weight))
a + geom_density() + geom_vline(aes(xintercept = mean(weight)), 
                                linetype = "dashed", size = 0.6)

a + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(weight)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")
# Change line color by sex
a + geom_density(aes(color = sex)) +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))
# Change fill color by sex and add mean line
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = sex), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = sex),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF"))

mu <- wdata %>% 
  group_by(sex) %>%
  summarise(grp.mean = mean(weight))
