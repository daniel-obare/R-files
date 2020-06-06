#load ggplot graphics packages
library(ggplot2)
library(lattice)

ggplot(data, aes(x,y, color=factorvariable))+
  theme_dark()+  #background theme ie bw, grey
  theme_econmist()+  #economist theme from ggtheme
  geom_line()+  #line graph
  geom_bar(position = position_dodge())+  #places values b side than stacking
  theme(legend.position = "bottom", legend.direction = "horizontal")+ #position of legend
  labs(x="...",y="...")+ #x and y labels
  ggtitle("...")  #title of graph

#plotting both line graph with points
ggplot(data, aes(x,y, colour=factvar, group=factvar))+
  geom_line()+
  geom_point()

#stacking factor var side by side
ggplot(data, aes(x,y, fill=factvar))+
  geom_bar(position = position_dodge(), stat = "identity")

# flipping vertical to horizontal and horizontal to vertical
coord_flip(clip = "off", expand = FALSE)

# default scales for continuous y and x aesthetics
scale_y_continuous(labels = comma)
scale_x_continuous()
scale_x_reverse()
scale_y_reverse()

# give names to continuous variables
scale_x_discrete()
scale_y_discrete()

