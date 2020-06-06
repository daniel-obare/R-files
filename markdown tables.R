require(knitr)
require(kableExtra)
require(magrittr)
require(DT)

#plain HTML table without any CSS filters
dt=mtcars[1:5,1:6]
kable(dt)

#Bootsrap theme
dt %>% kable() %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px") #scrolling table


##view datatables with row names as well as general overview
datatable(head(car))

##editable table on both client and server end with a caption
datatable(head(car), editable = "cell", caption = "table1:Histogram of mpg distribution")