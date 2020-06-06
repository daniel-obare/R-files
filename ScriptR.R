# Cleaning Data

#gather combines multiple columns into two rows with key and value ie wide to long
#spread moves key value columns to multiple columns with keys as column names ie long to wide
#separate splits a column by _ or whatever seperator you choose to multiple columns
#unite combines multiple columns to one column with _ as the seperator

# gsub in R - regular expressions
phone <-"(206) 555 - 1212"
gsub("[[:punct:][:blank:]]","",phone)
   "2065551212"
   
# sub in r - regular expression pattern matching
base <- "bnnnnnannannasplit"
gsub("n{1,3}","*",base)
   "b**a*a*asplit"
   

#With the following R code, you can replace the two colnames 
#Sepal.Width and Petal.Width by New1 and New2:
   # Rename two variable names
colnames(data_ex3)[colnames(data_ex3) %in% c("Sepal.Width", "Petal.Width")] <- c("New1", "New2")
  
  #Renaming one column
colnames(data_ex1)[colnames(data_ex1) == "Species"] <- "New_Name"

   #DATE TIMES
#Since we're dealing with elapsed time between two dates, let's start with Intervals.
#We can define an Interval using the %--% operator.

time.interval <- start %--% end
time.interval
## [1] 2017-03-11 05:21:00 EST--2017-03-12 05:21:00 EDT

#Another way to do it, without changing the whole computer time is using the 
#setenv command like this : 
Sys.setenv(TZ='GMT')

#Sys.time and Sys.Date returns the system's idea of the current date with and without time,
#and Sys.timezone returns the current time zone.

#Usage
Sys.time()
Sys.Date()
Sys.timezone()



#CATEGRICAL VARIABLES
require(gmodels)
CrossTable()
mosaicplot()

      #SHINY DASHBOARDS
library(shinydashboard)
library(shiny)
ui <- dashboardPage(
   dashboardHeader(title = "Quick Example"),
   dashboardSidebar(textInput("text", "Text")),
   dashboardBody(
      valueBox(100, "Basic example"),
      tableOutput("mtcars")
   )
)
server <- function(input, output) {
   output$mtcars <- renderTable(head(mtcars))
}
shinyApp(ui, server)

## app.R 

--------------------------------------------------------------------------------------------------------
#populated UI
--------------------------------------------------------------------------------------------------------   
   

library(shiny)
library(shinydashboard)

header = dashboardHeader(title = "Basic Dashboard")  #header title
   
sidebar = dashboardSidebar( sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
                                        menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
                                          href = "https://www.salesforce.com")),
                            textInput("name", "what is your name"),
                            numericInput("age", "how old are you", NA, min = 0, max = 150)) 
   
frow1 <- fluidRow(valueBoxOutput("value1"),valueBoxOutput("value2"),valueBoxOutput("value3"))
frow2 <- fluidRow(box(title = "Revenue per Account",status = "primary",solidHeader = TRUE,collapsible = TRUE,
                      plotOutput("revenuebyPrd", height = "300px")),
                  box(title = "Revenue per Product" ,status = "primary",solidHeader = TRUE,collapsible = TRUE,
                      plotOutput("revenuebyRegion", height = "300px")))
   
body <- dashboardBody(frow1, frow2)
   
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')  
   
--------------------------------------------------------------------------------------------------------
   #Populated Server
--------------------------------------------------------------------------------------------------------
   
server <- function(input, output) { total.revenue <- sum(recommendation$Revenue)
                                 sales.account <- recommendation %>% 
                                    group_by(Account) %>% 
                                    summarise(value = sum(Revenue)) %>% 
                                    filter(value==max(value))  
                                 prof.prod <- recommendation %>% 
                                    group_by(Product) %>% 
                                    summarise(value = sum(Revenue)) %>% 
                                    filter(value==max(value))
                                 output$value1 <- renderValueBox({valueBox(formatC(sales.account$value, format="d", big.mark=','),
                                                                           paste('Top Account:',sales.account$Account),
                                                                           icon = icon("stats",lib='glyphicon'),
                                                                           color = "purple")})
                                 output$value2 <- renderValueBox({valueBox(formatC(total.revenue, format="d", big.mark=','),
                                                                           'Total Expected Revenue',icon = icon("gbp",lib='glyphicon'),
                                                                           color = "green")})
                                 output$value3 <- renderValueBox({valueBox(formatC(prof.prod$value, format="d", big.mark=','),
                                                                           paste('Top Product:',prof.prod$Product),
                                                                           icon = icon("menu-hamburger",lib='glyphicon'),
                                                                           color = "yellow")})
                                 output$revenuebyPrd <- renderPlot({ggplot(data = recommendation,aes(x=Product, y=Revenue, 
                                                                                                     fill=factor(Region)))+
                                                                     geom_bar(position = "dodge", stat = "identity")+
                                                                     ylab("Revenue (in Euros)")+
                                                                     xlab("Product")+
                                                                     theme(legend.position="bottom",plot.title = element_text(size=15, face="bold"))+
                                                                     ggtitle("Revenue by Product")+
                                                                     labs(fill = "Region")})
                                 output$revenuebyRegion <- renderPlot({ggplot(data = recommendation,aes(x=Account, y=Revenue, fill=factor(Region)))+
                                                                        geom_bar(position = "dodge", stat = "identity")+
                                                                        ylab("Revenue (in Euros)")+ 
                                                                        xlab("Account")+ 
                                                                        theme(legend.position="bottom",plot.title = element_text(size=15, face="bold"))+ 
                                                                        ggtitle("Revenue by Region")+ 
                                                                        labs(fill = "Region")
                                                                        })  
}


   
--------------------------------------------------------------------------------------------------------------
      #shiny App
---------------------------------------------------------------------------------------------------------------

 appshinyApp(ui, server)
 
 
   
   
   # CLEAN COLUMN NAMES

-------------------------------------------------------------------------------------------------------   
#There is a very useful package for that, called janitor that makes cleaning up column names very simple.
#It removes all unique characters and replaces spaces with _

library(janitor)

#can be done by simply
ctm2 <- clean_names(ctm2)

#or piping through `dplyr`
ctm2 <- ctm2 %>%
        clean_names()
        
--------------------------------------------------------------------------------------------------------
 
 
make.names() # syntactically valid names out of character vectors   
#Additionally, flag unique=TRUE allows you to avoid possible dublicates in new column names.

names(d)<-make.names(names(d),unique = TRUE)

#To replace only the first space in each column you could also do:

names(ctm2) <- sub(" ", ".", names(ctm2))

#or to replace all spaces (which seems like it would be a little more useful):

names(ctm2) <- gsub(" ", "_", names(ctm2))

                                                                              
  #POPULATION PYRAMIDS
  
ages <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-')
males <- c(34,19,11,11,8,7,5)
females <- c(26,25,16,11,7,5,1)
data <- data.frame(males,females,ages)
pyramid(data)
                                                                              


library(dplyr)
agelevels <- c("0-4", "5-9","10-14","15-19",
               "20-24","25-29","30-34",
               "35-39","40-44","45-49",
               "50-54","55-59","60-64",
               "65-69","70-74","75-79",
               "80-84","85-89","90+")
areas <- c("Area 1", "Area 2", "Area 3")
pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2),
                                levels = agelevels),
                   Value = rep(sample(1000:3000, length(agelevels), replace = TRUE),
                               length(areas) * 2),
                   Sex = rep(rep(c("Male", "Female"),
                                 each = length(agelevels)), length(areas)),
                   AreaName = rep(areas, each = length(agelevels) * 2))

p <- population(pops,
                value = Value,
                sex = Sex,
                age = Age,
                area = AreaName,
                area_name = "Area 1",
                comparator_1 = "Area 3",
                comparator_2 = "Area 2",
                title = "Age Profile",
                subtitle = "2015/16",
                xlab = "% of total population")
p



------------------------------------------------------------------------------------------------------
# SUPERVISE PREDICTIVE MODELLING:
#   KNN ALGORITHM
--------------------------------------------------------------------------------------------------------
   # Majorly predicts CATEGORICAL data
   
load(iris)
set.seed(1234)

index <- sample(1:nrow(iris), 0.9*nrow(iris))


# normalize the numerical data

normalize <- function(x){
   return((x-min(x))/(max(x)-min(x)))
}

norm <- as.data.frame(lapply(iris[ ,1:4], normalize))


# extract train set and test set
train <- norm[index, ]
test <- norm[-index, ]

# extract 5th column of train dataset to be used as a cl argument
train_target <- iris[index,5]
test_target <- iris[-index,5]

# load class package to run KNN function
pr <- class::knn(train = train, test = test, cl = train_target, k = 13)

# confusion matrix
caret::confusionMatrix(pr, test_target)

# create a dataframe
d <- as.data.frame(pr, iris_test)







------------------------------------------------------------------------------------------------
   # UNSUPERVISED PREDICTIVE MODELLING;
   # PRINCIPAL COMPONENT ANALYSIS
-------------------------------------------------------------------------------------------------   
# does well with only numerical data ONLY

load(mtcars)
cars.pca <- prcomp(mtcars[c(1:7,10,11)], scale. = TRUE, center = TRUE)
summary(cars.pca)
str(cars.pca)

# plotting PCA model
# the labeling puts cars into categories
ggbiplot::ggbiplot(cars.pca, labels = row.names(mtcars))




----------------------------------------------------------------------------------------------------
   # SURPORT VECTO MACHINES: SVM
---------------------------------------------------------------------------------------------------
   
   # support vector classification
   
load(iris)
x <- subset(iris, select = -Species)
y <- Species
model <- e1071::svm(Species~., data = iris)
summary(model)
prediction <- predict(model, data = x)
table(prediction, y)

# tuning SVM for best COST and GAMMA
e1071::tune.svm(Species~., data = iris, gamma = 10^(-5:1), cost = 10^(-3:1))
   


-------------------------------------------------------------------------------------------------------
   # KMEANS CLUSTERING
---------------------------------------------------------------------------------------------------------
   # uses numerical data
   
load(USArrests)
df <- USArrests %>% na.omit() %>% scale()

# using any number of integer/cluster i.e 2,3,4,5,6... for center parameter
k <- kmeans(df, centers = 2, nstart = 25)
str(k)
factoextra::fviz_cluster(k, data = df)
gap_stat <- cluster::clusGap(df, FUNcluster = kmeans, nstart = 25, B = 50, K.max = 10)
factoextra::fviz_gap_stat(gap_stat = gap_stat)
# from this we find visual optimal cluster is 4
#fina analysis
k4 <- kmeans(df, centers = 4, nstart = 25)
factoextra::fviz_cluster(k4, df)
   
   
   
   # rCOLOR PICKERS
   
# Valid colors are:
  # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
   
   





# BACKWARD ELIMINATION OF MULTIPLE LINEAR REGRESSION
backwardElimination <- function(x, sl) {
   numVars = length(x)
   for (i in c(1:numVars)){
      regressor = lm(formula = Profit ~ ., data = x)
      maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
      if (maxVar > sl){
         j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
         x = x[, -j]
      }
      numVars = numVars - 1
   }
   return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)













