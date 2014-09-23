1.) Describe your algorithm for deciding how to compare "best popular" movies between years. This can be included 
    in R comments if you're not producing an R Markdown file.

   To compare the best popular movies between years , I would need group them by decades. I am thinking of creating a new field in the Data Set
called decade which basically categorizes the movie years into which decade where they were created. After creating the field, I would
group them and get the top most ratings per decade by using the split() and Lapply function and use the "ratings" field to determine which films where the best 
on each decade group.  

2.) Provide R code that supports your conclusions.
                        

week4Assignment2 <- function()  {
  # Read FileName
  fileName <- "C:/Anthony/School_CUNY/IS 607/Week4/movies.tab"
  v <- read.table(fileName, sep="\t", header=TRUE, quote="", comment="" )
  #v <- subset(m, m$year == "1900")
  #v <- subset(m, m$year %in% c("1900","1910"))
  #v[v$decade] <- NA
  v$decade <- NA
  
  for (i in 1:length(v$year)) {
    #print("A")
    if (v$year[i]<= 1900)  {
      # print("B")
      v$decade[i] <- 1900
    } else  
      if (v$year[i] > 1900 & v$year[i] <= 1910)  {
        v$decade[i] <- 1910
      } else  
        if (v$year[i] > 1910 & v$year[i] <= 1920)  {
          v$decade[i] <- 1920
        } else  
          if (v$year[i] > 1920 & v$year[i] <= 1930)  {
            v$decade[i] <- 1930
          } else  
            if (v$year[i] > 1930 & v$year[i] <= 1940)  {
              v$decade[i] <- 1940
            } else  
              if (v$year[i] > 1940 & v$year[i] <= 1950)  {
                v$decade[i] <- 1950
              } else  
                if (v$year[i] > 1950 & v$year[i] <= 1960)  {
                  v$decade[i] <- 1960
                } else  
                  if (v$year[i] > 1960 & v$year[i] <= 1970)  {
                    v$decade[i] <- 1970
                  } else  
                    if (v$year[i] > 1970 & v$year[i] <= 1980)  {
                      v$decade[i] <- 1980
                    } else  
                      if (v$year[i] > 1980 & v$year[i] <= 1990)  {
                        v$decade[i] <- 1900
                      } else  
                        if (v$year[i] > 1990 & v$year[i] <= 2000)  {
                          v$decade[i] <- 2000
                        } else  
                          if (v$year[i] > 2000 & v$year[i] <= 2010)  {
                            v$decade[i] <- 2010
                          }
  }

  
  vNew <- lapply(split(v, v$decade), 
                 function(x) {
                   x[which.max(x$rating), c(1,2,5)]
                 }
  )
  do.call(rbind, vNew)
  
}


3.) Use at least one visualization in support of your conclusion from the ggplot2 package.
install.packages("ggplot2")

ggplot(data=b, aes(x=paste( year,"-",title) , y=rating)) + geom_bar(stat="identity")


4.) Use at least one function in support of your conclusion from the plyr package
install.packages("plyr")

ddply(v, c("year"), function(v)max(v$rating))




