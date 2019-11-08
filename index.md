---
output: # rmarkdown::github_document
  pdf_document: default
  word_document: default
  html_document:
title: "Assignment 5.  Data Visualization with ggplot2"
---


```{r}
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
```


__***Submission Instruction***__.  You will need to submit on **Blackboard**, in the **Assignment** section, a link to your assignment on your Github Webpage.  Following [this](https://guides.github.com/features/pages/) to create a github webpage and posted a link to your assignment. 

This assignment works with the `c2015` dataset. 
```{r}
c2015=read_excel("C:/Users/student/Documents/Fall2019/c2015.xlsx")
```


1. **Clean the data for easy graphing**.  Do the follows to clean and reduce the size of the data

  - Remove all observations that have a cell being either (1) NA, (2)'Unknown', (3)'Not Rep', or (4)'Not Reported'
```{r}
y=c2015 %>% 
  filter_all(~!is.na(.)) %>% 
  filter_all(~!(.=='Unknown')) %>% 
  filter_all(~!(.=='Not Rep'))
```

  - Remove all observations that have a cell containing either (1)'Unknown', (2)'Not Rep', or (3)'Not Reported'. For instance, observations with `DRINKING` variable being `Unknown (Police Reported)` will be removed. 
```{r}
y=y %>% 
  filter_all(~!(.==str_detect(.,'Not Rep')))%>%
  filter_all(~!(.==str_detect(.,'Unknown')))%>% 
  filter_all(~!(.=='Not Reported'))
```

  - Fix `TRAV_SP` and `AGE` (following previous assignments) so that they are both numerics. 
  
```{r}
y=y %>% 
  mutate(TRAV_SP1=str_replace(TRAV_SP," MPH","")) %>%
  mutate(TRAV_SP1 = as.numeric(TRAV_SP1)) %>% 
   mutate(TRAV_SP1 =replace(TRAV_SP1,is.na(TRAV_SP1),mean(TRAV_SP1,na.rm=TRUE)))
```
```{r}
y=y%>% 
  mutate(AGE = replace(AGE, AGE == "Less than 1" , "0")) %>% 
  mutate(AGE = as.numeric(AGE))%>%
  mutate(AGE = replace(AGE,is.na(AGE),mean(AGE,na.rm=TRUE))) 
```

  - Filter so that there are only drivers in the data
```{r}
y=y %>% 
  filter(SEAT_POS=='Front Seat, Left Side')
```

2. Use `geom_point` to plot `AGE` and `TRAV_SP` coloring by `SEX`. 
```{r}
ggplot(y,aes(AGE,TRAV_SP1,color=SEX))+
  geom_point()
#I mutate TRAV_SP variable to make a new variable:TRAV_SP1)
```

3. There is overplotting in 2.  Overplotting is when many points are duplicated on the graph.  Use `geom_jitter` instead of `geom_point` for 2. to avoid overplotting. 
```{r}
ggplot(y,aes(AGE,TRAV_SP1,color=SEX))+
  geom_jitter()
```

4. Plot histograms of `AGE`, `TRAV_SP` with `bins` = 50. 
```{r}
ggplot(y,aes(AGE))+
  geom_histogram(bins = 50)
```
```{r}
ggplot(y,aes(TRAV_SP1))+
  geom_histogram(bins = 50)
```


5. Plot a histogram of `AGE` coloring (fill) by `SEX`. 
```{r}
ggplot(y,aes(AGE,color=SEX))+
  geom_histogram(bins = 50)
```


6. Using `geom_density` to plot estimated densities of `AGE` colored by `SEX`. 
```{r}
ggplot(y,aes(AGE,color=SEX))+
  geom_density()
```

7. Plot estimated densities of `TRAV_SP` colored by `INJ_SEV`. 
```{r}
ggplot(y,aes(TRAV_SP1,color=INJ_SEV))+
  geom_density()
```

8. Plot estimated densities of `TRAV_SP` seperated (colored) by weekdays and weekends. 
```{r}
ggplot(y,aes(TRAV_SP1,color=DAY_WEEK))+
  geom_density()
```

9. Implement `geom_bar` on `MONTH`. Implement `geom_bar` on `MONTH` filled by `SEX` 
```{r}
ggplot(y,aes(MONTH,color=INJ_SEV))+
  geom_bar()
```


10. Implement `geom_bar` on `MONTH` and `SEX` with `position='dodge'`
```{r}
ggplot(y,aes(MONTH))+
  geom_bar(position='dodge')
```
```{r}
ggplot(y,aes(SEX))+
  geom_bar(position='dodge')
```


11. Plot a bar chart of average speeds in months using `geom_col`
```{r}
y1=y %>% 
  group_by(MONTH) %>% 
  summarise(m_s=mean(TRAV_SP1))

ggplot(y1,aes(MONTH,m_s))+
  geom_col()
```


12. Plot a bar chart of average speeds in months using `geom_bar`
```{r}

ggplot(y1,aes(y=m_s, x = MONTH))+
  geom_bar(stat = 'identity')
```

13. Plot a bar chart of average speeds in months filled by `SEX`
```{r}
y2=y %>% 
  group_by(MONTH,SEX) %>% 
  summarise(m_s=mean(TRAV_SP1)) 
  
ggplot(y2,aes(MONTH,m_s,fill=SEX))+
    geom_col()
```

14. Plot a bar chart of average speeds in months colored by `INJ_SEV`

```{r}
y3=y %>% 
  group_by(MONTH,INJ_SEV) %>% 
  summarise(m_s=mean(TRAV_SP1)) 
  
ggplot(y3,aes(MONTH,m_s,fill=INJ_SEV))+
    geom_col()
```


15. Refer to this [link](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Diverging%20Bars) to have a similar followingplot: 

- Horizontal axis is for (monthly) average speed
- The vertical axis is for months
- Color by two colors: one for above overall average speed and the other for below the avarage speed
- The speed on the horizontal axis is standardized
```{r}
y4=y %>% 
  group_by(MONTH) %>% 
  summarise(m_s=mean(TRAV_SP1)) %>% 
  mutate(center_mean=mean(m_s)) %>%
  mutate(sd=m_s-center_mean) %>% 
  mutate(sd_level=ifelse(sd>0,"above","below"))


ggplot(y4, aes(x=reorder(MONTH,sd), y=sd)) + 
  geom_bar(stat='identity', aes(fill=sd_level), width=0.5)  +
  scale_fill_manual(name="Standarized mean speed per month", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised average speed from 'c2015'", 
       title= "Diverging Bars") + 
  coord_flip()
```
  
  
  
16. Refer to this [link](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Diverging%20Dot%20Plot) to have a similar followingplot: 
- Horizontal Axis is for mean speed
- Vertical Axis is for `INJ_SEV`
- Color by `SEX`
- The numbers of speed are shown in points. 
```{r}
n=y %>% 
  group_by(INJ_SEV,SEX) %>% 
  summarise(m_s=mean(TRAV_SP1)) %>% 
  mutate(center_mean=mean(m_s)) %>% 
  mutate(sd=m_s-center_mean)
ggplot(n, aes(x=INJ_SEV, y=sort(sd), label=sd)) + 
  geom_point(stat='identity', aes(col=SEX), size=3)  +
  scale_color_manual(name="SEX", 
                     labels = c("Female", "Male"), 
                     values = c("Female"="#00ba38", "Male"="#f8766d")) + 
  geom_text(color="white", size=1) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized speed from 'c2015': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()

```


17. Refer to this [link](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Population%20Pyramid) to have a similar followingplot: 
- Horizontal Axis is for speed
- Vertical Axis is for `DAY`
- Color by `SEX`
- The should be a `invisible` vertical line seperating the two sexes. 

```{r}
u=y %>% 
  mutate(SEXN=case_when(SEX=="Female"~"1",
                        SEX=="Male"~"-1")) %>% 
  mutate(SEXN=as.numeric(SEXN)) %>% 
  mutate(SPEED=TRAV_SP1*SEXN) 

  
ggplot(u, aes(x = DAY, y = SPEED, fill = SEX)) + 
                              geom_bar(stat = "identity", width = .6)+
                              coord_flip() +  
                              theme(plot.title = element_text(hjust = .5), 
                                    axis.ticks = element_blank()) +   
                              scale_fill_brewer(palette = "Dark2") 
  
```

18-20. Generate three other interesting graphs from the dataset. 
```{r}
y %>% 
  str
S=y %>% 
  group_by(STATE) %>% 
  summarise(m_s=mean(TRAV_SP1)) %>% 
  mutate(center_mean=mean(m_s)) %>%
  mutate(sd=m_s-center_mean) %>% 
  mutate(sd_level=ifelse(sd>0,"above","below"))


ggplot(S, aes(x=reorder(STATE,sd), y=sd)) + 
  geom_bar(stat='identity', aes(fill=sd_level), width=0.5)  +
  scale_fill_manual(name="Standarized mean speed for each State", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised average speed from 'c2015'", 
       title= "Diverging Bars") + 
  coord_flip()
```
```{r}
P=y %>% 
  group_by(DAY) %>% 
  summarise(m_s=mean(TRAV_SP1))
ggplot(P,aes(DAY, m_s))+
  geom_line()+
  geom_smooth()
  #This graph shows that drivers will drive faster than other days in the month.
```
```{r}
ggplot(y,aes(AGE,color=DEFORMED))+
  geom_histogram(bins = 50)
```

