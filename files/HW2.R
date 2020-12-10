library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(plotly)
library(RColorBrewer)
library(FactoMineR)
library(hrbrthemes)
library(RColorBrewer)

train_x<-fread( file= "data/UWave-20201126T225338Z-001/UWave/uWaveGestureLibrary_X_TRAIN", sep=" ")
train_y<-fread( file= "data/UWave-20201126T225338Z-001/UWave/uWaveGestureLibrary_Y_TRAIN", sep=" ")
train_z<-fread( file= "data/UWave-20201126T225338Z-001/UWave/uWaveGestureLibrary_Z_TRAIN", sep=" ")

train_x_df<-as.data.frame(train_x)
train_y_df<-as.data.frame(train_y)
train_z_df<-as.data.frame(train_z)

## formatting x data

names(train_x_df)[1]<-"class"
names(train_x_df)[2:316]<-1:315

train_x_df<-train_x_df%>%
  mutate(time_series_id=1:896)%>%
  mutate(class=factor(ifelse(class==1, "a",
                             ifelse(class==2, "b",
                                    ifelse(class==3, "c",
                                           ifelse(class==4, "d",
                                                  ifelse(class==5,"e",
                                                         ifelse(class==6,"f",
                                                                ifelse(class==7, "g",
                                                                       "h"))))))),
                      levels=c("a","b","c","d","e","f","g","h")))

x_raw<-train_x_df[-c(1,317)] %>% setNames(paste0('X.', names(.)))
y_raw<-train_y_df[-c(1,317)] %>% setNames(paste0('Y.', names(.)))
z_raw<-train_z_df[-c(1,317)] %>% setNames(paste0('Z.', names(.)))
mds_data<-cbind(train_x_df[c(317,1)],x_raw,y_raw,z_raw)

x_formatted<-pivot_longer(
  train_x_df,
  cols=-c(time_series_id,class),
  names_to = "time_index",
  values_to = "X",
)

x_formatted<-x_formatted%>%transform(time_index=as.integer(time_index))%>%relocate(class, .after=X)%>%arrange(time_series_id,class)

## formatting y data
train_y_df<-as.data.frame(train_y)

names(train_y_df)[1]<-"class"
names(train_y_df)[2:316]<-1:315

train_y_df<-train_y_df%>%
  mutate(time_series_id=1:896)%>%
  mutate(class=factor(ifelse(class==1, "a",
                             ifelse(class==2, "b",
                                    ifelse(class==3, "c",
                                           ifelse(class==4, "d",
                                                  ifelse(class==5,"e",
                                                         ifelse(class==6,"f",
                                                                ifelse(class==7, "g",
                                                                       "h"))))))),
                      levels=c("a","b","c","d","e","f","g","h")))

y_formatted<-pivot_longer(
  train_y_df,
  cols=-c(time_series_id,class),
  names_to = "time_index",
  values_to = "Y",
)

y_formatted<-y_formatted%>%transform(time_index=as.integer(time_index))%>%relocate(class, .after=Y)%>%
  arrange(time_series_id,class)

## formatting z data

names(train_z_df)[1]<-"class"
names(train_z_df)[2:316]<-1:315

train_z_df<-train_z_df%>%
  mutate(time_series_id=1:896)%>%
  mutate(class=factor(ifelse(class==1, "a",
                             ifelse(class==2, "b",
                                    ifelse(class==3, "c",
                                           ifelse(class==4, "d",
                                                  ifelse(class==5,"e",
                                                         ifelse(class==6,"f",
                                                                ifelse(class==7, "g",
                                                                       "h"))))))),
                      levels=c("a","b","c","d","e","f","g","h")))


z_formatted<-pivot_longer(
  train_z_df,
  cols=-c(time_series_id,class),
  names_to = "time_index",
  values_to = "Z",
)

z_formatted<-z_formatted%>%transform(time_index=as.integer(time_index))%>%
  relocate(class, .after=Z)%>%
  arrange(time_series_id,class)

combined<-cbind(x_formatted,y_formatted,z_formatted)
combined<-combined[c(1,2,3,7,11,12)]

ts_Position<-combined %>% 
  group_by(time_series_id) %>% 
  mutate(X_velocity = cumsum(X),
         Y_velocity = cumsum(Y),
         Z_velocity = cumsum(Z),
         X_position = cumsum(X_velocity),
         Y_position = cumsum(Y_velocity),
         Z_position = cumsum(Z_velocity))
ts_id<-list(list(11,"a"),list(15,"b"),list(4,"c"),list(5,"d"),list(2,"e"),list(1,"f"),list(7,"g"),list(6,"h"))


data<-ts_Position%>%filter(time_series_id==ts_id[[1]][[1]],class==ts_id[[1]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[1]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[1]][[2]]))

plot2
data<-ts_Position%>%filter(time_series_id==ts_id[[2]][[1]],class==ts_id[[2]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[2]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[2]][[2]]))

plot2

data<-ts_Position%>%filter(time_series_id==ts_id[[3]][[1]],class==ts_id[[3]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[3]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[3]][[2]]))

plot2


data<-ts_Position%>%filter(time_series_id==ts_id[[4]][[1]],class==ts_id[[4]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[4]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[4]][[2]]))

plot2

data<-ts_Position%>%filter(time_series_id==ts_id[[5]][[1]],class==ts_id[[5]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[5]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[5]][[2]]))

plot2

data<-ts_Position%>%filter(time_series_id==ts_id[[6]][[1]],class==ts_id[[6]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[6]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[6]][[2]]))

plot2

data<-ts_Position%>%filter(time_series_id==ts_id[[7]][[1]],class==ts_id[[7]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[7]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[7]][[2]]))

plot2

data<-ts_Position%>%filter(time_series_id==ts_id[[8]][[1]],class==ts_id[[8]][[2]])

plot1<-plot_ly(data,
               x=~X_velocity, y=~Y_velocity, z=~Z_velocity,color =~ceiling(time_index/100),
               type="scatter3d", mode="markers") %>% 
  layout(title = paste("Velocity Chart for a Gesture from Class",ts_id[[8]][[2]]))

plot1

plot2<-plot_ly(data,
               x=~X_position, y=~Y_position, z=~Z_position,
               color =~ceiling(time_index/100),
               type="scatter3d", mode="markers")%>% 
  layout(title = paste("Position Chart for a Gesture from Class",ts_id[[8]][[2]]))

plot2
##PART B
###PCA for all series combined

cor(ts_Position[,c(10,11,12)])

pca_all <- princomp(as.matrix(ts_Position[,c(10,11,12)]),cor=T)

summary(pca_all,loadings=TRUE)

PCA_vis<-PCA(ts_Position[,c(10,11,12)])
PCA_vis$eig
ts_Position_uni<-cbind(ts_Position,PC1=pca_all$scores[,1])


classes<-c("a","b","c","d","e","f","g","h")

for(i in 1:8){
  PCA_all_ts_a<-ts_Position_uni%>%filter(class==classes[i])%>%select(time_series_id,time_index,PC1,class)
  random_ids<-sample (unique(PCA_all_ts_a$time_series_id), size=2, replace =F)
  PCA_all_ts_a<-PCA_all_ts_a%>%
    filter(time_series_id %in% random_ids)%>%
    pivot_wider(.,names_from = time_series_id , values_from = PC1 ,names_prefix = "id")
  
  plot<-ggplot(PCA_all_ts_a,aes_string(x=names(PCA_all_ts_a)[1]))+
    geom_line(aes_string(y=names(PCA_all_ts_a)[3]),size=2,color="#CC6666")+
    geom_line(aes_string(y=names(PCA_all_ts_a)[4]),size=2,color="#9999CC")+
    theme_ipsum()+
    xlab("Time")+
    ylab("PC1")+
    ggtitle(paste("Univariate Time Series from Class",classes[i]))
  print(plot)
}
for(i in 1:8){
  
  print(paste("PCA for the Class",classes[i]))
  ts_Position_class<-ts_Position%>%filter(class==classes[i])
  cor(ts_Position_class[,c(10,11,12)])
  
  pca_class <- princomp(as.matrix(ts_Position_class[,c(10,11,12)]),cor=T)
  
  print(summary(pca_class,loadings=TRUE))
  
  PCA_vis_class<-PCA(ts_Position_class[,c(10,11,12)])
  print(PCA_vis_class$eig)
  
}

dist_matrix<-dist(mds_data[,-c(1,2)],method = "euclidean")
MDS<-cmdscale(dist_matrix)
colnames(MDS) <- c("x","y")
MDS<-data.frame(MDS)
MDS<-cbind(mds_data[c(1,2)],MDS)
mds_plot<-ggplot(MDS,aes(x=x,y=y))+
  geom_jitter(aes(color=class),size=3)+
  scale_colour_brewer(palette ="Accent" )+
  theme_minimal()+
  labs(color="Gesture Class")

mds_plot
mds_plot+facet_wrap(~class)
