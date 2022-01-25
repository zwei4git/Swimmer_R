##SWIMMER PLOT

#Subjcet: subject id
#trtdur: treatment duration in days
#prstart: the date (#days from trt start) PR started
#crstart: the date (#days from trt start) CR started
#pfs: Progression-Free survival
#hist: Histology in characters
#ncyc: number of cycles subject stayed on treatment

df%>%
  select('Subject','trtdur','prstart','crstart','pfsstat','pfs','hist','ncyc')%>%
  mutate(trtdur=trtdur/30.4375,
         prstart=prstart/30.4375,
         crstart=crstart/30.4375,
         pd=ifelse(pfsstat==1,pfs/30.4375,NA),
         cont=ifelse(Subject %in% off_tx,trtdur,NA))->swm1


swm2<-swm1[order(swm1[,'trtdur'],decreasing = FALSE),]
swm3<-cbind(swm2,x=barplot(swm2[,'trtdur']))

swm3%>%
  select(Subject,prstart,crstart,pd,x)%>%
  gather(key = variable,value = time,2:4)->swm_x

ggplot()+
  geom_bar(aes(swm3$x,y=swm3[,'trtdur'],fill=swm3[,'hist']),stat = 'identity')+
  #scake y for up to 12 months
  scale_y_continuous(limits=c(0,12), breaks=seq(0,12,3))+
  #myColors is self defined color palette
  scale_fill_manual(values = myColors)+
  #wrap labels of histology
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 60))+
  #only to add black border
  geom_point(data=swm_x,aes(x, time,shape=variable),size=6)+ 
  geom_point(data=swm_x,aes(x, time,shape=variable,color=variable),size=5)+
  #Add continued subjects
  geom_segment(aes(x=swm3$x,xend=swm3$x,y = swm3$cont, yend=swm3$cont+0.8),
               arrow = arrow(type="closed", length=unit(0.03,"npc")))+
  #Shape and coloe of RECIST outcomes
  scale_shape_manual(values=c(19,17,19),labels=c('CR','PD','PR')) +
  scale_color_manual(values=c('green','blue','yellow'),labels=c('CR','PD','PR'))+
  labs(fill="Histo", shape="Response", colour="Response",
       x="",y='Months from Start of Treatment')+
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        plot.margin=unit(c(0,0,0,5),'mm'),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_text(size=16),
        legend.key.size = unit(1.5, 'lines'),
        legend.text = element_text(size=12))+
  coord_flip()->swimmer_plot