setwd ('C:/Users/Gesche Huebner/WORK/ResearchProjects/CREDS/Health_Wellbeing_EHS/Analysis_Wellbeing')

#note, we need to run the main file (EHS_Wellbeing_Markdown) first and keep the outputs in workspace

#this figure produced the regression plots for only those variables that are significant. 

#reduced df for significant variables
keep_var_ls<-broom::tidy(model_life_dummy) %>% filter(p.value<0.05) %>% filter(term != '(Intercept)')
keep_var_worth<-broom::tidy(model_worth_dummy) %>% filter(p.value<0.05) %>% filter(term != '(Intercept)')
keep_var_happy<-broom::tidy(model_happy_dummy) %>% filter(p.value<0.05) %>% filter(term != '(Intercept)')
keep_var_anxious<-broom::tidy(model_anxious_dummy) %>% filter(p.value<0.05) %>% filter(term != '(Intercept)')
#combine them all
all_keep<-unique(c(keep_var_ls$term, keep_var_worth$term, keep_var_happy$term, keep_var_anxious$term))

#now, identify which variables to drop
all_var<-broom::tidy(model_life_dummy)
all_drop<-all_var %>% filter((term %in% all_keep)==FALSE)


# #reduced df for non-significant variables
# drop_var_ls<-broom::tidy(model_life_dummy) %>% filter(p.value>=0.05) 
# drop_var_worth<-broom::tidy(model_worth_dummy) %>% filter(p.value>=0.05) 
# drop_var_happy<-broom::tidy(model_happy_dummy) %>% filter(p.value>=0.05) 
# drop_var_anxious<-broom::tidy(model_anxious_dummy) %>% filter(p.value>=0.05) 
# #combine them and identify unique set of names
# all_drop<-unique(c(drop_var_ls, drop_var_worth, drop_var_happy, drop_var_anxious))
# #access var names with $term

#plot this 

all.models <- list()
all.models[[1]] <- model_life_dummy
all.models[[2]] <- model_worth_dummy
all.models[[3]] <- model_happy_dummy
all.models[[4]]<-model_anxious_dummy
set_theme(axis.textsize=.8, legend.pos="top", axis.title.size=1, axis.angle.y = 0)


plot_models(
  model_anxious_dummy, model_happy_dummy, model_worth_dummy, model_life_dummy, grid=T, 
  m.labels = c("Anxious", "Happy", "Worthwhile", "LifeSatisfaction" ),  
  show.values = F,  p.shape = T, wrap.labels = 60, show.legend=T,     
  dot.size=1.5, line.size=0.5, prefix.labels = c("varname"), axis.lim=c(0.08, 10), rm.terms=all_drop$term
)





tab_model(all.models,  show.reflvl = F, rm.terms=all_drop$term,
          p.style="stars", prefix.labels = c("varname"), show.p=T, 
          show.se = TRUE, show.ci=F, show.r2=T, file = "reg_reduced4.doc")


save_plot(
  "regression_significant76.jpg",
  fig = last_plot(),
  width = 24,
  height = 18,
  dpi = 300,
  theme = theme_get(),
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.8,
  axis.titlesize = 0.75,
  legend.textsize = 0.6,
  legend.titlesize = 0.65,
  legend.itemsize = 0.5
)

#now, plot the other model, going from EPC to Area to Census data
set_theme(axis.textsize=.75, legend.pos="top", axis.title.size=1, axis.angle.y = 0, )
plot_models(
  comp_ls2[[3]], comp_ls2[[2]], comp_ls2[[1]], grid=T, 
  m.labels = c("Census_Data", "Area_Data", "EPC_Data"),  
  show.values = F,  show.p=T,  wrap.labels = 60, show.legend=T,     
  dot.size=1.5, p.shape = T, value.size = 2, prefix.labels = c("varname"), axis.lim=c(0.08, 5)
)

save_plot(
  "regression_update2.jpg",
  fig = last_plot(),
  width = 24,
  height = 26,
  dpi = 300,
  theme = theme_get(),
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.8,
  axis.titlesize = 0.75,
  legend.textsize = 0.6,
  legend.titlesize = 0.65,
  legend.itemsize = 0.5
)
            