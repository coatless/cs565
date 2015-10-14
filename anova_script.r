inst_pkgs = load_pkgs =  c("ggplot2","grid","ggfortify")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)


## @knitr data_build
### Read data
# Very important that stringsAsFactors is false. You will have a bad time otherwise!
q1 = read.csv("~/Box Sync/Courses/CS 565/Assignment 4/design_feedback_q1.csv", comment.char="#", stringsAsFactors=FALSE)
q2 = read.csv("~/Box Sync/Courses/CS 565/Assignment 4/design_feedback_q2.csv", comment.char="#", stringsAsFactors=FALSE)
q3 = read.csv("~/Box Sync/Courses/CS 565/Assignment 4/design_feedback_q3.csv", comment.char="#", stringsAsFactors=FALSE)
q0 = read.csv("~/Box Sync/Courses/CS 565/Assignment 4/design_feedback.csv", comment.char="#", stringsAsFactors=FALSE)

## Prep time data
# Convert to Postix time stamp

# Accept Times
q0$AcceptTime = gsub(" PDT","",q0$AcceptTime)
q0$AcceptTime = strptime(q0$AcceptTime,format='%a %b %d %H:%M:%S %Y')

q1$AcceptTime = gsub(" PDT","",q1$AcceptTime)
q1$AcceptTime = strptime(q1$AcceptTime,format='%a %b %d %H:%M:%S %Y')

q2$AcceptTime = gsub(" PDT","",q2$AcceptTime)
q2$AcceptTime = strptime(q2$AcceptTime,format='%a %b %d %H:%M:%S %Y')

q3$AcceptTime = gsub(" PDT","",q3$AcceptTime)
q3$AcceptTime = strptime(q3$AcceptTime,format='%a %b %d %H:%M:%S %Y')

# Submit Times
q0$SubmitTime = gsub(" PDT","",q0$SubmitTime)
q0$SubmitTime = strptime(q0$SubmitTime,format='%a %b %d %H:%M:%S %Y')

q1$SubmitTime = gsub(" PDT","",q1$SubmitTime)
q1$SubmitTime = strptime(q1$SubmitTime,format='%a %b %d %H:%M:%S %Y')

q2$SubmitTime = gsub(" PDT","",q2$SubmitTime)
q2$SubmitTime = strptime(q2$SubmitTime,format='%a %b %d %H:%M:%S %Y')

q3$SubmitTime = gsub(" PDT","",q3$SubmitTime)
q3$SubmitTime = strptime(q3$SubmitTime,format='%a %b %d %H:%M:%S %Y')


## Make a data.frame

# Create empty data.frame first
obs = nrow(q1) + nrow(q2) + nrow(q3) + nrow(q0)

a = data.frame(rating = numeric(obs), 
               type = character(obs),
               type.combine = character(obs),
               tspent = numeric(obs),
               stringsAsFactors=F)

# Combine the data into the frame.
a$rating = c(q1$score,q2$score,q3$score,q0$score.1.7.)
a$type = factor(c(rep("Q1",nrow(q1)), rep("Q2",nrow(q2)), rep("Q3",nrow(q3)), rep("Q0",nrow(q0))))
a$tspent = as.numeric(
                      c(q1$SubmitTime-q1$AcceptTime,
                        q2$SubmitTime-q2$AcceptTime,
                        q3$SubmitTime-q3$AcceptTime,
                        q0$SubmitTime-q0$AcceptTime)
)

# Combined vs. Specific
a$type.combine = factor(
                        c(rep("Specific",nrow(q1)+nrow(q2)+nrow(q3)),
                          rep("Control",nrow(q0))
                          )
                        )

## @knitr do_graphics

# Do a few graphics
ggplot(a,aes(x = type, y = rating)) + geom_boxplot(aes(fill=type), alpha = 0.3) + xlab("Condition Type") +
  ylab("Rating") + ggtitle("Rating vs. Condition Type") + geom_jitter(alpha=0.5, position = position_jitter(height = 0, width=.5)) + theme_bw() +
  scale_fill_discrete(name="Condition Type")

ggsave(file="rating_boxplot.png", width = 8, height = 6)

ggplot(a,aes(x = rating)) + geom_bar(aes(fill=type, group = type), alpha = 0.5, position="dodge") + xlab("Scores Across Conditions") +
  ylab("Frequency") + ggtitle("Frequency vs. Scores Across Conditions") +theme_bw() + scale_fill_discrete(name="Condition Type")

ggsave(file="rating_barplot_all.png", width = 8, height = 6)

# Generate individual plots
for(i in 1:4){
ggplot(subset(a, type == paste0("Q",i-1)),aes(x = rating)) + geom_bar(aes(fill=type, group = type), alpha = 0.5, position="dodge") + xlab("Scores Across Conditions") +
  ylab("Frequency") + ggtitle("Frequency vs. Scores Across Conditions") +theme_bw() + scale_fill_discrete(name="Condition Type")

ggsave(file=paste0("rating_barplot_q",i-1,".png"), width = 8, height = 6)
}


# Combined Specifics
ggplot(a,aes(x = type.combine, y = rating)) + geom_boxplot(aes(fill=type.combine), alpha = 0.3) + xlab("Condition Type") +
  ylab("Rating") + ggtitle("Rating vs. Condition Type") + geom_jitter(alpha=0.5, position = position_jitter(height = 0, width=.5)) + theme_bw() +
  scale_fill_discrete(name="Condition Type")

ggsave(file="rating_boxplot_combined.png", width = 8, height = 6)

ggplot(a,aes(x = rating)) + geom_bar(aes(fill=type.combine, group = type.combine), alpha = 0.5, position="dodge") + xlab("Condition Scores") +
  ylab("Frequency") + ggtitle("Frequency vs. Condition Scores") +theme_bw() + scale_fill_discrete(name="Condition Type")

ggsave(file="rating_barplot_combined.png", width = 8, height = 6)

## Time Grid
# Combined Density with Barplot for time conditions
fig1 = ggplot(data = a, aes(x = tspent)) + 
  geom_density(aes(group=type,fill=type), alpha = 0.3, position="stack") + 
  xlab("Time Spent on HIT (Sec)") + ylab("Density") + ggtitle("Density vs. Time Spent on HIT (Sec)") +
  theme_bw() + scale_fill_discrete(name="Condition Type")

ggsave(file="rating_time_density.png", fig1, width = 8, height = 6)


fig2 = ggplot(data = a, aes(x = type, y = tspent)) +
  geom_boxplot(aes(group=type,fill=type), alpha = 0.3) +
  coord_flip() + 
  geom_jitter(alpha=0.5, position = position_jitter(height = 0, width=.5)) + 
  theme_bw() + 
  ylab("Time Spent on HIT (Sec)") + xlab("Condition Types") + ggtitle("Condition Types vs. Time Spent on HIT (Sec)")  +
  scale_fill_discrete(name="Condition Type")

ggsave(file="rating_time_box.png", fig2, width = 8, height = 6)

# Graphs for time between two
ggplot(data = a, aes(x = tspent)) + 
  geom_density(aes(group=type.combine,fill=type.combine), alpha = 0.3, position="stack") + 
  xlab("Time Spent on HIT (Sec)") + ylab("Density") + ggtitle("Density vs. Time Spent on HIT (Sec)") +
  theme_bw() + scale_fill_discrete(name="Condition Type")

ggsave(file="rating_time_density_split.png", width = 8, height = 6)

ggplot(data = a, aes(x = type.combine, y = tspent)) +
  geom_boxplot(aes(group=type.combine,fill=type.combine), alpha = 0.3) +
  coord_flip() + 
  geom_jitter(alpha=0.5, position = position_jitter(height = 0, width=.5)) + 
  theme_bw() + 
  ylab("Time Spent on HIT (Sec)") + xlab("Condition Types") + ggtitle("Condition Types vs. Time Spent on HIT (Sec)")  +
  scale_fill_discrete(name="Condition Type")

ggsave(file="rating_time_box_split.png", width = 8, height = 6)



### Statistical Data Analysis

## Split Condition Data

## ANOVA (One-Way)

# Easy way with Welch correction for nonhomogeneity:
oneway.test(rating ~ type, data=a)

# Normal way without correction:
fit = aov(rating ~ type, data=a)
summary(fit)

print(xtable(fit),type="html")

# Generate Diagnostic Plots

png("anova_split_diag.png", width = 1200, height = 600, units = "px")

autoplot(fit, which = 1:6, ncol = 3, 
         label.size = 3, colour = "type") + 
         theme_bw()

dev.off()

# Additional tests
bartlett.test(rating ~ type, data=a)

kruskal.test(rating ~ type, data=a)

for(i in 1:ncol(a)){
  print(shapiro.test(a$rating[a$type == paste0("Q",i-1)]))
}


# Tukey Ranges! 
(o = TukeyHSD(fit) )

print(xtable(o$type),type="html")

# Or graphically represent the hsd graph.
hsd.graph = data.frame(o$type)
hsd.graph$Comparison = row.names(hsd.graph)

ggplot(hsd.graph, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Scores by Condition") +
  coord_flip()

ggsave("hsd_split.png", width=8, height=7)



## Combined Condition Data

## ANOVA (One-Way)

# Easy way with Welch correction for nonhomogeneity:
oneway.test(rating ~ type.combine, data=a)

# Normal way without correction:
fit2 = aov(rating ~ type.combine, data=a)
summary(fit2)

print(xtable(fit2),type="html")

# Generate Diagnostic Plots

png("anova_combined_diag.png", width = 1300, height = 600, units = "px")

p = autoplot(fit2, which = 1:6, ncol = 3, 
         label.size = 3, colour = "type.combine") + 
  theme_bw()

p

dev.off()

# Additional tests
bartlett.test(rating ~ type.combine, data=a)

kruskal.test(rating ~ type.combine, data=a)

df.type = c("Specific","Control")
for(i in 1:length(df.type)){
  print(shapiro.test(a$rating[a$type.combine == df.type[i]]))
}

# Tukey Ranges! 
( o1 = TukeyHSD(fit2) )

print(xtable(o1$type),type="html")

# Or graphically represent the hsd graph.
hsd.graph = data.frame(o1$type.combine)
hsd.graph$Comparison = row.names(hsd.graph)

ggplot(hsd.graph, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Scores by Condition") +
  coord_flip()

ggsave("hsd_combined.png", width=8, height=7)


## Time Data
m = aov(tspent~type,data=a)

summary(m)

print(xtable(m),type="html")


out3 = TukeyHSD(m)

hsd.graph = data.frame(out3$type)
hsd.graph$Comparison = row.names(hsd.graph)

ggplot(hsd.graph, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Scores by Split Conditions") +
  coord_flip()

ggsave("hsd_time_split.png", width=8, height=7)




d = aov(tspent~type.combine,data=a)

summary(d)

print(xtable(d),type="html")

out4 = TukeyHSD(d)

hsd.graph = data.frame(out4$type)
hsd.graph$Comparison = row.names(hsd.graph)

ggplot(hsd.graph, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Scores by Combined Conditions") +
  coord_flip()

ggsave("hsd_time_combined.png", width=8, height=7)
