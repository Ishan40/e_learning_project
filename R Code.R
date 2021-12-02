# Import Libraries
library(ltm)
library(ggplot2)

# Read data
dataset<-read.csv("F:/MSc/Sem 3/Statistical Inference/Data/Data - Survey on e-learning satisfaction (219399L).csv")

# Preprocess data
dataset$e.learning_Effectivity = ifelse(dataset$How.effective.has.e.learning.been.for.you.=="Not at all effective", 1, 
                                 ifelse(dataset$How.effective.has.e.learning.been.for.you.=="Slightly effective", 2,
                                 ifelse(dataset$How.effective.has.e.learning.been.for.you.=="Moderately effective", 3,
                                 ifelse(dataset$How.effective.has.e.learning.been.for.you.=="Very effective", 4,
                                 ifelse(dataset$How.effective.has.e.learning.been.for.you.=="Extremely effective", 5, 0)))))

dataset$uni_offer_resources = ifelse(dataset$How.helpful.your.University.has.been.in.offering.you.the.resources.to.learn.from.e.learning.=="Not at all helpful", 1, 
                                        ifelse(dataset$How.helpful.your.University.has.been.in.offering.you.the.resources.to.learn.from.e.learning.=="Slightly helpful", 2,
                                               ifelse(dataset$How.helpful.your.University.has.been.in.offering.you.the.resources.to.learn.from.e.learning.=="Moderately helpful", 3,
                                                      ifelse(dataset$How.helpful.your.University.has.been.in.offering.you.the.resources.to.learn.from.e.learning.=="Very helpful", 4,
                                                             ifelse(dataset$How.helpful.your.University.has.been.in.offering.you.the.resources.to.learn.from.e.learning.=="Extremely helpfuL", 5, 0)))))


dataset$e_learning_enjoy = ifelse(dataset$Do.you.enjoy.learning.remotely.=="No, not at all", 1, 
                                     ifelse(dataset$Do.you.enjoy.learning.remotely.=="No, there are quite a few challenges", 2,
                                            ifelse(dataset$Do.you.enjoy.learning.remotely.=="Yes, but I would like to change a few things", 3,
                                                   ifelse(dataset$Do.you.enjoy.learning.remotely.=="Yes, absolutely", 4,0))))


dataset$teachers_help = ifelse(dataset$How.helpful.are.your.teachers.while.studying.online.=="Not at all helpful", 1, 
                                     ifelse(dataset$How.helpful.are.your.teachers.while.studying.online.=="Slightly helpful", 2,
                                            ifelse(dataset$How.helpful.are.your.teachers.while.studying.online.=="Moderately helpful", 3,
                                                   ifelse(dataset$How.helpful.are.your.teachers.while.studying.online.=="Very helpful", 4,
                                                          ifelse(dataset$How.helpful.are.your.teachers.while.studying.online.=="Extremely helpful", 5, 0)))))



dataset$course_content_satis = ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.content.of.the.courses.=="Very Dissatisfied", 1, 
                               ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.content.of.the.courses.=="Dissatisfied", 2,
                                      ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.content.of.the.courses.=="Neither Satisfied or Dissatisfied", 3,
                                             ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.content.of.the.courses.=="Satisfied", 4,
                                                    ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.content.of.the.courses.=="Very Satisfied", 5, 0)))))


dataset$course_format_satis = ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.format.of.the.courses.in.e.learning.=="Very Dissatisfied", 1, 
                                      ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.format.of.the.courses.in.e.learning.=="Dissatisfied", 2,
                                             ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.format.of.the.courses.in.e.learning.=="Neither Satisfied or Dissatisfied", 3,
                                                    ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.format.of.the.courses.in.e.learning.=="Satisfied", 4,
                                                           ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.format.of.the.courses.in.e.learning.=="Very Satisfied", 5, 0)))))


dataset$online_instructor_interaction = ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.online.interaction.you.had.with.the.instructor.=="Very Dissatisfied", 1, 
                                     ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.online.interaction.you.had.with.the.instructor.=="Dissatisfied", 2,
                                            ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.online.interaction.you.had.with.the.instructor.=="Neither Satisfied or Dissatisfied", 3,
                                                   ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.online.interaction.you.had.with.the.instructor.=="Satisfied", 4,
                                                          ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.online.interaction.you.had.with.the.instructor.=="Very Satisfied", 5, 0)))))

dataset$online_instructor_interaction_with_students = ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.amount.of.online.interaction.you.had.with.other.students.=="Very Dissatisfied", 1, 
                                               ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.amount.of.online.interaction.you.had.with.other.students.=="Dissatisfied", 2,
                                                      ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.amount.of.online.interaction.you.had.with.other.students.=="Neither Satisfied or Dissatisfied", 3,
                                                             ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.amount.of.online.interaction.you.had.with.other.students.=="Satisfied", 4,
                                                                    ifelse(dataset$How.satisfied.or.dissatisfied.were.you.with.the.amount.of.online.interaction.you.had.with.other.students.=="Very Satisfied", 5, 0)))))


dataset$instructor_accessible = ifelse(dataset$The.course.instructor.was.accessible.to.answer.questions.or.give.feedback.=="Strongly Disagree", 1, 
                                                             ifelse(dataset$The.course.instructor.was.accessible.to.answer.questions.or.give.feedback.=="Disagree", 2,
                                                                    ifelse(dataset$The.course.instructor.was.accessible.to.answer.questions.or.give.feedback.=="Undecided", 3,
                                                                           ifelse(dataset$The.course.instructor.was.accessible.to.answer.questions.or.give.feedback.=="Agree", 4,
                                                                                  ifelse(dataset$The.course.instructor.was.accessible.to.answer.questions.or.give.feedback.=="Strongly Agree", 5, 0)))))


dataset$clear_course_topics_ppt = ifelse(dataset$The.presentation.of.course.topics.was.clear.=="Strongly Disagree", 1, 
                                       ifelse(dataset$The.presentation.of.course.topics.was.clear.=="Disagree", 2,
                                              ifelse(dataset$The.presentation.of.course.topics.was.clear.=="Undecided", 3,
                                                     ifelse(dataset$The.presentation.of.course.topics.was.clear.=="Agree", 4,
                                                            ifelse(dataset$The.presentation.of.course.topics.was.clear.=="Strongly Agree", 5, 0)))))



dataset$overall_e.learning = ifelse(dataset$How.do.you.feel.overall.about.e.learning.=="Poor", 1, 
                                         ifelse(dataset$How.do.you.feel.overall.about.e.learning.=="Fair", 2,
                                                ifelse(dataset$How.do.you.feel.overall.about.e.learning.=="Good", 3,
                                                       ifelse(dataset$How.do.you.feel.overall.about.e.learning.=="Very good", 4,
                                                              ifelse(dataset$How.do.you.feel.overall.about.e.learning.=="Excellent", 5, 0)))))


dataset$student_stressful = ifelse(dataset$How.stressful.were.your.students.while.learning.remotely.during.the.COVID.Not.at.all.stressful9.pandemic.=="Not at all stressful", 5, 
                                    ifelse(dataset$How.stressful.were.your.students.while.learning.remotely.during.the.COVID.Not.at.all.stressful9.pandemic.=="Slightly stressful", 4,
                                           ifelse(dataset$How.stressful.were.your.students.while.learning.remotely.during.the.COVID.Not.at.all.stressful9.pandemic.=="Moderately stressful", 3,
                                                  ifelse(dataset$How.stressful.were.your.students.while.learning.remotely.during.the.COVID.Not.at.all.stressful9.pandemic.=="Very stressful", 2,
                                                         ifelse(dataset$How.stressful.were.your.students.while.learning.remotely.during.the.COVID.Not.at.all.stressful9.pandemic.=="Extremely stressful", 1, 0)))))


dataset$peaceful_env = ifelse(dataset$How.peaceful.is.the.environment.at.home.while.e.learning.=="Not at all helpful", 1, 
                                   ifelse(dataset$How.peaceful.is.the.environment.at.home.while.e.learning.=="Slightly helpful", 2,
                                          ifelse(dataset$How.peaceful.is.the.environment.at.home.while.e.learning.=="Moderately helpful", 3,
                                                 ifelse(dataset$How.peaceful.is.the.environment.at.home.while.e.learning.=="Very helpful", 4,
                                                        ifelse(dataset$How.peaceful.is.the.environment.at.home.while.e.learning.=="Extremely helpful", 5, 5)))))


dataset$internet_connection = ifelse(dataset$How.much.satisfied.with.the.internet.connection.for.e.learning.=="Very Dissatisfied", 1, 
                              ifelse(dataset$How.much.satisfied.with.the.internet.connection.for.e.learning.=="Dissatisfied", 2,
                                     ifelse(dataset$How.much.satisfied.with.the.internet.connection.for.e.learning.=="Neither Satisfied or Dissatisfied", 3,
                                            ifelse(dataset$How.much.satisfied.with.the.internet.connection.for.e.learning.=="Satisfied", 4,
                                                   ifelse(dataset$How.much.satisfied.with.the.internet.connection.for.e.learning.=="Very Satisfied", 5, 5)))))





# Descriptive Charts

# Gender
ggplot(dataset, aes(What.is.your.Gender.)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


# University
t<-table(dataset$X02..What.is.your.university.)
percentlabels<- round(100*t/sum(t), 2)
pielabels<- paste(names(t)," ", percentlabels, "%", sep="")
pie(table(dataset$X02..What.is.your.university.),
    labels = pielabels, 
    main = "Ditribution of University")


# faculty
ggplot(dataset, aes(What.is.your.Faculty.)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Acedamic Year
t<-table(dataset$X03..What.is.your.Academic.Year)
percentlabels<- round(100*t/sum(t), 2)
pielabels<- paste(names(t)," , ", percentlabels, "%", sep="")
pie(table(dataset$X03..What.is.your.Academic.Year),
    labels = pielabels, 
    main = "Ditribution of Academic Year")


# Study Location
t<-table(dataset$What.is.your.study.location.)
percentlabels<- round(100*t/sum(t), 2)
pielabels<- paste(names(t)," , ", percentlabels, "%", sep="")
pie(table(dataset$What.is.your.study.location.),
    labels = pielabels, 
    main = "Ditribution of Study Location")


# Mode of internet connection
t<-table(dataset$What.is.the.mode.of.your.internet.connection.)
percentlabels<- round(100*t/sum(t), 2)
pielabels<- paste(names(t)," , ", percentlabels, "%", sep="")
pie(table(dataset$What.is.the.mode.of.your.internet.connection.),
    labels = pielabels, 
    main = "Mode of internet connection")


# Frequency Tables

# Gender
table(dataset$What.is.your.Gender.)
round(table(dataset$What.is.your.Gender.)/sum(table(dataset$What.is.your.Gender.)), 2)*100

# University
table(dataset$What.is.your.study.location.)
round(table(dataset$What.is.your.study.location.)/sum(table(dataset$What.is.your.study.location.)), 2)*100

# Mode of internet connection.
table(dataset$What.is.the.mode.of.your.internet.connection)
round(table(dataset$What.is.the.mode.of.your.internet.connection)/sum(table(dataset$What.is.the.mode.of.your.internet.connection)), 2)*100

# Device
table(dataset$What.device.do.you.use.for.e.learning.)
round(table(dataset$What.device.do.you.use.for.e.learning.)/sum(table(dataset$What.device.do.you.use.for.e.learning.)), 2)*100

# Study Hours
table(dataset$How.much.time.do.you.spend.each.day.on.an.average.on.distance.education)
round(table(dataset$How.much.time.do.you.spend.each.day.on.an.average.on.distance.education)/sum(table(dataset$How.much.time.do.you.spend.each.day.on.an.average.on.distance.education)), 2)*100

# Overall E-learning Satisfaction
table(dataset$How.do.you.feel.overall.about.e.learning.)
round(table(dataset$How.do.you.feel.overall.about.e.learning.)/sum(table(dataset$How.do.you.feel.overall.about.e.learning.)), 2)*100

# cronbach.alpha

# Acedemic factors
cronbach.alpha(dataset[c("uni_offer_resources", "teachers_help", "instructor_accessible","course_content_satis", "course_format_satis", "clear_course_topics_ppt")])

# Psychological factors
cronbach.alpha(dataset[c("online_instructor_interaction", "online_instructor_interaction_with_students", "student_stressful")])

# Pysical resources availability factors
cronbach.alpha(dataset[c("peaceful_env", "internet_connection")])



# Overall E-learning satisfaction between males and females
dataset$gender = ifelse(dataset$What.is.your.Gender.=="Male",1,0)
t.test(dataset$gender, dataset$Overall_satisfaction)

boxplot(dataset$overall_e.learning~dataset$gender, xlab="Gender", ylab="E-learning Satisfaction")


# Correlation analysis 
dataset$Overall_satisfaction = dataset$overall_e.learning
dataset$Acedemic_score<-rowMeans(dataset[c("uni_offer_resources", "teachers_help","instructor_accessible", "course_content_satis", "course_format_satis","clear_course_topics_ppt")])
dataset$Psychological_Score <-rowMeans(dataset[c("online_instructor_interaction_with_students", "online_instructor_interaction_with_students", "student_stressful")])
dataset$Physical_Resources_Availability_Score<- rowMeans(dataset[c("peaceful_env", "internet_connection")])

cor(dataset[c("Overall_satisfaction", "Psychological_Score", "Psychological_Score", "Physical_Resources_Availability_Score")])
cor.test(dataset$Overall_satisfaction, dataset$Acedemic_score)
cor.test(dataset$Overall_satisfaction, dataset$Psychological_Score)
cor.test(dataset$Overall_satisfaction, dataset$Physical_Resources_Availability_Score)


# SL egression
regression <- lm(Overall_satisfaction~gender+Acedemic_score+Psychological_Score+Physical_Resources_Availability_Score, data=dataset)
summary(regression)
confint(regression)
