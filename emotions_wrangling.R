#!!!PREPARATION TO RUN THE COD!!!!#
turn_data_correct <-NAME OF YOUR DATASET FOR CORRECT TURN DATASET (new one)
call_data_correct <-NAME OF YOUR DATASET FOR CORRECT CALL DATASET (new one)

library(mltools)
library(vctrs)
library(hms)
library(data.table)
library(tidyverse)
####################


#######SCORE OF BAD EMOTION FROM AGENT DURING THE CALL#####
turn_data_summary<-turn_data_correct%>%rename(Dial_Str=`Dialogue Structure`)

filtering_data<-filter(turn_data_summary, Dial_Str != "missing")

#one-hot-encoding for both emotion and call phase data (columns with 0/1 values)
newdata <- mltools::one_hot(as.data.table(filtering_data), cols=c("Dial_Str", "Emotion"))
#agent data only
agent_index_data<-filter(newdata, Person == "agent")

#giving different importance for each call phase
agent_index_data$Dial_Str_nds<-ifelse(agent_index_data$Dial_Str_nds==1, 0, 0)
agent_index_data$Dial_Str_back_office<-ifelse(agent_index_data$Dial_Str_back_office==1, 2, 0)
agent_index_data$Dial_Str_consultation <-ifelse(agent_index_data$Dial_Str_consultation==1, 3, 0)
agent_index_data$Dial_Str_customer_recognition <-ifelse(agent_index_data$Dial_Str_customer_recognition==1, 0, 0)
agent_index_data$Dial_Str_farewell  <-ifelse(agent_index_data$Dial_Str_farewell ==1, 4, 0)
agent_index_data$Dial_Str_follow_procedure  <-ifelse(agent_index_data$Dial_Str_follow_procedure ==1, 0, 0)
agent_index_data$Dial_Str_greeting  <-ifelse(agent_index_data$Dial_Str_greeting ==1, 1, 0)
agent_index_data$Dial_Str_mechanic_planned  <-ifelse(agent_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
agent_index_data$Dial_Str_my_problem_is   <-ifelse(agent_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
agent_index_data$Dial_Str_solved   <-ifelse(agent_index_data$Dial_Str_solved  ==1, 4, 0)
agent_index_data$Dial_Str_suggest_solution    <-ifelse(agent_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
agent_index_data$Dial_Str_transfer    <-ifelse(agent_index_data$Dial_Str_transfer   ==1, 2, 0)

#giving different importance for each emotion 
agent_index_data$Emotion_angry    <-ifelse(agent_index_data$Emotion_angry   ==1, 2, 0)
agent_index_data$Emotion_fear    <-ifelse(agent_index_data$Emotion_fear   ==1, 2, 0)
agent_index_data$Emotion_happy    <-ifelse(agent_index_data$Emotion_happy   ==1, -1, 0)
agent_index_data$Emotion_sad    <-ifelse(agent_index_data$Emotion_sad   ==1, 1, 0)
agent_index_data$Emotion_nvt   <-ifelse(agent_index_data$Emotion_nvt   ==1, 0, 0)

#calculating emotional score for each call stage with given "weights" 

agent_index_data$w.Dial_Str_nds<-agent_index_data$Dial_Str_nds*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_nds*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_back_office<-agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_consultation<-agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_customer_recognition<-agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_farewell<-agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_follow_procedure<-agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_greeting<-agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_mechanic_planned<-agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_my_problem_is<-agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_solved<-agent_index_data$Dial_Str_solved*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_solved*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_suggest_solution<-agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_transfer<-agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_sad

#summing it and grouping based on calls ID 
agent_index_data$index<- rowSums(agent_index_data[,c(23:33)])
groups2<-agent_index_data%>%group_by(GENESYS_CONVERSATION_ID)%>%summarise(suming=sum(index))


###############################Personal characterictics of an agent - emotion handling##################
str(turn_data_correct)
agent <- filter(turn_data_correct, Person == "agent") #to choose only agent's emotions 
customer <- filter(turn_data_correct, Person == "customer") ##to choose only customers's emotions 

#extracting only hours from the timestamp
library(hms)
agent$hour<-as_hms(agent$Timestamp)
class(agent$hour)


#EXTRACTING ONLY HOURS FROM THE TIMESTAMP - IMPROTANT!!!
turn_data_correct$hour<-as_hms(turn_data_correct$Timestamp)

#just some graphs - cannot remind what, try to run  - probably "road maps"
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3")%>%ggplot(.,aes(x=`Dialogue Structure`,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3" & Emotion != "nvt")%>%ggplot(.,aes(x=hour,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)

#to joint data of 2 dataset - call and turn data
joinicek<-full_join(call_data_correct, turn_data_correct, "AO_transcription_id")

#only top 100 employees - TO DISCUSS AND PROBABLY TO MAKE IT FOR MORE/EVERYONE?
frequent_employees<-call_data_correct%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(n=n())%>%arrange(desc(n))%>%slice(1:100)
only_frequent<-joinicek%>%filter(AO_GENESYS_EMPLOYEE_LOGIN %in% c(frequent_employees$AO_GENESYS_EMPLOYEE_LOGIN))

#coding emotions into binary num variables
only_frequent_agent<-filter(only_frequent,Person == "agent")
only_frequent_agent$angry<-ifelse(only_frequent_agent$Emotion == "angry", 1, 0)
only_frequent_agent$happy<-ifelse(only_frequent_agent$Emotion == "happy", 1, 0)
only_frequent_agent$sad<-ifelse(only_frequent_agent$Emotion == "sad", 1, 0)
only_frequent_agent$fear<-ifelse(only_frequent_agent$Emotion == "fear", 1, 0)
only_frequent_agent$nvt<-ifelse(only_frequent_agent$Emotion == "nvt", 1, 0)

#sum of angre of the agents in different call phases
agent_angry<-only_frequent_agent%>%group_by( `Dialogue Structure` ,AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(s=sum(angry))

#weights for each phase -> TO UPDATE - state just random
agent_angry$w<-ifelse(agent_angry$`Dialogue Structure`=="greeting",0.2,ifelse(agent_angry$`Dialogue Structure`=="follow_procedure",0.1,ifelse(agent_angry$`Dialogue Structure`=="customer_recognition", 0.1,ifelse(agent_angry$`Dialogue Structure`=="my_problem_is",0.3,ifelse(agent_angry$`Dialogue Structure`=="consultation",0.8,ifelse(agent_angry$`Dialogue Structure`=="transfer",0.4, ifelse(agent_angry$`Dialogue Structure`=="suggest_solution",0.8,ifelse(agent_angry$`Dialogue Structure`=="back_office",0.2, ifelse(agent_angry$`Dialogue Structure`=="mechanic_planed",0.3, ifelse(agent_angry$`Dialogue Structure`=="solved",1,ifelse(agent_angry$`Dialogue Structure`=="farewell",1,0.1)))))))))))

#joining data about call frequencies and angre
agent_angry$frequency<-right_join(agent_angry, frequent_employees, "AO_GENESYS_EMPLOYEE_LOGIN")$n

#calculating already with weights
agent_angry$agent_r<-(agent_angry$w*agent_angry$s)

#making it relative to the frequencies of the calls of each agent being comparable among them
agent_angry$agent_rk<-((agent_angry$w*agent_angry$s)/agent_angry$frequency)*100

#graph
agent_angry%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(m=mean(agent_rk))%>%ggplot(.,aes(x=AO_GENESYS_EMPLOYEE_LOGIN,y=m))+geom_bar(stat="identity")+coord_flip()

################CHANGES OF EMOTIONS BETWEEN DIFFERENT CALL STAGES
customer<-turn_data_correct%>%filter(Person =="customer")

#choosing the stages to compare (my_problem_is; farewell);grouping by a call, dialogue and emotion; COUNT FOR EACH EMOTION (how many times observed in each phase)
and making the data "wide" (just run and have a look)
ab<-customer%>%filter(`Dialogue Structure` == "my_problem_is" | `Dialogue Structure` == "farewell")%>%group_by(AO_transcription_id, `Dialogue Structure`,Emotion)%>%count(Emotion)%>%spread(key=c("Emotion"), value=n, fill=0)

#the most frequent counts (emotions) in the given call and call stage
ab$Max <- apply(ab[,3:7], 1, function(x) max(x))

#CONDITIONS:
#if MAX (ab$Max) value (the line above) is equal to ANGRY OR equal to ANGRY and NVT -> to state emotion as ANGRY (happens that nvt and other emotions have the same counts,
#to focus on more relevant emotions - the other than nvt is choosen)
ab$general_mood<-ifelse(ab$angry==ab$Max | (ab$angry==ab$Max & ab$nvt==ab$Max),"angry",
                        ifelse(ab$fear==ab$Max | (ab$fear==ab$Max & ab$nvt==ab$Max) ,"fear",
                               ifelse(ab$sad==ab$Max| (ab$sad==ab$Max & ab$nvt==ab$Max),"sad",
                                      ifelse(ab$nvt==ab$Max & (ab$nvt!=ab$angry | ab$nvt!=ab$fear | ab$nvt!=ab$sad| ab$nvt!=ab$happy),"neutral",
                                             ifelse(ab$happy==ab$Max| (ab$happy==ab$Max & ab$nvt==ab$Max),"happy",0)))))

#again making data "wide" (just run and check for understanding);transforming char. to fcts and making some basic dplyr you understand
customer_moods<-ab%>%spread(key=c(`Dialogue Structure`), value=general_mood)
customer_moods<-customer_moods%>%mutate_if(is.character, as.factor)
cust_fin<-customer_moods%>%group_by(AO_transcription_id)%>%select("farewell", "my_problem_is")%>%count(farewell, my_problem_is)

summary(customer_moods)
  
summary(as.factor(customer$`Dialogue Structure`))

###the final process - LOOP with CONDITIONS: ###


frame<-tibble()
for (u in unique(cust_fin$AO_transcription_id)[1:100]){ #unique calls to loops through = only 100 for testing purposes
  c <- setDT(cust_fin)[AO_transcription_id ==  as.character(u),] #data.table package filtering (faster than dplyr)
  if(nrow(c)>1){ #conditions for n of rows in selected call - some of them have only 1 and so emotions are not comparable (some stage is missing)
  
  ##CONDITIONS:
  #c[1,2] = emotions for the given call for "farewell"; c[2,3]=emotions for the given call for "my_problem_is".
  #if my_problem_is = angry and farewell = "happy" -> the final value = "happy-to-angry" etc....
  if(c[1,2] == "happy" & c[2,3] == "angry"){ 
    mood<-"angry-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "neutral"){
    mood<-"neutral-to-happy"
    }else if(c[1,2] == "happy" & c[2,3] == "sad"){mood<-"sad-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "fear"){mood<-"fear-to-happy"}else if(c[1,2] == "happy" & c[2,3] == "happy"){mood<-"no-change"}
    
    else if(c[1,2] == "sad" & c[2,3] == "angry"){
      mood<-"angry-to-sad"}else if(c[1,2] == "sad" & c[2,3] == "neutral"){
        mood<-"neutral-to-sad"
      }else if(c[1,2] == "sad" & c[2,3] == "sad"){mood<-"no-change"}else if(c[1,2] == "sad" & c[2,3] == "fear"){mood<-"fear-to-sad"}else if(c[1,2] == "sad" & c[2,3] == "happy"){mood<-"sad-to-happy"}    
    
    else if(c[1,2] == "fear" & c[2,3] == "angry"){
      mood<-"angry-to-fear"}else if(c[1,2] == "fear" & c[2,3] == "neutral"){
        mood<-"neutral-to-fear"
      }else if(c[1,2] == "fear" & c[2,3] == "sad"){mood<-"sad-to-fear"}else if(c[1,2] == "fear" & c[2,3] == "fear"){mood<-"no-change"}else if(c[1,2] == "fear" & c[2,3] == "happy"){mood<-"fear-to-happy"}    
    
    else if(c[1,2] == "neutral" & c[2,3] == "angry"){
      mood<-"angry-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "neutral"){
        mood<-"no-change"
      }else if(c[1,2] == "neutral" & c[2,3] == "sad"){mood<-"sad-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "fear"){mood<-"fear-to-neutral"}else if(c[1,2] == "neutral" & c[2,3] == "happy"){mood<-"neutral-to-happy"}    
  
    else if(c[1,2] == "angry" & c[2,3] == "angry"){
      mood<-"no-change"}else if(c[1,2] == "angry" & c[2,3] == "neutral"){
        mood<-"neutral-to-angry"
      }else if(c[1,2] == "angry" & c[2,3] == "sad"){mood<-"sad-to-angry"}else if(c[1,2] == "angry" & c[2,3] == "fear"){mood<-"fear-to-angry"}else if(c[1,2] == "angry" & c[2,3] == "happy"){mood<-"angry-to-happy"}    

    else{mood<-"error"}
  }else{
    mood<-"no_data" #for call with missing stages
  }
  data<-tibble(ID=u, moods=mood)
  frame<-rbind(frame,data)
}

#######WORKING WITH TIME DATA and EMOTIONS #####

#function to extract the most frequent emotion (modus)
extract_max <- function(.vec){
  my_max <- .vec %>% vctrs::vec_count()
  my_max <- my_max$key[1]  }

#LOOP AGAIN
ccccc<-tibble()
for (u in unique(customer$AO_transcription_id)[1:1000]){  #unique calls, 1000 for testing purposes
#data.table filtering, cols selecting and sorting
  c2<-setDT(customer)[AO_transcription_id ==  as.character(u),c("Emotion", "hour")][order(hour)]
  if(nrow(c2)>4){ #as we take into account at least 4 observation to do conclusion about the most frequnet emotion (WE CAN DISCUSS HOW MANY)
  start<-c(extract_max(c2$Emotion[1:4])) #extracting modus of emotion at the begging of the call (first 4 observations (timestamps) in data that are order by time)
  end<-c(extract_max(c2$Emotion[c(length(c2$Emotion)-4):length(c2$Emotion)])) #last 4 observations (timestamps)
  }else{next} #if there are not enough data (4), just to skip to another one 
  data<-tibble(ID=u, mood_start=c4, mood_end=end)
  ccccc<-rbind(ccccc,data)
  rm(data)
}

###THE SAME IS NEEDED TO MAKE FOR AGENTS DATA - for comparison purposes between ametions of agent and customer 

#to see what stages are the richest for emotion data
nm<-customer%>%group_by(`Dialogue Structure`)%>%count(Emotion)%>%arrange(desc(n))
