#############
#SETTING UP:#
#############

#PLEASE SETUP YOUR WORKING DIRECTORY BELOW:
setwd("............/MATH 335 Final Project")

#NECESSARY LIBRARIES TO WORK WITH:
library(readxl)
library(lpSolve)
library(lpSolveAPI)

#################################################################################################################################################################################################################################

############################
#GETTING DATA IN AND READY:#
############################

#RAs' SCHEDULE DATASET:
schedule = as.matrix(read_excel("Schedule.xlsx"))
hours = as.matrix(read_excel("Hours.xlsx"))
namesOfPeople = schedule[,1]
availableShifts = colnames(schedule)[2:ncol(schedule)]
scheduleMatrix = matrix(as.numeric(schedule[,2:ncol(schedule)]),nrow = nrow(schedule),ncol = ncol(schedule) -1)
hourMatrix = t(matrix(as.numeric(hours)))
scheduleHourMatrix = matrix(0,0,length(availableShifts))
for(rowCounter in 1:NROW(scheduleMatrix)){
  newScheduleHourMatrixRow = scheduleMatrix[rowCounter,] * hourMatrix[]
  scheduleHourMatrix = rbind(scheduleHourMatrix,newScheduleHourMatrixRow)
}
colnames(scheduleMatrix) = colnames(schedule)[2:ncol(schedule)]
rownames(scheduleMatrix) = namesOfPeople
colnames(scheduleHourMatrix) = availableShifts
rownames(scheduleHourMatrix) = namesOfPeople


#GIVING PROPER NAMES TO VARIBALES:
namesOfVariables = c()
for(ra in namesOfPeople){
  for(shift in availableShifts){
    newVariable = paste("x",ra,shift,sep = ".") 
    namesOfVariables = c(namesOfVariables,newVariable)
  }
}
#################################################################################################################################################################################################################################

################################
#SETTING UP OBJECTIVE FUNCTION:#
################################

objectiveValues = c()
for(n in 1:nrow(scheduleMatrix)){
  for(objRow in schedule[n,1:ncol(scheduleMatrix)+1]){
    newValue = objRow
    objectiveValues = c(objectiveValues,newValue)
  }
}
objectiveMatrix = matrix(objectiveValues,1,length(namesOfVariables))
colnames(objectiveMatrix) = namesOfVariables

#################################################################################################################################################################################################################################

#########################
#SETTING UP CONSTRAINTS:#
#########################

#INITIALIZING PLACEHOLDERS FOR CONTRAINT MATRIX
constraintMatrix = matrix(0,0,length(namesOfVariables))
colnames(constraintMatrix) = namesOfVariables
inequalities = matrix("",0,1)
rightHandSide = matrix(0,0,1)

#CONSTRAINT FAMILY NO.1 - EVERY SHIFT NEEDS TO TAKEN AND BY ONLY ONE RA:
########################################################################
for(shift in availableShifts){
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  regularExpressionToLookFor = paste0("^x\\..*\\.",shift,"$")
  indicesToModify = grep(pattern = regularExpressionToLookFor,namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  #UPDATING ALL THREE PARTS OF THE CONSTRAINT MATRIX
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,"=")
  rightHandSide = rbind(rightHandSide,1)
}

#CONSTRAINT FAMILY NO.2 - EACH RA CAN TAKE SHIFT WHEN THEY ARE AVAILABLE:
#########################################################################
for(ra in namesOfPeople){
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  rowForThatPerson = scheduleMatrix[ra,]
  shiftsToGrabIndices = which(rowForThatPerson == 1)
  shiftsToGrab = availableShifts[shiftsToGrabIndices]
  shiftsToGrabRegExp = paste0("(",paste(shiftsToGrab,collapse = "|"),")")
  pattern = paste0("^x\\.", ra, "\\.",shiftsToGrabRegExp,"$")
  indicesToModify = grep(pattern,namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  #UPDATING ALL THREE PARTS OF THE CONSTRAINT MATRIX
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,">=")
  rightHandSide = rbind(rightHandSide,1)
}


##CONSTRAINT FAMILY NO.3 - NO RA CAN WORK FOR MORE THAN 4 HOURS BACK-TO-BACK:
#############################################################################
for(ra in namesOfPeople)
{
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  rowForThatPerson = scheduleHourMatrix[ra,]
  x=1
  while(x<=3){
    if(x==1){
      shiftsToGrabRegExp = paste0("(Fri6-8pm|Fri8-10pm|Fri10-12am)")
      pattern = paste0("^x\\.", ra, "\\.",shiftsToGrabRegExp,"$")
      indicesToModify = grep(pattern,namesOfVariables)
    }
    else if(x==2){
      shiftsToGrabRegExp = paste0("(Sat6-8pm|Sat8-10pm|Sat10-12am)")
      pattern = paste0("^x\\.", ra, "\\.",shiftsToGrabRegExp,"$")
      indicesToModify = grep(pattern,namesOfVariables)
    }
    else if (x==3){
      shiftsToGrabRegExp = paste0("(Sun3-5pm|Sun5-7pm|Sun7-9pm)")
      pattern = paste0("^x\\.", ra, "\\.",shiftsToGrabRegExp,"$")
      indicesToModify = grep(pattern,namesOfVariables)
    }
    newConstraint[indicesToModify] = 2
    newConstraint<- ifelse(newConstraint<0,0,newConstraint)
    
    #UPDATING ALL THREE PARTS OF THE CONSTRAINT MATRIX
    constraintMatrix = rbind(constraintMatrix,newConstraint)
    inequalities = rbind(inequalities,"<=")
    rightHandSide = rbind(rightHandSide,4)
    indicesToModify = NULL
    x = x+1
  }
}

#CONSTRAINT FAMILY NO.4 - NO RA CAN WORK FOR MORE THAN 5 HOURS A WEEK:
######################################################################
for(ra in namesOfPeople)
{
  hangingConstraint = c()
  newConstraint = matrix(0,1,0)
  rowForThatPerson = scheduleHourMatrix[ra,]
  hangingConstraint = t(c(rowForThatPerson))
  zeroConstraint = matrix(0,1,20)
  for(ra2 in namesOfPeople)
  {
    if(ra==ra2){
      newConstraint = cbind(newConstraint, hangingConstraint)
    }
    else{
      zeroConstraint = matrix(0,1,20)
      newConstraint = cbind(newConstraint, zeroConstraint)
    }
  }
  colnames(newConstraint) = NULL
  newConstraint<- ifelse(newConstraint<0,0,newConstraint)
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,"<=")
  rightHandSide = rbind(rightHandSide,5)
  
}

#################################################################################################################################################################################################################################

#############################
#SOLVING THE LINEAR PROGRAM:#
#############################

#DECLARING THE LP:
satisfyLP = make.lp(NROW(constraintMatrix),NCOL(constraintMatrix))

#SETTING UP LP AS BINARY:
set.type(satisfyLP,1:NCOL(constraintMatrix),type=c("binary"))

#ASSIGNING OBJECTIVE FUNCTION:
set.objfn(satisfyLP,as.numeric(objectiveMatrix))

#CONTROLLING THE PROGRAM TYPE
lp.control(satisfyLP,sense='max')

#SETTING UP EACH ROW OF THE LP OBJECT:
for(rowCounter in 1:NROW(constraintMatrix)){
  set.row(satisfyLP,rowCounter,constraintMatrix[rowCounter,])
  set.constr.type(satisfyLP,inequalities[rowCounter,1],rowCounter)
  set.rhs(satisfyLP, rightHandSide[rowCounter,1], rowCounter)
}

#SOLVING THE LP:
solve(satisfyLP)

#################################################################################################################################################################################################################################

##########
#RESULTS:#
##########

optimalObjectiveValue =get.objective(satisfyLP)
optimalSolutionVector = matrix(get.variables(satisfyLP),1,length(namesOfVariables))
colnames(optimalSolutionVector) = namesOfVariables

#CONCLUSION:
optimalObjectiveValue
View(optimalSolutionVector)

#################################################################################################################################################################################################################################
