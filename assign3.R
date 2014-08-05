best("TX","heart attack")
best("TX","heart failure")

best("MD","heart attack")
best("MD","pneumonia")
best("BB","heart attack")
best("NY","hert attack")

rankhospital("TX","heart failure",4)
rankhospital("MD","heart attack","worst")

rankhospital("MN","heart attack",5000)
rankall("heart attack",4)
head(rankall("heart attack",20),10)
tail(rankall("pneumonia","worst"),3)
tail(rankall("heart failure"),10)

