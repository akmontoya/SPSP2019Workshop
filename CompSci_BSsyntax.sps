
regression /dep = interest /method = enter cond. 
regression /dep = CScomm /method = enter cond. 
regression /dep = interest /method = enter cond CScomm. 

PROCESS x = cond /m = CScomm /y = interest / model = 4 /total = 1/save = 1. 