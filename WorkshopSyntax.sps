* Encoding: UTF-8.
MEMORE m= comm_G comm_I /y = int_G int_I. 

MEMORE m= comm_G comm_I /y = int_G int_I /xmint = 0. 

MEMORE m= HazSimp HazComp /y = DoseSimp DoseComp /xmint = 0 /normal = 1. 


VARSTOCASES
  /ID=id
  /MAKE Hazard FROM HazSimp HazComp
  /MAKE Dose FROM DoseSimp DoseComp
  /INDEX=Simple(2) 
  /KEEP=
  /NULL=KEEP.

DATASET ACTIVATE DataSet12.
RECODE Simple (2=0) (1=1).
EXECUTE.

MIXED Dose WITH Hazard
    /Fixed = Hazard | SSTYPE(3)
    /Method = REML
    /Print = G Solution Testcov
    /Random = INTERCEPT Hazard | Subject(id) COVTYPE(UN). 

AGGREGATE
   /OUTFILE = * MODE = ADDVARIABLES
   /BREAK = id
   /Hazard_m = MEAN(Hazard). 

COMPUTE Hazard_groupc = Hazard - Hazard_m. 
Execute. 


MIXED Dose WITH Hazard_groupc Hazard_m
    /Fixed = Hazard_groupc Hazard_m | SSTYPE(3)
    /Method = REML
    /Print = G Solution Testcov
    /Random = INTERCEPT Hazard_groupc | Subject(id) COVTYPE(UN).

AGGREGATE
	/OUTFILE = * MODE = ADDVARIABLES
/BREAK = id
/Simple_m = MEAN(Simple). 

COMPUTE Simple_groupc = Simple - Simple_m. 
EXECUTE. 

MIXED Hazard WITH Simple_groupc Simple_m
/FIXED = Simple_groupc Simple_m | SSTYPE(3)
/METHOD = REML
/PRINT = G SOLUTION TESTCOV
/RANDOM = Intercept Simple_groupc| Subject(id) COVTYPE(UN). 

MIXED Hazard WITH Simple_groupc
/FIXED = Simple_groupc | SSTYPE(3)
/METHOD = REML
/PRINT = G SOLUTION TESTCOV
/RANDOM = Intercept Simple_groupc| Subject(id) COVTYPE(UN). 

MIXED Hazard WITH Simple_groupc
/FIXED = Simple_groupc | SSTYPE(3)
/METHOD = REML
/PRINT = G SOLUTION TESTCOV
/RANDOM = Intercept | Subject(id) COVTYPE(UN). 

MIXED Dose WITH Hazard_groupc Hazard_m Simple_groupc
/FIXED = Hazard_groupc Hazard_m Simple_groupc | SSTYPE(3)
/METHOD = REML
/PRINT = G SOLUTION TESTCOV
/RANDOM = Intercept | Subject(id) COVTYPE(UN). 

Mlmed data = dataset1
/x = Simple
/xB = 0
/m1 = Hazard
/y = Dose
/cluster = id
/covmat = UN
/folder = /Users/Akmontoya/Desktop/. 

Mlmed data = dataset3 
/x = Simple 
/xB = 0 
/randx = 01
/m1 = Hazard 
/randm = 1
/y = Dose 
/cluster = id 
/covmat = UN 
/folder = /Users/Akmontoya/Desktop/

Mlmed data = dataset4 /x = tutor /m1 = motiv /y = post /cov1 = pre /cluster = classid /covmat = UN 
/folder = /Users/Akmontoya/Desktop/

MLmed data = dataset4 /x = train /xW = 0 /m1 = motiv /y = post /cov1 = pre /cluster = classid /folder = /Users/Akmontoya/Desktop/




