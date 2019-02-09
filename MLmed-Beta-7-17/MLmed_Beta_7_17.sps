* Encoding: UTF-8.

/* MLmed for SPSS - Beta Version */.
/* Written by Nicholas J. Rockwood */.
/* www.njrockwood.com Copyright 2017 */.
/* Online distribution other than through */.
/* www.njrockwood.com is not authorized */.
/* Please read the documentation available online */.

preserve.
set printback=off.

/* Permission is hereby granted, free of charge, to any person obtaining a copy of this software */. 
/* and associated documentation files (the "Software"), to use the software in this form.  Distribution */.
/* after modification is prohibited, as is its use for any commercial purpose without authorization */.  
/* This software should not be posted or stored on any webpage, server, or directory accessible to */. 
/* the public whether free or for a charge unless written permission has been granted by the copyright */.
/* holder.  The copyright holder requests that this software be distributed by directing users to */.
/* njrockwood.com where the latest release of the software and documentation is archived and */.
/* can be downloaded.  Permission is granted to install this software in university computing labs for */.
/* noncommercial/nonprofit use */.


/* THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, */ 
/* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF */.
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT */.
/* IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, */. 
/*  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT */.
/* OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE */.
/* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE */. 

/* The above text should be included in any distribution of the software */.


DEFINE LRT (D1 = !charend('/')/ D0 = !charend('/')/ P1 = !charend('/')/ P0 = !charend('/')).
MATRIX.
compute chi = !D0 - !D1.
compute df = !P1 - !P0.
compute p = 1 - CHICDF(chi,df).
compute table = {chi, df, p}.
print table /title = "Likelihood Ratio Test" /clabels = "Dif" "df" "p" /format = F8.4.
END MATRIX.
OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT HEADINGS 
  /IF COMMANDS=["Matrix(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES.
!ENDDEFINE.

DEFINE mlmed (data = !charend('/') !default(DataSet1)
         /x = !charend('/')
         /randx = !charend('/') !default(0000)
         /xB =  !charend('/') !default(1)
         /xW = !charend('/') !default(1)
         /m1 = !charend('/')
         /m2 = !charend('/')
         /m3 = !charend('/')
         /randm = !charend('/') !default(000)
         /randMint = !charend('/') !default(111)
         /mB = !charend('/') !default(111)
         /y = !charend('/')
         /randYint = !charend('/') !default(1)
         /cluster = !charend('/')
         /cov1 = !charend('/')
         /randc1 = !charend('/') !default(0000)
         /cov1B = !charend('/') !default(1) 
         /cov1c = !charend('/')
         /cov2 = !charend('/')
         /randc2 = !charend('/') !default(0000)
         /cov2B = !charend('/') !default(1)
         /cov2c = !charend('/') 
         /cov3 = !charend('/')        
         /randc3 = !charend('/') !default(0000)
         /cov3B = !charend('/') !default(1)
         /cov3c = !charend('/') 
         /l2cov1 = !charend('/')
         /l2cov2 = !charend('/')
         /l2cov3 = !charend('/')
         /modM = !charend('/')
         /modMcent = !charend('/') !default(0)
         /modMB = !charend('/') !default(1)
         /modY = !charend('/')
         /modYcent = !charend('/') !default(0)
         /modYB = !charend('/') !default(1)
         /indint = !charend('/') !default(1)
         /covmat = !charend('/') !default(DIAG)
         /mcovmat = !charend('/') !default(DIAG)
         /ycov = !charend('/') !default(0)
         /rescovmat = !charend('/') !default(DIAG)
         /est=!charend('/') !default(REML)
         /iters = !charend('/') !default(100)
         /mxstep = !charend('/') !default(10)
         /scoring = !charend('/') !default(1)
         /asym = !charend('/') !default(NO)
         /samples = !charend('/') !default(10000)
         /conf = !charend('/') !default(95)
         /eor = !charend('/') !default(0)
         /folder = !charend('!')).

DATASET ACTIVATE !data.
DATASET COPY mlmdata.
DATASET ACTIVATE mlmdata.

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK= !cluster
  /meanx=MEAN(!x) 
  /meanm1=MEAN(!m1).
COMPUTE x_c = !x - meanx.
COMPUTE m1_c = !m1 - meanm1.
EXECUTE.



!LEt !x_name = !QUOTE(!x).
!LET !y_name = !QUOTE(!y).
!LET !m1_name = !QUOTE(!m1).
!LET !eor_1 = (!eor = 1).

!IF (!m2 ~= !NULL) !THEN
   !LET !m2_1 = 1.
   !LET !m2_name = !QUOTE(!m2).
   AGGREGATE
     /OUTFILE=* MODE=ADDVARIABLES
     /BREAK= !cluster
     /meanm2=MEAN(!m2).
   COMPUTE m2_c = !m2 - meanm2.
   EXECUTE.
!ELSE
   !LET !m2_1 = 0.
   !LET !m2_name = !QUOTE(none).
!IFEND

!IF (!m3 ~= !NULL) !THEN
   !LET !m3_1 = 1.
   !LET !m3_name = !QUOTE(!m3).
   AGGREGATE
     /OUTFILE=* MODE=ADDVARIABLES
     /BREAK= !cluster
     /meanm3=MEAN(!m3).
   COMPUTE m3_c = !m3 - meanm3.
   EXECUTE.
!ELSE
   !LET !m3_1 = 0.
   !LET !m3_name = !QUOTE(none).
!IFEND

!IF (!cov1 ~= !NULL) !THEN
   !LET !cov1_1 = 1.
   AGGREGATE
     /OUTFILE=* MODE=ADDVARIABLES
     /BREAK= !cluster
     /meanc1=MEAN(!cov1).
   !IF (!cov1c = !NULL) !THEN
      COMPUTE c1_c = !cov1 - meanc1.
      EXECUTE.
   !ELSE
      COMPUTE c1_c = !cov1 - !cov1c.
      EXECUTE.
   !IFEND
!ELSE
   !LET !cov1_1 = 0.
!IFEND

!IF (!cov2 ~= !NULL) !THEN
   !LET !cov2_1 = 1.
   AGGREGATE
     /OUTFILE=* MODE=ADDVARIABLES
     /BREAK= !cluster
     /meanc2=MEAN(!cov2).
   !IF (!cov2c = !NULL) !THEN
      COMPUTE c2_c = !cov2 - meanc2.
      EXECUTE.
   !ELSE
      COMPUTE c2_c = !cov2 - !cov2c.
      EXECUTE.
   !IFEND
!ELSE !LET !cov2_1 = 0.
!IFEND

!IF (!cov3 ~= !NULL) !THEN
   !LET !cov3_1 = 1.
   AGGREGATE
     /OUTFILE=* MODE=ADDVARIABLES
     /BREAK= !cluster
     /meanc3=MEAN(!cov3).
   !IF (!cov3c = !NULL) !THEN
      COMPUTE c3_c = !cov3 - meanc3.
      EXECUTE.
   !ELSE
      COMPUTE c3_c = !cov3 - !cov3c.
      EXECUTE.
   !IFEND
!ELSE
   !LET !cov3_1 = 0.
!IFEND

!IF (!l2cov1 ~= !NULL) !THEN
   !LET !l2cov1_1 = 1.
!ELSE
   !LET !l2cov1_1 = 0.
!IFEND

!IF (!l2cov2 ~= !NULL) !THEN
   !LET !l2cov2_1 = 1.
!ELSE
   !LET !l2cov2_1 = 0.
!IFEND

!IF (!l2cov3 ~= !NULL) !THEN
   !LET !l2cov3_1 = 1.
!ELSE
   !LET !l2cov3_1 = 0.
!IFEND


!LET !samemod = 0.

!IF (!modM = !modY) !THEN
   !LET !modYc = !modMcent.
!ELSE
   !LET !modYc = !ModYcent.
!IFEND



*Center Moderators around specified value

!IF (!modM ~= !NULL) !THEN
   !LET !modM_1 = 1.
   !LET !modM_n = !QUOTE(!modM).
   COMPUTE modM_c = !modM - !modMcent.
   EXECUTE.
!ELSE
   !LET !modM_1 = 0.
   !LET !modM_n = !QUOTE(none).
!IFEND

!IF (!modY ~= !NULL) !THEN
   !LET !modY_1 = 1.
   !LET !modY_n = !QUOTE(!modY).
   !LET !samemod = (!modY = !modM).
   COMPUTE modY_c = !modY - !modYc.
   EXECUTE.
!ELSE
   !LET !modY_1 = 0.
   !LET !modY_n = !QUOTE(none).
!IFEND

!LET !covmat_1 = (!covmat = UN).
!LET !rescov_1 = (!rescovmat = UN).
!LET !bet_x_1 = (!xB = 1).
!LET !wit_x_1 = (!xW = 1).
!LET !modMB_1 = (!modMB = 1).
!LET !modYB_1 = (!modYB = 1 !AND !SUBSTR(!mB,1,1) = 1).
!LET !bet_c1_1 = (!cov1B = 1).
!LET !bet_c2_1 = (!cov2B = 1).
!LET !bet_c3_1 = (!cov3B = 1).
!LET !bet_m1_1 = 0.
!LET !bet_m2_1 = 0.
!LET !bet_m3_1 = 0.


* Combine DVs of each equation into one vector

VARSTOCASES /ID = case_num
 /MAKE Z FROM !m1 !m2 !m3 !y
 /INDEX =DV(Z).

OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT TABLES 
  /IF COMMANDS=["Variables to Cases(LAST)"] LABELS=[EXACT("Processing Statistics")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES
  /SELECT TABLES 
  /IF COMMANDS=["Variables to Cases(LAST)"] LABELS=[EXACT("Generated Variables")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES
  /SELECT TEXTS 
  /IF COMMANDS=["Variables to Cases(LAST)"] LABELS=[EXACT("Active Dataset")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES
  /SELECT HEADINGS 
  /IF COMMANDS=["Variables to Cases(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES.

COMPUTE DV = UPCASE(DV).
EXECUTE.

RECODE
DV
(!QUOTE(!UPCASE(!y))=1) (ELSE=0) INTO y_ind.
EXECUTE.



*Create indicator variables for the different equations.

RECODE
DV
(!QUOTE(!UPCASE(!y))=1) (ELSE=0) INTO y_int.
EXECUTE.

RECODE
DV
(!QUOTE(!UPCASE(!m1))=1) (ELSE=0) INTO m1_int.
EXECUTE.

!IF (!m2 ~= !NULL) !THEN
   RECODE
   DV
   (!QUOTE(!UPCASE(!m2))=1) (ELSE=0) INTO m2_int.
   EXECUTE.
!IFEND

!IF (!m3 ~= !NULL) !THEN
   RECODE
   DV
   (!QUOTE(!UPCASE(!m3))=1) (ELSE=0) INTO m3_int.
   EXECUTE.
!IFEND

RECODE DV (!QUOTE(!UPCASE(!y))='1').
EXECUTE.
RECODE DV (!QUOTE(!UPCASE(!m1))='2').
EXECUTE.

!IF (!m2 ~= !NULL) !THEN
   RECODE DV (!QUOTE(!UPCASE(!m2))='3').
   EXECUTE.
!IFEND

!IF (!m3 ~= !NULL) !THEN
   RECODE DV (!QUOTE(!UPCASE(!m3))='4').
   EXECUTE.
!IFEND

*Multiply variables by indicators to create within variables

COMPUTE a1_w = m1_int*x_c.
EXECUTE.
COMPUTE cp_w = y_int*x_c.
EXECUTE.
COMPUTE b1_w = y_int*m1_c.
EXECUTE.

COMPUTE a1_b = meanx*m1_int.
EXECUTE.
COMPUTE cp_b = meanx*y_int.
EXECUTE.
COMPUTE b1_b = meanm1*y_int.
EXECUTE.


!IF (!modM ~= !NULL) !THEN
   COMPUTE qM = modM_c*m1_int.
   EXECUTE.
   COMPUTE qM_x_a_w = qM*a1_w.
   COMPUTE qM_x_a_b = qM*a1_b.
   EXECUTE.
!IFEND

!IF (!modY ~= !NULL) !THEN
   COMPUTE qY = ModY_c*y_int.
   EXECUTE.
   COMPUTE qY_x_b_w = qY*b1_w.
   COMPUTE qY_x_b_b = qY*b1_b.
   EXECUTE.
!IFEND


!IF (!cov1 ~= !NULL) !THEN
   COMPUTE g1m1_w = c1_c*m1_int.
   COMPUTE g1y_w = c1_c*y_int.
   COMPUTE g1m1_b = meanc1*m1_int.
   COMPUTE g1y_b = meanc1*y_int.
   EXECUTE.
!IFEND

!IF (!cov2 ~= !NULL) !THEN
   COMPUTE g2m1_w = c2_c*m1_int.
   COMPUTE g2y_w = c2_c*y_int.
   COMPUTE g2m1_b = meanc2*m1_int.
   COMPUTE g2y_b = meanc2*y_int.
   EXECUTE.
!IFEND

!IF (!cov3 ~= !NULL) !THEN
   COMPUTE g3m1_w = c3_c*m1_int.
   COMPUTE g3y_w = c3_c*y_int.
   COMPUTE g3m1_b = meanc3*m1_int.
   COMPUTE g3y_b = meanc3*y_int.
   EXECUTE.
!IFEND


!IF (!l2cov1 ~= !NULL) !THEN
   COMPUTE g1m1_2 = !l2cov1*m1_int.
   COMPUTE g1y_2 = !l2cov1*y_int.
   EXECUTE.
!IFEND

!IF (!l2cov2 ~= !NULL) !THEN
   COMPUTE g2m1_2 = !l2cov2*m1_int.
   COMPUTE g2y_2 = !l2cov2*y_int.
   EXECUTE.
!IFEND

!IF (!l2cov3 ~= !NULL) !THEN
   COMPUTE g3m1_2 = !l2cov3*m1_int.
   COMPUTE g3y_2 = !l2cov3*y_int.
   EXECUTE.
!IFEND


!IF (!m2 ~= !NULL) !THEN
   COMPUTE a2_w = m2_int*x_c.
   COMPUTE b2_w = y_int*m2_c.
   COMPUTE a2_b = m2_int*meanx.
   COMPUTE b2_b = y_int*meanm2.
   EXECUTE.
   !IF (!cov1 ~= !NULL) !THEN
      COMPUTE g1m2_w = c1_c*m2_int.
      COMPUTE g1m2_b = meanc1*m2_int.
      EXECUTE.
   !IFEND
   !IF (!cov2 ~= !NULL) !THEN
      COMPUTE g2m2_w = c2_c*m2_int.
      COMPUTE g2m2_b = meanc2*m2_int.
      EXECUTE.
   !IFEND
   !IF (!cov3 ~= !NULL) !THEN
      COMPUTE g3m2_w = c3_c*m2_int.
      COMPUTE g3m2_b = meanc3*m2_int.
      EXECUTE.
   !IFEND
   !IF (!l2cov1 ~= !NULL) !THEN
      COMPUTE g1m2_2 = !l2cov1*m2_int.
      EXECUTE.
   !IFEND
   !IF (!l2cov2 ~= !NULL) !THEN
      COMPUTE g2m2_2 = !l2cov2*m2_int.
      EXECUTE.
   !IFEND
   !IF (!l2cov3 ~= !NULL) !THEN
      COMPUTE g3m2_2 = !l2cov3*m2_int.
      EXECUTE.
   !IFEND
!IFEND


!IF (!m3 ~= !NULL) !THEN
   COMPUTE a3_w = m3_int*x_c.
   COMPUTE b3_w = y_int*m3_c.
   COMPUTE a3_b = m3_int*meanx.
   COMPUTE b3_b = y_int*meanm3.
   EXECUTE.
   !IF (!cov1 ~= !NULL) !THEN
      COMPUTE g1m3_w = c1_c*m3_int.
      COMPUTE g1m3_b = meanc1*m3_int.
      EXECUTE.
   !IFEND
   !IF (!cov2 ~= !NULL) !THEN
      COMPUTE g2m3_w = c2_c*m3_int.
      COMPUTE g2m3_b = meanc2*m3_int.
      EXECUTE.
   !IFEND
   !IF (!cov3 ~= !NULL) !THEN
      COMPUTE g3m3_w = c3_c*m3_int.
      COMPUTE g3m3_B = meanc3*m3_int. 
      EXECUTE.
   !IFEND
   !IF (!l2cov1 ~= !NULL) !THEN
      COMPUTE g1m3_2 = !l2cov1*m3_int.
      EXECUTE.
   !IFEND
   !IF (!l2cov2 ~= !NULL) !THEN
      COMPUTE g2m3_2 = !l2cov2*m3_int.
      EXECUTE.
   !IFEND
   !IF (!l2cov3 ~= !NULL) !THEN
      COMPUTE g3m3_2 = !l2cov3*m3_int.
      EXECUTE.
   !IFEND
!IFEND


!LET !rm2_1 = 0.
!LET !rm3_1 = 0.
!LET !rm1int_1 = 0.
!LET !rm2int_1 = 0.
!LET !rm3int_1 = 0.
!LET !ryint_1 = 0.

!LET !ylist_w = !CONCAT(constant, !BLANKS(1))
!LET !ylist_b = !CONCAT(!BLANKS(0))
!LET !m1list_w = !CONCAT(constant, !BLANKS(1))
!LET !m1list_b = !CONCAT(!BLANKS(0))
!LET !ranvar = !CONCAT(!BLANKS(0))
!LET !ranint = !CONCAT(!BLANKS(0)) 
!LET !ranslope = !CONCAT(!BLANKS(0)) 
!LET !ranslop2 = !CONCAT(!BLANKS(0)) 
!LET !varlist = !CONCAT(y_int, !BLANKS(1), m1_int)

!LET !mB_list = !CONCAT(!BLANKS(0)).

!IF (!randYint = 1) !THEN
   !LET !ranvar = !CONCAT(!ranvar, y_int)
   !IF (!ycov = 1) !THEN
      !LET !ranslope = !CONCAT(!ranslope, y_int)
   !IFEND
   !LET !ryint_1 = 1.
!IFEND
!IF (!SUBSTR(!randMint,1,1) = 1) !THEN
   !LET !ranvar = !CONCAT(!ranvar,!BLANKS(1), m1_int)
   !LET !ranint = !CONCAT(!ranint, m1_int)
   !LET !rm1int_1 = 1.
!IFEND


!IF (!m2 ~= !NULL) !THEN
   !LET !rm2_1 = 1.
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), m2_int)
   !IF (!SUBSTR(!randMint,2,1) = 1) !THEN
      !LET !ranvar =  !CONCAT(!ranvar, !BLANKS(1), m2_int)
      !LET !ranint =  !CONCAT(!ranint, !BLANKS(1), m2_int)
      !LET !rm2int_1 = 1.
   !IFEND
   !LET !m2list_w = !CONCAT(constant, !BLANKS(1))
   !LET !m2list_b = !CONCAT(!BLANKS(0))
!IFEND
!IF (!m3 ~= !NULL) !THEN
   !LET !rm3_1 = 1.
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), m3_int)
   !IF (!SUBSTR(!randMint,3,1) = 1) !THEN
      !LET !ranvar =  !CONCAT(!ranvar, !BLANKS(1), m3_int)
      !LET !ranint =  !CONCAT(!ranint, !BLANKS(1), m3_int)
      !LET !rm3int_1 = 1.
   !IFEND
   !LET !m3list_w = !CONCAT(Int, !BLANKS(1))
   !LET !m3list_b = !CONCAT(!BLANKS(0))
!IFEND


!IF (!modM ~= !NULL) !THEN
   !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qM)
       !IF (!xW = 1) !THEN
            !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qM_x_a_w)
       !IFEND
   !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !modM)
       !IF (!xW = 1) !THEN
            !LET !m1list_w = !CONCAT(!m1list_w, !BLANKS(1), int_1)
       !IFEND
   !IF (!Xb = 1) !THEN
      !IF (!modMB = 1) !THEN
         !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qM_x_a_b)
         !IF (!xW = 1) !THEN
              !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), int_2)
         !ELSE
              !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), int_1)
         !IFEND
      !IFEND
   !IFEND
!IFEND
!IF (!modY ~= !NULL) !THEN
    !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qY)
    !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qY_x_b_w) 
    !IF (!modYB_1 = 1) !THEN
       !LET !varlist =  !CONCAT(!varlist, !BLANKS(1), qY_x_b_b)
    !IFEND
    !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), int_1)
    !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !modY) 
    !IF (!modYB_1 = 1) !THEN
       !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), int_2)
    !IFEND
!IFEND

!IF (!xW = 1) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), cp_w)
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !x)
!IFEND

!IF (!xB = 1) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), cp_b)
   !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !x)
!IFEND

!IF (!xW = 1) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), a1_w)
   !LET !m1list_w = !CONCAT(!m1list_w, !BLANKS(1), !x)
!IFEND

!IF (!xB = 1) !THEN
   !LET !varlist = !CONCAT(!varlist,  !BLANKS(1), a1_b)
   !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !x)
!IFEND

!LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !m1)
!LET !varlist = !CONCAT(!varlist, !BLANKS(1), b1_w) 

!IF (!SUBSTR(!mB,1,1) = 1) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), b1_b)
   !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !m1)
   !LET !bet_m1_1 = 1.
   !LET !mb_list = !CONCAT(!mB_list, !BLANKS(1), !m1_name).
!IFEND

!IF (!m2 ~= !NULL) !THEN
   !IF (!xW = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), a2_w)
      !LET !m2list_w = !CONCAT(!m2list_w, !BLANKS(1), !x)
   !IFEND
      !IF (!xB = 1) !THEN
         !LET !varlist = !CONCAT(!varlist,  !BLANKS(1), a2_b)
         !LET !m2list_b = !CONCAT(!m2list_b, !BLANKS(1), !x)
      !IFEND
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !m2)
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), b2_w)
   !IF (!SUBSTR(!mB,2,1) = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), b2_b)
      !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !m2)
      !LET !bet_m2_1 = 1.
   !LET !mb_list = !CONCAT(!mB_list, !BLANKS(1), !m2_name).
   !IFEND
!IFEND

!IF (!m3 ~= !NULL) !THEN
   !IF (!xW = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), a3_w) 
      !LET !m3list_w = !CONCAT(!m3list_w, !BLANKS(1), !x)
   !IFEND
      !IF (!xB = 1) !THEN
         !LET !varlist = !CONCAT(!varlist,  !BLANKS(1), a3_b)
         !LET !m3list_b = !CONCAT(!m3list_b, !BLANKS(1), !x)
      !IFEND
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !m3)
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), b3_w)
   !IF (!SUBSTR(!mB,3,1) = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), b3_b)
      !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !m3)
      !LET !bet_m3_1 = 1.
   !LET !mb_list = !CONCAT(!mB_list, !BLANKS(1), !m3_name).
   !IFEND
!IFEND

!IF (!cov1 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1y_w)
   !IF (!bet_c1_1 = 1) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1y_b)
   !IFEND
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m1_w)
   !IF (!bet_c1_1 = 1) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m1_b)
   !IFEND
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !cov1)
   !IF (!bet_c1_1 = 1) !THEN
      !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !cov1)
   !IFEND
   !LET !m1list_w = !CONCAT(!m1list_w, !BLANKS(1), !cov1)
   !IF (!bet_c1_1 = 1) !THEN
      !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !cov1)
   !IFEND
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m2_w)
       !IF (!bet_c1_1 = 1) !THEN
          !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m2_b)
       !IFEND
       !LET !m2list_w = !CONCAT(!m2list_w, !BLANKS(1), !cov1)
       !IF (!bet_c1_1 = 1) !THEN
          !LET !m2list_b = !CONCAT(!m2list_b, !BLANKS(1), !cov1)
       !IFEND
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m3_w)
          !IF (!bet_c1_1 = 1) !THEN
             !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m3_b)
          !IFEND
       !LET !m3list_w = !CONCAT(!m3list_w, !BLANKS(1), !cov1)
       !IF (!bet_c1_1 = 1) !THEN
          !LET !m3list_b = !CONCAT(!m3list_b, !BLANKS(1), !cov1)
       !IFEND
   !IFEND
!IFEND

!IF (!cov2 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2y_w)
   !IF (!bet_c2_1 = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2y_b)
   !IFEND
    !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m1_w)
   !IF (!bet_c2_1 = 1) !THEN
      !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m1_b)
   !IFEND
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !cov2)
   !IF (!bet_c2_1 = 1) !THEN
       !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !cov2)
       !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !cov2)
   !IFEND
   !LET !m1list_w = !CONCAT(!m1list_w, !BLANKS(1), !cov2)
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m2_w)
       !IF (!bet_c2_1 = 1) !THEN
          !LET !varlist = !CONCAT(!varlist,  !BLANKS(1), g2m2_b)
       !IFEND
       !LET !m2list_w = !CONCAT(!m2list_w, !BLANKS(1), !cov2)
       !IF (!bet_c2_1 = 1) !THEN
          !LET !m2list_b = !CONCAT(!m2list_b, !BLANKS(1), !cov2)
       !IFEND
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m3_w)
       !IF (!bet_c2_1 = 1) !THEN
           !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m3_b)
       !IFEND
       !LET !m3list_w = !CONCAT(!m3list_w, !BLANKS(1), !cov2)
       !IF (!bet_c2_1 = 1) !THEN
          !LET !m3list_b = !CONCAT(!m3list_b, !BLANKS(1), !cov2)
       !IFEND
   !IFEND
!IFEND

!IF (!cov3 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3y_w)
   !IF (!bet_c3_1 = 1) !THEN
      !LET !varlist = !CONCAT(!varlist,!BLANKS(1), g3y_b)
   !IFEND
   !LET !varlist = !CONCAT(!varlist,!BLANKS(1), g3m1_w)
   !IF (!bet_c3_1 = 1) !THEN
      !LET !varlist = !CONCAT(!varlist,!BLANKS(1), g3m1_b)
   !IFEND
   !LET !ylist_w = !CONCAT(!ylist_w, !BLANKS(1), !cov3)
   !LET !m1list_w = !CONCAT(!m1list_w, !BLANKS(1), !cov3)
   !IF (!bet_c3_1 = 1) !THEN
      !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !cov3)
      !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !cov3)
   !IFEND
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3m2_w)
       !IF (!bet_c3_1 = 1) !THEN
          !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3m2_b)
          !LET !m2list_b = !CONCAT(!m2list_b, !BLANKS(1), !cov3)
       !IFEND
       !LET !m2list_w = !CONCAT(!m2list_w, !BLANKS(1), !cov3)
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3m3_w)
       !IF (!bet_c3_1 = 1) !THEN
          !LET !varlist = !CONCAT(!varlist,  !BLANKS(1), g3m3_b)
          !LET !m3list_b = !CONCAT(!m3list_b, !BLANKS(1), !cov3)
       !IFEND
       !LET !m3list_w = !CONCAT(!m3list_w, !BLANKS(1), !cov3)
   !IFEND
!IFEND


!IF (!l2cov1 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1y_2, !BLANKS(1), g1m1_2)
   !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !l2cov1)
   !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov1)
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m2_2)
       !LET !m2list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov1)
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g1m3_2)
       !LET !m3list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov1)
   !IFEND
!IFEND

!IF (!l2cov2 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2y_2, !BLANKS(1), g2m1_2)
   !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !l2cov2)
   !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov2)
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m2_2)
       !LET !m2list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov2)
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g2m3_2)
       !LET !m3list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov2)
   !IFEND
!IFEND

!IF (!l2cov3 ~= !NULL) !THEN
   !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3y_2, !BLANKS(1), g3m1_2)
   !LET !ylist_b = !CONCAT(!ylist_b, !BLANKS(1), !l2cov3)
   !LET !m1list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov3)
   !IF (!m2 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3m2_2)
       !LET !m2list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov3)
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
       !LET !varlist = !CONCAT(!varlist, !BLANKS(1), g3m3_2)
       !LET !m3list_b = !CONCAT(!m1list_b, !BLANKS(1), !l2cov3)
   !IFEND
!IFEND



!LET !rcp_1 = 0.
!LET !ra1_1 = 0.
!LET !ra2_1 = 0.
!LET !ra3_1 = 0.
!LET !rb1_1 = 0.
!LET !rb2_1 = 0.
!LET !rb3_1 = 0.


!IF (!SUBSTR(!randx,1,1) = 1) !THEN
   !LET !rcp_1 = 1.
   !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1),cp_w).
   !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1),cp_w).
   !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1),cp_w).
!IFEND

!IF (!SUBSTR(!randx,2,1) = 1) !THEN
   !LET !ra1_1 = 1.
   !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), a1_w).
   !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), a1_w).
   !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), a1_w).
!IFEND

!IF (!m2 ~= !NULL) !THEN
   !IF (!SUBSTR(!randx,3,1) = 1) !THEN
      !LET !ra2_1 = 1.
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), a2_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), a2_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), a2_w).
   !IFEND
!IFEND

!IF (!m3 ~= !NULL) !THEN
   !IF (!SUBSTR(!randx,4,1) = 1) !THEN
      !LET !ra3_1 = 1.
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), a3_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), a3_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), a3_w).
   !IFEND
!IFEND


!IF (!SUBSTR(!randm,1,1) = 1) !THEN
   !LET !rb1_1 = 1.
   !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1),b1_w).
   !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1),b1_w).
   !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1),b1_w).
!IFEND

!IF (!m2 ~= !NULL) !THEN
   !IF (!SUBSTR(!randm,2,1) = 1) !THEN
      !LET !rb2_1 = 1.
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), b2_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), b2_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), b2_w).
   !IFEND
!IFEND

!IF (!m3 ~= !NULL) !THEN
   !IF (!SUBSTR(!randm,3,1) = 1) !THEN
      !LET !rb3_1 = 1.
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), b3_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), b3_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), b3_w).
   !IFEND
!IFEND


!IF (!cov1 ~= !NULL) !THEN
   !IF (!SUBSTR(!randc1,1,1) = 1) !THEN
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g1y_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g1y_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g1y_w).
   !IFEND
      !IF (!SUBSTR(!randc1,2,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g1m1_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g1m1_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g1m1_w).
      !IFEND
   !IF (!m2 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc1,3,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g1m2_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g1m2_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g1m2_w).
      !IFEND
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc1,4,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g1m3_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g1m3_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g1m3_w).
      !IFEND
   !IFEND
!IFEND

!IF (!cov2 ~= !NULL) !THEN
   !IF (!SUBSTR(!randc2,1,1) = 1) !THEN
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g2y_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g2y_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g2y_w).
   !IFEND
      !IF (!SUBSTR(!randc2,2,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g2m1_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g2m1_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g2m1_w).
      !IFEND
   !IF (!m2 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc2,3,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g2m2_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g2m2_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g2m2_w).
      !IFEND
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc2,4,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g2m3_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g2m3_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g2m3_w).
      !IFEND
   !IFEND
!IFEND

!IF (!cov3 ~= !NULL) !THEN
   !IF (!SUBSTR(!randc3,1,1) = 1) !THEN
      !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g3y_w).
      !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g3y_w).
      !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g3y_w).
   !IFEND
      !IF (!SUBSTR(!randc3,2,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g3m1_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g3m1_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g3m1_w).
      !IFEND
   !IF (!m2 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc3,3,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranvar, !BLANKS(1), g3m2_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g3m2_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g3m2_w).
      !IFEND
   !IFEND
   !IF (!m3 ~= !NULL) !THEN
      !IF (!SUBSTR(!randc3,4,1) = 1) !THEN
         !LET !ranvar = !CONCAT(!ranslope, !BLANKS(1), g3m3_w).
         !LET !ranslope = !CONCAT(!ranslope, !BLANKS(1), g3m3_w).
         !LET !ranslop2 = !CONCAT(!ranslop2, !BLANKS(1), g3m3_w).
      !IFEND
   !IFEND
!IFEND

!LET !rmint = 0.

!IF (!rm1int_1 = 1) !THEN
   !LET !rmint = 1.
!IFEND
!IF (!rm2int_1 = 1) !THEN
   !LET !rmint = 1.
!IFEND
!IF (!rm3int_1 = 1) !THEN
   !LET !rmint = 1.
!IFEND

!LET !intcovmat = ID.
!IF (!rm1int_1 = 1 !AND !rm2int_1 = 1) !THEN
   !LET !intcovmat = !mcovmat.
!IFEND
!IF (!rm1int_1 = 1 !AND !rm3int_1 = 1) !THEN
   !LET !intcovmat = !mcovmat.
!IFEND
!IF (!rm2int_1 = 1 !AND !rm3int_1 = 1) !THEN
   !LET !intcovmat = !mcovmat.
!IFEND

!LET !int_1 = (!intcovmat = UN).
!LET !ycov_1 = (!ycov = 1).
!LET !indint_1 = (!indint = 1).

!LET !covs = 1.



OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['G Matrix']
  /DESTINATION FORMAT=SAV
   OUTFILE=!QUOTE(!CONCAT(!folder,g_mat.sav)) VIEWER=NO.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Parameter Estimates']
  /DESTINATION FORMAT=SAV NUMBERED=TableNumber_
   OUTFILE=!QUOTE(!CONCAT(!folder,p_est.sav)) VIEWER=NO.
* OMS.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Covariance Matrix']
  /DESTINATION FORMAT=SAV NUMBERED=TableNumber_
   OUTFILE=!QUOTE(!CONCAT(!folder,cov_mat.sav)) VIEWER=!asym
  /COLUMNS SEQUENCE=[RALL CALL LALL].
* OMS.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Tests of Fixed Effects']
  /DESTINATION VIEWER=NO.
* OMS.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Model Dimension']
  /DESTINATION FORMAT=SAV NUMBERED=TableNumber_
   OUTFILE=!QUOTE(!CONCAT(!folder,mod_dim.sav)) VIEWER=NO.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Covariance Parameter Estimates']
  /DESTINATION FORMAT=SAV NUMBERED=TableNumber_
   OUTFILE=!QUOTE(!CONCAT(!folder,cov_p_est.sav)) VIEWER=!asym.
OMS
  /SELECT TABLES
  /IF COMMANDS=['Mixed'] SUBTYPES=['Information Criteria']
  /DESTINATION FORMAT=SAV NUMBERED=TableNumber_
   OUTFILE=!QUOTE(!CONCAT(!folder,fit_stat.sav)) VIEWER=NO.

!IF (!indint = 0 !OR !rmint = 0) !THEN
   !IF (!ycov = 1 !OR !indint = 0) !THEN
      MIXED
       Z WITH !varlist
       /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
       /FIXED =  !varlist | NOINT SSTYPE(3)
       /METHOD = !est
       /PRINT = COVB G SOLUTION TESTCOV
       /RANDOM !ranvar | SUBJECT(!cluster) COVTYPE(!covmat)
       /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
      EXECUTE.
   !ELSE
      !IF (!ranslope ~= !BLANKS(0)) !THEN
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM y_int | SUBJECT(!cluster) COVTYPE(ID)
          /RANDOM !ranslope | SUBJECT(!cluster) COVTYPE(!covmat)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
      !ELSE
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM y_int | SUBJECT(!cluster) COVTYPE(ID)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
         !LET !covs = 0.
      !IFEND
   !IFEND
!ELSE
   !IF (!ycov = 1) !THEN
      !IF (!ranslop2 ~= !BLANKS(0)) !THEN      
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM !ranint | SUBJECT(!cluster) COVTYPE(!intcovmat)
          /RANDOM !ranslope | SUBJECT(!cluster) COVTYPE(!covmat)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
      !ELSE
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM !ranint | SUBJECT(!cluster) COVTYPE(!intcovmat)
          /RANDOM !ranslope | SUBJECT(!cluster) COVTYPE(ID)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
         !LET !covs = 0.
      !IFEND
   !ELSE
      !IF (!ranslope ~= !BLANKS(0)) !THEN
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM !ranint | SUBJECT(!cluster) COVTYPE(!intcovmat)
          /RANDOM y_int | SUBJECT(!cluster) COVTYPE(ID)
          /RANDOM !ranslope | SUBJECT(!cluster) COVTYPE(!covmat)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
      !ELSE
         MIXED
          Z WITH !varlist
          /CRITERIA = CIN(!conf) MXITER(!iters) MXSTEP(!mxstep) SCORING(!scoring)
          /FIXED =  !varlist | NOINT SSTYPE(3)
          /METHOD = !est
          /PRINT = COVB G SOLUTION TESTCOV
          /RANDOM !ranint | SUBJECT(!cluster) COVTYPE(!intcovmat)
          /RANDOM y_int | SUBJECT(!cluster) COVTYPE(ID)
          /REPEATED DV | SUBJECT(case_num*!cluster) COVTYPE(!rescovmat).
         EXECUTE.
         !LET !covs = 0.
      !IFEND
   !IFEND
!IFEND

OMSEND.

OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT HEADINGS 
  /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[3]
  /DELETEOBJECT DELETE=YES
  /SELECT HEADINGS 
  /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[2]
  /DELETEOBJECT DELETE=YES
  /SELECT HEADINGS 
  /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES.

!IF (!indint = 0 !OR !rmint = 0) !THEN
   !IF (!ycov = 0) !THEN
      OUTPUT MODIFY
        /REPORT PRINTREPORT=NO
        /SELECT HEADINGS 
        /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
        /DELETEOBJECT DELETE=YES.
   !IFEND
!ELSE
   !IF (!ycov = 1) !THEN
      OUTPUT MODIFY
        /REPORT PRINTREPORT=NO
        /SELECT HEADINGS 
        /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
        /DELETEOBJECT DELETE=YES.
   !ELSE
      OUTPUT MODIFY
        /REPORT PRINTREPORT=NO
        /SELECT HEADINGS 
        /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
        /DELETEOBJECT DELETE=YES.
      OUTPUT MODIFY
        /REPORT PRINTREPORT=NO
        /SELECT HEADINGS 
        /IF COMMANDS=["Mixed(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
        /DELETEOBJECT DELETE=YES.
   !IFEND
!IFEND



DATASET CLOSE mlmdata.

DATASET ACTIVATE !data.




MATRIX.
GET fit_stat/file=!QUOTE(!CONCAT(!folder,fit_stat.sav))/variables = Var2/MISSING = ACCEPT/SYSMIS=-999. 
GET estim/file=!QUOTE(!CONCAT(!folder,p_est.sav))/variables = Estimate To UpperBound/MISSING = ACCEPT/SYSMIS=-999. 
GET g/file=!QUOTE(!CONCAT(!folder,g_mat.sav))/MISSING = ACCEPT/SYSMIS=0. 
GET cov_mat/file=!QUOTE(!CONCAT(!folder,cov_mat.sav))/MISSING = ACCEPT/SYSMIS=-999. 
GET cov_p/file=!QUOTE(!CONCAT(!folder,cov_p_est.sav))/variables = Estimate TO UpperBound/MISSING = ACCEPT/SYSMIS=-999. 
GET mod_dim/file=!QUOTE(!CONCAT(!folder,mod_dim.sav))/variables =NumberofParameters NumberofSubjects/MISSING = ACCEPT/SYSMIS=-999. 

print /title = "***********************  MLMED -  BETA VERSION ************************".
  print/title = "                     Written by Nicholas J. Rockwood        ".
  print/title = "              Documentation available at www.njrockwood.com        ".
print/title = "              Please report any bugs to rockwood.19@osu.edu                   ".
 print /title = "***********************************************************************".

compute est = estim(:,1).

compute yeq = estim(1,:).
compute m1eq = estim(2,:).

compute num_int = 2.
compute num_rint = 0.
do if (!ryint_1 = 1).
   compute num_rint = num_rint + 1.
end if.
do if (!rm1int_1 = 1).
   compute num_rint = num_rint + 1.
end if.

do if (!m2_1 = 1).
   compute num_int = num_int + 1.
   compute m2eq = estim(num_int,:).
   do if (!rm2int_1 = 1).
      compute num_rint = num_rint + 1.
   end if.
end if.
do if (!m3_1 = 1).
   compute num_int = num_int + 1.
   compute m3eq = estim(num_int,:).
   do if (!rm3int_1 = 1).
      compute num_rint = num_rint + 1.
   end if.
end if.

compute sca1b1b = 0.
compute sca2b2b = 0.
compute sca3b3b = 0.


compute num_fix = nrow(est).
compute num_ran = nrow(g).
compute g = g(1:num_ran, 5:(4 + num_ran)). 
compute cov_fix = reshape(cov_mat(1, 5:(4 + num_fix**2)), num_fix, num_fix).


do if (!rescov_1 = 1).
   compute rfx = cov_p(((num_int&*(num_int + 1)/2) + 1):nrow(cov_p),:).
   compute resfx = cov_p(1:(num_int&*(num_int + 1)/2),:). 
else.
   compute rfx = cov_p((num_int + 1):nrow(cov_p),:).
   compute resfx = cov_p(1:num_int,:).
end if.

compute sampsize = mod_dim((nrow(mod_dim)-1),2).
compute tot_par = mod_dim(nrow(mod_dim),1).
compute fix_par = num_fix.
compute ran_l1_p = nrow(resfx).
compute ran_l2_p = tot_par-fix_par-ran_l1_p.
compute mod_spec = {sampsize; fix_par; ran_l1_p; ran_l2_p; tot_par}.

compute ran_mat =(cov_mat(2,((5 + num_fix**2):ncol(cov_mat)))).
compute ranmatn = sqrt(ncol(ran_mat)). 
compute cov_ran = reshape(ran_mat, ranmatn, ranmatn). 

do if (!rescov_1 = 1).
   compute cov_ran = cov_ran(((num_rint&**2 + 1):ncol(cov_ran)), ((num_rint&**2 + 1):ncol(cov_ran))).
else.
   compute cov_ran = cov_ran(((num_rint + 1):ncol(cov_ran)), ((num_rint + 1):ncol(cov_ran))).
end if.

compute m1tab_b = {0,0,0,0,0,0,0}.
compute m2tab_b = {0,0,0,0,0,0,0}.
compute m3tab_b = {0,0,0,0,0,0,0}.
compute ytab_b = {0,0,0,0,0,0,0}.

compute fcount = 1.
compute y_int = est(fcount, :).
compute ytab_w = estim(fcount,:).
compute svy_int = cov_fix(fcount, fcount). 
compute fcount = fcount + 1.
compute m1_int = est(fcount,:).
compute m1tab_w = estim(fcount,:).
compute svm1_int = cov_fix(fcount, fcount). 
compute fcount = fcount + 1.

do if (!m2_1 = 1).
   compute m2_int = est(fcount,:).
   compute m2tab_w = estim(fcount,:).
   compute svm2_int = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
end if.

do if (!m3_1 = 1).
   compute m3_int = est(fcount,:).
   compute m3tab_w = estim(fcount,:).
   compute svm3_int = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
end if.

do if (!ModM_1 = 1).
   compute qM = est(fcount,:).
   compute m1tab_b = {m1tab_b; estim(fcount,:)}.
   compute svqM = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
   do if (!wit_x_1 = 1).
      compute qMxaw = est(fcount,:).
      compute m1tab_w = {m1tab_w; estim(fcount,:)}.   
      compute qMxaw_l = fcount.
      compute svqMxaw = cov_fix(fcount, fcount). 
      compute fcount = fcount + 1.
   end if.
   do if (!bet_x_1 = 1 AND !modMB_1 = 1).
      compute qMxab = est(fcount,:).
      compute m1tab_b = {m1tab_b; estim(fcount,:)}.
      compute qMxab_l = fcount.
      compute svqMxab = cov_fix(fcount, fcount). 
      compute fcount = fcount + 1.
   end if.
end if.

do if (!ModY_1 = 1).
   compute qY = est(fcount,:).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute qY_l = fcount.
   compute svqY = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
   compute qYxbw = est(fcount,:).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute qYxbw_l = fcount.
   compute svqYxbw = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
   do if (!modYB_1 = 1).
      compute qYxbb = est(fcount,:).
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute qYxbb_l = fcount.
      compute svqYxbb = cov_fix(fcount, fcount). 
      compute fcount = fcount + 1.
   end if.
end if.

do if (!wit_x_1 = 1).
   compute cp_w = est(fcount,:).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute cp_w_l = fcount.
   compute svcp_w = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
end if.
do if (!bet_x_1 = 1).
   compute cp_b = est(fcount,:).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute cp_b_l = fcount.
   compute svcp_b = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
end if.
do if (!wit_x_1 = 1).
   compute a1_w = est(fcount,:).
   compute m1tab_w = {m1tab_w; estim(fcount,:)}.
   compute a1_w_l = fcount. 
   compute sva1_w = cov_fix(fcount, fcount). 
   do if (!bet_x_1 = 1).
      compute sca1b1w = cov_fix(fcount, fcount + 2).
   else.
      compute sca1b1w = cov_fix(fcount, fcount + 1).
   end if.
   compute fcount = fcount + 1.
end if.
do if (!bet_x_1 = 1).
   compute a1_b = est(fcount,:).
   compute m1tab_b = {m1tab_b; estim(fcount,:)}.
   compute a1_b_l = fcount. 
   compute sva1_b = cov_fix(fcount, fcount). 
   do if (!bet_m1_1 = 1).
      compute sca1b1b = cov_fix(fcount, fcount + 2). 
   end if.
   compute fcount = fcount + 1.
end if.

compute b1_w = est(fcount,:).
compute Ytab_w = {Ytab_w; estim(fcount,:)}.
compute b1_w_l = fcount. 
compute svb1_w = cov_fix(fcount, fcount). 
compute fcount = fcount + 1.

do if (!bet_m1_1 = 1).
   compute b1_b = est(fcount,:).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute b1_b_l = fcount.
   compute svb1_b = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
end if.

do if (!wit_x_1 = 1).
   compute sca1b1w = cov_fix(a1_w_l, b1_w_l).
end if.
do if (!bet_x_1 = 1 AND !bet_m1_1 = 1).
   compute sca1b1b = cov_fix(a1_b_l, b1_b_l). 
end if. 


do if (!m2_1 = 1).
   do if (!wit_x_1 = 1).
      compute a2_w = est(fcount,:).
      compute m2tab_w = {m2tab_w; estim(fcount,:)}.
      compute a2_w_l = fcount.
      compute sva2_w = cov_fix(fcount, fcount). 
      do if (!bet_x_1 = 1).
         compute sca2b2w = cov_fix(fcount, fcount + 2). 
      else.
         compute sca2b2w = cov_fix(fcount, fcount + 1). 
      end if.
      compute fcount = fcount + 1.
   end if.
   do if (!bet_x_1 = 1).
      compute a2_b = est(fcount,:).
      compute m2tab_b = {m2tab_b; estim(fcount,:)}.
      compute a2_b_l = fcount.
      compute sva2_b = cov_fix(fcount, fcount). 
      do if (!bet_m2_1 = 1).
         compute sca2b2b = cov_fix(fcount, fcount + 2). 
      end if.
      compute fcount = fcount + 1.
   end if.
   compute b2_w = est(fcount,:).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute b2_w_l = fcount.
   compute svb2_w = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
   do if (!bet_m2_1 = 1).
      compute b2_b = est(fcount,:).
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute b2_b_l = fcount.
      compute svb2_b = cov_fix(fcount, fcount). 
      compute fcount = fcount + 1.
   end if.
   do if (!wit_x_1 = 1).
      compute sca2b2w = cov_fix(a2_w_l, b2_w_l).
   end if.
   do if (!bet_x_1 = 1 AND !bet_m2_1 = 1).
      compute sca2b2b = cov_fix(a2_b_l, b2_b_l). 
   end if. 
end if.

do if (!m3_1 = 1).
   do if (!wit_x_1 = 1).
      compute a3_w = est(fcount,:).
      compute m3tab_w = {m3tab_w; estim(fcount,:)}.
      compute a3_w_l = fcount.
      compute sva3_w = cov_fix(fcount, fcount). 
      do if (!bet_x_1 = 1).
         compute sca3b3w = cov_fix(fcount, fcount + 2). 
      else.
         compute sca3b3w = cov_fix(fcount, fcount + 1). 
      end if.
      compute fcount = fcount + 1.
   end if.
   do if (!bet_x_1 = 1).
      compute a3_b = est(fcount,:).
      compute m3tab_b = {m3tab_b; estim(fcount,:)}.
      compute a3_b_l = fcount.
      compute sva3_b = cov_fix(fcount, fcount). 
      do if (!bet_m3_1 = 1).
         compute sca3b3b = cov_fix(fcount, fcount + 2). 
      end if.
      compute fcount = fcount + 1.
   end if.
   compute b3_w = est(fcount,:).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute b3_w_l = fcount.
   compute svb3_w = cov_fix(fcount, fcount). 
   compute fcount = fcount + 1.
   do if (!bet_m3_1 = 1).
      compute b3_b = est(fcount,:).
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute b3_b_l = fcount.
      compute svb3_b = cov_fix(fcount, fcount). 
      compute fcount = fcount + 1.
   end if.
   do if (!wit_x_1 = 1).
      compute sca3b3w = cov_fix(a3_w_l, b3_w_l).
   end if.
   do if (!bet_x_1 = 1).
      compute sca3b3b = cov_fix(a3_b_l, b3_b_l). 
   end if. 
end if.

do if (!cov1_1 = 1).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c1_1 = 1).  
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute fcount = fcount + 1.
   end if.
   compute m1tab_w = {m1tab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c1_1 = 1).  
      compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m2_1 = 1).
      compute m2tab_w = {m2tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c1_1 = 1).  
         compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_w = {m3tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c1_1 = 1).  
         compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
end if. 

do if (!cov2_1 = 1).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c2_1 = 1).
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute fcount = fcount + 1.
   end if.
   compute m1tab_w = {m1tab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c2_1 = 1).
      compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m2_1 = 1).
      compute m2tab_w = {m2tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c2_1 = 1).
         compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_w = {m3tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c2_1 = 1).
         compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
end if. 

do if (!cov3_1 = 1).
   compute Ytab_w = {Ytab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c3_1 = 1).
      compute Ytab_b = {Ytab_b; estim(fcount,:)}.
      compute fcount = fcount + 1.
   end if.
   compute m1tab_w = {m1tab_w; estim(fcount,:)}.
   compute fcount = fcount + 1.
   do if (!bet_c3_1 = 1).
      compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m2_1 = 1).
      compute m2tab_w = {m2tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c3_1 = 1).
         compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_w = {m3tab_w; estim(fcount,:)}.
      compute fcount = fcount + 1.
      do if (!bet_c3_1 = 1).
         compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
         compute fcount = fcount + 1.
      end if.
   end if.
end if. 


do if (!l2cov1_1 = 1).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute fcount = fcount + 1.
   compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
   compute fcount = fcount + 1.
   do if (!m2_1 = 1).
      compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
end if. 


do if (!l2cov2_1 = 1).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute fcount = fcount + 1.
   compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
   compute fcount = fcount + 1.
   do if (!m2_1 = 1).
      compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
end if. 


do if (!l2cov3_1 = 1).
   compute Ytab_b = {Ytab_b; estim(fcount,:)}.
   compute fcount = fcount + 1.
   compute m1tab_b = {m1tab_b; estim(fcount,:)}.   
   compute fcount = fcount + 1.
   do if (!m2_1 = 1).
      compute m2tab_b = {m2tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
   do if (!m3_1 = 1).
      compute m3tab_b = {m3tab_b; estim(fcount,:)}.   
      compute fcount = fcount + 1.
   end if.
end if. 


compute m1b = 0.
compute m2b = 0.
compute m3b = 0.
compute yb = 0.

do if (nrow(ytab_b) > 1).
   compute ytab_b = ytab_b(2:nrow(ytab_b),:).
   compute yb = 1.
end if. 

do if (nrow(m1tab_b) > 1).
   compute m1tab_b = m1tab_b(2:nrow(m1tab_b),:).
   compute m1b = 1.
end if.
do if (!m2_1 = 1).
   do if (nrow(m2tab_b) > 1).
      compute m2tab_b = m2tab_b(2:nrow(m2tab_b),:).
      compute m2b = 1.
   end if.
end if.
do if (!m3_1 = 1).
   do if (nrow(m3tab_b) > 1).
      compute m3tab_b = m3tab_b(2:nrow(m3tab_b),:).
      compute m3b = 1.
   end if.
end if.




compute f_nc = csum(estim(:,4) = -999).
compute r_nc = csum(cov_p(:,3) = -999).

compute e1 = (estim(:,4) = -999)

do if (f_nc > 0).
   print /title = "***Warning: One or more fixed effect parameters could not be estimated.".
   print f_nc /title = "Number of fixed effect parameters that could not be estimated:".
   print /title = "***********************************************************************"/ format = A8.
end if. 



do if (r_nc > 0).
   print /title = "***Warning: One or more random effect parameters could not be estimated.".
   print r_nc /title = "Number of random effect parameters that could not be estimated:".
   print /title = "***********************************************************************"/ format = A8.
end if. 





do if (r_nc = 0 AND f_nc = 0).
   print mod_spec /title "Model Specification"/format = F8.0 / rlabels = "N" "Fixed" "Rand(L1)" "Rand(L2)" "Total".
   print fit_stat /title = "Model Fit Statistics"/format = F8.4/ rlabels = "-2LL" "AIC" "AICC" "CAIC" "BIC"/ clabels = "Value".
   print /title = " ".
end if. 






do if ((r_nc = 0 AND f_nc = 0) OR !eor_1 = 1).

   print /title = "***************************  FIXED EFFECTS  ***************************".
   compute m1ntab = {"Outcome:", !m1_name}.
   print m1ntab/title = "***********************************************************************"/ format = A8.
   print m1tab_w/ title = "Within- Effects" /rlabels = !m1list_w "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      do if (m1b = 1).
         print m1tab_b/ title = "Between- Effects" /rlabels = !m1list_b "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      else.
         print /title = "Note: No Between- Effect(s) Specified.". 
      end if. 

   do if (!ModM_1 = 1).
      compute indtab1 = {"0", "0", "0", "0", "0", "0"}.
      do if (!xW = 1).
         compute indtab1 = {indtab1; "Within-", !modM_n, "   x   ", !x_name, "   ->   ", !m1_name}.
      end if.
      do if (!bet_x_1 = 1 AND !modMB_1 = 1).
         compute indtab1 = {indtab1; "Between-", !modM_n, "   x   ", !x_name, "   ->   ", !m1_name}.
      end if.
      compute indtab1 = indtab1(2:nrow(indtab1),:).
      print indtab1 /title = "Interaction Codes" /rlabels = "int_1   " "int_2   " /format = A8.
   end if.


   do if (!m2_1 = 1).
      compute m2ntab = {"Outcome:", !m2_name}.
      print m2ntab/title = "***********************************************************************"/ format = A8.
      print m2tab_w/ title = "Within- Effects" /rlabels = !m2list_w "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      do if (m2b = 1).
         print m2tab_b/ title = "Between- Effects" /rlabels = !m2list_b "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      else.
         print /title = "Note: No Between- Effects Specified.". 
      end if. 
   end if.


   do if (!m3_1 = 1).
      compute m3ntab = {"Outcome:", !m3_name}.
      print m3ntab/title = "***********************************************************************"/ format = A8.
      print m3tab_w/ title = "Within- Effects" /rlabels = !m3list_w "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      do if (m3b = 1).
         print m3tab_b/ title = "Between- Effects" /rlabels = !m3list_b "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
      else.
         print /title = "Note: No Between- Effects Specified.". 
      end if. 
   end if.


   compute yntab = {"Outcome:", !y_name}.
   print yntab/title = "***********************************************************************"/format = A8.
   print ytab_w/ title = "Within- Effects" /rlabels = !ylist_w "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
   do if (yb = 1).
      print ytab_b/ title = "Between- Effects" /rlabels = !ylist_b "-"/format = F8.4/clabels = "Estimate" "S.E." "df" "t" "p" "LL" "UL".
   else.
      print /title = "Note: No Between- Effects Specified.". 
   end if. 

   do if (!ModY_1 = 1).
      compute indtab2 = {"Within-", !modY_n, "   x   ", !m1_name, "   ->   ", !y_name}.
      do if (!modYB_1 = 1).
         compute indtab2 = {indtab2; "Between-", !modY_n, "   x   ", !m1_name, "   ->   ", !y_name}.
      end if.
      print indtab2 /title = "Interaction Codes" /rlabels = "int_1   " "int_2   " /format = A8.
   end if.

   print /title = "***********************************************************************"/format = A8.

else if (f_nc > 0).

   print /title = "***************************  FIXED EFFECTS  ***************************".
   compute m1ntab = {"Outcome:", !m1_name}.
   print m1ntab/title = "***********************************************************************"/ format = A8.
   print (m1tab_w(:,3) = -999)/ title = "Within- Effects" /rlabels = !m1list_w "-"/format = F8.0/clabels = "Error".
      do if (m1b = 1).
         print (m1tab_b(:,3) = -999)/ title = "Between- Effects" /rlabels = !m1list_b "-"/format = F8.0/clabels = "Error".
      else.
         print /title = "Note: No Between- Effect(s) Specified.". 
      end if. 

   do if (!ModM_1 = 1).
      compute indtab1 = {"0", "0", "0", "0", "0", "0"}.
      do if (!xW = 1).
         compute indtab1 = {indtab1; "Within-", !modM_n, "   x   ", !x_name, "   ->   ", !m1_name}.
      end if.
      do if (!bet_x_1 = 1 AND !modMB_1 = 1).
         compute indtab1 = {indtab1; "Between-", !modM_n, "   x   ", !x_name, "   ->   ", !m1_name}.
      end if.
      compute indtab1 = indtab1(2:nrow(indtab1),:).
      print indtab1 /title = "Interaction Codes" /rlabels = "int_1   " "int_2   " /format = A8.
   end if.



   do if (!m2_1 = 1).
      compute m2ntab = {"Outcome:", !m2_name}.
      print m2ntab/title = "***********************************************************************"/ format = A8.
      print (m2tab_w(:,3) = -999)/ title = "Within- Effects" /rlabels = !m2list_w "-"/format = F8.0/clabels = "Error".
      do if (m2b = 1).
         print (m2tab_b(:,3) = -999)/ title = "Between- Effects" /rlabels = !m2list_b "-"/format = F8.0/clabels = "Error".
      else.
         print /title = "Note: No Between- Effects Specified.". 
      end if. 
   end if.


   do if (!m3_1 = 1).
      compute m3ntab = {"Outcome:", !m3_name}.
      print m3ntab/title = "***********************************************************************"/ format = A8.
      print (m3tab_w(:,3) = -999)/ title = "Within- Effects" /rlabels = !m3list_w "-"/format = F8.0/clabels = "Error".
      do if (m3b = 1).
         print (m3tab_b(:,3) = -999)/ title = "Between- Effects" /rlabels = !m3list_b "-"/format = F8.0/clabels = "Error".
      else.
         print /title = "Note: No Between- Effects Specified.". 
      end if. 
   end if.

   compute yntab = {"Outcome:", !y_name}.
   print yntab/title = "***********************************************************************"/format = A8.
   print (ytab_w(:,3) = -999)/ title = "Within- Effects" /rlabels = !ylist_w "-"/format = F8.0/clabels = "Error".
   do if (yb = 1).
      print (ytab_b(:,3) = -999)/ title = "Between- Effects" /rlabels = !ylist_b "-"/format = F8.0/clabels = "Error".
   else.
      print /title = "Note: No Between- Effects Specified.". 
   end if. 

   do if (!ModY_1 = 1).
      compute indtab2 = {"Within-", !modY_n, "   x   ", !m1_name, "   ->   ", !y_name}.
      do if (!modYB_1 = 1).
         compute indtab2 = {indtab2; "Between-", !modY_n, "   x   ", !m1_name, "   ->   ", !y_name}.
      end if.
      print indtab2 /title = "Interaction Codes" /rlabels = "int_1   " "int_2   " /format = A8.
   end if.

   print /title = "***********************************************************************"/format = A8.

end if.








do if (!rescov_1 = 1).
   compute rfx = cov_p(((num_int&*(num_int + 1)/2) + 1):nrow(cov_p),:).
   compute resfx = cov_p(1:(num_int&*(num_int + 1)/2),:). 
else.
   compute rfx = cov_p((num_int + 1):nrow(cov_p),:).
   compute resfx = cov_p(1:num_int,:).
end if.

compute reskey = {!y_name; !m1_name}.
do if (!m2_1 = 1).
   compute reskey = {reskey; !m2_name}.
end if.
do if (!m3_1 = 1).
   compute reskey = {reskey; !m3_name}.
end if.


compute numrint = num_rint - (!ryint_1 = 1).


do if ((r_nc = 0 AND f_nc = 0) OR !eor_1 = 1).

   print /title = "**************************  RANDOM EFFECTS  ***************************".

   do if (!rescov_1 = 1).
      print resfx /title = "Level-1 Residual Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL"
         /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)"  /format = F8.4.
      print reskey/ title = "Level-1 Residual Names"/ rlabels = "1" "2" "3" "4" /format = A8.
   else.
      compute list = t(reskey).
      print resfx /title = "Level-1 Residual Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" /rlabels = !y !m1 !m2 !m3 "-"/format = F8.4.
   end if.



   do if ((!covmat_1 = 0 OR !covs = 0) AND !int_1 = 0).
      print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" /rlabels = "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"/ format = F8.4. 
   else.
      
      do if (!indint = 0).
        print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
              /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,1)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
              "(7,1)" "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,1)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
      
      else if (!int_1 = 0 AND !covmat_1 = 1).
         do if ((numrint = -1 AND !ycov = 0) OR (numrint = 1 AND !ycov = 1)).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,1)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,1)" "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,1)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 0 AND !ycov = 0) OR (numrint = 1 AND !ycov = 1)).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,2)" "(3,3)" "(4,2)" "(4,3)" "(4,4)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 1 AND !ycov = 0) OR (numrint = 2 AND !ycov = 1)).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,3)" "(4,4)" "(5,3)" "(5,4)" "(5,5)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 2 AND !ycov = 0) OR (numrint = 3 AND !ycov = 1)).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 0).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,4)" "(5,5)" "(6,5)" "(6,6)" 
                     "(7,5)" "(7,6)" "(7,7)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
        end if.
      else if (!int_1 = 1 AND !covmat_1 = 1).
         do if (numrint = 2 AND !ycov = 1).
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)" "(4,3)" "(4,4)" "(5,3)" "(5,4)" "(5,5)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 2 AND !ycov = 0). 
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 1). 
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 0). 
            print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,5)" "(6,5)" "(6,6)" 
              "(7,5)" "(7,6)" "(7,7)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         end if.
        else if (!int_1 = 1 AND !covmat_1 = 0).
            do if (numrint = 2).
               print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                  /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)"  "(4,4)" "(5,5)" "(6,6)" 
                  "(7,7)" "(8,8)"/ format = F8.4. 
            else if (numrint = 3).
               print rfx/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                  /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,5)" "(6,6)" 
                  "(7,7)" "(8,8)"/ format = F8.4. 
            end if.
         end if.

      
compute rfxtab = {1:20}.
      compute rfxtab = t(rfxtab).
      compute rfxtab = rfxtab(1:nrow(g)).
      print g/title = "Random Effect Covariance Matrix" /clabels = "1" "2" "3" "4" "5" "6" "7" "8" / rlabels = "1" "2" "3" "4" "5" "6" "7" "8" /format = F8.4.
      compute sds = sqrt(mdiag(diag(g))).
      compute sds_inv = inv(sds).
      compute cor_mat = sds_inv*g*sds_inv.
      print cor_mat/title = "Random Effect Correlation Matrix" /clabels = "1" "2" "3" "4" "5" "6" "7" "8" / rlabels = "1" "2" "3" "4" "5" "6" "7" "8" /format = F8.4.
   end if.

else if (r_nc > 0).

   print /title = "**************************  RANDOM EFFECTS  ***************************".

   do if (!rescov_1 = 1).
      print (resfx(:,3) = -999)/title = "Level-1 Residual Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL"
         /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)"  /format = F8.4.
      print reskey/ title = "Level-1 Residual Names"/ rlabels = "1" "2" "3" "4" /format = A8.
   else.
      compute list = t(reskey).
      print (resfx(:,3) = -999) /title = "Level-1 Residual Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" /rlabels = !y !m1 !m2 !m3 "-"/format = F8.4.
   end if.



   do if ((!covmat_1 = 0 OR !covs = 0) AND !int_1 = 0).
      print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" /rlabels = "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"/ format = F8.4. 
   else.
      
      do if (!indint = 0).
        print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
              /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,1)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
              "(7,1)" "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,1)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
      
      else if (!int_1 = 0 AND !covmat_1 = 1).
         do if ((numrint = -1 AND !ycov = 0) OR (numrint = 1 AND !ycov = 1)).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,1)" "(4,2)" "(4,3)" "(4,4)" "(5,1)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,1)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,1)" "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,1)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 0 AND !ycov = 0) OR (numrint = 1 AND !ycov = 1)).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,2)" "(3,3)" "(4,2)" "(4,3)" "(4,4)" "(5,2)" "(5,3)" "(5,4)" "(5,5)" "(6,2)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,2)" "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,2)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 1 AND !ycov = 0) OR (numrint = 2 AND !ycov = 1)).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,3)" "(4,4)" "(5,3)" "(5,4)" "(5,5)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if ((numrint = 2 AND !ycov = 0) OR (numrint = 3 AND !ycov = 1)).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
                     "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 0).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                     /rlabels = "(1,1)" "(2,2)" "(3,3)" "(4,4)" "(5,5)" "(6,5)" "(6,6)" 
                     "(7,5)" "(7,6)" "(7,7)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
        end if.
      else if (!int_1 = 1 AND !covmat_1 = 1).
         do if (numrint = 2 AND !ycov = 1).
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)" "(4,3)" "(4,4)" "(5,3)" "(5,4)" "(5,5)" "(6,3)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,3)" "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,3)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 2 AND !ycov = 0). 
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 1). 
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,4)" "(5,5)" "(6,4)" "(6,5)" "(6,6)" 
               "(7,4)" "(7,5)" "(7,6)" "(7,7)" "(8,4)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         else if (numrint = 3 AND !ycov = 0). 
            print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
               /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,5)" "(6,5)" "(6,6)" 
              "(7,5)" "(7,6)" "(7,7)" "(8,5)" "(8,6)" "(8,7)" "(8,8)"/ format = F8.4. 
         end if.
        else if (!int_1 = 1 AND !covmat_1 = 0).
            do if (numrint = 2).
               print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                  /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,3)"  "(4,4)" "(5,5)" "(6,6)" 
                  "(7,7)" "(8,8)"/ format = F8.4. 
            else if (numrint = 3).
               print (rfx(:,3) = -999)/ title = "Random Effect Estimates" /clabels = "Estimate" "S.E." "Wald Z" "p" "LL" "UL" 
                  /rlabels = "(1,1)" "(2,1)" "(2,2)" "(3,1)" "(3,2)" "(3,3)" "(4,4)" "(5,5)" "(6,6)" 
                  "(7,7)" "(8,8)"/ format = F8.4. 
            end if.
         end if.
   end if. 
end if.










compute va1 = 0.
compute va2 = 0.
compute va3 = 0.
compute vb1 = 0.
compute vb2 = 0.
compute vb3 = 0.

compute rtable = {"0", "0", "0", "0"}.
compute rcount = 1.
compute covcount = 1.



do if (!rm1int_1 = 1).
   compute rtable = {rtable; "Int", !m1_name, " ", " "}.
   compute vm1_int = g(rcount, rcount). 
   compute rcount = rcount + 1.
end if.

do if (!rm2_1 = 1 AND !rm2int_1 = 1).
   compute rtable = {rtable; "Int", !m2_name, " ", " "}.
   compute vm2_int = g(rcount, rcount).
   compute rcount = rcount + 1.
end if.

do if (!rm3_1 = 1 AND !rm3int_1 = 1).
   compute rtable = {rtable; "Int", !m3_name, " ", " "}.
   compute vm3_int = g(rcount, rcount).
   compute rcount = rcount + 1.
end if.

compute counter = rcount - 1.
do if (!int_1 = 1 AND counter > 0).
   compute counter = (counter*(counter + 1))/2.
end if. 

compute counter = counter + 1.
compute counter2 = 1.

do if (!ryint_1 = 1).
   compute rtable = {rtable; "Int", !y_name, " ", " "}.
   compute vy_int = g(rcount, rcount).
   compute rcount = rcount + 1.
   compute covcount = covcount + 1.
   do if (!ycov_1 = 1).
      compute counter2 = counter2 + 1.
   else.
      compute counter = counter + 1.
   end if.
end if.

do if (!rcp_1 = 1).
   compute rtable = {rtable; "Slope", !x_name, "   ->", !y_name}.
   compute vcp = g(rcount, rcount).
   compute rcount = rcount + 1.
   compute counter2 = counter2 + 1.
end if.

do if (!ra1_1 = 1).
   compute rtable = {rtable; "Slope", !x_name, "   ->", !m1_name}.
   compute va1 = g(rcount, rcount).
   compute a1loc = rcount.
   compute rcount = rcount + 1.
   compute a1loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

do if (!ra2_1 = 1).
   compute rtable = {rtable; "Slope", !x_name, "   ->", !m2_name}.
   compute va2 = g(rcount, rcount).
   compute a2loc = rcount.
   compute rcount = rcount + 1.
   compute a2loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

do if (!ra3_1 = 1).
   compute rtable = {rtable; "Slope", !x_name, "   ->", !m3_name}.
   compute va3 = g(rcount, rcount).
   compute a3loc = rcount.
   compute rcount = rcount + 1.
   compute a3loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

do if (!rb1_1 = 1).
   compute rtable = {rtable; "Slope", !m1_name, "   ->", !y_name}.
   compute vb1 = g(rcount, rcount).
   compute b1loc = rcount.
   compute rcount = rcount + 1.
   compute b1loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

do if (!rb2_1 = 1).
   compute rtable = {rtable; "Slope", !m2_name, "   ->", !y_name}.
   compute vb2 = g(rcount, rcount).
   compute b2loc = rcount.
   compute rcount = rcount + 1.
   compute b2loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

do if (!rb3_1 = 1).
   compute rtable = {rtable; "Slope", !m3_name, "   ->", !y_name}.
   compute vb3 = g(rcount, rcount).
   compute b3loc = rcount.
   compute rcount = rcount + 1.
   compute b3loc2 = counter2.
   compute counter2 = counter2 + 1.
end if.

compute rtable = rtable(2:nrow(rtable),:).

print rtable /title = "Random Effect Key" /rlabels = "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11"/ format = A10.
print /title = "***********************************************************************"/format = A8.

compute ca1a2 = 0.
compute ca1a3 = 0.
compute ca1b1 = 0.
compute ca1b2 = 0.
compute ca1b3 = 0.
compute ca2a3 = 0.
compute ca2b1 = 0.
compute ca2b2 = 0.
compute ca2b3 = 0.
compute ca3b1 = 0.
compute ca3b2 = 0.
compute ca3b3 = 0.
compute cb1b2 = 0.
compute cb1b3 = 0.
compute cb2b3 = 0.
compute sv_ca1b1 = 0.
compute sv_ca2b2 = 0.
compute sv_ca3b3 = 0.
compute sc_12 = 0.
compute sc_13 = 0.
compute sc_23 = 0.
compute v_a1b1_w = 0.
compute v_a2b2_w = 0.
compute v_a3b3_w = 0.

do if (!ra1_1 = 1).
   do if (!ra2_1 = 1).
      compute ca1a2 = g(a1loc, a2loc).
   end if.
   do if (!ra3_1 = 1).
      compute ca1a3 = g(a1loc, a3loc).
   end if.
   do if (!rb1_1 = 1).
      compute ca1b1 = g(a1loc, b1loc).
   end if. 
   do if (!rb2_1 = 1).
      compute ca1b2 = g(a1loc, b2loc).
   end if.
   do if (!rb3_1 = 1).
      compute ca1b3 = g(a1loc, b3loc).
   end if.
end if. 

do if (!ra2_1 = 1).
   do if (!ra3_1 = 1).
      compute ca2a3 = g(a2loc, a3loc).
   end if.
   do if (!rb1_1 = 1).
      compute ca2b1 = g(a2loc, b1loc).
   end if. 
   do if (!rb2_1 = 1).
      compute ca2b2 = g(a2loc, b2loc).
   end if.
   do if (!rb3_1 = 1).
      compute ca2b3 = g(a2loc, b3loc).
   end if.
end if. 

do if (!ra3_1 = 1).
   do if (!rb1_1 = 1).
      compute ca3b1 = g(a3loc, b1loc).
   end if. 
   do if (!rb2_1 = 1).
      compute ca3b2 = g(a3loc, b2loc).
   end if.
   do if (!rb3_1 = 1).
      compute ca3b3 = g(a3loc, b3loc).
   end if.
end if. 

do if (!rb1_1 = 1).
   do if (!rb2_1 = 1).
      compute cb1b2 = g(b1loc, b2loc).
   end if.
   do if (!rb3_1 = 1).
      compute cb1b3 = g(b1loc, b3loc).
   end if.
end if. 

do if (!rb2_1 = 1).
   do if (!rb3_1 = 1).
      compute cb2b3 = g(b2loc, b3loc).
   end if.
end if. 


compute mc_ca1b1 = make(!samples, 1, 0).
compute mc_ca2b2 = make(!samples, 1, 0).
compute mc_ca3b3 = make(!samples, 1, 0).



do if (!indint = 1 AND counter < nrow(cov_ran)).
   compute cov_ran = cov_ran(counter:nrow(cov_ran), counter:nrow(cov_ran)). 
end if.





do if (!covmat_1 = 1).
   do if (!ra1_1 = 1 AND !rb1_1 = 1).
      do if (!indint = 1).
         compute a1b1loc = {b1loc2, a1loc2}.
      else.
         compute a1b1loc = {b1loc, a1loc}.
      end if.
      compute rowa1b1 = (((a1b1loc(1,1) - 1)*a1b1loc(1,1))/2) + a1b1loc(1,2).
      compute sv_ca1b1 = cov_ran(rowa1b1, rowa1b1).
   end if.
   do if (!ra2_1 = 1 AND !rb2_1 = 1).
      do if (!indint = 1).
         compute a2b2loc = {b2loc2, a2loc2}.
      else.
         compute a2b2loc = {b2loc, a2loc}. 
      end if.
      compute rowa2b2 = (((a2b2loc(1,1) - 1)*a2b2loc(1,1))/2) + a2b2loc(1,2).
      compute sv_ca2b2 = cov_ran(rowa2b2, rowa2b2).
   end if.
   do if (!ra3_1 = 1 AND !rb3_1 = 1).
      do if (!indint = 1).
         compute a3b3loc = {b3loc2, a3loc2}.
      else.
         compute a3b3loc = {b3loc, a3loc}.
      end if.
      compute rowa3b3 = (((a3b3loc(1,1) - 1)*a3b3loc(1,1))/2) + a3b3loc(1,2).
       compute sv_ca3b3 = cov_ran(rowa3b3, rowa3b3).
   end if.

    do if (!ra1_1 = 1 AND !rb1_1 = 1 AND !ra2_1 = 1 AND !rb2_1 = 1).  
      compute sc_12 = cov_ran(rowa1b1, rowa2b2).
   end if.
    do if (!ra1_1 = 1 AND !rb1_1 = 1 AND !ra3_1 = 1 AND !rb3_1 = 1).  
      compute sc_13 = cov_ran(rowa1b1, rowa3b3).
   end if.
    do if (!ra2_1 = 1 AND !rb2_1 = 1 AND !ra3_1 = 1 AND !rb3_1 = 1).  
      compute sc_23 = cov_ran(rowa2b2, rowa3b3).
   end if.

   compute rcovs = {ca1b1; ca2b2; ca3b3}.
   compute rcovmat = {sv_ca1b1, sc_12, sc_13; sc_12, sv_ca2b2, sc_23; sc_13, sc_23, sv_ca3b3}.
   do if (rcovmat(1,1) = 0).
      compute rcovmat(1,1) = .0000000000001.
      compute rcovmat(1,2) = 0.
      compute rcovmat(1,3) = 0.
      compute rcovmat(2,1) = 0.
      compute rcovmat(3,1) = 0.
   end if.
   do if (rcovmat(2,2) = 0).
      compute rcovmat(2,2) = .0000000000001.
      compute rcovmat(1,2) = 0.
      compute rcovmat(2,1) = 0.
      compute rcovmat(2,3) = 0.
      compute rcovmat(3,2) = 0.
   end if.
   do if (rcovmat(3,3) = 0).
      compute rcovmat(3,3) = .0000000000001.
      compute rcovmat(1,3) = 0.
      compute rcovmat(3,1) = 0.
      compute rcovmat(2,3) = 0.
      compute rcovmat(3,2) = 0.
   end if.
   compute rvec = make(!samples, 1, rcovs(1)).
   loop i=2 TO nrow(rcovs).
      compute rvec_2 = make(!samples, 1, rcovs(i)).
      compute rvec = {rvec,rvec_2}.
   end loop. 
   compute rx2 = sqrt(-2*ln(uniform(!samples,ncol(rcovmat))))&*cos((2*3.14159265358979)*uniform(!samples,ncol(rcovmat))).
   compute rx2 = (rx2*chol(rcovmat)) + rvec.
   
   do if (!ra1_1 = 1 AND !rb1_1 = 1).
      compute mc_ca1b1 = rx2(:,1).
   end if.
   do if (!ra2_1 = 1 AND !rb2_1 = 1).
      compute mc_ca2b2 = rx2(:,2).
   end if.
    do if (!ra3_1 = 1 AND !rb3_1 = 1).
      compute mc_ca3b3 = rx2(:,3).
   end if.
end if.

compute cov_12 = 0.
compute cov_13 = 0.
compute cov_23 = 0.


***** Start of Output Not Shown If Error 

do if (f_nc = 0 AND r_nc = 0).


do if (!wit_x_1 = 1 AND f_nc = 0).
   compute a1b1_w = a1_w*b1_w + ca1b1.
   compute v_a1b1_w = (b1_w**2)*va1 + (a1_w**2)*vb1 + va1*vb1 + 2*a1_w*b1_w*ca1b1 + (ca1b1**2).
   compute sva1b1_w = (b1_w**2)*sva1_w + (a1_w**2)*svb1_w + sva1_w*svb1_w + 2*a1_w*b1_w*sca1b1w + (sca1b1w**2) + sv_ca1b1.
   compute se_a1b1w = sqrt(sva1b1_w).
   compute a1b1_w_z = a1b1_w/se_a1b1w.
   compute a1b1_w_p = (1 - cdfnorm(abs(a1b1_w_z)))*2.
end if.
do if (!bet_x_1 = 1 AND !bet_m1_1 = 1 AND f_nc = 0).
   compute a1b1_b = a1_b*b1_b.
   compute sva1b1b = (b1_b**2)*sva1_b + (a1_b**2)*svb1_b + sva1_b*svb1_b + 2*a1_b*b1_b*sca1b1b + (sca1b1b**2).
   compute se_a1b1b = sqrt(sva1b1b).
   compute a1b1_b_z = a1b1_b/se_a1b1b.
   compute a1b1_b_p = (1 - cdfnorm(abs(a1b1_b_z)))*2.
end if. 


do if (!m2_1 = 1).
   do if (!wit_x_1 = 1 AND f_nc = 0).
      compute a2b2_w = a2_w*b2_w + ca2b2.
      compute v_a2b2_w = (b2_w**2)*va2 + (a2_w**2)*vb2 + va2*vb2 + 2*a2_w*b2_w*ca2b2 + (ca2b2**2).
      compute cov_12 = a1_w*a2_w*cb1b2 + a1_w*b2_w*ca2b1 + b1_w*a2_w*ca1b2 + b1_w*b2_w*ca1a2 + ca1a2*cb1b2 + ca1b2*ca2b1.
      compute sva2b2_w = (b2_w**2)*sva2_w + (a2_w**2)*svb2_w + sva2_w*svb2_w + 2*a2_w*b2_w*sca2b2w + (sca2b2w**2) + sv_ca2b2.
      compute se_a2b2w = sqrt(sva2b2_w).
      compute a2b2_w_z = a2b2_w/se_a2b2w.
      compute a2b2_w_p = (1 - cdfnorm(abs(a2b2_w_z)))*2.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m2_1 = 1 AND f_nc = 0).
      compute a2b2_b = a2_b*b2_b.
      compute sva2b2b = (b2_b**2)*sva2_b + (a2_b**2)*svb2_b + sva2_b*svb2_b + 2*a2_b*b2_b*sca2b2b + (sca2b2b**2).
      compute se_a2b2b = sqrt(sva2b2b).
      compute a2b2_b_z = a2b2_b/se_a2b2b.
      compute a2b2_b_p = (1 - cdfnorm(abs(a2b2_b_z)))*2.
   end if. 
end if.


do if (!m3_1 = 1).
   do if (!wit_x_1 = 1 AND f_nc = 0).
      compute a3b3_w = a3_w*b3_w + ca3b3.
      compute v_a3b3_w = (b3_w**2)*va3 + (a3_w**2)*vb3 + va3*vb3 + 2*a3_w*b3_w*ca3b3 + (ca3b3**2).
      compute cov_13 = a1_w*a3_w*cb1b3 + a1_w*b3_w*ca3b1 + b1_w*a3_w*ca1b3 + b1_w*b3_w*ca1a3 + ca1a3*cb1b3 + ca1b3*ca3b1.
      compute cov_23 = a2_w*a3_w*cb2b3 + a2_w*b3_w*ca3b2 + b2_w*a3_w*ca2b3 + b2_w*b3_w*ca2a3 + ca2a3*cb2b3 + ca2b3*ca3b2.
      compute sva3b3_w = (b3_w**2)*sva3_w + (a3_w**2)*svb3_w + sva3_w*svb3_w + 2*a3_w*b3_w*sca3b3w + (sca3b3w**2) + sv_ca3b3.
      compute se_a3b3w = sqrt(sva3b3_w).
      compute a3b3_w_z = a3b3_w/se_a3b3w.
      compute a3b3_w_p = (1 - cdfnorm(abs(a3b3_w_z)))*2.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m3_1 = 1 AND f_nc = 0).
      compute a3b3_b = a3_b*b3_b.
      compute sva3b3b = (b3_b**2)*sva3_b + (a3_b**2)*svb3_b + sva3_b*svb3_b + 2*a3_b*b3_b*sca3b3b + (sca3b3b**2).
      compute se_a3b3b = sqrt(sva3b3b).
      compute a3b3_b_z = a3b3_b/se_a3b3b.
      compute a3b3_b_p = (1 - cdfnorm(abs(a3b3_b_z)))*2.
   end if. 
end if. 



compute conf = !conf.
compute cilow=((100-conf)/200).
compute cihigh=1-cilow.
compute cilow=trunc(!samples*cilow).
compute cihigh=trunc((!samples*cihigh)+.999)+1.




do if (!modM_1 = 1 OR !modY_1 = 1).
   print /title = "*******************  INDEX OF MODERATED MEDIATION  ********************".
end if. 

compute cov2  = cov_fix.
compute mvec2 = make(!samples, 1, est(1)).
loop i=2 TO nrow(est).
   compute mvec2_2 = make(!samples, 1, est(i)).
   compute mvec2 = {mvec2,mvec2_2}.
end loop. 



compute x2 = sqrt(-2*ln(uniform(!samples,ncol(cov_fix))))&*cos((2*3.14159265358979)*uniform(!samples,ncol(cov_fix))).
compute x2 = (x2*chol(cov2)) + mvec2.
do if (!wit_x_1 = 1).
   compute mc_ab1_w = x2(:,a1_w_l)&*x2(:,b1_w_l) + mc_ca1b1.
end if.
do if (!bet_x_1 = 1 AND !bet_m1_1 = 1).
   compute mc_ab1_b = x2(:,a1_b_l)&*x2(:,b1_b_l).
end if.
do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND !bet_m1_1 = 1).
   compute mc_ab1_c = mc_ab1_b - mc_ab1_w.
end if.

do if (!m2_1 = 1).
   do if (!wit_x_1 = 1).
      compute mc_ab2_w = x2(:,a2_w_l)&*x2(:,b2_w_l) + mc_ca2b2.
      compute ab_12_w = mc_ab2_w - mc_ab1_w.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m2_1 = 1).
      compute mc_ab2_b = x2(:,a2_b_l)&*x2(:,b2_b_l).
      do if (!bet_m1_1 = 1).
         compute ab_12_b = mc_ab2_b  - mc_ab1_b.
      end if.
   end if.
   do if (!bet_x_1 = 1 AND !wit_x_1 = 1AND !bet_m2_1 = 1).
      compute mc_ab2_c = mc_ab2_b - mc_ab2_w.
   end if.
end if.

do if (!m3_1 = 1).
   do if (!wit_x_1 = 1).
      compute mc_ab3_w = x2(:,a3_w_l)&*x2(:,b3_w_l) + mc_ca3b3.
      compute ab_13_w = mc_ab3_w - mc_ab1_w.
      compute ab_23_w = mc_ab3_w - mc_ab2_w.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m3_1 = 1).
      compute mc_ab3_b = x2(:,a3_b_l)&*x2(:,b3_b_l).
      do if (!bet_m1_1 = 1).
         compute ab_13_b = mc_ab3_b  - mc_ab1_b.
      end if.
      do if (!bet_m2_1 = 1).
         compute ab_23_b = mc_ab3_b  - mc_ab2_b.
      end if.
   end if.
   do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND !bet_m3_1 = 1).
      compute mc_ab3_c = mc_ab3_b - mc_ab3_w.
   end if.
end if.



do if (!modM_1 = 1).
   do if (!wit_x_1 = 1).
      compute QM_w = qMxaw&*b1_w.
      compute mc_QM_w = x2(:,qMxaw_l)&*x2(:,b1_w_l).
      compute i_sav_w = mc_QM_w.  
      compute QMwtmp = mc_QM_w.
      compute QMwtmp(GRADE(mc_QM_w)) = mc_QM_w.
      compute mc_QM_w = QMwtmp.
      compute QM_wci = {mc_QM_w(cilow,1), mc_QM_w(cihigh,1)}.
      compute tab6 = {QM_w, QM_wci}.
   end if.
   do if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !bet_m1_1 = 1).
      compute QM_b = qMxab&*b1_b.
      compute mc_QM_b = x2(:,qMxab_l)&*x2(:,b1_b_l).
      compute i_sav_b = mc_QM_b.
      compute QMbtmp = mc_QM_b.
      compute QMbtmp(GRADE(mc_QM_b)) = mc_QM_b.
      compute mc_QM_b = QMbtmp.
      compute QM_bci = {mc_QM_b(cilow,1), mc_QM_b(cihigh,1)}.
      compute tab7 = {QM_b,QM_bci}.
   end if.
end if.



do if (!modY_1 = 1).
   do if (!wit_x_1 = 1).
      compute QY_w = qYxbw&*a1_w.
      compute mc_QY_w = x2(:,qYxbw_l)&*x2(:,a1_w_l).
      compute QYwtmp = mc_QY_w.
      compute QYwtmp(GRADE(mc_QY_w)) = mc_QY_w.
      compute mc_QY_w = QYwtmp.
      compute QY_wci = {mc_QY_w(cilow,1), mc_QY_w(cihigh,1)}.
      compute tab8 = {QY_w, QY_wci}.
   end if.
   do if (!bet_x_1 = 1 AND !modYB_1 = 1 AND !bet_m1_1 = 1).
      compute QY_b = qYxbb&*a1_b.
      compute mc_QY_b = x2(:,qYxbb_l)&*x2(:,a1_b_l).
      compute QYbtmp = mc_QY_b.
      compute QYbtmp(GRADE(mc_QY_b)) = mc_QY_b.
      compute mc_QY_b = QYbtmp.
      compute QY_bci = {mc_QY_b(cilow,1), mc_QY_b(cihigh,1)}.
      compute tab9 = {QY_b, QY_bci}.
   end if.
end if. 


compute modmedW = 0.
compute modmedB = 0.


do if ((!modY_1 = 1) AND (!samemod = 1)).
      do if (!wit_x_1 = 1).
         compute QMY2_w = qYxbw&*qMxaw.
         compute QMY1_w = qMxaw&*b1_w + qYxbw&*a1_w.
         compute mcQMY2_w = x2(:,qYxbw_l)&*x2(:,qMxaw_l).
         compute mcQMY1_w = i_sav_w + mc_QY_w.

         compute QMYwt2 = mcQMY2_w.
         compute QMYwt2(GRADE(mcQMY2_w)) = mcQMY2_w.
         compute mcQMY2_w = QMYwt2.
         compute QMY2_wci = {mcQMY2_w(cilow,1), mcQMY2_w(cihigh,1)}.

         compute QMYwt1 = mcQMY1_w.
         compute QMYwt1(GRADE(mcQMY1_w)) = mcQMY1_w.
         compute mcQMY1_w = QMYwt1.
         compute QMY1_wci = {mcQMY1_w(cilow,1), mcQMY1_w(cihigh,1)}.

         compute tab10 = {QMY1_w, QMY1_wci; QMY2_w, QMY2_wci}.  
         print tab10/ title = "Within- Index of Moderated Mediation" 
            /clabels = "Est" "MCLL" "MCUL" /rlabels = "linear" "quad" /format = F8.4.
         compute modmedW = 1.
      end if.

      do if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !modYB_1 = 1 AND !bet_m1_1 = 1).
         compute QMY2_b = qYxbb&*qMxab.
         compute QMY1_b = qMxab&*b1_b + qYxbb&*a1_b. 
         compute mcQMY2_b = x2(:,qYxbb_l)&*x2(:,qMxab_l).
         compute mcQMY1_b = i_sav_b + mc_QY_b.

          compute QMYbt2 = mcQMY2_b.
         compute QMYbt2(GRADE(mcQMY2_b)) = mcQMY2_b.
         compute mcQMY2_b = QMYbt2.
         compute QMY2_bci = {mcQMY2_b(cilow,1), mcQMY2_b(cihigh,1)}.

          compute QMYbt1 = mcQMY1_b.
         compute QMYbt1(GRADE(mcQMY1_b)) = mcQMY1_b.
         compute mcQMY1_b = QMYbt1.
         compute QMY1_bci = {mcQMY1_b(cilow,1), mcQMY1_b(cihigh,1)}.

         compute tab11 = {QMY1_b, QMY1_bci; QMY2_b, QMY2_bci}.
         print tab11/ title = "Between- Index of Moderated Mediation"
            /clabels = "Est" "MCLL" "MCUL" /rlabels = "linear" "quad" /format = F8.4.
         compute modmedB = 1.
      else if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !modYB_1 = 0).
          print tab7/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM "modM"/format = F8.4.
         compute modmedB = 1.
      else if (!bet_x_1 = 1 AND !modMB_1 = 0 AND !modYB_1 = 1).
          print tab9/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modY "modY"/format = F8.4.
         compute modmedB = 1.
      end if.

      compute val = {!modMcent}.
      print /title = "***********************************************************************".
      print /title = "************************  INDIRECT EFFECT(S)  *************************".
      do if (modmedW = 1 AND modmedB = 1).
         print val/title = "NOTE: First Indirect Effects are Conditional on a Moderator Value of:"
            /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
      else if (modmedW = 1 AND modmedB = 0).
         print val/title = "NOTE: First Within- Indirect Effect is Conditional on a Moderator Value of:"
            /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
      else if (modmedW = 0 AND modmedB = 1).
         print val/title = "NOTE: First Between- Indirect Effect is Conditional on a Moderator Value of:"
            /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
      end if.
 end if.





do if (!samemod = 0 AND !modM_1 = 1 AND !modY_1 = 1).
         do if (!wit_x_1 = 1).
         compute comtab = {tab6;tab8}.
         print comtab/ title = "Within- Index of Moderated Mediation"
            /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM !modY "ModM" "modY" /format = F8.4.
         compute modmedW = 1.
         end if.
         do if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !modYB_1 = 1 AND !bet_m1_1 = 1).
            compute comtab2 = {tab7; tab9}.
            print comtab2/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM !modY "modM" "ModY" /format = F8.4.
            compute modmedB = 1.
         else if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !modYB_1 = 0).
            print tab7/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM "modM" /format = F8.4.
            compute modmedB = 1.
         else if (!bet_x_1 = 1 AND !modMB_1 = 0 AND !modYB_1 = 1).
            print tab9/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modY "modY" /format = F8.4.
            compute modmedB = 1.
         end if.
         print /title = "***********************************************************************".
         print /title = "************************  INDIRECT EFFECT(S)  *************************".
         do if (modmedW = 1 AND modmedB = 1).
            print {!modMcent;!modYcent} /title = "NOTE: First Indirect Effects are Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM !modY "ModM" "ModY" /format = F8.4.
         else if (modmedW = 1 AND modmedB = 0).
            print {!modMcent;!modYcent} /title = "NOTE: First Within- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM !modY "ModM" "ModY" /format = F8.4.
         else if (modmedW = 0 AND modmedB = 1).
            print {!modMcent;!modYcent} /title = "NOTE: First Between- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM !modY "ModM" "ModY" /format = F8.4.
         end if.
end if. 

do if (!samemod = 0 AND !modY_1 = 0 AND !modM_1 = 1).
         do if (!wit_x_1 = 1).
         print tab6/ title = "Within- Index of Moderated Mediation"
            /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM "modM"/format = F8.4.
         compute modmedW = 1.
         end if.
         do if (!bet_x_1 = 1 AND !modMB_1 = 1 AND !bet_m1_1 = 1).
            print tab7/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modM "modM"/format = F8.4.
         compute modmedB = 1.
         end if.
         print /title = "***********************************************************************".
         print /title = "************************  INDIRECT EFFECT(S)  *************************".
         do if (modmedW = 1 AND modmedB = 1).
            print !modMcent /title = "NOTE: First Indirect Effects are Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
         else if (modmedW = 1 AND modmedB = 0).
            print !modMcent /title = "NOTE: First Within- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
         else if (modmedW = 0 AND modmedB = 1).
            print !modMcent /title = "NOTE: First Between- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modM "modM" /format = F8.4.
         end if.
end if.
do if (!samemod = 0 AND !modY_1 = 1 AND !modM_1 = 0). 
      do if (!wit_x_1 = 1).
         print tab8/ title = "Within- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modY "modY" /format = F8.4.
      compute modmedW = 1.
      end if.
      do if (!bet_x_1 = 1 AND !modYB_1 = 1 AND !bet_m1_1 = 1).
         print tab9/ title = "Between- Index of Moderated Mediation"
               /clabels = "Est" "MCLL" "MCUL" /rlabels = !modY "modY"  /format = F8.4.
         compute modmedB = 1.
      end if.
      print /title = "***********************************************************************".
      print /title = "************************  INDIRECT EFFECT(S)  *************************".
      do if (modmedW = 1 AND modmedB = 1).
         print !modYcent /title = "NOTE: First Indirect Effects are Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modY "modY" /format = F8.4.
      else if (modmedW = 1 AND modmedB = 0).
         print !modYcent /title = "NOTE: First Within- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modY "modY" /format = F8.4.
      else if (modmedW = 0 AND modmedB = 1).
         print !modYcent /title = "NOTE: First Within- Indirect Effect is Conditional on a Moderator Value of:"
               /clabels = "value" /rlabels = !modY "modY" /format = F8.4.
      end if.
end if.

do if (!modM_1 = 0 AND !modY_1 = 0).
   print /title = "************************  INDIRECT EFFECT(S)  *************************".
end if.

compute cont_tab = {0,0,0}.

do if (!wit_x_1 = 1).
   compute ab1wtmp = mc_ab1_w.
   compute ab1wtmp(GRADE(mc_ab1_w)) = mc_ab1_w.
   compute mc_ab1_w = ab1wtmp.
   compute ab1_wci = {mc_ab1_w(cilow,1), mc_ab1_w(cihigh,1)}.
end if.

do if (!bet_x_1 = 1 AND !bet_m1_1 = 1).
   compute ab1btmp = mc_ab1_b.
   compute ab1btmp(GRADE(mc_ab1_b)) = mc_ab1_b.
   compute mc_ab1_b = ab1btmp.
   compute ab1_bci = {mc_ab1_b(cilow,1), mc_ab1_b(cihigh,1)}.
end if. 

do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND !bet_m1_1 = 1).
   compute ab1ctmp = mc_ab1_c.
   compute ab1ctmp(GRADE(mc_ab1_c)) = mc_ab1_c.
   compute mc_ab1_c = ab1ctmp.
   compute ab1_cci = {mc_ab1_c(cilow,1), mc_ab1_c(cihigh,1)}.
   compute ab1_c = a1b1_b - a1b1_w.
   compute cont_tab = {cont_tab; ab1_c, ab1_cci}.
end if. 


do if (!m2_1 = 1).
   do if (!wit_x_1 = 1).
      compute ab12_w = a2b2_w - a1b1_w.
      compute ab2wtmp = mc_ab2_w.
      compute ab2wtmp(GRADE(mc_ab2_w)) = mc_ab2_w.
      compute mc_ab2_w = ab2wtmp.
      compute ab2_wci = {mc_ab2_w(cilow,1), mc_ab2_w(cihigh,1)}.
      compute ab12wtmp = ab_12_w.
      compute ab12wtmp(GRADE(ab_12_w)) = ab_12_w.
      compute ab_12_w = ab12wtmp.
      compute ab12_wci = {ab_12_w(cilow,1), ab_12_w(cihigh,1)}.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m2_1 = 1).
      compute ab2btmp = mc_ab2_b.
      compute ab2btmp(GRADE(mc_ab2_b)) = mc_ab2_b.
      compute mc_ab2_b = ab2btmp.
      compute ab2_bci = {mc_ab2_b(cilow,1), mc_ab2_b(cihigh,1)}.
      do if (!bet_m1_1 = 1).
         compute ab12_b = a2b2_b - a1b1_b. 
         compute ab12btmp = ab_12_b.
         compute ab12btmp(GRADE(ab_12_b)) = ab_12_b.
         compute ab_12_b = ab12btmp.
         compute ab12_bci = {ab_12_b(cilow,1), ab_12_b(cihigh,1)}.
      end if.
   end if.
   do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND !bet_m2_1 = 1).
      compute ab2ctmp = mc_ab2_c.
      compute ab2ctmp(GRADE(mc_ab2_c)) = mc_ab2_c.
      compute mc_ab2_c = ab2ctmp.
      compute ab2_cci = {mc_ab2_c(cilow,1), mc_ab2_c(cihigh,1)}.
      compute ab2_c = a2b2_b - a2b2_w.
      compute cont_tab = {cont_tab; ab2_c, ab2_cci}.
   end if. 
end if. 
 

do if (!m3_1 = 1).
   do if (!wit_x_1 = 1).
      compute ab13_w = a3b3_w - a1b1_w.
      compute ab23_w = a3b3_w - a2b2_w.
      compute ab3wtmp = mc_ab3_w.
      compute ab3wtmp(GRADE(mc_ab3_w)) = mc_ab3_w.
      compute mc_ab3_w = ab3wtmp.
      compute ab3_wci = {mc_ab3_w(cilow,1), mc_ab3_w(cihigh,1)}.

      compute ab13wtmp = ab_13_w.
      compute ab13wtmp(GRADE(ab_13_w)) = ab_13_w.
      compute ab_13_w = ab13wtmp.
      compute ab13_wci = {ab_13_w(cilow,1), ab_13_w(cihigh,1)}.

      compute ab23wtmp = ab_23_w.
      compute ab23wtmp(GRADE(ab_23_w)) = ab_23_w.
      compute ab_23_w = ab23wtmp.
      compute ab23_wci = {ab_23_w(cilow,1), ab_23_w(cihigh,1)}.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m3_1 = 1).
      compute ab3btmp = mc_ab3_b.
      compute ab3btmp(GRADE(mc_ab3_b)) = mc_ab3_b.
      compute mc_ab3_b = ab3btmp.
      compute ab3_bci = {mc_ab3_b(cilow,1), mc_ab3_b(cihigh,1)}.
      do if (!bet_m1_1 = 1).
         compute ab13_b = a3b3_b - a1b1_b.
         compute ab13btmp = ab_13_b.
         compute ab13btmp(GRADE(ab_13_b)) = ab_13_b.
         compute ab_13_b = ab13btmp.
         compute ab13_bci = {ab_13_b(cilow,1), ab_13_b(cihigh,1)}.
      end if.
      do if (!bet_m2_1 = 1).
         compute ab23_b = a3b3_b - a2b2_b.
         compute ab23btmp = ab_23_b.
         compute ab23btmp(GRADE(ab_23_b)) = ab_23_b.
         compute ab_23_b = ab23btmp.
         compute ab23_bci = {ab_23_b(cilow,1), ab_23_b(cihigh,1)}.
      end if.
   end if.
   do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND !bet_m3_1 = 1).
      compute ab3ctmp = mc_ab3_c.
      compute ab3ctmp(GRADE(mc_ab3_c)) = mc_ab3_c.
      compute mc_ab3_c = ab3ctmp.
      compute ab3_cci = {mc_ab3_c(cilow,1), mc_ab3_c(cihigh,1)}.
      compute ab3_c = a3b3_b - a3b3_w.
      compute cont_tab = {cont_tab; ab3_c, ab3_cci}.
   end if. 
end if. 

compute tab3 = {0,0,0,0,0,0}.
compute tab5 = {0,0,0}.

do if (!wit_x_1 = 1).
   compute tab1 = {a1b1_w, v_a1b1_w}.
   compute tab2 = {a1b1_w, se_a1b1w, a1b1_w_z, a1b1_w_p, ab1_wci}.
end if.
do if (!bet_x_1 = 1 AND !bet_m1_1 = 1).
   compute tab3_1 = {a1b1_b, se_a1b1b, a1b1_b_z, a1b1_b_p, ab1_bci}.
   compute tab3 = {tab3;tab3_1}.
end if.

do if (!m2_1 = 1).
   do if (!wit_x_1 = 1).
      compute tab1_2 =  {a2b2_w, v_a2b2_w}.
      compute tab2_2 = {a2b2_w, se_a2b2w, a2b2_w_z, a2b2_w_p, ab2_wci}.
      compute tab1 = {tab1; tab1_2}.
      compute tab2 = {tab2; tab2_2}.
      compute tab4 = {ab12_w, ab12_wci}.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m2_1 = 1).
      compute tab3_2 = {a2b2_b, se_a2b2b, a2b2_b_z, a2b2_b_p, ab2_bci}.
      compute tab3 = {tab3; tab3_2}.
      do if (!bet_m1_1 = 1).
         compute tab5 = {tab5; ab12_b, ab12_bci}.
      end if.
   end if.
end if.

do if (!m3_1 = 1).
   do if (!wit_x_1 = 1).
      compute tab1_2 =  {a3b3_w, v_a3b3_w}.
      compute tab2_2 = {a3b3_w, se_a3b3w, a3b3_w_z, a3b3_w_p, ab3_wci}.
      compute tab4_2 = {ab13_w, ab13_wci; ab23_w, ab23_wci}.
      compute tab1 = {tab1; tab1_2}.
      compute tab2 = {tab2; tab2_2}.
      compute tab4 = {tab4; tab4_2}.
   end if.
   do if (!bet_x_1 = 1 AND !bet_m3_1 = 1).
      compute tab3_2 = {a3b3_b, se_a3b3b, a3b3_b_z, a3b3_b_p, ab3_bci}.
      compute tab3 = {tab3; tab3_2}.
      do if (!bet_m1_1 = 1).
         compute tab5_2 = {ab13_b, ab13_bci}.
         compute tab5 = {tab5; tab5_2}.
      end if.
      do if (!bet_m2_1 = 1).
         compute tab5_3 = { ab23_b, ab23_bci}.
         compute tab5 = {tab5; tab5_3}.
      end if.
   end if.
end if.

do if (!wit_x_1 = 1).
   compute tab1 = {tab1, sqrt(tab1(:,2))}.
   compute numvary = (csum(tab1(:,1) NE 0)).
   compute co1_0 = 0.
   compute co2_0 = 0.
   compute co3_0 = 0.
end if.

do if (!wit_x_1 = 1).
   print tab1/ title = "Within- Indirect Effect(s)" /clabels = "E(ab)" "Var(ab)" "SD(ab)"/rlabels = !m1 !m2 !m3 /format =F8.4.
   do if (cov_12 NE 0 OR cov_13 NE 0 OR cov_23 NE 0).
      compute ind_cov = {v_a1b1_w, cov_12; cov_12, v_a2b2_w}. 
      do if (!m3_1 = 1).
         compute newcol = {cov_13; cov_23}.
         compute newrow = {cov_13, cov_23, v_a3b3_w}.
         compute ind_cov = {ind_cov, newcol}.
         compute ind_cov = {ind_cov; newrow}.
      end if. 
      print ind_cov /title = "Within- Indirect Effect Covariance Matrix" /clabels = !m1 !m2 !m3 "-" /rlables = !m1 !m2 !m3 "-" /format = F8.4.    
      
      do if (ind_cov(1,1) = 0).
         compute co1_0 = 1.
         compute ind_cov(1,1) = .001. 
      end if.
      do if (ind_cov(2,2) = 0).
         compute co2_0 = 1.
         compute ind_cov(2,2) = .001.
      end if.
      do if (nrow(ind_cov) > 2 AND ind_cov(3,3) = 0).
         compute co3_0 = 1.
         compute ind_cov(3,3) = .001.
      end if. 
      compute sds = sqrt(mdiag(diag(ind_cov))).
      compute sds_inv = inv(sds).
      compute cor_mat = sds_inv*ind_cov*sds_inv.
      print cor_mat /title = "Within- Indirect Effect Correlation Matrix" /clabels = !m1 !m2 !m3 "-" /rlables = !m1 !m2 !m3 "-" /format = F8.4. 

   end if.
   print tab2/ title = "Within- Indirect Effect(s)" /clabels "Effect" "SE" "Z" "p" "MCLL" "MCUL" /rlabels = !m1 !m2 !m3 /format = F8.4.
else.
   print /title = "Note: No Within- Indirect Effect(s) Specified.".
end if.

do if (!bet_x_1 = 1 AND nrow(tab3) > 1).
   print tab3(2:nrow(tab3),:)/ title = "Between- Indirect Effect(s)" /clabels "Effect" "SE" "Z" "p" "MCLL" "MCUL" /rlabels = !mb_list /format = F8.4.
else.
   print /title = "Note: No Between- Indirect Effect(s) Specified.".
end if.
do if (!m2_1 = 1).
   print /title = "***********************************************************************".
   print tab4/ title = "Within- Indirect Effect Contrasts" /clabels "Dif" "MCLL" "MCUL" /rlabels = "ab2-ab1" "ab3-ab1" "ab3-ab2" /format = F8.4.
   do if (!bet_x_1 = 1 AND nrow(tab5) > 1).
      print tab5(2:nrow(tab5),:)/ title = "Between- Indirect Effect Contrasts" /clabels "Dif" "MCLL" "MCUL" /rlabels = "ab2-ab1" "ab3-ab1" "ab3-ab2" /format = F8.4.
   end if.
end if.

do if (!bet_x_1 = 1 AND !wit_x_1 = 1 AND nrow(cont_tab) > 1).
   print cont_tab(2:nrow(cont_tab),:)/ title = "Test of Indirect Contextual Effect(s): Between - Within" /clabels "Dif" "MCLL" "MCUL" /rlabels = !mb_list /format = F8.4.
end if.


**** End Output Not Shown if Error

else.
   print /title = "Note: Indirect Effects Not Calculated Due to Error(s) in Estimated Fixed or Random Effects.".
end if.

END MATRIX.

OUTPUT MODIFY
  /REPORT PRINTREPORT=NO
  /SELECT TEXTS 
  /IF COMMANDS=["Matrix(LAST)"] LABELS=[EXACT("Active Dataset")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES
  /SELECT HEADINGS 
  /IF COMMANDS=["Matrix(LAST)"] LABELS=[EXACT("Title")] INSTANCES=[1]
  /DELETEOBJECT DELETE=YES.


set printback = on.
!ENDDEFINE.
restore.

