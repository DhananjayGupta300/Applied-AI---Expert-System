;---------------------------------------------
; GLOBAL VARIABLES USED BY THE RULES
;---------------------------------------------

(defglobal ?*upper_limit* = 100000)

(defglobal ?*investment_limit* = 40000)


(deftemplate Business_loan_applicant
    (slot applicant_name)

    
    ; Applicant's age in number(Integer VALUE only)
    (slot age (type INTEGER))
    
    
    ; Amount currently owed by applicant in Dollars(Integer VALUE only)
    (slot current_owe (type INTEGER))
    
    
    ; Any pending payment of previosuly sanctioned loans?
    (slot paid (allowed-values Yes No))
    
    
    ; Applicant's Years of experience in related business 
    (slot exp (type FlOAT))
    
    
    ; Total number of businesses owned by applicant
    (slot bus (type INTEGER))
    
    
    ; Years of education of an applicant
    (slot edu (type INTEGER))
    
    
    ; Total amount invested so far by the applicant in the business
    (slot inv (type INTEGER))
    
    
    ; Ability to repay loan
    (slot repay (allowed-values Good Poor))
    
    
    ; Management Ability of an applicant
    (slot mgmt (allowed-values Good Average Poor))
    
    
    ; Credit History of an applicant
    (slot credit (allowed-values Good Poor))       
    )


;---------------------------------------------
; RULE 1:  WELCOME TO THE EXPERT SYSTEM
;---------------------------------------------

(defrule LoanSanction
    (declare (salience 100))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t crlf)
    (printout t "----------------------------------------------------------" crlf)
    (printout t "WELCOME TO THE EXPERT SYSTEM FOR BUSINESS LOAN APPLICATION" crlf)
    (printout t "Assert the initial values of an applicant and the decision will be made" crlf)
    (printout t "-----------------------------------------------------------------------" crlf)
    (printout t crlf)    
)
    


;-----------------------------------------------------------
; RULE 2:  PRINTING THE USER's INPUT(asserted by the grader)
;-----------------------------------------------------------    
 (defrule initial
    (declare (salience 99))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t "BUSINESS loan application for "?Business_loan_applicant.applicant_name " under review." crlf)
    (printout t "Following is the Applicant's information that was feeded into our system" crlf)
    (printout t "Applicants legal age: " ?Business_loan_applicant.age " years" crlf)
    (printout t "Amount currently owed by applicant is: " ?Business_loan_applicant.current_owe " Dollars" crlf)
    (printout t "Any pending payment of previosuly sanctioned loans?(Yes/No): " ?Business_loan_applicant.paid crlf)
    (printout t "Applicant's Years of experience in related business: " ?Business_loan_applicant.exp crlf)
    (printout t "Total number of businesses owned by applicant: " ?Business_loan_applicant.bus crlf)
    (printout t "Years of education: " ?Business_loan_applicant.edu "years" crlf)
    (printout t "Total amount invested so far by the applicant in the business is: " ?Business_loan_applicant.inv " Dollars" crlf)
    (printout t "Does Applicant have the ability to repay the loan?(Good/Poor): " ?Business_loan_applicant.repay crlf)
    (printout t "Applicant's ability to manage the loan?(Good/Average/Poor): " ?Business_loan_applicant.mgmt crlf)
    (printout t "Credit Rating?(Good/Poor): " ?Business_loan_applicant.credit crlf)
  	)


    
;----------------------------------------------------------------------------------------
; RULE 3:  SUGGESTIONS PROVIDED BY THE SYSTEM TO THE BANK ON THE BASIS OF APPLICANTS INFO
;----------------------------------------------------------------------------------------- 
(defrule LoanSanctionSuggestions
    (declare (salience 98))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (printout t crlf)
    (printout t "--------------------------------------------------------" crlf)
    (printout t "Following are the suggestions & interpretations derived: " crlf)
    (printout t "---------------------------------------------------------" crlf)
    (printout t crlf) 
    )



;---------------------------------------------------------------------------------------
; RULE 4:  TO CHECK WHETHER APPLICANT'S AGE SATISFIES THE BANK'S LOAN SACNTION CRITERIA
;---------------------------------------------------------------------------------------

(defrule ageCheck
    (declare (salience 97))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.age 18) then
        (printout t "Applicant is under the legal age set by the bank to apply for the loan." crlf)
        )
    )



;---------------------------------------------------------------------------------------------
; RULE 5:  TO CHECK WHETHER APPLCIANT'S CURRENTLY OWED AMOUNT IS HIGHER than THE BANK'S lIMIT
;--------------------------------------------------------------------------------------------

(defrule currentOwedAmountCheck
    (declare (salience 96))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (> ?Business_loan_applicant.current_owe ?*upper_limit*) then
        (printout t "Applicant owns more than $100000 as his/her debt" crlf)
        )
   
    )
    


;----------------------------------------------------------------------------------------------
; RULE 6:  IF THE APPLICANT HAS PREVIOUSLY BORROWED LOANS UNPAID, BANK REJECTS THE LOAN REQUEST
;----------------------------------------------------------------------------------------------  
 
(defrule borrowedLoansCheck
    (declare (salience 95))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.paid  Yes) then
        (printout t "Applicant haven't cleared the loan/s previously borrowed by him/her" crlf)
        )
    )
       


 
;------------------------------------------------------------------------------------
; RULE 7:  IF YEARS OF EXPERIENCE IS LESS THAN THE NEEDED YEARS,LOAN GETS REJETCED.
;------------------------------------------------------------------------------------  

(defrule experienceCheck
    (declare (salience 94))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.exp 7.0) then
        (printout t "Applicant have insufficient experience in the business. While this is not the only criteria but you will be qualified after " (- 7 ?Business_loan_applicant.exp)  " years"  crlf)
        ) 
    )
    


;-------------------------------------------------------------------------------------------------------
; RULE 8:  IF THE NUMBER OF BUSINESSES OWNED BY APPLICANT DOESNOT MATCH CRITERIA,CAN'T APPLY FOR THE LOAN
;--------------------------------------------------------------------------------------------------------

(defrule businessesOwnedCheck
    (declare (salience 93))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.bus 3) then
        (printout t "Applicant do not have enough businesses as an asset to apply for the loan" crlf)
        )
    )
      



;---------------------------------------------------------------------------------------------------
; RULE 9:  MINIMUM YEARS OF EDUCATION REQUIRED,IF NOT,APPLICANT IS NOT ELIGIBLE TO APPLY FOR THE LOAN
;----------------------------------------------------------------------------------------------------- 
 
(defrule educationCheck
    (declare (salience 92))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.edu 12) then
        (printout t "The required qualification and the education background(i.e 12 years) is not satisifed." crlf)
        )
    )
         



;-----------------------------------------------------------------------------------------------------
; RULE 10:  IF INVESTMENT MADE BY APPLICANT IS NOT MORE THAN A CERTAIN AMOUNT,CANNOT APPLY FOR THE LOAN
;-----------------------------------------------------------------------------------------------------

(defrule investmentCheck
    (declare (salience 91))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (< ?Business_loan_applicant.inv ?*investment_limit*) then
        (printout t "Applicant have NOT made ENOUGH investment in the business" crlf)
        )
    )
      


;----------------------------------------------------------------------------------------------------------------------------------------------
; RULE 11:  IF APPLICANT HAS A POOR ABILITY TO REPAY THE LOAN,LOAN APLICATION REJECTED.
;-----------------------------------------------------------------------------------------------------------------------------------------------

(defrule repayAbilityCheck
    (declare (salience 90))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.repay Poor) then
        (printout t "High risk of no returns since his/her ability to repay the loan back is poor" crlf)
        )
    )
    


;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 12:  IF APPLICANT HAS A POOR MANAGEMENT ABILITY,LOAN APPLICATION REJECTED.
;----------------------------------------------------------------------------------------------------------------------------------------- 

(defrule managementAbilityCheck
    (declare (salience 89))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.mgmt Poor) then
        (printout t "Applicant has poor management ability" crlf)
        )
    )
    


;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 13:  IF CREDIT SCORE IS POOR,LOAN APPLICATION REJECTED.
;----------------------------------------------------------------------------------------------------------------------------------------- 

(defrule creditScoreCheck
    (declare (salience 88))
    ?Business_loan_applicant <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
    (if (= ?Business_loan_applicant.credit Poor) then
        (printout t "Applicant has a low credit score which is considered to be a negative flag for a loan application" crlf)
        )
    )



;----------------------------------------------------------------------------------------------------------------------------------------
; RULE 14:  FINAL DECISION (ELIGIBLE TO APPLY FOR A LOAN OR NOT)
;----------------------------------------------------------------------------------------------------------------------------------------- 
(defrule finalDecision_Impparameters
    (declare (salience 87))
    ?x <- (Business_loan_applicant (applicant_name ?applicant_name))
    =>
 (if (or (< ?x.age 18) (= ?x.paid no) (> ?x.current_owe ?*upper_limit*) (= ?x.credit Poor)) then
        (printout t " " crlf)
		(printout t "--The Final Decision Suggested by the Expert System is :--" crlf)
		(printout t " " crlf)
        (printout t "Applicant is not eligible to apply for the Business loan since one or all of the top 4 parameter/s(Legal age, Amount owed, previous loans paid and credit score) are not satisfied" crlf)
  else
        (printout t " " crlf)
		(printout t "--The Final Decision Suggested by the Expert System is that:--" crlf)
		(printout t " " crlf)
        (printout t "Applicant is eligible to apply for the Business loan since one or all of the top 4 parameter/s((Legal age, Amount owed, previous loans paid and credit score) ) are satisfied" crlf)
        )
    )

 
 
      
;---------------------------------------------
; TO GET THE USER's INPUT
;---------------------------------------------

(assert (Business_loan_applicant (applicant_name "Shaktiman_2")
            (age 23) (current_owe 5) (paid Yes)
            (exp 7)  (bus 1)  (edu 5)  (inv 300000)
            (repay Good)  (mgmt Poor)  (credit Good)))
(run)                  