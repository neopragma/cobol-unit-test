      **********************************************************************
      * Author:    Dave Nicolette
      * Date:      07 Aug 2014
      * Purpose:   Sample program that works with dates.
      *
      * Usage:     invdate
      **********************************************************************
       identification division.
       program-id. invdate.
       environment division.
       data division.
       working-storage section.
           copy datetime.
       01  ws-next-invoice-date pic x(8).   
       01  ws-quotient          pic s9(4) comp.
       01  ws-remainder         pic s9(4) comp. 
       procedure division.

       0000-main.

           perform 0500-initialize
           perform 1000-process-invoices
           goback
           .

       0500-initialize.
           .

       1000-process-invoices.
           perform 2000-next-invoice-date
           .

       2000-next-invoice-date.  
           evaluate true
               when february 
                    perform 2100-handle-february
               when 30-day-month
                    move 30 to ws-current-day
               when other 
                    move 31 to ws-current-day
           end-evaluate              
           move ws-current-date to ws-next-invoice-date
           .

       2100-handle-february.
           divide 4 into ws-current-year
               giving ws-quotient
               remainder ws-remainder
           end-divide
           if  ws-remainder equal zero
               move 29 to ws-current-day
           else
               move 28 to ws-current-day
           end-if            
           .

       9999-end.
           .     
