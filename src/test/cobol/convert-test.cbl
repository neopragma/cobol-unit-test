      ******************************************************************
      * Author:    Dave Nicolette
      * Date:      18 Jul 2014
      * Purpose:   Functional test driver for convert.cbl.
      *            Reads sequential file with expected records and 
      *            sequential output file from 'convert' and compares
      *            them record by record.
      *
      * Usage:     convert-test expected-output-file actual-output-file
      ******************************************************************
       identification division.
       program-id. convert-test.
       environment division.
       input-output section.
       file-control.
  
           select expected-result-file 
               assign to expected-result-filename
               organization is line sequential.

           select actual-result-file 
               assign to actual-result-filename
               organization is line sequential.

       data division.
       file section.

       fd  expected-result-file.
       01  expected-result-record.
           copy output.

       fd  actual-result-file.
       01  actual-result-record.
           copy output.

       working-storage section.

       01  args pic x(500).

       01  arg-values.
           05  expected-result-filename pic x(120).
           05  actual-result-filename   pic x(120).
 
       01  eof-expected pic x value spaces.
           88  end-of-expected          value "y".
       01  eof-actual pic x value spaces.
           88  end-of-actual            value "y".
       01  test-status                  pic x(11) value "     PASS: ".
           88  test-pass                value "     PASS: ".
           88  test-fail                value "**** FAIL: ".
       01  expected-line-number         pic 9(6) value zero.
       01  actual-line-number           pic 9(6) value zero.
       01  status-message               pic x(100) value spaces.

       procedure division.

       0000-main.

           perform 0500-initialize
           perform 1000-compare-files
           goback
           .

       0500-initialize.

           accept args from command-line end-accept
           unstring args delimited by space
               into expected-result-filename actual-result-filename
           end-unstring

           if  expected-result-filename = spaces 
           or actual-result-filename = spaces
               display 'Usage: convert expected-result-filename' 
                   ' actual-result-filename'
               goback
           end-if
           .

       1000-compare-files.
           open input expected-result-file
           open input actual-result-file

           display "==================================================="
           display " Functional tests for convert.cbl"
           display " Expected output file: " expected-result-filename
           display " Actual output file:   " actual-result-filename

           perform 2000-compare-records
               until test-fail or end-of-expected or end-of-actual

           if  test-fail
               display test-status 
               function substitute
                   (status-message; "LINE"; expected-line-number)
           else
               display test-status "Actual result matches"
                   " expected result"    
           end-if

           display "==================================================="

           close expected-result-file
           close actual-result-file
           .

       2000-compare-records.

           if  not end-of-expected
               read expected-result-file
               at end
                   set end-of-expected to true
               not at end
                   add 1 to expected-line-number    
               end-read
           end-if

           if  not end-of-actual
               read actual-result-file
               at end
                   set end-of-actual to true
               not at end
                   add 1 to actual-line-number
               end-read
           end-if

           if  end-of-expected and not end-of-actual
               set test-fail to true
               move "More records than expected were written" 
                 to status-message
           end-if

           if  not end-of-expected and end-of-actual
               set test-fail to true
               move "Fewer records than expected were written" 
                 to status-message
           end-if

           if  expected-result-record not = actual-result-record
               set test-fail to true
               move "Files do not match starting at line LINE" 
                 to status-message
           end-if
           .
 