      **********************************************************************
      * Author:    Dave Nicolette
      * Date:      18 Jul 2014
      * Purpose:   Example of bad design.
      *
      * Usage:     convert input-filename output-filename
      **********************************************************************
       identification division.
       program-id. convert-bad.
       environment division.
       input-output section.
       file-control.

       select input-file assign to input-filename
           organization is line sequential.

       select output-file assign to output-filename
           organization is line sequential.

       data division.
       file section.

       fd input-file.
       01 input-record pic x(200).

       fd output-file.
       01 output-record.
          copy output.

       working-storage section.

       01  args pic x(120).

       01  arg-values.
           05  input-filename   pic x(40).
           05  output-filename  pic x(40).
 
       01  eof			pic x value spaces.
           88  end-of-file		      value "y".
 
       01  input-values.
           05  text-value-1     pic x(12).
           05  state-code-in    pic x(02).
           05  text-value-2     pic x(24).
           05  decimal-value-1  pic 9(3)V9(4). 

       01  to-upper-case        pic x(30).

       01  state-values.
           05  filler           pic x(32) value "AKAlaska".
           05  filler           pic x(32) value "ARArkansas".
           05  filler           pic x(32) value "AZArizona".

       01  state-table redefines state-values.
           05  state-table-data occurs 3 times 
                          ascending key state-table-code
                          indexed by state-index.
               10  state-table-code   pic x(02).
               10  state-table-name   pic x(30).

       procedure division.

           accept args from command-line end-accept
           unstring args delimited by space
               into input-filename output-filename
           end-unstring

           if  input-filename = spaces or output-filename = spaces
               display 'Usage: convert input-filename output-filename'
               goback
           end-if

           open output output-file
           open input input-file
           perform until end-of-file
               read input-file
                   at end
                       set end-of-file to true
                   not at end
                       move spaces to output-record
                       unstring input-record delimited by ','
                           into text-value-1
                               state-code-in
                               text-value-2
                               decimal-value-1
                       end-unstring
                       if  text-value-1 = low-values
                           move spaces to text-out-1
                       else
                           move text-value-1 to to-upper-case
                           call "C$TOUPPER" 
                               using to-upper-case
                               by value 
                               length to-upper-case
                           end-call
                           move to-upper-case to text-out-1
                       end-if    
                       move state-code-in to to-upper-case
                       call "C$TOUPPER" 
                           using to-upper-case
                           by value 
                           length to-upper-case
                       end-call
                       move to-upper-case to state-code-in
                       search all state-table-data
                           at end
                               move spaces to state-name-out
                           when state-table-code (state-index) = state-code-in
                               move state-table-name (state-index) to state-name-out
                       end-search
                       if  text-value-2 = low-values
                           move spaces to text-value-2
                       else    
                           call "C$TOUPPER" 
                               using text-value-2
                               by value 
                               length 1
                           end-call

                           call "C$JUSTIFY" 
                               using text-value-2
                               "C"
                           end-call
                       end-if    
                       move text-value-2 to text-out-2
                       move decimal-value-1 to decimal-out-1
                       write 
                           output-record from output-record
                       end-write
               end-read
           end-perform
           close output-file
           close input-file
           .
