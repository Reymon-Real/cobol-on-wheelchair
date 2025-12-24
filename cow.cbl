      **************************************
      *** Author:  Azac                  ***
      *** License: MIT                   ***
      *** Date:    DECEMBER 21 FROM 2013 ***
      *** UPDATE:  AGUST 11 FROM 2022    ***
      **************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. cow.
       
       AUTHOR. AZAC.
       PROGRAM-WRITTEN.  DECEMBER 21 FROM 2013.
       PROGRAM-COMPILED. AGUST 11 FROM 2022.
      
      *********************************************
      *** Division where variables are declared ***
      ***              and                      ***
      ***        files are described            ***
      *********************************************
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.

       77 newline        PIC X VALUE X'0a'.
       77 analyzed-query PIC X(1600).

       01 the-great-dispatch.
          03  nroutes       PIC 99 USAGE COMP-5.
          03  routing-table OCCURS 10 TIMES.
            05   routing-pattern PIC X(999).
            05   routing-destiny PIC X(999).

       77 tester   PIC X VALUE "n".  
       77 anyfound PIC X VALUE "n".
       77 ctr      PIC 9(2) USAGE COMP-5.

       01 the-values.
          05 query-values OCCURS 10 TIMES.
            10 query-value-name PIC X(90).
            10 query-value      PIC X(90).

      *********************
      *** Program Logic ***
      *********************

       PROCEDURE DIVISION.
      
      ********************************
      *** Initialize of main logic ***
      ********************************
           COPY "config.cbl".
      
      ******************************************
      *** Procedure for print the web header ***
      ******************************************
           PERFORM web-header.
     
      *************************************
      *** Call to the function getquery ***
      *************************************
           CALL "getquery" USING analyzed-query.
      
      **********************************
      *** Controller loop for routes ***
      **********************************
           PERFORM VARYING ctr FROM 1 BY 1 UNTIL ctr > nroutes
      
      *** Verify if the query exist ***
               CALL 'checkquery'
               USING analyzed-query routing-pattern(ctr) tester
               the-values
      
      *** End varying perform ***
           END-PERFORM
      
      *** Conditional for to know if the testes equal true ***
           IF (tester="y")
      
      *** Display routing-pattern(ctr) "<hr>" ***
               MOVE "y" TO anyfound

      *** Display "ctr:" ctr ***
               CALL routing-destiny(ctr) USING the-values.

      *** End Tester IF ***
           END-IF
      
      *************************************
      *** Check if nothing is available ***
      *************************************
           IF (anyfound="n")
               PERFORM bad-query-error
      *     ELSE
      *         CALL "showvars" USING the-values.
           END-IF

      ***********************
      *** Function Return ***
      ***********************
       goback.

      ********************************
      *** Handle erroneous queries ***
      ********************************
       bad-query-error.
           DISPLAY "<b>Cobol-on-Wheelchair error:</b>"
           "query pattern not found (<i>"
           FUNCTION TRIM(analyzed-query) "</i>)".
      
      ************************************
      *** Show which is the web header ***
      ************************************
       web-header.
           DISPLAY "content-type: text/html; charset=utf-8" newline.














       identification division.
       program-id. getquery.

       environment division.

              input-output section.
       file-control.
           select webinput assign to keyboard.


       data division.
       file section.
       fd webinput.
          01 postchunk       pic x(1024).

       working-storage section.

       78 name-count      value 34.
       01 name-index      pic 99 usage comp-5.
       01 value-string    pic x(256).
       01 environment-names.
          02 name-strings.
             03 filler    pic x(20) value 'AUTH_TYPE'.
             03 filler    pic x(20) value 'CONTENT_LENGTH'.
             03 filler    pic x(20) value 'CONTENT_TYPE'.
             03 filler    pic x(20) value 'DOCUMENT_ROOT'.
             03 filler    pic x(20) value 'GATEWAY_INTERFACE'.
             03 filler    pic x(20) value 'HTTP_ACCEPT'.
             03 filler    pic x(20) value 'HTTP_ACCEPT_CHARSET'.
             03 filler    pic x(20) value 'HTTP_ACCEPT_ENCODING'.
             03 filler    pic x(20) value 'HTTP_ACCEPT_LANGUAGE'.
             03 filler    pic x(20) value 'HTTP_COOKIE'.
             03 filler    pic x(20) value 'HTTP_CONNECTION'.
             03 filler    pic x(20) value 'HTTP_HOST'.
             03 filler    pic x(20) value 'HTTP_REFERER'.
             03 filler    pic x(20) value 'HTTP_USER_AGENT'.
             03 filler    pic x(20) value 'LIB_PATH'.
             03 filler    pic x(20) value 'PATH'.
             03 filler    pic x(20) value 'PATH_INFO'.
             03 filler    pic x(20) value 'PATH_TRANSLATED'.
             03 filler    pic x(20) value 'QUERY_STRING'.
             03 filler    pic x(20) value 'REMOTE_ADDR'.
             03 filler    pic x(20) value 'REMOTE_HOST'.
             03 filler    pic x(20) value 'REMOTE_IDENT'.
             03 filler    pic x(20) value 'REMOTE_PORT'.
             03 filler    pic x(20) value 'REQUEST_METHOD'.
             03 filler    pic x(20) value 'REQUEST_URI'.
             03 filler    pic x(20) value 'SCRIPT_FILENAME'.
             03 filler    pic x(20) value 'SCRIPT_NAME'.
             03 filler    pic x(20) value 'SERVER_ADDR'.
             03 filler    pic x(20) value 'SERVER_ADMIN'.
             03 filler    pic x(20) value 'SERVER_NAME'.
             03 filler    pic x(20) value 'SERVER_PORT'.
             03 filler    pic x(20) value 'SERVER_PROTOCOL'.
             03 filler    pic x(20) value 'SERVER_SIGNATURE'.
             03 filler    pic x(20) value 'SERVER_SOFTWARE'.
          02 filler redefines name-strings.
             03 name-string   pic x(20) occurs name-count times.



       linkage section.

       01 the-query pic x(1600).  

       procedure division using the-query.


         perform varying name-index from 1 by 1
             until name-index > name-count
                 accept value-string from environment
                     name-string(name-index)
                 end-accept

                 if (name-string(name-index) = "PATH_INFO")
                    
                    move value-string to the-query

                 end-if   

         end-perform.

      
       goback.

       end program getquery.













       identification division.
       program-id. showvars.

       data division.
       working-storage section.

       01 ctr      pic 99 usage comp-5.

       linkage section.

       01 the-values.

          05 query-values           occurs 10 times.
            10 query-value-name     pic x(90).
            10 query-value          pic x(90).


       procedure division using the-values.



           display "<table cellpadding=10 border=1 cellspacing=4 bgcolor=lightgray>"

           perform varying ctr from 1 by 1
             until ctr > 10

               display "<tr><td>" query-value-name(ctr) "</td><td>" query-value(ctr) "</td></tr>"

           end-perform


           display "</table>"
      
       goback.

       end program showvars.





       identification division.
       program-id. checkquery.

       data division.
       working-storage section.



       01 choppery.

          05 chopped-path-pieces occurs 99 times.                        
             10 chopped-path-piece pic x(80) value spaces. 
          05 chopped-pattern-pieces occurs 99 times.
             10 chopped-pattern-piece pic x(80) value spaces.                           
                                                                    
       01 counter       pic s9(04) comp. 
       01 positio       pic s9(04).                             
       01 tmp-pointer      pic s9(04) comp value +1. 
       01 tmp-pointer2      pic s9(04) comp value +1. 


       01 counter-of-values      pic s9(2).

       01 query-analysis.

          05 query-values           occurs 10 times.
            10 query-value-name     pic x(90).
            10 query-value          pic x(90).



       linkage section.
       01  the-query pic x(255).
       01  the-pattern pic x(255).
       01  result  pic x(1).

       01 query-analysis-out.

          05 query-values-out           occurs 10 times.
            10 query-value-name-out     pic x(90).
            10 query-value-out          pic x(90).



       procedure division using the-query the-pattern result query-analysis-out.


          move spaces to choppery.
          move "y" to result.
          move 0 to counter-of-values.



           move 1 to tmp-pointer.
           move 1 to tmp-pointer2.


           perform varying counter from 2 by 1 until counter > 99   

           subtract 1 from counter giving positio
           
                unstring the-query delimited by '/'          
                    into chopped-path-piece(positio)                    
                   with pointer tmp-pointer    

                unstring the-pattern delimited by '/'          
                    into chopped-pattern-piece(positio)                    
                   with pointer tmp-pointer2   

            end-perform.

            move 0 to counter.

            *> display "<h3>" the-query " vs " the-pattern "</h3>"


            perform varying counter from 1 by 1 until counter > 99 or result = "n"

                *> for filling only entered values
                *> if (chopped-path-piece(counter) equal spaces and counter>1)

                *>  display "break"
                *>  exit perform

               *> else

               *> display counter "::" result " (" chopped-path-piece(counter) "/" chopped-pattern-piece(counter) ")<P>"

                 if (chopped-pattern-piece(counter)(1:1) equal "%")

                    add 1 to counter-of-values
                    move chopped-pattern-piece(counter) to query-value-name(counter-of-values) 
                    move chopped-path-piece(counter) to query-value(counter-of-values) 

                  *>  display "got val " chopped-pattern-piece(counter) "<P>"

                 end-if    
              
                if 
                  (chopped-path-piece(counter) not equal chopped-pattern-piece(counter) 
                    and 
                    chopped-pattern-piece(counter)(1:1) not equal "%") 
                move "n" to result
               *> display "<P><b>fail at " counter "</b> (" chopped-path-piece(counter) " :: "  chopped-pattern-piece(counter) ")</p>"
                
                end-if

               *> end-if


            end-perform.

                if (result="y") 
                    move query-analysis to query-analysis-out
                end-if

      
       goback.

       end program checkquery.

       end program cow.


 
