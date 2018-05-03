PRO prettyhashofhash,hash,unit=unit,level=level
;-----------------------------------------------------------------------------
;+
; NAME:
;	PRETTYHASHOFHASH
;
; PURPOSE:
;       Procedure to make pretty text printout of a hash of hashes or lists with
; 	increasing indents at each level of the hierarchy.  It prints out both
;	the keywords and the values or leaves of the data tree.
;       Mostly used now for seeing a hash produced when reading in XML with 
;	read_xml8.pro.
;
; CATEGORY:
;       Datafile handling; XML; General IDL HASH/LIST
;
; CALLING SEQUENCE:
;          
;	prettyhashofhash,hash,[unit=unit,level=level]
;
; INPUTS:
;       HASH - IDL version 8 or above hash.  The hash could contain IDL native 
;	values, hashes, IDL lists
;
; Keywords:
;	UNIT - If unit is set, it will print to file handled by UNIT.  
;		Otherwise, it prints to screen.
;       LEVEL - This is only used for recursion (ie internally)
;
; PACKAGE LOCATION:
;         http://www.astro.umd.edu/~eshaya/PDS/pds4readxml.tar
;
;
; IDL VERSIONS TESTED: 8.2
;
; MODIFICATION HISTORY:
;       Written Nov. 7, 2013 by Ed Shaya / U. of Maryland 
;-
;------------------------------------------------------------------

; Default for unit is to print to screen
IF ~KEYWORD_SET(unit) THEN unit = -1
IF ~KEYWORD_SET(level) THEN level = 1                     
format = '(a'+STRTRIM(STRING(level*4-4),2)+',$)'
nextlevel = level + 1
FOREACH value, hash, key DO BEGIN
       IF ISA(key,/number) THEN skey = strtrim(string(key),2) ELSE skey = key
       PRINTF,unit,format=format,' '  ; Print indent ahead of time
       IF ISA(value,/scalar,/number)  OR ISA(value,/scalar,'String') THEN $
           PRINTF,unit,skey,': ',value $
       ELSE $
           IF ISA(value,/array,/number) THEN BEGIN
              more = ' ...'
              nshow =  n_elements(value)-1 < 2
              IF (n_elements(value) lt 3) then  more = ''
              PRINTF,unit,skey,': '+strjoin(string(value[0:nshow])+' ')+more 
	         ENDIF ELSE BEGIN
              PRINTF,unit,skey
           ENDELSE
           
       ; If value of hash is a hash, send it to next level    
       IF ISA(value,'HASH') THEN BEGIN
              prettyhashofhash,value,unit=unit,level=nextlevel
       ; If value of hash is a list, send each item to next level       
       ENDIF ELSE BEGIN
           IF ISA(value,'LIST') THEN $
                FOREACH inList, value DO prettyhashofhash,inList,$
                        unit=unit,level=nextlevel
       ENDELSE
ENDFOREACH
RETURN
END
