;------------------------------------------------
;+
; NAME:
;	PRETTYXML
;
; PURPOSE:
;	Print contents of an XML file with indentation indicating depth of node
;Category:
;      Datafile Handling: XML
;
; CALLING SEQUENCE:
;	prettyxml,filename
; INPUTS:
;	XML filename
; Outputs:
;	None
; Keywords:
;	None
; Procedures Used:
;	IDL built-in Document DOM handling routines
; Modification History
;	Written by Ed Shaya / U. of Maryland [Feb 5, 2014]
;
;-
;--------------------------------
 
PRO prettyXML_recurse, oNode, indent
   ; print node and its name and value 
   ; Trim node value
   value = STRTRIM(oNode->GetNodeValue(),2)
   ; Remove Carriage return at the beginning of value
   IF  (STREGEX(value,string(10b)) EQ 0) THEN value = strmid(value,1)
   ; Remove Carriage return at the end of value
   IF  (STREGEX(value,string(10b)+'$') NE -1) THEN $
	   value = strmid(value,1,/reverse_offset)
   ; If textnode is blank, do not print it
   noprint = 0
   IF (oNode->GetNodeName() EQ "#text")  THEN BEGIN
	   vlen = strlen(value)
	   IF vlen EQ 0 THEN noprint = 1
	   IF (vlen EQ 1 and stregex(value,string(10b)) EQ 0) THEN noprint = 1
   ENDIF

   ; Print nodename and value with progressive indentation
   IF ~noprint THEN PRINT, indent,  oNode->GetNodeName(), ': ',  value

   ; Gett Attribute nodemap
   oAttMap = oNode->GetAttributes()
   IF obj_valid(oAttMap) THEN nAtt = oAttMap->GetLength() else nAtt = 0
   IF (nAtt GT 0) THEN BEGIN
      	FOR i = 0, nAtt-1 DO BEGIN
	     oAtt = oAttMap->Item(i)
   	     PRINT, indent+'  ', "#att: ",oAtt->GetName(), ': ', oAtt->GetValue() 
	ENDFOR
   ENDIF
 
   ; Now work on first child
   oSibling = oNode->GetFirstChild() 
   WHILE OBJ_VALID(oSibling) DO BEGIN 
      prettyXML_recurse, oSibling, indent+'  '
      ; Now work on next child
      oSibling = oSibling->GetNextSibling() 
   ENDWHILE 
END 
 
PRO prettyXML, filename 
   oDoc = OBJ_NEW('IDLffXMLDOMDocument') 
   oDoc->Load,/exclude_ignorable_whitespace, FILENAME= filename
   prettyXML_RECURSE, oDoc, ' ' 
   OBJ_DESTROY, oDoc 
END
