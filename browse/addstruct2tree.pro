<<<<<<< HEAD
PRO addstruct2tree,metadata,tag,fulltag,parent,metaidhash
; Add a structure (as a folder) to a widget tree
; Needs to recurse to go down the tree
; Written Nov. 7, 2013 by Ed Shaya / U. of Maryland 
ttag = idl_validname("t_"+tag,/convert_all)
Result = EXECUTE(ttag +" = widget_tree(parent,value='"+tag+"',uvalue='metadatatree',/folder)")
Result = Execute('t_tag  = '+ttag)
metaidhash += hash(fulltag,t_tag)

; Now get ready to recurse down this branch
result = execute('tags = tag_names('+fulltag+')')
result = execute('parentc='+ttag)
FOR i = 0, N_ELEMENTS(tags)-1 DO BEGIN
	tag = tags[i]
	fulltagc = fulltag+'.'+tag
	result = EXECUTE('type = size('+fulltagc+',/tname)')
	result = EXECUTE('dim = size('+fulltagc+',/dimension)')
	dim = dim > 1
	FOR j = 0,dim[0]-1 DO BEGIN
		tagp = tag
		IF (dim GT 1) then tagp = tag+'['+strtrim(string(j),1)+']'
		fulltagcp = fulltagc+'['+strtrim(string(j),1)+']'
		IF (type EQ 'STRUCT') THEN BEGIN  ; Branches
			addstruct2tree,metadata,tagp,fulltagcp,parentc,metaidhash
		ENDIF ELSE BEGIN
			result=EXECUTE('tagvalue = '+fulltagcp)
			addleaf2tree,metadata,tag,tagvalue,parentc
		ENDELSE
	ENDFOR
ENDFOR
return
END ; end addstruct2tree
=======
PRO addstruct2tree,data,tag,fulltag,parent,ahash,meta=meta
; Add a structure (as a folder) to a widget tree
; Needs to recurse to go down the tree
; Written Nov. 7, 2013 by Ed Shaya / U. of Maryland 
if ~keyword_set(meta) then meta = 0
if meta eq 1 then begin
  uvalue ='metadatatree' 
  pre= 'META'
endif else begin
  uvalue = 'datatree'
  pre=''
endelse

; Create a folder in the widget tree
ttag = widget_tree(parent,value=tag,uvalue=uvalue,/folder)
; Keep track of where this element is in the data/metadata
ahash[pre+fulltag] = ttag
; This is now parent for next folder
parentc=ttag

; Now get ready to recurse down this branch
; Get all child elements
result = EXECUTE('tags = tag_names('+fulltag+')')

FOREACH tag, tags DO BEGIN
	fulltagc = fulltag+'.'+tag
	result = EXECUTE('val = '+fulltagc)
	type = SIZE(val,/tname)
	dims = SIZE(val,/dimension)
	dim = dims[0]
	dim = dim > 1L
	
	; If structure then recurse, otherwise handle it below
  IF (type EQ 'STRUCT') THEN BEGIN  ; Branches
       ; Loop over this STRUCT if it is an array of them
       ; otherwise dim = 1 and it is just done once.
	     FOR j = 0, dim - 1 DO BEGIN
		     IF (dim GT 1) then tagp = tag+'['+STRTRIM(STRING(j),1)+']' else tagp = tag
		     fulltagcp = fulltagc+'['+STRTRIM(STRING(j),1)+']'		
			   addstruct2tree,data,tagp,fulltagcp,parentc,ahash,meta=meta
			ENDFOR
	ENDIF ELSE BEGIN
	    ; We have attributes, text nodes, arrays, and headers
			IF (tag NE '_TEXT') THEN BEGIN
			    ttag = widget_tree(parentc,value=tag,uvalue=uvalue,/folder) 
			    ahash[pre+fulltagc] = ttag
			    parentd=ttag
		  endif else parentd = parentc
		  if (dim eq 1) then begin
			    if type ne 'STRING' then val=strtrim(val,2)
			    if val ne '' then $
	            ttag = widget_tree(parentd,value=val,uvalue='text') 
      ENDIF
		ENDELSE
ENDFOREACH ;tag
return
END ; end addstruct2tree
>>>>>>> 83a289cf8732b60848bbc032838a77c496098c77
