FUNCTION READ_FITSWHOLE,datafile,compress=compress
; Written Nov. 7, 2013 by Ed Shaya / U. of Maryland 
execString = ''
OPENR, lunit, datafile, /get_lun, error = error,/swap_if_little,compress=compress

; Read primary header/data
primary = mrdfits(lunit,0,hdr_0,/dscale,status=status,silent=2)
IF (N_ELEMENTS(primary EQ 1)) THEN $
	execString +=  ', header : hdr_0 ' $
ELSE $
	execString +=  ', header : hdr_0, data : primary '

; The header will either give number of extensions or it will just set extend=1
; If it only has extend=1, then we read in a maximum of 100 extensions here.
nextend = fxpar(hdr_0,'NEXTEND',count=matches)
if (nextend EQ 0) then begin
	extend = fxpar(hdr_0,'EXTEND')
	if (extend eq 1) then nextend = 100
endif
IF (nextend NE 0) THEN BEGIN
  FOR j = 1, nextend DO BEGIN
  	data = mrdfits(lunit,0,hdr,/dscale,status=status,/silent)
	IF (status EQ -2) THEN BREAK
	extname = fxpar(hdr,'EXTNAME',count=matches)
	extname=strtrim(extname,2)
	IF (matches NE 0) THEN BEGIN
        	; Create a structure for extensions using extname
		extname = IDL_VALIDNAME(extname, /CONVERT_ALL)
        	extname = STRTRIM(extname,2) + '_' + STRTRIM(STRING(j),2)
		Result = EXECUTE(extname+' = {header : hdr, data : data}')
		; Prepare string for creating data_struct
		execString +=  ',' + extname + ' : ' + extname
	ENDIF ELSE BEGIN
	  IF (j EQ 0) THEN BEGIN
	  ENDIF ELSE BEGIN
		execString +=  ', header : hdr, data : data '
	  ENDELSE
	ENDELSE
  ENDFOR
ENDIF
execString = STRMID(execstring,1)
Result = Execute('data_struc = { ' + execString + '}')
FREE_LUN, lunit
RETURN, data_struc
END ; FUNCTION READ_FITSWHOLE

