.model small ; declaraci?n del modelo de memoria
bufSize EQU 121
.stack
.const
        VAL_LF      EQU 10  ; Constante para el Line Feed.
        VAL_RET     EQU 13  ; Constante para el Return.
        CHR_FIN     EQU '$' ; Indica fin de cadena.
        index       EQU 0

.data 
        varPassword        DB "tallerac", CHR_FIN
        varMensajePassword     DB "Ingrese su varPassword: ", CHR_FIN
        
        signoResta      DB "-", CHR_FIN
        coma            DB ",", CHR_FIN
        msgpasinvalida  db VAL_LF, VAL_RET,"Password Invalida", VAL_LF, VAL_RET, CHR_FIN
        msgpasswordok   db VAL_LF, VAL_RET,"Password Correcta",VAL_LF, VAL_RET, CHR_FIN
        
        varMensajeEsperaLectura      db VAL_LF, VAL_RET,"Presione Cualquier tecla para continuar...",VAL_LF, VAL_RET, CHR_FIN
        varMenu               DB VAL_LF, VAL_RET,"|------------------------------------|", VAL_LF, VAL_RET;
        varOpcion0            DB "|--------------- MENU ---------------|", VAL_LF, VAL_RET;
        varOpcion1            DB "|  1.- Suma y resta                  |", VAL_LF, VAL_RET;
        varOpcion2            DB "|  2.- Multiplicacion y Division     |", VAL_LF, VAL_RET;
        varOpcion3            DB "|  3.- Operaciones con Hexadecimal   |", VAL_LF, VAL_RET;
        varOpcion4            DB "|  4.- Serie Fibonacci               |", VAL_LF, VAL_RET;
        varOpcion5            DB "|------------------------------------|", VAL_LF, VAL_RET;
        varOpcion6            DB "|  0.- Salir                         |", VAL_LF, VAL_RET;
        varOpcion7            DB "|------------------------------------|", VAL_LF, VAL_RET, CHR_FIN;
        
        varMensajeInvalido     DB VAL_LF, VAL_RET,"Datos ingresados fuera de rango", VAL_LF, VAL_RET, CHR_FIN;

        varLecturaOpcion             DB "Digite la opcion a realizar: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensaje1         DB "Ingrese primer numero entre 0 y 255: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensaje2         DB "Ingrese segundo numero entre 0 y 255: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensaje3         DB "Ingrese primer numero entre 0 y 65535: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensaje4         DB "Ingrese segundo numero entre 0 y 65535: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensajeHexadecimal         DB "Ingrese un numero Hexadecimal de 2 digitos: ", VAL_LF, VAL_RET, CHR_FIN;
        varMensajeOpcionInvalida   DB VAL_LF, VAL_RET,"Opcion no valida ", VAL_LF, VAL_RET, CHR_FIN;
        varMensajeResultadoSuma   DB VAL_LF, VAL_RET, "Resultado Suma: ",CHR_FIN
        varMensajeResultadoResta   DB VAL_LF, VAL_RET, "Resultado Resta: ",CHR_FIN
        varMensajeResultadoMultiplicacion        DB VAL_LF, VAL_RET, "Resultado Multiplicacion: ",CHR_FIN
        varMensajeResultadoDivision        DB VAL_LF, VAL_RET, "Resultado Division: ",CHR_FIN
        varMensajeErrorDivisionPor0        DB VAL_LF, VAL_RET, "No se puede dividir entre 0",CHR_FIN
        varMensajeBinario      DB " ", CHR_FIN;
        varMensajePrimerNumeroHexa        DB VAL_LF, VAL_RET,"Primer Hexadecimal en Binario: ", CHR_FIN;
        varMensajeSegundoNumeroHexa        DB "Segundo Hexadecimal en Binario:  ", CHR_FIN;
        varMensajePrimerNumeroHexaNegado        DB "Primer Hexadecimal Negado en Binario: ", CHR_FIN;
        varMensajeSegundoNumeroHexaNegado       DB "Segundo Hexadecimal Negado en Binario: ", CHR_FIN;
        varMensajeResultadoAND       DB "Resultado AND: ", CHR_FIN;
        varMensajeResultadoOR       DB "Resultado OR: ", CHR_FIN;
        varMensajeResultadoXOR       DB "Resultado XOR: ", CHR_FIN;
        varBinario         DB "00000000", VAL_LF, VAL_RET, CHR_FIN;
        varLetra DB "A", CHR_FIN;
        numero          Dw 0 
        varNumero1 db 6 dup(' '),'$'
        varNumero2 db 6 dup(' '),'$'
        varAuxiliar db  8 dup(' '),'$'
        num db 2 dup(' '),'$'
        varCont db 0
        varOpcion1 dw ?
        varOpcion2 dw ?
        varOpcion1C dw ?
        varOpcion2C dw ?
        varTemporal dw ?
        varResultadoCociente dw ?
        varResultadoResiduo dw ?
        varAuxiliarRes dw ?
        varResultado db 6 DUP(?)
        varResultadoNot db 6 DUP(?)
        varResultadoAND db 6 DUP(?)
        varResultadoOR db 6 DUP(?)
        varResultadoMultiplicicacion dB 1000 dup(' '),'$'
        varResultadoDivision dB 1000 dup(' '),'$'
        varResiduo db ?
        
        
        
.code

impStr PROC
    mov ah, 09h
    int 21h
    RET
ENDP
ingresarpasword PROC 
    mov dx, offset varMensajePassword
    call impStr
    mov di,offset varAuxiliar
    mov cx,8h
lecture:    
    ;leer un caracter
    mov ah,7h
    int 21h
    
    ;guardar el caracter leido
    stosb
    
    ;imprimir asterisco
    mov ah,6h   
    mov dl,'*'
    int 21h
    
    
    loop lecture
    ret 
ENDP 
    ;AH = 06
    ;DL = (0-FE) character to output
    ;= FF if console input request
    ;
       ;on return:
    ;AL = input character if console input request (DL=FF)
    ;ZF = 0  if console request character available (in AL)
    ;= 1  if no character is ready, and function request
       ;was console input
         ;
         ;
         ;- reads from or writes to the console device depending on
    ;the value of DL
      ;- cannot output character FF  (DL=FF indicates read function)
    ;- for console read, no echo is produced
    ;- returns 0 for extended keystroke, then function must be
    ;called again to return scan code
    ;    - ignores Ctrl-Break and Ctrl-PrtSc

ingNum PROC
    mov dx, offset varMensaje3
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero1
    int 21h
    
    mov si, offset varNumero1
    call conv_charNum
    mov varOpcion1, bx
    mov varOpcion1C,bx
    
    mov dx, offset varMensaje4
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero2
    int 21h
    
    mov si, offset varNumero2
    call conv_charNum
    mov varOpcion2, bx
    mov varOpcion2C, bx
    
    RET
ENDP
ingvarNumero2 PROC
    mov dx, offset varMensaje1
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero1
    int 21h
    
    mov si, offset varNumero1
    call conv_charNum
    mov varOpcion1, bx
    mov varOpcion1C,bx
    
    mov dx, offset varMensaje2
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero2
    int 21h
    
    mov si, offset varNumero2
    call conv_charNum
    mov varOpcion2, bx
    mov varOpcion2C, bx
    
    RET
ENDP
impR proc
mov dx, offset varResultado
   call impStr
RET
ENDP
;conv_charNum
; ========= Convertir cadena a numero =====================
; Parametros
; si: offset inicial de la cadena con respecto a DS
; Retorna
; bx: valor
conv_charNum proc NEAR
  xor bx,bx   ;BX = 0

  conv_charNum_1:
  lodsb       ;carga byte apuntado por SI en AL
              ;e incrementa si
  cmp al,'0'  ;es numero ascii? [0-9]
  jl noascii  ;no, salir
  cmp al,'9'
  jg noascii  ;no, salir

  sub al,30h  ;ascii '0'=30h, ascii '1'=31h...etc.
  cbw         ;byte a word
  push ax
  mov ax,bx   ;BX tendra el valor final
  mov cx,10
  mul cx      ;AX=AX*10
  mov bx,ax
  pop ax
  add bx,ax
  jmp conv_charNum_1  ;seguir mientras SI apunte a un numero ascii
  noascii:
  ret         ;BX tiene el valor final
conv_charNum endp
;conv_charNum H
; ========= Convertir cadena a numero =====================
; Parametros
; si: offset inicial de la cadena con respecto a DS
; Retorna
; bx: valor
conv_charNumh proc NEAR
  xor bx,bx   ;BX = 0

  conv_charNum_1h:
  lodsb       ;carga byte apuntado por SI en AL
              ;e incrementa si
  cmp al,'A'
  je ascstr
  cmp al,'B'
  je ascstr
  cmp al,'C'
  je ascstr
  cmp al,'D'
  je ascstr
  cmp al,'E'
  je ascstr
  cmp al,'F'
  je ascstr
  cmp al,'0'  ;es numero ascii? [0-9]
  jb noasciii  ;no, salir
  cmp al,'9'
  ja noasciii  ;no, salir
  jmp ascnum
  ascnum:
  sub al,30h  ;ascii '0'=30h, ascii '1'=31h...etc.
  jmp general
  ascstr:
  sub al,37h
  jmp general
  general:
  cbw         ;byte a word
  push ax
  mov ax,bx   ;BX tendra el valor final
  mov cx,16
  mul cx      ;AX=AX*16
  mov bx,ax
  pop ax
  add bx,ax
  jmp conv_charNum_1h  ;seguir mientras SI apunte a un numero ascii
  noasciii:
  ret         ;BX tiene el valor final
  conv_charNumh endp
; =============== Convertir numero a cadena ===============
; Parametros
; ax: valor
; bx: donde guardar la cadena final
; Retorna
; cadena
conv_numChar proc NEAR
  xor cx,cx  ;CX = 0

  conv_numChar_1:
  cmp ax,0   ; El ciclo conv_numChar_1 extrae los digitos del
  je conv_numChar_2  ; menos al mas significativo de AX y los
             ; guarda en el stack. Al finalizar el 
  xor dx,dx  ; ciclo el digito mas significativo esta
  push bx    ; arriba del stack.
  mov bx,10  ; CX contiene el numero de digitos
  div bx
  pop bx
  push dx
  inc cx
  jmp conv_numChar_1

  conv_numChar_2:
  cmp cx,0    ; Esta seccion maneja el caso cuando
  ja conv_numChar_3   ; el numero a convertir (AX) es 0.
  mov ax,'0'  ; En este caso, el ciclo anterior
  mov [bx],ax ; no guarda valores en el stack y
  inc bx      ; CX tiene el valor 0
  jmp conv_numChar_4

  conv_numChar_3:
  pop ax      ; Extraemos los numero del stack
  add ax,30h  ; lo pasamos a su valor ascii
  mov [bx],ax ; lo guardamos en la cadena final
  inc bx
  loop conv_numChar_3

  conv_numChar_4:
  mov ax,'$'  ; terminar cadena con '$' para 
  mov [bx],ax ; imprimirla con la INT21h/AH=9
  ret
conv_numChar endp

convBin proc near
       mov ax, numero + 0
            
       cmp ax, 128
        jae resto128
        mov varBinario + 0, 48                  ; 128   64  32  16  8  4  2  1
        jmp cmp64                           ;  0     1   0   0   0   0 0  1 
        
        resto128:
        mov varBinario +0 , 49 
        sub ax, 128
        
        cmp64:
        cmp ax, 64
        jae resto64
        mov varBinario + 1, 48
        jmp cmp32
        
        resto64:
        mov varBinario + 1 , 49 
        sub ax, 64
        
        cmp32:
        cmp ax, 32
        jae resto32
        mov varBinario + 2, 48
        jmp cmp16
        
        resto32:
        mov varBinario + 2 , 49 
        sub ax, 32
        
        cmp16:
        cmp ax, 16
        jae resto16
        mov varBinario + 3, 48
        jmp cmp8
        
        resto16:
        mov varBinario + 3, 49 
        sub ax, 16
        
        cmp8:
        cmp ax, 8
        jae resto8
        mov varBinario + 4, 48
        jmp cmp4
        
        resto8:
        mov varBinario + 4, 49 
        sub ax, 8
        
        cmp4:
        cmp ax, 4
        jae resto4
        mov varBinario + 5, 48
        jmp cmp2
        
        resto4:
        mov varBinario + 5, 49 
        sub ax, 4
        
        cmp2:
        cmp ax, 2
        jae resto2
        mov varBinario + 6, 48
        jmp cmp1
        
        resto2:
        mov varBinario + 6, 49 
        sub ax, 2
        
        cmp1:
        cmp ax, 1
        jae resto1
        mov varBinario + 7, 48
        jmp cmp0
        
        resto1:
        mov varBinario + 7, 49 
        sub ax, 1
        
        cmp0:
        mov dx,offset varMensajeBinario
        call impStr
        mov dx,offset varBinario
        call impStr
ret
convBin endp
auxhtob proc
    mov bx, offset varResultado
    call conv_numChar
    mov dx, offset varResultado
    call impStr
    mov dx, offset signoResta
    call impStr
ret
endp
limpiar PROC
        mov ah,06h
        mov bH,01110000B
        mov cx,0000h
        mov dx,184fh
        int 10h
           
        MOV AH,02H   ;Para posicionar el cursor
        MOV BH,00H   ;Coloco en la pagina 0
        MOV DX,0000H ;Establesco las coordenadas, x=dh=renglon y=dl=columna
        INT 10H      ;ejecuto la interrupci?n 
        ret
        ENDP
comp PROC
    cmp ax, 9
    ja mayor
    jbe menor
    menor:
    mov bx, offset varResultado
    call conv_numChar
    mov dx, offset varResultado
    call impStr
    ret
    mayor:
    cmp ax, 10
    je a
    cmp ax, 11
    je b
    cmp ax, 12
    je c
    cmp ax, 13
    je d
    cmp ax, 14
    je e
    cmp ax, 15
    je f
    a:
    mov varLetra, 'A'
    mov dx, offset varLetra
    call impStr
    ret
    b:
    mov varLetra, 'B'
    mov dx, offset varLetra
    call impStr
    ret
    c:
    mov varLetra, 'C'
    mov dx, offset varLetra
    call impStr
    ret
    d:
    mov varLetra, 'D'
    mov dx, offset varLetra
    call impStr
    ret
    e:
    mov varLetra, 'E'
    mov dx, offset varLetra
    call impStr
    ret
    f:
    mov varLetra, 'F'
    mov dx, offset varLetra
    call impStr
    ret
ret
ENDP
decHex PROC
    mov ax, bx
   mov cx, 4
   ror ax,cl
   and ax,0fh
   mov varResultadoCociente,ax
   mov ax, 16
   mul varResultadoCociente
   mov varAuxiliarRes, ax
   mov ax, bx
   sub ax, varAuxiliarRes
   mov varResultadoResiduo, ax
   mov ax, varResultadoCociente
   call comp
   mov ax, varResultadoResiduo
   call comp
RET
ENDP 
readKey PROC
        mov dx, offset varMensajeEsperaLectura
        call impStr
        mov ah, 00  ;leer caracer del teclado
        int 16h
        call limpiar;
        call limpiar;
    ret
ENDP

sumarrestar PROC
        call ingNum
        
        mov ax, varOpcion1
        add ax, varOpcion2
        cmp ax, 65535
        jl mostrarPunto1
        mov dx, offset varMensajeInvalido
        call impStr
        call readKey
        jmp showMenu
        
mostrarPunto1:

        mov ax, varOpcion1
        add ax, varOpcion2
        mov dx, offset varMensajeResultadoSuma
        call impStr
        mov bx, offset varResultado
        call conv_numChar
        
        mov dx, offset varResultado
        call impStr
        
        mov ax, varOpcion1
        cmp ax, varOpcion2
        jae resta
        jmp restan

restan:        
        mov dx, offset varMensajeResultadoResta
        call impStr
        mov ax, varOpcion1
        sub ax, varOpcion2
        neg ax
        mov bx, offset varResultado
        call conv_numChar
        
        mov dx, offset signoResta
        call impStr
        
        mov dx, offset varResultado
        call impStr
        call readKey
        jmp showMenu
resta:
        mov dx, offset varMensajeResultadoResta
        call impStr
        mov ax, varOpcion1
        sub ax, varOpcion2
        mov bx, offset varResultado
        call conv_numChar
        
        mov dx, offset varResultado
        call impStr
        call readKey
        jmp showMenu
RET
ENDP
divMulti Proc
        call ingvarNumero2
        ;Division
        mov ax,varOpcion2
        cmp ax,0
        je DivCero
        jmp divi

        DivCero: 
        mov dx,offset varMensajeErrorDivisionPor0
        call impStr
        call readKey
        jmp showMenu

        divi:
        mov ax,varOpcion1
        div varOpcion2

        mov bx,offset varResultadoDivision
        call conv_numChar
        mov dx, offset varMensajeResultadoDivision
        call impStr
        mov dx,offset varResultadoDivision
        call impStr
        ;multiplicacion
        mov ax,varOpcion1
        mul varOpcion2

        mov bx,offset varResultadoMultiplicicacion
        call conv_numChar
        mov dx,offset varMensajeResultadoMultiplicacion
        call impStr

        mov dx,offset varResultadoMultiplicicacion
        call impStr
        call readKey
        jmp showMenu
RET
ENDP
leerH proc
    mov dx, offset varMensajeHexadecimal
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero1
    int 21h
    mov si, offset varNumero1
    call conv_charNumh
    mov varOpcion1, bx
    mov varOpcion1C, bx
    mov dx, offset varMensajeHexadecimal
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset varNumero2
    int 21h
    mov si, offset varNumero2
    call conv_charNumh
    mov varOpcion2, bx
    mov varOpcion2C, bx
RET
ENDP 
hexadecimal proc
    call leerH
    mov dx, offset varMensajePrimerNumeroHexa
    call impStr
    mov bx, varOpcion1
    mov numero, bx
    call convBin
    mov dx, offset varMensajeSegundoNumeroHexa
    call impStr
    mov bx, varOpcion2
    mov numero, bx
    call convBin
     mov dx, offset varMensajePrimerNumeroHexaNegado
     call impStr
     NOT varOpcion1C
     mov ax, varOpcion1C
     sub ax, 65280
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset varMensajeSegundoNumeroHexaNegado
     call impStr
     NOT varOpcion2C
     mov ax, varOpcion2C
     sub ax, 65280
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset varMensajeResultadoAND
     call impStr
     mov ax, varOpcion1
     mov bx, varOpcion2
     and ax, bx
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset varMensajeResultadoOR
     call impStr
     mov ax, varOpcion1
     mov bx, varOpcion2
     OR ax, bx
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset varMensajeResultadoXOR
     call impStr
     mov ax, varOpcion1
     mov bx, varOpcion2
     XOR ax, bx
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     call readKey
     call limpiar
RET
ENDP 
fibonacci PROC
    mov varOpcion1, 1
    mov varOpcion2, 1
    mov varCont, 1
    mov ax, varOpcion1
    mov bx, offset varResultado
    call conv_numChar
    mov dx, offset varResultado
    call impStr
    mov dx, offset coma
    call impStr
    mov ax, varOpcion2
    mov bx, offset varResultado
    call conv_numChar
    mov dx, offset varResultado
    call impStr
    cicloFibo:
    mov dx, offset coma
    call impStr
    add varCont,1
    mov ax, varOpcion2
    mov varTemporal, ax
    add ax, varOpcion1
    mov varOpcion2, ax
    mov bx, offset varResultado
    call conv_numChar
    mov dx, offset varResultado
    call impStr
    mov ax, varTemporal
    mov varOpcion1, ax
    mov al, varCont
    cmp al, 14
    jb cicloFibo
    call readKey
ret 
ENDP
valPasword PROC
    
    add varCont,1
    mov al, varCont
    cmp al,3 ; valida el numero de intentos
    jg salir
    
    call ingresarpasword ;pide contrase?a
    lea si,varAuxiliar  ;cargamos en si la contrase?a ingresada
    lea di,varPassword ;cargamos en di la contrase?a almacenada
    mov cx,8
    repe cmpsb  ;valida contrase?a
    je posswordOk
    mov dx, offset msgpasinvalida
    call impStr
    call valPasword 
    RET
ENDP

;main
INICIO PROC
        call limpiar
        mov ax, @data  ; Se apunta al segmento de datos
        mov ds, ax
        mov es, ax
        call limpiar;
        call valPasword
        posswordOk: 
        mov dx, offset msgpasswordok
        call impStr
        jmp showMenu
       
showMenu:   

        mov dx, offset varMenu     
        call impStr

opciones:
        mov dx, offset varLecturaOpcion
        call impStr
        
        mov ah, 00  ;leer caracer del teclado
        int 16h
        
        cmp al, "1"
        je opcion1
        
        cmp al, "2"
        je opcion2
        
        cmp al, "3"
        je opcion3
        
        cmp al, "4"
        je opcion4
        
        cmp al, "0"
        je salir
        
        mov dx, offset varMensajeOpcionInvalida
        call impStr
        call readKey
        jmp showMenu ; se vuelve a pedir el numero en caso que la opcion digitada no sea valida 
salir:        
        mov ah,4ch ;terminacion del programa
        int 21h
opcion1:
        call sumarrestar
        
opcion2:
        call divMulti
        jmp showMenu
opcion3:
       call hexadecimal
        jmp showMenu
opcion4:
        call fibonacci
        jmp showMenu
ENDP
END INICIO