.model small ; declaraci?n del modelo de memoria
bufSize EQU 121
.stack
.const
        VAL_LF      EQU 10  ; Constante para el Line Feed.
        VAL_RET     EQU 13  ; Constante para el Return.
        CHR_FIN     EQU '$' ; Indica fin de cadena.
        index       EQU 0

.data 
        password            DB "talleraq", CHR_FIN
        msgpassword     DB "Ingrese su password: ", CHR_FIN
        
        signo           DB "-", CHR_FIN
        coma            DB ",", CHR_FIN
        fh              DB "1111", CHR_FIN
        msgpasinvalida  db VAL_LF, VAL_RET,"Password Invaida", VAL_LF, VAL_RET, CHR_FIN
        msgpasswordok   db VAL_LF, VAL_RET,"Password Correcta",VAL_LF, VAL_RET, CHR_FIN
        
        msgReadKey      db VAL_LF, VAL_RET,"Presione Cualquier tecla para continuar...",VAL_LF, VAL_RET, CHR_FIN
        menu            DB VAL_LF, VAL_RET,"::::: MENU :::::" , VAL_LF, VAL_RET;
        opc1            DB "1.- Suma-Diferencia", VAL_LF, VAL_RET;
        opc2            DB "2.- Multiplicacion-Division", VAL_LF, VAL_RET;
        opc3            DB "3.- Operacion Hexadecimal", VAL_LF, VAL_RET;
        opc4            DB "4.- Fibonacci", VAL_LF, VAL_RET;
        opc5            DB "5.- Salir", VAL_LF, VAL_RET, CHR_FIN;
        
        msg             DB "Digite una Opcion entre 1 y 5: ", VAL_LF, VAL_RET, CHR_FIN;
        msgNum1         DB "Ingrese primer numero entre 0 y 255: ", VAL_LF, VAL_RET, CHR_FIN;
        msgNum2         DB "Ingrese segundo numero entre 0 y 255: ", VAL_LF, VAL_RET, CHR_FIN;
        msgNumH         DB "Ingrese un numero Hexadecimal de 2 digitos: ", VAL_LF, VAL_RET, CHR_FIN;
        msgOpNoValida   DB VAL_LF, VAL_RET,"Opcion no valida ", VAL_LF, VAL_RET, CHR_FIN;
        msgresultado1   DB VAL_LF, VAL_RET, "Resultado Suma: ",CHR_FIN
        msgresultado2   DB VAL_LF, VAL_RET, "Resultado Resta: ",CHR_FIN
        mensajeM        DB VAL_LF, VAL_RET, "Resultado Multiplicacion: ",CHR_FIN
        mensajeD        DB VAL_LF, VAL_RET, "Resultado Division: ",CHR_FIN
        mensajeE        DB VAL_LF, VAL_RET, "No se puede dividir entre 0",CHR_FIN
        msgBinario      DB " ", CHR_FIN;
        msgNum1H        DB VAL_LF, VAL_RET,"Primer Hexadecimal en Binario: ", CHR_FIN;
        msgNum2H        DB "Segundo Hexadecimal en Binario:  ", CHR_FIN;
        msgNum1HN        DB "Primer Hexadecimal Negado en Binario: ", CHR_FIN;
        msgNum2HN       DB "Segundo Hexadecimal Negado en Binario: ", CHR_FIN;
        msgAND       DB "Resultado AND: ", CHR_FIN;
        msgOR       DB "Resultado OR: ", CHR_FIN;
        msgXOR       DB "Resultado XOR: ", CHR_FIN;
        binario         DB "00000000", VAL_LF, VAL_RET, CHR_FIN;
        letra DB "A", CHR_FIN;
        ;resultado       DB 0;
        numero          Dw 0 
        num1 db 6 dup(' '),'$'
        num2 db 6 dup(' '),'$'
        aux db  8 dup(' '),'$'
        num db 2 dup(' '),'$'
        cont db 0
        op1 dw ?
        op2 dw ?
        op1c dw ?
        op2c dw ?
        temp dw ?
        cociente dw ?
        residuoo dw ?
        auxres dw ?
        resultado db 6 DUP(?)
        resultadoNOT db 6 DUP(?)
        resultadoAND db 6 DUP(?)
        resultadoOR db 6 DUP(?)
        resultadoM dB 1000 dup(' '),'$'
        resultadoD dB 1000 dup(' '),'$'
        residuo db ?
        
        
        
.code

impStr PROC
    mov ah, 09h
    int 21h
    RET
ENDP
ingresarpasword PROC 
    mov dx, offset msgpassword
    call impStr 
    mov ah,3fh
    mov bx,00
    mov cx,8
    mov dx,offset aux
    int 21h
    ret 
ENDP 

ingNum PROC
    mov dx, offset msgNum1
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset num1
    int 21h
    
    mov si, offset num1
    call atoi
    mov op1, bx
    mov op1c,bx
    
    mov dx, offset msgNum2
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset num2
    int 21h
    
    mov si, offset num2
    call atoi
    mov op2, bx
    mov op2c, bx
    
    RET
ENDP
impR proc
mov dx, offset resultado
   call impStr
RET
ENDP
;atoi 
; ========= Convertir cadena a numero =====================
; Parametros
; si: offset inicial de la cadena con respecto a DS
; Retorna
; bx: valor
atoi proc NEAR
  xor bx,bx   ;BX = 0

  atoi_1:
  lodsb       ;carga byte apuntado por SI en AL
              ;e incrementa si
  cmp al,'0'  ;es numero ascii? [0-9]
  jb noascii  ;no, salir
  cmp al,'9'
  ja noascii  ;no, salir

  sub al,30h  ;ascii '0'=30h, ascii '1'=31h...etc.
  cbw         ;byte a word
  push ax
  mov ax,bx   ;BX tendra el valor final
  mov cx,10
  mul cx      ;AX=AX*10
  mov bx,ax
  pop ax
  add bx,ax
  jmp atoi_1  ;seguir mientras SI apunte a un numero ascii
  noascii:
  ret         ;BX tiene el valor final
atoi endp
;atoi H
; ========= Convertir cadena a numero =====================
; Parametros
; si: offset inicial de la cadena con respecto a DS
; Retorna
; bx: valor
atoih proc NEAR
  xor bx,bx   ;BX = 0

  atoi_1h:
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
  jmp atoi_1h  ;seguir mientras SI apunte a un numero ascii
  noasciii:
  ret         ;BX tiene el valor final
  atoih endp
; =============== Convertir numero a cadena ===============
; Parametros
; ax: valor
; bx: donde guardar la cadena final
; Retorna
; cadena
itoa proc NEAR
  xor cx,cx  ;CX = 0

  itoa_1:
  cmp ax,0   ; El ciclo itoa_1 extrae los digitos del
  je itoa_2  ; menos al mas significativo de AX y los
             ; guarda en el stack. Al finalizar el 
  xor dx,dx  ; ciclo el digito mas significativo esta
  push bx    ; arriba del stack.
  mov bx,10  ; CX contiene el numero de digitos
  div bx
  pop bx
  push dx
  inc cx
  jmp itoa_1

  itoa_2:
  cmp cx,0    ; Esta seccion maneja el caso cuando
  ja itoa_3   ; el numero a convertir (AX) es 0.
  mov ax,'0'  ; En este caso, el ciclo anterior
  mov [bx],ax ; no guarda valores en el stack y
  inc bx      ; CX tiene el valor 0
  jmp itoa_4

  itoa_3:
  pop ax      ; Extraemos los numero del stack
  add ax,30h  ; lo pasamos a su valor ascii
  mov [bx],ax ; lo guardamos en la cadena final
  inc bx
  loop itoa_3

  itoa_4:
  mov ax,'$'  ; terminar cadena con '$' para 
  mov [bx],ax ; imprimirla con la INT21h/AH=9
  ret
itoa endp

convBin proc near
       mov ax, numero + 0
            
       cmp ax, 128
        jae resto128
        mov binario + 0, 48                  ; 128   64  32  16  8  4  2  1
        jmp cmp64                           ;  0     1   0   0   0   0 0  1 
        
        resto128:
        mov binario +0 , 49 
        sub ax, 128
        
        cmp64:
        cmp ax, 64
        jae resto64
        mov binario + 1, 48
        jmp cmp32
        
        resto64:
        mov binario + 1 , 49 
        sub ax, 64
        
        cmp32:
        cmp ax, 32
        jae resto32
        mov binario + 2, 48
        jmp cmp16
        
        resto32:
        mov binario + 2 , 49 
        sub ax, 32
        
        cmp16:
        cmp ax, 16
        jae resto16
        mov binario + 3, 48
        jmp cmp8
        
        resto16:
        mov binario + 3, 49 
        sub ax, 16
        
        cmp8:
        cmp ax, 8
        jae resto8
        mov binario + 4, 48
        jmp cmp4
        
        resto8:
        mov binario + 4, 49 
        sub ax, 8
        
        cmp4:
        cmp ax, 4
        jae resto4
        mov binario + 5, 48
        jmp cmp2
        
        resto4:
        mov binario + 5, 49 
        sub ax, 4
        
        cmp2:
        cmp ax, 2
        jae resto2
        mov binario + 6, 48
        jmp cmp1
        
        resto2:
        mov binario + 6, 49 
        sub ax, 2
        
        cmp1:
        cmp ax, 1
        jae resto1
        mov binario + 7, 48
        jmp cmp0
        
        resto1:
        mov binario + 7, 49 
        sub ax, 1
        
        cmp0:
        mov dx,offset msgBinario
        call impStr
        mov dx,offset binario
        call impStr
ret
convBin endp
auxhtob proc
    mov bx, offset resultado
    call itoa
    mov dx, offset resultado
    call impStr
    mov dx, offset signo
    call impStr
ret
endp
limpiar PROC
         mov ah,06h
         mov bH,1fh
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
    mov bx, offset resultado
    call itoa
    mov dx, offset resultado
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
    mov letra, 'A'
    mov dx, offset letra
    call impStr
    ret
    b:
    mov letra, 'B'
    mov dx, offset letra
    call impStr
    ret
    c:
    mov letra, 'C'
    mov dx, offset letra
    call impStr
    ret
    d:
    mov letra, 'D'
    mov dx, offset letra
    call impStr
    ret
    e:
    mov letra, 'E'
    mov dx, offset letra
    call impStr
    ret
    f:
    mov letra, 'F'
    mov dx, offset letra
    call impStr
    ret
ret
ENDP
decHex PROC
    mov ax, bx
   mov cx, 4
   ror ax,cl
   and ax,0fh
   mov cociente,ax
   mov ax, 16
   mul cociente
   mov auxres, ax
   mov ax, bx
   sub ax, auxres
   mov residuoo, ax
   mov ax, cociente
   call comp
   mov ax, residuoo
   call comp
RET
ENDP 
readKey PROC
        mov dx, offset msgReadKey
        call impStr
        mov ah, 00  ;leer caracer del teclado
        int 16h
        call limpiar;
        call limpiar;
    ret
ENDP

sumarrestar PROC
        call ingNum
        mov dx, offset msgresultado1
        call impStr
        mov ax, op1
        add ax, op2
        mov bx, offset resultado
        call itoa
        
        mov dx, offset resultado
        call impStr
        
        mov ax, op1
        cmp ax, op2
        jae resta
        jmp restan
        
restan:        
        mov dx, offset msgresultado2
        call impStr
        mov ax, op1
        sub ax, op2
        neg ax
        mov bx, offset resultado
        call itoa
        
        mov dx, offset signo
        call impStr
        
        mov dx, offset resultado
        call impStr
        call readKey
        jmp iniMenu
resta:
        mov dx, offset msgresultado2
        call impStr
        mov ax, op1
        sub ax, op2
        mov bx, offset resultado
        call itoa
        
        mov dx, offset resultado
        call impStr
        call readKey
        jmp iniMenu
RET
ENDP
divMulti Proc
        call ingNum
        ;Division
        mov ax,op2
        cmp ax,0
        je DivCero
        jmp divi

        DivCero: 
        mov dx,offset mensajeE
        call impStr
        call readKey
        jmp iniMenu

        divi:
        mov ax,op1
        div op2

        mov bx,offset resultadoD
        call itoa
        mov dx, offset mensajeD
        call impStr
        mov dx,offset resultadoD
        call impStr
        ;multiplicacion
        mov ax,op1
        mul op2

        mov bx,offset resultadoM
        call itoa
        mov dx,offset mensajeM
        call impStr

        mov dx,offset resultadoM
        call impStr
        call readKey
        jmp iniMenu
RET
ENDP
leerH proc
    mov dx, offset msgNumH
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset num1
    int 21h
    mov si, offset num1
    call atoih
    mov op1, bx
    mov op1c, bx
    mov dx, offset msgNumH
    call impStr
    mov ah,3fh
    mov bx,00
    mov cx,6
    mov dx,offset num2
    int 21h
    mov si, offset num2
    call atoih
    mov op2, bx
    mov op2c, bx
RET
ENDP 
hexadecimal proc
    call leerH
    mov dx, offset msgNum1H
    call impStr
    mov bx, op1
    mov numero, bx
    call convBin
    mov dx, offset msgNum2H
    call impStr
    mov bx, op2
    mov numero, bx
    call convBin
     mov dx, offset msgNum1HN
     call impStr
     NOT op1c
     mov ax, op1c
     sub ax, 65280
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset msgNum2HN
     call impStr
     NOT op2c
     mov ax, op2c
     sub ax, 65280
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset msgAND
     call impStr
     mov ax, op1
     mov bx, op2
     and ax, bx
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset msgOR
     call impStr
     mov ax, op1
     mov bx, op2
     OR ax, bx
     mov numero, ax
     mov bx, ax
     call decHex
     call convBin
     mov dx, offset msgXOR
     call impStr
     mov ax, op1
     mov bx, op2
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
    mov op1, 1
    mov op2, 1
    mov cont, 1
    mov ax, op1
    mov bx, offset resultado
    call itoa
    mov dx, offset resultado
    call impStr
    mov dx, offset coma
    call impStr
    mov ax, op2
    mov bx, offset resultado
    call itoa
    mov dx, offset resultado
    call impStr
    cicloFibo:
    mov dx, offset coma
    call impStr
    add cont,1
    mov ax, op2
    mov temp, ax
    add ax, op1
    mov op2, ax
    mov bx, offset resultado
    call itoa
    mov dx, offset resultado
    call impStr
    mov ax, temp
    mov op1, ax
    mov al, cont
    cmp al, 14
    jb cicloFibo
    call readKey
ret 
ENDP
valPasword PROC
    
    add cont,1
    mov al, cont
    cmp al,3 ; valida el numero de intentos
    jg salir
    
    call ingresarpasword ;pide contrase?a
    lea si,aux  ;cargamos en si la contrase?a ingresada
    lea di,password ;cargamos en di la contrase?a almacenada
    repe cmpsb  ;valida contrase?a
    je posswordOk
    mov dx, offset msgpasinvalida
    call impStr
    call valPasword 
    RET
ENDP
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
        jmp iniMenu
       
iniMenu:   
        
        mov dx, offset menu     
        call impStr

opciones:
        mov dx, offset msg
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
        
        cmp al, "5"
        je salir
        
        mov dx, offset msgOpNoValida
        call impStr
        call readKey
        jmp iniMenu ; se vuelve a pedir el numero en caso que la opcion digitada no sea valida 
salir:        
        mov ah,4ch ;terminacion del programa
        int 21h
opcion1:
        call sumarrestar
        
opcion2:
        call divMulti
        jmp iniMenu
opcion3:
       call hexadecimal
        jmp iniMenu
opcion4:
        call fibonacci
        jmp iniMenu
ENDP
END INICIO