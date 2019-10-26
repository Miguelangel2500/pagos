
CD C:\vispagos 
set decimals to 2
SET DATE TO ITALIAN
SET DELETE ON
SET SEPARATOR TO "."
*READ EVENTS
SET POINT TO ","
wmayus=CAPSLOCK()

CAPSLOCK(.t.)

SET CENTURY ON

SET EXCLUSIVE OFF
_screen.FontSize=15
_screen.FontName="courier new"
_screen.Caption="Programa Pagos"
*BASES DE DATOS E INDICES

*PROVEEDORES.DBF    INPROV (NOMBRE)    IN1CLAVE  (CLAVE)
*PAGOS              IN1NUMERO (NUMERO)
*DIAMES             INDIA    (DIA)
*LETRADIN           INDIN    (NUMERO)


SET TALK OFF
use pagos INDEX in1numero

do while .t.
       respuesta="s"
       clear

       *@ 1,1 to 26,79 double
       *@ 2,8 to 5,50
       *@ 3,9  SAY "PAGOS A PROVEEDORES"

       *@ 6,9 say "(M)antenimiento proveedores"
       *@ 8,9 say "Creacion (c)heque"
       *@ 10,9 say "Creacion (p)agare"
       *@ 12,9 say "Emision c(a)rta pago"
       *@ 14,9 say "Pagos e(f)ectuados"
       *@ 16,9 say "Pagos por pro(v)eedor"
       *@ 18,9 say "Car(t)as"
       *@ 20,9 SAY "(G)estion de pagos"
       *@ 22,9 say "Reinde(x)ar"
       *@ 24,9 say "(S)alir            " GET RESPUESTA
       
       **@ 28,9 say "(Q)uit               " GET RESPUESTA
       
       
       *read
       DO FORM VISPAGOS_MENUINICIO
       do  case
             case lower(respuesta)="g"
                       do gestion
             case lower(respuesta)="m"
                       do mancli
             case lower(respuesta)="c"
                       do cheque
             case lower(respuesta)="p"
                       do pagare
             case lower(respuesta)="a"
                      *do cartapag
             case lower(respuesta)="f"
                      do respag

             case lower(respuesta)="t"
                      do cartas

             case lower(respuesta)="v"
                      *do pagprov
             case lower(respuesta)="s"
                       CAPSLOCK(wmayus)
                       close index
                       close databases
                       *CLEAR EVENTS
                       
                       exit
                       

             case lower(respuesta)="q"
                       CAPSLOCK(wmayus)
                       
                       close index
                       close databases
                       *CLEAR EVENTS
                       quit
             case lower(respuesta)="x"
                     SET EXCLUSIVE ON  
                       USE PAGOS INDEX IN1NUMERO
                       REINDEX
                       use proveedo index inprov,in1clave,innif
                       reindex
                       USE BEDATE INDEX IN2NUMER
                     SET EXCLUSIVE OFF
             CASE LOWER(RESPUESTA)="r"
                     DO RESPAGPROV      
       
       endcase
enddo
*************************************************
*************PROCEDIMIENTO RESUMEN DE PAGOS
********************************************
********************************
PROCEDURE RESPAG
LOCAL WIMPORTE
WIMPORTE=0
WFECHA=CTOD("  -  -    ")
WVENCIMIENT=CTOD("  -  -    ")
USE PAGOS INDEX in1numero
set century on
CLEAR
@ 5,5 SAY "LISTADO DE PAGOS"

@ 10,2 SAY "INTRODUCE FECHA   " GET WFECHA
@ 11,2 SAY "FECHA VENCIMIENTO " GET WVENCIMIENT 
@ 12,2 SAY "EN EL MENU DE IMPRESION PULSAR PROPIEDADES Y ELEGIR HORIZONTAL"
READ
     XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivoA.txt
	   ELSE 
	    CLEAR 
	    @ 1,1 SAY "PULSA PROPIEDADES EN EL CONTROLADOR DE LA IMPRESORA"
	    @ 2,1 SAY "Y ELIJE HORIZONTAL"
	    WAIT WINDOW
	    
	    SET PRINT ON prompt
	    set device to print
	    
	    SET PRINT FONT "COURIER NEW",7
	    
	ENDIF
set century off

SET CENTURY OFF
@ 2,1 SAY " IMPORTE  RECEPTOR   NIF      FECHA   VENCIMIENT  NUMERO   NUMFAC1  NUMFAC2   NUMFAC3   NUMFAC4   NUMFAC5   NUMFAC6   NUMFAC7   NUMFAC8   DESFAC1   DESFAC2   DESFAC3   DESFAC4 "      
@ 3,1 SAY "============================================================================================================================================================================"
GO TOP
DO WHILE .NOT.EOF()
  if (fecha=wfecha).AND.(VENCIMIENT=WVENCIMIENT) 
   @ PROW()+1,1 SAY STR(IMPORTE,8,2)
   @ PROW(),10 SAY RECEPTOR
   @ prow(),21 say nif
   @ PROW(),31 SAY DTOC(FECHA)
   @ PROW(),40 SAY DTOC(VENCIMIENT)
   @ PROW(),50 SAY NUMERO
   @ PROW(),60 SAY NUMFAC1       
   @ PROW(),70 SAY NUMFAC2
   @ PROW(),80 SAY NUMFAC3
   @ PROW(),90 SAY NUMFAC4
   @ PROW(),100 SAY NUMFAC5
   @ PROW(),110 SAY NUMFAC6
   @ PROW(),120 SAY NUMFAC7
   @ PROW(),130 SAY NUMFAC8
   @ PROW(),140 SAY desfac1
   @ PROW(),150 SAY DESFAC2
   @ PROW(),160 SAY DESFAC3
   @ PROW(),170 SAY DESFAC4
   *@ prow(),170 say nif
   
   WIMPORTE=WIMPORTE+IMPORTE   
  endif 
   SKIP
ENDDO
@ PROW()+3,1 SAY STR(WIMPORTE,10,2)
@ PROW()+1,1 SAY STR(WIMPORTE*166.386,12,2)

 do case
		   	case xx="I"
			   
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivoA.txt noedit
		        
		       delete file archivoA.txt
		  endcase

RETURN

*******************************************
********MANTENIMIENTO DE proveedores
************************************

PROCEDURE MANCLI
volver=" "
SEGUIR=.T.
do while SEGUIR=.T.
  clear
  @ 1,1 to 28,90 double
  @ 4,9 to 7,50
  @ 5,10 SAY "MANTENIMIENTO proveedores"

  @ 10,10 say "(A)ltas"
  @ 12,10 say "(B)ajas"
  @ 14,10 say "(M)odificaciones"
  @ 16,10 say "(C)onsultas"
  @ 18,10 say "Consulta por c(l)ave"
  @ 20,10 say "Consulta por (n)if"
  @ 24,10 say "(V)olver        " get volver
  read
  volver=lower(volver)
  do case
       case volver="a"

            use proveedo index inprov,IN1CLAVE,innif

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(11) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            @ 21,5 say "Banco:         " get wbanco
            @ 22,5 say "Nº cuenta:     " get wn_cuenta


            read
            wnformapago=wformapago
            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF
            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago
            replace banco     with wbanco
            replace n_cuenta  with wn_cuenta


       case volver="b"

            use proveedo index inprov,IN1CLAVE,innif
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            SEEK wnombre
            browse fields nombre,poblacion,NIF noappend nomenu NOEDIT NODELETE
            clear
            @ 10,5 say " " +nombre
            wrespuesta="n"
            @ 11,5 say "¿Esta seguro de que desea borrarlo? s/n" get wrespuesta
            read
            wrespuesta=lower(wrespuesta)
            if wrespuesta="s"
                    delete
                    WAIT WINDOW "El proveedor ha sido borrado"
            endif
       case volver="m"



            use proveedo index inprov,IN1CLAVE,innif
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            SEEK wnombre
            browse fields nombre,poblacion,NIF noappend nomenu NOEDIT NODELETE
            clear
            @ 6,5 say  "Nombre:        " get nombre
            @ 7,5 say  "Direccion:     " get direccion
            @ 8,5 say  "Codigo Postal: " get codpostal
            @ 9,5 say  "Poblacion:     " get poblacion
            @ 10,5 say "Provincia:     " get provincia
            @ 11,5 say "Pais:          " get pais
            @ 12,5 say "Nif:           " get nif
            @ 13,5 say "Clave:         " +clave
            @ 14,5 say "Departamento:  " get departamen
            @ 15,5 say "Poligono:      " get poligono
            @ 16,5 say "Telefono:      " get telefono
            @ 17,5 say "Apartado:      " get apdo
            @ 18,5 say "Contacto:      " get contacto
            @ 19,5 say "Actividad:     " get actividad
            @ 20,5 say "Forma de pago: " get formapago

            @ 21,5 say "Banco:         " get banco
            @ 22,5 say "Nº cuenta:     " get n_cuenta

            read

       case volver="c"


            use proveedo index inprov,IN1CLAVE,innif
            store space(40) to wnombre
            clear
            @ 6,15 say "Introduce el proveedor: " get wnombre
            read
            wprueba=trim(wnombre)
            wprueba=upper(wprueba)
            
            SEEK wprueba
            
            browse fields nombre,POBLACION,NIF NOMENU NOAPPEND NOEDIT NODELETE
            WCLAVE=CLAVE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT WINDOW
            
            WS="N"
            CLEAR
            @ 1,1 SAY "¿IMPRIMIR SOBRE S/N?   "  GET WS
            READ
            WS=LOWER(WS)
            IF WS="s"
            
                SELECT * FROM PROVEEDO WHERE clave=wclave INTO CURSOR CURSORSOBRES
                REPORT FORM informedireccionsobresproveedores TO PRINTER prompt preview
                *WAIT window
                CLOSE INDEXES
                CLOSE DATABASES 
            
            ENDIF

       case volver="l"

            use proveedo index IN1CLAVE,inprov,innif
            store space(10) to wclave
            clear
            @ 6,15 say "Introduce la clave del proveedor: " get wclave
            read
            wprueba=trim(wclave)
            wprueba=upper(wprueba)
            SEEK wprueba
            browse fields clave,POBLACION,NIF NOMENU NOAPPEND NOEDIT NODELETE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT WINDOW

        case volver="n"
                  use proveedo index INnif,inprov,in1clave
            store space(9) to wnif
            clear
            @ 6,15 say "Introduce el nif del proveedor: " get wnif
            read
            wprueba=trim(wnif)
            wprueba=upper(wprueba)
            SEEK wprueba
            browse fields nif,nombre,POBLACION NOMENU NOAPPEND NOEDIT NODELETE
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago

     @ 18,5 say "Banco:         " +banco
     @ 19,5 say "Nº cuenta:     " +n_cuenta
            ?
            ?
            WAIT WINDOW

        case volver="v"

                 SEGUIR=.F.
     endcase
enddo











RETURN

*********************************
*******
*******PROCEDIMIENTO CHEQUE
*********************************
*********************************
procedure cheque
do while .t.
        resp="v"
        clear
        @ 1,1 to 28,90 double
        @ 4,4 to 7,50
        @ 5,5 SAY "PAGOS POR CHEQUE"

        @ 12,5 say "(a)ltas"
        @ 14,5 say "(m)odificaciones"
        @ 16,5 say "(c)onsultas"
        @ 18,5 say "(b)ajas"
        @ 20,5 say "(i)mpresion"
        @ 22,5 say "(v)olver a menu principal"
        @ 24,5 SAY "                         " get resp
        read
        resp=lower(resp)
        do case
              case resp="a"
                       clear
                       store space(10) to wreceptor
                       STORE SPACE(10) TO WNIF
                       @ 2,2 say "Receptor" get wreceptor
                       @ 3,2 SAY "NIF:    " GET WNIF
                       read
        IF WNIF=" "
            use proveedo index inprov,IN1CLAVE,innif

                       wreceptor=upper(wreceptor)
                       wreceptor=trim(wreceptor)
                       SEEK wreceptor
          ELSE
            USE PROVEEDO INDEX INNIF,INPROV,IN1CLAVE
            WNIF=UPPER(WNIF)
            WNIF=TRIM(WNIF)
            SEEK WNIF
       ENDIF               
                       
                       BROWSE FIELDS nombre,poblacion,NIF NOMENU NOAPPEND NODELETE
                       clear
                       resp="s"
                       @ 2,2 say "Encontrado s/n" get resp
                       READ
                       RESP=LOWER(RESP)
                       if resp="s"

                           wclave=clave
                           WNOMBRE=NOMBRE
                           wnif=nif
                       else


            use proveedo index IN1CLAVE,inprov,innif

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(11) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            @ 21,5 say "Banco:         " get wbanco
            @ 22,5 say "Nº cuenta:     " get wn_cuenta


            read

            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF

            SEEK WCLAVE
            IF FOUND()
                  CLEAR
                  @ 13,5 SAY "LA CLAVE "+WCLAVE+" YA EXISTE"
                  @ 14,5 SAY "ELIJA OTRA: " GET WCLAVE
                  READ
            ENDIF


            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago
            replace banco     with wbanco
            replace n_cuenta  with wn_cuenta
                           endif

                       use
                       WIMPORTE=0
                       WFECHA=CTOD("01-01-2006")
                       STORE SPACE(10) TO WNUMERO
                       STORE SPACE(10) TO WNUMFAC1
                       STORE SPACE(10) TO WNUMFAC2
                       STORE SPACE(10) TO WNUMFAC3
                       STORE SPACE(10) TO WNUMFAC4

                       STORE SPACE(10) TO WNUMFAC5
                       STORE SPACE(10) TO WNUMFAC6
                       STORE SPACE(10) TO WNUMFAC7
                       store space(10) to wnumfac8

                       store space(10) to wdesfac1
                       store space(10) to wdesfac2
                       store space(10) to wdesfac3
                       store space(10) to wdesfac4

                       wimport1=0
                       wimport2=0
                       wimport3=0
                       wimport4=0
                       wimport5=0
                       wimport6=0
                       wimport7=0
                       wimport8=0
                       wimpdesc1=0
                       wimpdesc2=0
                       wimpdesc3=0
                       wimpdesc4=0


                       STORE SPACE(50) TO WLETRA

                       STORE SPACE(50) TO WLETRA2



                       clear

                       set century on

                     

                       *@ 3,2  SAY "fecha:         " get wfecha
                       *@ 4,2  SAY "numero:        " get wnumero
                       *@ 5,2  SAY "factura1 " get wnumfac1
                       *@ 5,24 say " " get wimport1 picture "999,999.99"
                       *@ 5,42 say "factura2 " get wnumfac2
                       *@ 5,64 say " " get wimport2 picture "999,999.99"
                       *@ 6,2  SAY "factura3 " get wnumfac3
                       *@ 6,24 say " " get wimport3 picture "999,999.99"
                       *@ 6,42 say "factura4 " get wnumfac4
                       *@ 6,64 say " " get wimport4 picture "999,999.99"
                       *@ 7,2  SAY "factura5 " get wnumfac5
                       *@ 7,24 say " " get wimport5 picture "999,999.99"
                       *@ 7,42 say "factura6 " get wnumfac6
                       *@ 7,64 say " " get wimport6 picture "999,999.99"
                       *@ 8,2  SAY "factura7 " get wnumfac7
                       *@ 8,24 say " " get wimport7 picture "999,999.99"
                       *@ 8,42 say "factura8 " get wnumfac8
                       *@ 8,64 say " " get wimport8 picture "999,999.99"


                      *@ 10,2  say "desc.1 " get wdesfac1
                      *@ 10,24 say " " get wimpdesc1 picture "999,999.99"
                      *@ 10,42 say "desc.2 " get wdesfac2
                      *@ 10,64 say " " get wimpdesc2 picture "999,999.99"
                      *@ 11,2  say "desc.3 " get wdesfac3
                      *@ 11,24 say " " get wimpdesc3 picture "999,999.99"
                      *@ 11,42 say "desc.4 " get wdesfac4
                      *@ 11,64 say " " get wimpdesc4 picture "999,999.99"



                       *@ 13,2 SAY "RECEPTOR       " +WNOMBRE
DO FORM vispagos_cintroducciondedatos

                       wtipo="CHEQUE"
                       WMONEDA="Euros"
                       *read
wimporte=wimport1+wimport2+wimport3+wimport4+wimport5+wimport6+wimport7+wimport8-wimpdesc1-wimpdesc2-wimpdesc3-wimpdesc4

****************************************************************************
CLEAR
*********RELLENO DE NUMEROS RELLI.PRG
*********BASE DE DATOS BEDATE ;  INDICE IN2NUMERO

*************CAMPOS:  NUMERO , LETRA
***************PROCESADOR DE TEXTO PROCTEXT.PRG



ENTERA=INT(Wimporte)
DECIMAL=Wimporte-ENTERA
USE BEDATE INDE IN2NUMER
CENTERA=STR(ENTERA,10)
CDECIMAL=STR(DECIMAL,10,2)
CDECIMAL=SUBSTR(CDECIMAL,9)
***********PARA QUITAR EL CERO DE DELANTE
IF VAL(CDECIMAL)<10
    CDECIMAL=SUBSTR(CDECIMAL,2)


    CDECIMAL="         "+CDECIMAL
ELSE

     CDECIMAL="        "+CDECIMAL
ENDIF


SEEK CENTERA
LCENTERA=LETRA



SEEK CDECIMAL
LCDECIMAL=LETRA


if decimal<>0



    PALABRA=RTRIM(LCENTERA)+" CON "+RTRIM(LCDECIMAL)+" CENTIMOS"

  else


    PALABRA=RTRIM(LCENTERA)
endif


**********procedure proctext



a=palabra
l=len(palabra)
IF L>=50
  L=49
ENDIF

a=a+replicate(" ",50-l)


N=50
         B=SUBSTR(A,N,1)
         POSICION=0
         IF B<>" "
             M=1
             C="A"
             DO WHILE M<=15.AND.C<>" "



                C=SUBSTR(A,N-M,1)
                IF C=" "

                    POSICION=M
                ENDIF
             M=M+1
             ENDDO
         ENDIF

CAD1=trim(SUBSTR(A,1,50-POSICION))
lin=len(cad1)
cad1=cad1+replicate("=",50-lin)

CAD2=SUBSTR(A,50-POSICION)
LONGi=LEN(CAD2)
CAD2=CAD2+REPLICATE("=",50-LONGi)








****************************************************************************************




                IF WIMPORTE<=10000

                      @ 15,2 SAY "LETRA          " +CAD1

                      @ 16,2 SAY "LETRA          " +CAD2

                ELSE


                      @ 15,2 SAY "LETRA          " GET CAD1

                      @ 16,2 SAY "LETRA          " GET CAD2

                ENDIF



                       VAR=STR(WIMPORTE,10,2)
                       IMPOVAR=VAR


                       @ 18,2  say "Importe:       " +IMPOVAR

read

                       use pagos INDEX IN1NUMERo


                       APPEND BLANK
                           replace importe with wimporte
                           replace fecha with wfecha
                           replace tipo with wtipo
                           replace RECEPTOR with wclave
                           replace nif with wnif
                           replace numero with wnumero
                           replace numfac1 with wnumfac1
                           replace numfac2 with wnumfac2
                           replace numfac3 with wnumfac3
                           replace numfac4 with wnumfac4

                           replace numfac5 with wnumfac5
                           replace numfac6 with wnumfac6
                           replace numfac7 with wnumfac7
                           replace numfac8 with wnumfac8

                           replace desfac1 with wdesfac1
                           replace desfac2 with wdesfac2
                           replace desfac3 with wdesfac3
                           replace desfac4 with wdesfac4

                           replace import1 with wimport1
                           replace import2 with wimport2
                           replace import3 with wimport3
                           replace import4 with wimport4
                           replace import5 with wimport5
                           replace import6 with wimport6
                           replace import7 with wimport7
                           replace import8 with wimport8

                           replace impdesc1 with wimpdesc1
                           replace impdesc2 with wimpdesc2
                           replace impdesc3 with wimpdesc3
                           replace impdesc4 with wimpdesc4

                           REPLACE TIPO WITH "CHEQUE"

                           REPLACE LETRA WITH CAD1



                           REPLACE LETRA2 WITH CAD2
                       use
                       clear
                       resp="n"
                       @ 2,2 SAY "¿Emision carta pago? s/n" get resp
                       read
                            if lower(resp)="s"
                            *********emision carta pago

            use proveedo index IN1CLAVE,inprov,innif
SEEK wclave
wnombre=nombre
wdireccion=direccion
wapdo=apdo
wpoligono=poligono
wcodpostal=codpostal
wpoblacion=poblacion
wprovincia=provincia
wpais=pais
wnif=nif



SET PRINT ON prompt
set device to printer
*set printer font "courier new",11
SET PRINTER FONT "Monospac821 BT",11
SET CENTURY OFF

@ 9,42 say wnombre

@ 10,42 say wdireccion
IF WAPDO<>" "
         
         @ 11,42 say wapdo   
ENDIF

if wpoligono<>" "
         
         @ 12,42 say wpoligono
endif

@ 13,42 say wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   
   @ 14,42 say wprovincia+" "+wpais
else
   @ 14,42 say wprovincia
endif

if wpoligono= " "
        *?
endif

SET CENTURY ON
CLEAR

fecar=wfecha
set device to screen
@ 5,5 SAY "FECHA DE CARTA:     " GET FECAR
READ

set device to printer



*set print font "courier new",10
SET PRINTER FONT "Monospac821 BT",10
@ 23,48 SAY "Valladolid, "+STR(day(FECAR),2)+" de "+cmonth(FECAR)+" de "+STR(year(FECAR),4)
SET CENTURY OFF

*@ 26,8 SAY "ASUNTO: Reposicion de fondos"

@ 30,8 SAY "Muy Sres. nuestros:"

@ 34,20 SAY "Nos es grato adjuntar a la presente, CHEQUE correspondiente"


@ 35,13 SAY "al pago de su/s factura/s segun se detalla:"

@ 38,8 SAY "- Banco       :"
@ 38,24 SAY "SANTANDER"

@ 41,8 SAY "- CHEQUE      :"
@ 41,24 SAY WNUMERO


        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



       

       r1=10
       r2=10
       r3=10
       r4=10
       r5=10
       r6=10
       r7=10
       r8=10
       s1=2
       s2=2
       s3=2
       s4=2
       s5=2
       s6=2
       s7=2
       s8=2
       T1=";"
       T2=";"
       T3=";"
       T4=";"
       T5=";"
       T6=";"
       T7=";"
       T8=";"

if wimport1=0
                        r1=0
                        s1=0
                        T1=""
endif
if wimport2=0
                        r2=0
                        s2=0
                        T2=""
endif
if wimport3=0
                        r3=0
                        s3=0
                        T3=""
endif
if wimport4=0
                        r4=0
                        s4=0
                        T4=""
endif
if wimport5=0
                        r5=0
                        s5=0
                        T5=""
endif
if wimport6=0
                        r6=0
                        s6=0
                        T6=""
endif
if wimport7=0
                        r7=0
                        s7=0
                        T7=""
endif
if wimport8=0
                        r8=0
                        s8=0
                        T8=""
endif





@ 44,8 SAY "- Su/s Fra/s  : "+TRIM(wnumfac1)+" "+str(wimport1,r1,s1)+" "+T2+TRIM(wnumfac2)+" "+str(wimport2,r2,s2)+" "+T3+trim(wnumfac3)+" "+str(wimport3,r3,s3)

@ 45,24 SAY TRIM(wnumfac4)+" "+str(wimport4,r4,s4)+" "+T5+TRIM(WNUMFAC5)+" "+STR(WIMPORT5,R5,S5)+" "+T6+TRIM(WNUMFAC6)+" "+STR(WIMPORT6,R6,S6)

@ 46,24 SAY trim(WNUMFAC7)+" "+str(wimport7,r7,s7)+" "+T8+trim(WNUMFAC8)+" "+str(wimport8,r8,s8)


       U1=10
       U2=10
       U3=10
       U4=10


       V1=2
       V2=2
       V3=2
       V4=2
       X1=";"
       x2=";"
       X3=";"
       x4=";"
if wimpdesc1=0
                        U1=0
                        V1=0
                        X1=""
endif
if wimpdesc2=0
                        U2=0
                        V2=0
                        X2=""
endif
if wimpdesc3=0
                        U3=0
                        V3=0
                        X3=""
endif
if wimpdesc4=0
                        U4=0
                        V4=0
                        X4=""
endif


@ 47,8 SAY "- Fact. desc. :"+" "+trim(wdesfac1)+" "+str(wimpdesc1,U1,V1)+" "+X2+trim(wdesfac2)+" "+str(wimpdesc2,U2,V2)


@ 48,24 SAY trim(wdesfac3)+" "+str(wimpdesc3,U3,V3)+" "+X4+trim(wdesfac4)+" "+str(wimpdesc4,U4,V4)


@ 50,8 SAY "- Importe     :"+" "+"***"+LTRIM(SALIDA)+"***"+" "+wmoneda


@ 53,20 SAY "En espera de que sea de la conformidad de Vds., aprovechamos la"

@ 54,13 SAY "ocasion para saludarles atentamente."


SET PRINT OFF
SET PRINTER TO
SET DEVICE TO SCREEN

ENDIF
CLEAR
@ 2,2 SAY "¿IMPRIMIR EL CHEQUE? S/N" GET RESP
READ
IF LOWER(RESP)="s"



use proveedo index in1clave,inprov,innif
SEEK wclave
wnombre=nombre
wdireccion=direccion
wapdo=apdo
wpoligono=poligono
wcodpostal=codpostal
wpoblacion=poblacion
wprovincia=provincia
wpais=pais
wnif=nif
        SET PRINT ON PROMPT
        SET DEVICE TO PRINTER
        *SET PRINT FONT "COURIER NEW",10
        SET PRINTER FONT "Monospac821 BT",10
        ?
        ?

        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



      
        
        @ 3,54 SAY "***"+LTRIM(SALIDA)+"***"
        
        @ 4,19 SAY WNOMBRE
        
        @ 5,9 SAY CAD1

        
        @ 6,9 SAY CAD2
        a=day(wfecha)
        IF A<10
            B=STR(A,1)
            B="0"+B
          ELSE
            b=str(a,2)
        ENDIF

        B=TRIM(B)
        use diames index india
        SEEK b
        vari=letra
        use
        *igualacion mes
        p=upper(cmonth(wfecha))
        n=len(p)
        d=10-n
        r=space(d)
        palabra=p+r

        
        @ 7,4 SAY "VALLADOLID,"
        @ 7,23 SAY VARI
        @ 7,52 SAY PALABRA
        @ 7,74 SAY STR(YEAR(WFECHA),4)
        set print off
        SET PRINTER TO
        SET DEVICE TO SCREEN
        
endif

              case resp="m"
                   *do modificaciones
              case resp="c"
                   *do consultas
              case resp="b"
                   *do bajas
              case resp="i"
                   do CHEQUE_impresion
              case resp="v"
                    return
        endcase
enddo
       
       ********************************************
       ********* PROCEDIMIENTO IMPRESION
       ********************************************
       
       PROCEDURE CHEQUE_IMPRESION
	   CLEAR
	   wNUMERO="       "
	   @ 2,2 SAY "¿NUMERO DE CHEQUE?" GET wNUMERO
	   READ
	   USE PAGOS INDEX IN1NUMERO 
	   SEEK(wNUMERO)
	   IF FOUND()
	                       
	                       Wimporte=importe
                           Wfecha=fecha
                           Wtipo=tipo
                           WCLAVE=RECEPTOR
                           Wnif=nif
                           Wnumero=numero
                           Wnumfac1=numfac1
                           Wnumfac2=numfac2
                           Wnumfac3=numfac3
                           Wnumfac4=numfac4

                           Wnumfac5=numfac5
                           Wnumfac6=numfac6
                           Wnumfac7=numfac7
                           Wnumfac8=numfac8

                           Wdesfac1=desfac1
                           Wdesfac2=desfac2
                           Wdesfac3=desfac3
                           Wdesfac4=desfac4

                           Wimport1=import1
                           Wimport2=import2
                           Wimport3=import3
                           Wimport4=import4
                           Wimport5=import5
                           Wimport6=import6
                           Wimport7=import7
                           Wimport8=import8

                           Wimpdesc1=impdesc1
                           Wimpdesc2=impdesc2
                           Wimpdesc3=impdesc3
                           Wimpdesc4=impdesc4

                           

                           CAD1=LETRA



                           CAD2=LETRA2
	     
	   	         ELSE
	   	                   CLEAR
	   	                   @ 2,2 SAY "NO EXISTE ESTE NUMERO DE CHEQUE"
	   	                   WAIT WINDOW
	   	                   RETURN
	   	         ENDIF          
		                       clear
		                       resp="n"
		                       @ 2,2 SAY "¿Emision carta pago? s/n" get resp
		                       read
		                            if lower(resp)="s"
		                            *********emision carta pago

		            use proveedo index IN1CLAVE,inprov,innif
		SEEK wclave
		wnombre=nombre
		wdireccion=direccion
		wapdo=apdo
		wpoligono=poligono
		wcodpostal=codpostal
		wpoblacion=poblacion
		wprovincia=provincia
		wpais=pais
		wnif=nif



		SET PRINT ON prompt
		set device to printer
		*set printer font "courier new",11
		SET PRINTER FONT "Monospac821 BT",11
		SET CENTURY OFF
		*impresion de la direccion del cliente
		
		@ 9,42 say wnombre
		
		@ 10,42 say wdireccion
		IF WAPDO<>" "
		         
		         @ 11,42 say wapdo   
		ENDIF

		if wpoligono<>" "
		         
		         @ 12,42 say wpoligono
		endif
		
		@ 13,42 say wcodpostal+" "+left(wpoblacion,25)
		if wpais<>"ESPAÑA"
		   
		   @ 14,42 say wprovincia+" "+wpais
		else
		   @ 14,42 say wprovincia
		endif
		
		if wpoligono= " "
		        
		endif
		
		SET CENTURY ON
		CLEAR
		
		fecar=wfecha
		set device to screen
		@ 5,5 SAY "FECHA DE CARTA:     " GET FECAR
		READ

		set device to printer


		
		*set print font "courier new",10
		SET PRINTER FONT "Monospac821 BT",10
		@ 23,48 SAY "Valladolid, "+STR(day(FECAR),2)+" de "+cmonth(FECAR)+" de "+STR(year(FECAR),4)
		SET CENTURY OFF
		
		*@ 26,8 SAY "ASUNTO: Reposicion de fondos"
		
		@ 30,8 SAY "Muy Sres. nuestros:"
		
		@ 34,20 SAY "Nos es grato adjuntar a la presente, CHEQUE correspondiente"

		
		@ 35,13 SAY "al pago de su/s factura/s segun se detalla:"
		
		@ 38,8 SAY "- Banco       :"
		@ 38,24 SAY "SANTANDER"
		
		@ 41,8 SAY "- CHEQUE      :"
		@ 41,24 SAY WNUMERO
		

		        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



		       

		       r1=10
		       r2=10
		       r3=10
		       r4=10
		       r5=10
		       r6=10
		       r7=10
		       r8=10
		       s1=2
		       s2=2
		       s3=2
		       s4=2
		       s5=2
		       s6=2
		       s7=2
		       s8=2
		       T1=";"
		       T2=";"
		       T3=";"
		       T4=";"
		       T5=";"
		       T6=";"
		       T7=";"
		       T8=";"

		if wimport1=0
		                        r1=0
		                        s1=0
		                        T1=""
		endif
		if wimport2=0
		                        r2=0
		                        s2=0
		                        T2=""
		endif
		if wimport3=0
		                        r3=0
		                        s3=0
		                        T3=""
		endif
		if wimport4=0
		                        r4=0
		                        s4=0
		                        T4=""
		endif
		if wimport5=0
		                        r5=0
		                        s5=0
		                        T5=""
		endif
		if wimport6=0
		                        r6=0
		                        s6=0
		                        T6=""
		endif
		if wimport7=0
		                        r7=0
		                        s7=0
		                        T7=""
		endif
		if wimport8=0
		                        r8=0
		                        s8=0
		                        T8=""
		endif




		
		@ 44,8 SAY "- Su/s Fra/s  : "+TRIM(wnumfac1)+" "+str(wimport1,r1,s1)+" "+T2+TRIM(wnumfac2)+" "+str(wimport2,r2,s2)+" "+T3+trim(wnumfac3)+" "+str(wimport3,r3,s3)
		
		@ 45,24 SAY TRIM(wnumfac4)+" "+str(wimport4,r4,s4)+" "+T5+TRIM(WNUMFAC5)+" "+STR(WIMPORT5,R5,S5)+" "+T6+TRIM(WNUMFAC6)+" "+STR(WIMPORT6,R6,S6)
		
		@ 46,24 SAY trim(WNUMFAC7)+" "+str(wimport7,r7,s7)+" "+T8+trim(WNUMFAC8)+" "+str(wimport8,r8,s8)


		       U1=10
		       U2=10
		       U3=10
		       U4=10


		       V1=2
		       V2=2
		       V3=2
		       V4=2
		       X1=";"
		       x2=";"
		       X3=";"
		       x4=";"
		if wimpdesc1=0
		                        U1=0
		                        V1=0
		                        X1=""
		endif
		if wimpdesc2=0
		                        U2=0
		                        V2=0
		                        X2=""
		endif
		if wimpdesc3=0
		                        U3=0
		                        V3=0
		                        X3=""
		endif
		if wimpdesc4=0
		                        U4=0
		                        V4=0
		                        X4=""
		endif

		
		@ 47,8 SAY "- Fact. desc. :"+" "+trim(wdesfac1)+" "+str(wimpdesc1,U1,V1)+" "+X2+trim(wdesfac2)+" "+str(wimpdesc2,U2,V2)

		
		@ 48,24 SAY trim(wdesfac3)+" "+str(wimpdesc3,U3,V3)+" "+X4+trim(wdesfac4)+" "+str(wimpdesc4,U4,V4)

		
		@ 50,8 SAY "- Importe     :"+" "+"***"+LTRIM(SALIDA)+"***"

		
		@ 53,20 SAY "En espera de que sea de la conformidad de Vds., aprovechamos la"
		
		@ 54,13 SAY "ocasion para saludarles atentamente."

		
		SET PRINT OFF
		SET PRINTER TO
		SET DEVICE TO SCREEN
		
		ENDIF
		RESP="n"
		CLEAR
		@ 2,2 SAY "¿IMPRIMIR EL CHEQUE? S/N" GET RESP
		READ
		IF LOWER(RESP)="s"



		use proveedo index in1clave,inprov,innif
		SEEK wclave
		wnombre=nombre
		wdireccion=direccion
		wapdo=apdo
		wpoligono=poligono
		wcodpostal=codpostal
		wpoblacion=poblacion
		wprovincia=provincia
		wpais=pais
		wnif=nif
		        SET PRINT ON PROMPT
		        SET DEVICE TO PRINTER
		        *SET PRINT FONT "COURIER NEW",10
                SET PRINTER FONT  "Monospac821 BT",10
		        

		        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



		       
		        
		        @ 3,54 SAY "***"+LTRIM(SALIDA)+"***"
		        
		        @ 4,19 SAY WNOMBRE
		        
		        @ 5,9 SAY CAD1

		        
		        @ 6,9 SAY CAD2
		        a=day(wfecha)
		        IF A<10
		            B=STR(A,1)
		            B="0"+B
		          ELSE
		            b=str(a,2)
		        ENDIF

		        B=TRIM(B)
		        use diames index india
		        SEEK b
		        vari=letra
		        use
		        *igualacion mes
		        p=upper(cmonth(wfecha))
		        n=len(p)
		        d=10-n
		        r=space(d)
		        palabra=p+r

		        
		        @ 7,4 SAY "VALLADOLID,"
		        @ 7,23 SAY VARI
		        @ 7,52 SAY PALABRA
		        @ 7,74 SAY STR(YEAR(WFECHA),4)
		        set print off
		        SET PRINTER TO
		        SET DEVICE TO SCREEN
		        
		endif

       
       
       
       ***FIN PROCEDIMIENTO IMPRESION DE CHEQUES
       RETURN





return

*********fin del procedimiento cheque


*********************************
*******
*******PROCEDIMIENTO pagare
*********************************
*********************************
procedure pagare
do while .t.
        resp="v"
        clear
        @ 1,1 to 28,90 double
        @ 4,4 to 7,50
        @ 5,5 SAY "PAGARES"

        @ 12,5 say "(a)ltas"
        @ 14,5 say "(m)odificaciones"
        @ 16,5 say "(c)onsultas"
        @ 18,5 say "(b)ajas"
        @ 20,5 say "(i)mpresion"
        @ 22,5 say "(v)olver a menu principal"
        @ 24,5 SAY "                         " get resp
        read
        RESP=LOWER(RESP)
        do case
              case resp="a"

                       clear
                       store space(10) to wreceptor
                       STORE SPACE(10) TO WNIF
                       @ 2,2 say "Receptor" get wreceptor
                       @ 3,2 SAY "NIF:    " GET WNIF
                       read
       IF WNIF=" "
            use proveedo index inprov,IN1CLAVE,innif

                       wreceptor=upper(wreceptor)
                       wreceptor=trim(wreceptor)
                       SEEK wreceptor
          ELSE
            USE PROVEEDO INDEX INNIF,INPROV,IN1CLAVE
            WNIF=UPPER(WNIF)
            WNIF=TRIM(WNIF)
            SEEK WNIF
       ENDIF
                       
                       brow fiel nombre,poblacion,NIF NOMENU NOAPPEND NODELETE NOEDIT
                       clear
                       resp="s"
                       @ 2,2 say "Encontrado s/n" get resp
                       READ
                       RESP=LOWER(RESP)
                       if resp="s"

                           wclave=clave
                           WNOMBRE=NOMBRE
                           wnif=nif
                       else


            use proveedo index IN1CLAVE,inprov,innif

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(11) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            @ 21,5 say "Banco:         " get wbanco
            @ 22,5 say "Nº cuenta:     " get wn_cuenta


            read



            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF

            SEEK WCLAVE
            IF FOUND()
                  CLEAR
                  @ 13,5 SAY "LA CLAVE "+WCLAVE+" YA EXISTE"
                  @ 14,5 SAY "ELIJA OTRA: " GET WCLAVE
                  READ
            ENDIF

            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago
            replace banco     with wbanco
            replace n_cuenta  with wn_cuenta
                           endif

                       use
                       WIMPORTE=0
                       WFECHA=CTOD("01-01-2007")

                       Wemis=CTOD("01-01-2006")
                       STORE SPACE(10) TO WNUMERO
                       STORE SPACE(10) TO WNUMFAC1
                       STORE SPACE(10) TO WNUMFAC2
                       STORE SPACE(10) TO WNUMFAC3
                       STORE SPACE(10) TO WNUMFAC4

                       STORE SPACE(10) TO WNUMFAC5
                       STORE SPACE(10) TO WNUMFAC6
                       STORE SPACE(10) TO WNUMFAC7
                       STORE SPACE(10) TO WNUMFAC8

                       store space(10) to wdesfac1
                       store space(10) to wdesfac2
                       store space(10) to wdesfac3
                       store space(10) to wdesfac4
                       WIMPORT1=0
                       WIMPORT2=0
                       WIMPORT3=0
                       WIMPORT4=0
                       WIMPORT5=0
                       WIMPORT6=0
                       WIMPORT7=0
                       WIMPORT8=0
                       WIMPDESC1=0
                       WIMPDESC2=0
                       WIMPDESC3=0
                       WIMPDESC4=0

                       STORE SPACE(50) TO WLETRA

                       STORE SPACE(50) TO WLETRA2



                       clear
                       set century on

                      
                       *@ 2,2  SAY "Fecha emision  " get wemis
                       *@ 3,2  SAY "Fecha vto.     " get wfecha
                       
                       *@ 4,2  SAY "numero:        " get wnumero
                       *@ 5,2  SAY "factura1 " get wnumfac1
                       *@ 5,24 say " " get wimport1 picture "999,999.99"
                       *@ 5,42 say "factura2 " get wnumfac2
                       *@ 5,64 say " " get wimport2 picture "999,999.99"
                       *@ 6,2  SAY "factura3 " get wnumfac3
                       *@ 6,24 say " " get wimport3 picture "999,999.99"
                       *@ 6,42 say "factura4 " get wnumfac4
                       *@ 6,64 say " " get wimport4 picture "999,999.99"
                       *@ 7,2  SAY "factura5 " get wnumfac5
                       *@ 7,24 say " " get wimport5 picture "999,999.99"
                       *@ 7,42 say "factura6 " get wnumfac6
                       *@ 7,64 say " " get wimport6 picture "999,999.99"
                       *@ 8,2  SAY "factura7 " get wnumfac7
                       *@ 8,24 say " " get wimport7 picture "999,999.99"
                       *@ 8,42 say "factura8 " get wnumfac8
                       *@ 8,64 say " " get wimport8 picture "999,999.99"


                      *@ 10,2  say "desc.1 " get wdesfac1
                      *@ 10,24 say " " get wimpdesc1 picture "999,999.99"
                      *@ 10,42 say "desc.2 " get wdesfac2
                      *@ 10,64 say " " get wimpdesc2 picture "999,999.99"
                      *@ 11,2  say "desc.3 " get wdesfac3
                      *@ 11,24 say " " get wimpdesc3 picture "999,999.99"
                      *@ 11,42 say "desc.4 " get wdesfac4
                      *@ 11,64 say " " get wimpdesc4 picture "999,999.99"



                       *@ 13,2 SAY "RECEPTOR       " +WNOMBRE

DO FORM vispagos_pintroducciondedatos
                       wtipo="PAGARE"
                       WMONEDA="Euros"
                       *read
wimporte=wimport1+wimport2+wimport3+wimport4+wimport5+wimport6+wimport7+wimport8-wimpdesc1-wimpdesc2-wimpdesc3-wimpdesc4
****************************************************************************
CLEAR
*********RELLENO DE NUMEROS RELLI.PRG
*********BASE DE DATOS BEDATE ;  INDICE IN2NUMERO

*************CAMPOS:  NUMERO , LETRA
***************PROCESADOR DE TEXTO PROCTEXT.PRG



ENTERA=INT(Wimporte)
DECIMAL=Wimporte-ENTERA
USE BEDATE INDEX IN2NUMER
CENTERA=STR(ENTERA,10)
CDECIMAL=STR(DECIMAL,10,2)
CDECIMAL=SUBSTR(CDECIMAL,9)


***********PARA QUITAR EL CERO DE DELANTE
IF VAL(CDECIMAL)<10
    CDECIMAL=SUBSTR(CDECIMAL,2)


    CDECIMAL="         "+CDECIMAL
ELSE

     CDECIMAL="        "+CDECIMAL
ENDIF




SEEK CENTERA
LCENTERA=LETRA
SEEK CDECIMAL
LCDECIMAL=LETRA

if decimal<>0



    PALABRA=RTRIM(LCENTERA)+" CON "+RTRIM(LCDECIMAL)+" CENTIMOS"

  else


    PALABRA=RTRIM(LCENTERA)
endif


**********procedure proctext



a=palabra
l=len(palabra)
IF L>=50
  L=49
ENDIF
REPETICION=50-L
INTERMEDIA=REPLICATE(" ",REPETICION)
a=a+INTERMEDIA


N=50
         B=SUBSTR(A,N,1)
         POSICION=0
         IF B<>" "
             M=1
             C="A"
             DO WHILE M<=15.AND.C<>" "



                C=SUBSTR(A,N-M,1)
                IF C=" "

                    POSICION=M
                ENDIF
             M=M+1
             ENDDO
         ENDIF

CAD1=trim(SUBSTR(A,1,50-POSICION))
lin=len(cad1)
cad1=cad1+replicate("=",50-lin)

CAD2=SUBSTR(A,50-POSICION)
LONGi=LEN(CAD2)
CAD2=CAD2+REPLICATE("=",50-LONGi)








****************************************************************************************




                IF WIMPORTE<=10000

                      @ 15,2 SAY "LETRA          " +CAD1

                      @ 16,2 SAY "LETRA          " +CAD2

                ELSE


                      @ 15,2 SAY "LETRA          " GET CAD1

                      @ 16,2 SAY "LETRA          " GET CAD2

                ENDIF



                       VAR=STR(WIMPORTE,10,2)
                       IMPOVAR=VAR


                       @ 18,2  say "Importe:       " +IMPOVAR

read

                       use pagos INDEX IN1NUMERo


                       APPEND BLANK
                           replace importe with wimporte
                           replace fecha with wemis
                           replace vencimient with wfecha
                           replace tipo with wtipo
                           replace RECEPTOR with wclave
                           replace nif with wnif
                           replace numero with wnumero
                           replace numfac1 with wnumfac1
                           replace numfac2 with wnumfac2
                           replace numfac3 with wnumfac3
                           replace numfac4 with wnumfac4

                           replace numfac5 with wnumfac5
                           replace numfac6 with wnumfac6
                           replace numfac7 with wnumfac7
                           replace numfac8 with wnumfac8

                           replace desfac1 with wdesfac1
                           replace desfac2 with wdesfac2
                           replace desfac3 with wdesfac3
                           replace desfac4 with wdesfac4

                           replace import1 with wimport1
                           replace import2 with wimport2
                           replace import3 with wimport3
                           replace import4 with wimport4
                           replace import5 with wimport5
                           replace import6 with wimport6
                           replace import7 with wimport7
                           replace import8 with wimport8

                           replace impdesc1 with wimpdesc1
                           replace impdesc2 with wimpdesc2
                           replace impdesc3 with wimpdesc3
                           replace impdesc4 with wimpdesc4

                           REPLACE TIPO WITH "PAGARE"

                           REPLACE LETRA WITH CAD1



                           REPLACE LETRA2 WITH CAD2


                       use
                       clear
                       resp="n"
                       @ 2,2 SAY "¿Emision carta de pago? s/n" get resp
                       read
                            if lower(resp)="s"
                            *********emision carta pago

            use proveedo index IN1CLAVE,inprov,innif
SEEK wclave
wnombre=nombre
wdireccion=direccion
wapdo=apdo
wpoligono=poligono
wcodpostal=codpostal
wpoblacion=poblacion
wprovincia=provincia
wpais=pais
wnif=nif



SET PRINT ON PROMPT
SET DEVICE TO PRINT
*SET PRINT FONT "COURIER NEW",11
SET PRINTER FONT "Monospac821 BT",11
*SET PRINTER FONT "lucida console",11
SET CENTURY OFF
*impresion de la direccion del PROVEEDOR

@ 9,42 SAY WNOMBRE

@ 10,42 SAY WDIRECCION
IF WAPDO<>" "
         
         @ 11,42 SAY WAPDO     
ENDIF

if wpoligono<>" "
         
         @ 12,42 SAY WPOLIGONO
endif

@ 13,42 SAY WCODPOSTAL+" "+LEFT(WPOBLACION,25)
if wpais<>"ESPAÑA"
   
   @ 14,42 SAY wprovincia+" "+WPAIS
else
   @ 14,42 say wprovincia
endif

if wpoligono= " "
        
endif

SET CENTURY ON


CLEAR

fecar=wemis
SET DEVICE TO SCREEN
@ 5,5 SAY "FECHA DE CARTA:     " GET FECAR
READ

SET DEVICE TO PRINT
*SET PRINT FONT "COURIER NEW",10
SET PRINTER FONT "Monospac821 BT",10
*SET PRINTER FONT "lucida console",10
@ 23,48 SAY "Valladolid, "+STR(DAY(FECAR),2)+" de "+cmonth(FECAR)+" de "+STR(year(FECAR),4)

SET CENTURY OFF

*@ 26,8 SAY "ASUNTO: Reposicion de fondos"

@ 30,8 SAY "Muy Sres. nuestros:"

@ 34,20 SAY "Nos es grato adjuntar a la presente, PAGARE correspondiente"

@ 35,13 SAY "al pago de su/s factura/s segun se detalla:"

@ 38,8 SAY "- Banco       :"
@ 38,24 say "SANTANDER"

@ 41,8 SAY "- PAGARE      :"
@ 41,24 SAY WNUMERO 


        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



       

       r1=10
       r2=10
       r3=10
       r4=10
       r5=10
       r6=10
       r7=10
       r8=10
       s1=2
       s2=2
       s3=2
       s4=2
       s5=2
       s6=2
       s7=2
       s8=2
       T1=";"
       T2=";"
       T3=";"
       T4=";"
       T5=";"
       T6=";"
       T7=";"
       T8=";"

if wimport1=0
                        r1=0
                        s1=0
                        T1=""
endif
if wimport2=0
                        r2=0
                        s2=0
                        T2=""
endif
if wimport3=0
                        r3=0
                        s3=0
                        T3=""
endif
if wimport4=0
                        r4=0
                        s4=0
                        T4=""
endif
if wimport5=0
                        r5=0
                        s5=0
                        T5=""
endif
if wimport6=0
                        r6=0
                        s6=0
                        T6=""
endif
if wimport7=0
                        r7=0
                        s7=0
                        T7=""
endif
if wimport8=0
                        r8=0
                        s8=0
                        T8=""
endif





@ 44,8 SAY "- Su/s Fra/s  :"+" "+TRIM(wnumfac1)+" "+str(wimport1,r1,s1)+" "+T2+TRIM(wnumfac2)+" "+str(wimport2,r2,s2)+" "+T3+trim(wnumfac3)+" "+str(wimport3,r3,s3)
@ 45,24 SAY TRIM(wnumfac4)+" "+str(wimport4,r4,s4)+" "+T5+TRIM(WNUMFAC5)+" "+STR(WIMPORT5,R5,S5)+" "+T6+TRIM(WNUMFAC6)+" "+STR(WIMPORT6,R6,S6)
@ 46,24 SAY trim(WNUMFAC7)+" "+str(wimport7,r7,s7)+" "+T8+trim(WNUMFAC8)+" "+str(wimport8,r8,s8)


       U1=10
       U2=10
       U3=10
       U4=10


       V1=2
       V2=2
       V3=2
       V4=2
       X1=";"
       x2=";"
       X3=";"
       x4=";"
if wimpdesc1=0
                        U1=0
                        V1=0
                        X1=""
endif
if wimpdesc2=0
                        U2=0
                        V2=0
                        X2=""
endif
if wimpdesc3=0
                        U3=0
                        V3=0
                        X3=""
endif
if wimpdesc4=0
                        U4=0
                        V4=0
                        X4=""
endif

@ 47,8 SAY "- Fact. desc. :"+" "+trim(wdesfac1)+" "+str(wimpdesc1,U1,V1)+" "+X2+trim(wdesfac2)+" "+str(wimpdesc2,U2,V2)
@ 48,24 SAY trim(wdesfac3)+" "+str(wimpdesc3,U3,V3)+" "+X4+trim(wdesfac4)+" "+str(wimpdesc4,U4,V4)


@ 50,8 SAY "- Importe     :"+" "+"***"+LTRIM(SALIDA)+"***"+" "+wmoneda

@ 53,20 SAY "En espera de que sea de la conformidad de Vds., aprovechamos la"
@ 54,13 SAY "ocasion para saludarles atentamente."
SET PRINT OFF
SET PRINTER TO

SET DEVICE TO SCREEN
ENDIF
RESP="N"
CLEAR
@ 2,2 SAY "¿IMPRIMIR EL PAGARE? S/N" GET RESP
READ
IF LOWER(RESP)="s"


            use proveedo index iN1CLAVE,inprov,innif
SEEK wclave
wnombre=nombre
wdireccion=direccion
wapdo=apdo
wpoligono=poligono
wcodpostal=codpostal
wpoblacion=poblacion
wprovincia=provincia
wpais=pais
wnif=nif
        SET PRINT ON prompt
        set device to print
        *set print font "courier new",10
        SET PRINTER FONT "Monospac821 BT",10
        
        *SET PRINTER FONT "lucida console",10 

        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



      
        ********igualacion de mes
        p=upper(cmonth(wfecha))
        palabra=p
        n=len(p)
        d=10-n
        r=space(d)
        palabra=p+r



        @ 3,15 say str(day(wfecha),2)
        @ 3,20 say palabra
        @ 3,34 say str(year(wfecha),4)
        @ 3,57 say "***"+LTRIM(SALIDA)+"***"
       
        @ 5,11 say wnombre
        
        @ 6,13 say cad1 
        
        @ 7,13 say cad2
        a=day(wemis)
        IF A<10
              B=STR(A,1)
              B="0"+B
           ELSE
              b=str(a,2)

        ENDIF

        B=TRIM(B)
        use diames index india
        SEEK b
        vari=letra
        use
        *****igualacion mes
        p=upper(cmonth(wemis))
        palabra=p
        n=len(p)
        d=10-n
        r=space(d)
        palabra=p+r

        *no hace falta igualar el dia por la longitud del campo en la bd

    
        @ 8,9 say "VALLADOLID,"
        @ 8,27 SAY VARI
        @ 8,56 SAY PALABRA
        @ 8,79 SAY  str(year(wemis),4)
        
        set print off
        set printer to
        set device to screen
        
endif








              case resp="m"
                  * do modificaciones
              case resp="c"
                  * do consultas
              case resp="b"
                  * do bajas
              case resp="i"
                  do PAGARE_impresion
              case resp="v"
                    return
        endcase
enddo
               
               *********************************************
               ************** PROCEDIMIENTO IMPRESION
               *********************************************
               
               
               procedure PAGARE_impresion
		                CLEAR
			   wNUMERO="       "
			   @ 2,2 SAY "¿NUMERO DE PAGARE?" GET wNUMERO
			   READ
			   USE PAGOS INDEX IN1NUMERO 
			   SEEK(wNUMERO)
			   IF FOUND()
		                                              


		                       
		                           Wimporte=importe
		                           wemis=FECHA
		                           wfecha=VENCIMIENT
		                           wtipo=TIPO
		                           wclave=RECEPTOR
		                           Wnif=nif
		                           Wnumero=numero
		                           Wnumfac1=numfac1
		                           Wnumfac2=numfac2
		                           Wnumfac3=numfac3
		                           Wnumfac4=numfac4

		                           Wnumfac5=numfac5
		                           Wnumfac6=numfac6
		                           Wnumfac7=numfac7
		                           Wnumfac8=numfac8

		                           Wdesfac1=desfac1
		                           Wdesfac2=desfac2
		                           Wdesfac3=desfac3
		                           Wdesfac4=desfac4

		                           Wimport1=import1
		                           Wimport2=import2
		                           Wimport3=import3
		                           Wimport4=import4
		                           Wimport5=import5
		                           Wimport6=import6
		                           Wimport7=import7
		                           Wimport8=import8

		                           Wimpdesc1=impdesc1
		                           Wimpdesc2=impdesc2
		                           Wimpdesc3=impdesc3
		                           Wimpdesc4=impdesc4

		                           

		                           CAD1=LETRA



		                           CAD2=LETRA2
                 ELSE
	   	                   CLEAR
	   	                   @ 2,2 SAY "NO EXISTE ESTE NUMERO DE PAGARE"
	   	                   WAIT WINDOW
	   	                   RETURN
	   	         ENDIF

		                       use
		                       clear
		                       resp="n"
		                       @ 2,2 SAY "¿emision carta pago? s/n" get resp
		                       read
		                            if lower(resp)="s"
		                            *********emision carta pago

		            use proveedo index IN1CLAVE,inprov,innif
		SEEK wclave
		wnombre=nombre
		wdireccion=direccion
		wapdo=apdo
		wpoligono=poligono
		wcodpostal=codpostal
		wpoblacion=poblacion
		wprovincia=provincia
		wpais=pais
		wnif=nif



		SET PRINT ON PROMPT
		SET DEVICE TO PRINT
		*SET PRINT FONT "COURIER NEW",11
		SET PRINTER FONT "Monospac821 BT",11
		*SET PRINTER FONT "Monospac821Greek BT",11
		SET CENTURY OFF
		*impresion de la direccion del PROVEEDOR
		
		@ 9,42 SAY WNOMBRE
		
		@ 10,42 SAY WDIRECCION
		IF WAPDO<>" "
		         
		         @ 11,42 SAY WAPDO     
		ENDIF

		if wpoligono<>" "
		         
		         @ 12,42 SAY WPOLIGONO
		endif
		@ 13,42 SAY WCODPOSTAL+" "+LEFT(WPOBLACION,25)
		if wpais<>"ESPAÑA"
		   @ 14,42 SAY wprovincia+" "+WPAIS
		else
		   @ 14,42 say wprovincia
		endif
		
		if wpoligono= " "
		       
		endif
		
		SET CENTURY ON


		CLEAR
		fecar=wemis
		SET DEVICE TO SCREEN
		@ 5,5 SAY "FECHA DE CARTA:     " GET FECAR
		READ
		SET DEVICE TO PRINT
		*SET PRINT FONT "COURIER NEW",10
		SET PRINTER FONT "Monospac821 BT",10
		*SET PRINTER FONT "Monospac821Greek BT",10
		
		@ 23,48 SAY "Valladolid, "+STR(DAY(FECAR),2)+" de "+cmonth(FECAR)+" de "+STR(year(FECAR),4)

		SET CENTURY OFF
		
		*@ 26,8 SAY "ASUNTO: Reposicion de fondos"
		
		@ 30,8 SAY "Muy Sres. nuestros:"
		
		@ 34,20 SAY "Nos es grato adjuntar a la presente, PAGARE correspondiente"
		@ 35,13 SAY "al pago de su/s factura/s segun se detalla:"
		
		@ 38,8 SAY "- Banco       :"
		@ 38,24 say "SANTANDER"
		
		@ 41,8 SAY "- PAGARE      :"
		@ 41,24 SAY WNUMERO 
		

		        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



		    

		       r1=10
		       r2=10
		       r3=10
		       r4=10
		       r5=10
		       r6=10
		       r7=10
		       r8=10
		       s1=2
		       s2=2
		       s3=2
		       s4=2
		       s5=2
		       s6=2
		       s7=2
		       s8=2
		       T1=";"
		       T2=";"
		       T3=";"
		       T4=";"
		       T5=";"
		       T6=";"
		       T7=";"
		       T8=";"

		if wimport1=0
		                        r1=0
		                        s1=0
		                        T1=""
		endif
		if wimport2=0
		                        r2=0
		                        s2=0
		                        T2=""
		endif
		if wimport3=0
		                        r3=0
		                        s3=0
		                        T3=""
		endif
		if wimport4=0
		                        r4=0
		                        s4=0
		                        T4=""
		endif
		if wimport5=0
		                        r5=0
		                        s5=0
		                        T5=""
		endif
		if wimport6=0
		                        r6=0
		                        s6=0
		                        T6=""
		endif
		if wimport7=0
		                        r7=0
		                        s7=0
		                        T7=""
		endif
		if wimport8=0
		                        r8=0
		                        s8=0
		                        T8=""
		endif




		@ 44,8 SAY "- Su/s Fra/s  :"+" "+TRIM(wnumfac1)+" "+str(wimport1,r1,s1)+" "+T2+TRIM(wnumfac2)+" "+str(wimport2,r2,s2)+" "+T3+trim(wnumfac3)+" "+str(wimport3,r3,s3)
		@ 45,24 SAY TRIM(wnumfac4)+" "+str(wimport4,r4,s4)+" "+T5+TRIM(WNUMFAC5)+" "+STR(WIMPORT5,R5,S5)+" "+T6+TRIM(WNUMFAC6)+" "+STR(WIMPORT6,R6,S6)
		@ 46,24 SAY trim(WNUMFAC7)+" "+str(wimport7,r7,s7)+" "+T8+trim(WNUMFAC8)+" "+str(wimport8,r8,s8)


		       U1=10
		       U2=10
		       U3=10
		       U4=10


		       V1=2
		       V2=2
		       V3=2
		       V4=2
		       X1=";"
		       x2=";"
		       X3=";"
		       x4=";"
		if wimpdesc1=0
		                        U1=0
		                        V1=0
		                        X1=""
		endif
		if wimpdesc2=0
		                        U2=0
		                        V2=0
		                        X2=""
		endif
		if wimpdesc3=0
		                        U3=0
		                        V3=0
		                        X3=""
		endif
		if wimpdesc4=0
		                        U4=0
		                        V4=0
		                        X4=""
		endif

		@ 47,8 SAY "- Fact. desc. :"+" "+trim(wdesfac1)+" "+str(wimpdesc1,U1,V1)+" "+X2+trim(wdesfac2)+" "+str(wimpdesc2,U2,V2)
		@ 48,24 SAY trim(wdesfac3)+" "+str(wimpdesc3,U3,V3)+" "+X4+trim(wdesfac4)+" "+str(wimpdesc4,U4,V4)
	
		@ 50,8 SAY "- Importe     :"+" "+"***"+LTRIM(SALIDA)+"***"
		
		@ 53,20 SAY "En espera de que sea de la conformidad de Vds., aprovechamos la"
		@ 54,13 SAY "ocasion para saludarles atentamente."
		SET PRINT OFF
		SET PRINTER TO
		
		SET DEVICE TO SCREEN
		ENDIF
        RESP="n"
		CLEAR
		@ 2,2 SAY "¿IMPRIMIR EL PAGARE? S/N" GET RESP
		READ
		IF LOWER(RESP)="s"
      

		            use proveedo index IN1CLAVE,inprov,innif
		SEEK wclave
		wnombre=nombre
		wdireccion=direccion
		wapdo=apdo
		wpoligono=poligono
		wcodpostal=codpostal
		wpoblacion=poblacion
		wprovincia=provincia
		wpais=pais
		wnif=nif
		        SET PRINT ON prompt
		        set device to print
		        *set print font "courier new",10
		        SET PRINTER FONT "Monospac821 BT",10

		        SALIDA=TRANSFORM(WIMPORTE,"99,999.99")



		      
		        ********igualacion de mes
		        p=upper(cmonth(wfecha))
		        palabra=p
		        n=len(p)
		        d=10-n
		        r=space(d)
		        palabra=p+r



		        @ 3,15 say str(day(wfecha),2)
		        @ 3,20 say palabra
		        @ 3,34 say str(year(wfecha),4)
		        @ 3,57 say "***"+LTRIM(SALIDA)+"***"
		        
		        @ 5,11 say wnombre
		        
		        @ 6,13 say cad1 
		        
		        @ 7,13 say cad2
		        a=day(wemis)
		        IF A<10
		              B=STR(A,1)
		              B="0"+B
		           ELSE
		              b=str(a,2)

		        ENDIF

		        B=TRIM(B)
		        use diames index india
		        SEEK b
		        vari=letra
		        use
		        *****igualacion mes
		        p=upper(cmonth(wemis))
		        palabra=p
		        n=len(p)
		        d=10-n
		        r=space(d)
		        palabra=p+r

		       
		        @ 8,9 say "VALLADOLID,"
		        @ 8,27 SAY VARI
		        @ 8,56 SAY PALABRA
		        @ 8,79 SAY  str(year(wemis),4)
		       
		        set print off
		        set printer to
		        set device to screen
               
        ENDIF       
               
               
               
               
               return





return

*********fin del procedimiento PAGARE

*********************************
*******
*******PROCEDIMIENTO CARTA
*********************************
*********************************
procedure CARTAs
do while .t.
        resp="v"
        clear
        @ 1,1 to 24,79 double
        @ 4,4 to 8,50
        @ 5,5 SAY "CARTA"

        @ 12,5 say "(a)ltas"
        @ 17,5 say "(v)olver a menu principal"
        @ 19,5 SAY "                         " get resp
        read
        resp=lower(resp)
        do case
              case resp="a"
                       clear
                       store space(10) to wreceptor
                       @ 2,2 say "receptor" get wreceptor
                       read


            USE proveedo index inprov,IN1CLAVE,innif

                       wreceptor=upper(wreceptor)
                       wreceptor=trim(wreceptor)
                       SEEK wreceptor
                       brow fiel nombre,poblacion,NIF NOMENU NOAPPEND NOEDIT NODELETE
                       clear
                       resp="s"
                       @ 2,2 say "encontrado s/n" get resp
                       READ
                       RESP=LOWER(RESP)
                       if resp="s"

                           wclave=clave
                           WNOMBRE=NOMBRE
                       else


            use proveedo index IN1CLAVE,inprov,innif

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(11) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            store space(15) to wbanco
            store space(30) to wn_cuenta

            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            @ 21,5 say "Banco:         " get wbanco
            @ 22,5 say "Nº cuenta:     " get wn_cuenta


            read

            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF
            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago
            replace banco     with wbanco
            replace n_cuenta  with wn_cuenta
                           endif

                      use

store space(65) to TEXTO01
store space(65) to TEXTO02
store space(65) to TEXTO03
store space(65) to TEXTO04
store space(65) to TEXTO05
store space(65) to TEXTO06
store space(65) to TEXTO07
store space(65) to TEXTO08
store space(65) to TEXTO09
store space(65) to TEXTO10
store space(65) to TEXTO11
store space(65) to TEXTO12
store space(65) to TEXTO13
store space(65) to TEXTO14
store space(65) to TEXTO15
store space(65) to TEXTO16
store space(65) to TEXTO17
store space(65) to TEXTO18
store space(65) to TEXTO19
store space(65) to TEXTO20
store space(65) to TEXTO21
store space(65) to TEXTO22
store space(65) to TEXTO23
store space(65) to TEXTO24
store space(65) to TEXTO25
store space(65) to TEXTO26
store space(65) to TEXTO27
store space(65) to TEXTO28
store space(65) to TEXTO29
store space(65) to TEXTO30
store space(65) to TEXTO31
store space(65) to TEXTO32



                       CLEAR
                      SET CENTURY ON
                       @ 1,1 say "01          " GET TEXTO01
                       @ 2,1 SAY "02          " get TEXTO02
                       @ 3,1 SAY "03          " get TEXTO03
                       @ 4,1 SAY "04          " get TEXTO04
                       @ 5,1 SAY "05          " get TEXTO05
                       @ 6,1 SAY "06          " get TEXTO06
                       @ 7,1 SAY "07          " get TEXTO07
                       @ 8,1 SAY "08          " get TEXTO08
                       @ 9,1 SAY "09          " get TEXTO09
                       @ 10,1 SAY "10          " get TEXTO10
                       @ 11,1 SAY "11          " GET TEXTO11
                      @ 12,1 SAY "12          " GET TEXTO12
                      @ 13,1 SAY "13          " GET TEXTO13
@ 14,1 say "14          " get TEXTO14
@ 15,1 say "15          " get TEXTO15
@ 16,1 say "16          " get TEXTO16
@ 17,1 say "18          " get TEXTO17
@ 18,1 say "18          " get TEXTO18
@ 19,1 say "19          " get TEXTO19
@ 20,1 say "20          " get TEXTO20
@ 21,1 say "21          " get TEXTO21
@ 22,1 say "22          " get TEXTO22
@ 23,1 say "23          " get TEXTO23

                       use
                       
                       resp="n"
                       @ 24,1 SAY "¿emision carta ? s/n" get resp
                       read
                            if lower(resp)="s"
                            *********emision carta pago
use proveedo index IN1CLAVE,inprov,innif
SEEK wclave
wnombre=nombre
wdireccion=direccion
wapdo=apdo
wpoligono=poligono
wcodpostal=codpostal
wpoblacion=poblacion
wprovincia=provincia
wpais=pais
wnif=nif



SET PRINT ON PROMPT
SET DEVICE TO PRINT
*SET PRINT FONT "COURIER NEW",11
SET PRINTER FONT "Monospac821 BT",11
SET CENTURY OFF

@ 10,42 SAY WNOMBRE

@ 11,42 SAY WDIRECCION
IF WAPDO<>" "
         
         @ 12,42 SAY WAPDO
ENDIF

if wpoligono<>" "
         
         @ 13,42 SAY WPOLIGONO    
endif

@ 14,42 SAY wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   
   @ 15,42 SAY wprovincia+" "+WPAIS 
else
   @ 15,42 say wprovincia
endif




SET CENTURY ON
CLEAR
FECAR=CTOD("  -  -    ")
SET DEVICE TO SCREEN
@ 5,5 SAY "FECHA DE CARTA:     " GET FECAR
READ
SET DEVICE TO PRINT

*SET PRINT FONT "COURIER NEW",10
SET PRINTER FONT "Monospac821 BT",10

@ 23,48 SAY "Valladolid, "+STR(day(FECAR),2)+" de "+cmonth(FECAR)+" de "+STR(year(FECAR),4)
SET CENTURY OFF


@ 26,8 SAY TEXTO01

@ 27,8 SAY TEXTO02

@ 28,8 SAY TEXTO03

@ 29,8 SAY TEXTO04

@ 30,8 SAY TEXTO05

@ 31,8 SAY TEXTO06

@ 32,8 SAY TEXTO07

@ 33,8 SAY TEXTO08

@ 34,8 SAY TEXTO09

@ 35,8 SAY TEXTO10

@ 36,8 SAY TEXTO11

@ 37,8 SAY TEXTO12

@ 38,8 SAY TEXTO13

@ 39,8 SAY TEXTO14

@ 40,8 SAY TEXTO15

@ 41,8 SAY TEXTO16

@ 42,8 SAY TEXTO17

@ 43,8 SAY TEXTO18

@ 44,8 SAY TEXTO19

@ 45,8 SAY TEXTO20

@ 46,8 SAY TEXTO21

@ 47,8 SAY TEXTO22

@ 48,8 SAY TEXTO23


SET PRINT OFF
SET PRINTER TO
SET DEVICE TO SCREEN

ENDIF

      CASE resp="v"

        return
endcase

enddo

return

*********fin del procedimiento CARTA

************************************
************* PROCEDIMIENTO GESTION
************************************

procedure gestion

	use pagos && PARA QUE NO ORDENE LOS REGISTROS NO ABRO LOS INDICES
	
	go bottom
	browse FIELDS nif,IMPORTE,RECEPTOR,FECHA,VENCIMIENT,NUMERO,NUMFAC1,NUMFAC2,NUMFAC3,NUMFAC4,NUMFAC5,NUMFAC6,NUMFAC7,NUMFAC8,DESFAC1,DESFAC2,DESFAC3,DESFAC4
	SET EXCLUSIVE on
	USE pagos INDEX in1numero
	reindex
    SET EXCLUSIVE off
RETURN



*********** FIN DEL PROCEDIMIENTO GESTION




*************************************************
*************PROCEDIMIENTO RESUMEN DE PAGOS por proveedor
********************************************
********************************
PROCEDURE RESPAGprov
LOCAL WIMPORTE
WNIF="          "
WIMPORTE=0
WFECHA=CTOD("  -  -    ")
WVENCIMIENT=CTOD("  -  -    ")
USE PAGOS INDEX in1numero
set century on
CLEAR
@ 5,5 SAY "LISTADO DE PAGOS"

@ 10,2 SAY "INTRODUCE FECHA   " GET WFECHA
@ 11,2 SAY "FECHA VENCIMIENTO " GET WVENCIMIENT 
@ 12,2 say "NIF PROVEEDOR     " GET WNIF 
@ 13,2 SAY "EN EL MENU DE IMPRESION PULSAR PROPIEDADES Y ELEGIR HORIZONTAL"
READ
     XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivoA.txt
	   ELSE 
	    CLEAR 
	    @ 1,1 SAY "PULSA PROPIEDADES EN EL CONTROLADOR DE LA IMPRESORA"
	    @ 2,1 SAY "Y ELIJE HORIZONTAL"
	    WAIT WINDOW
	    
	    SET PRINT ON prompt
	    set device to print
	    
	    SET PRINT FONT "COURIER NEW",7
	    
	ENDIF
set century off

SET CENTURY OFF
@ 2,1 SAY " IMPORTE  RECEPTOR   NIF      FECHA   VENCIMIENT  NUMERO   NUMFAC1  NUMFAC2   NUMFAC3   NUMFAC4   NUMFAC5   NUMFAC6   NUMFAC7   NUMFAC8   DESFAC1   DESFAC2   DESFAC3   DESFAC4 "      
@ 3,1 SAY "============================================================================================================================================================================"
GO TOP
DO WHILE .NOT.EOF()
  if (fecha=wfecha).AND.(VENCIMIENT=WVENCIMIENT).and.(NIF=wNIF) 
   @ PROW()+1,1 SAY STR(IMPORTE,8,2)
   @ PROW(),10 SAY RECEPTOR
   @ prow(),21 say nif
   @ PROW(),31 SAY DTOC(FECHA)
   @ PROW(),40 SAY DTOC(VENCIMIENT)
   @ PROW(),50 SAY NUMERO
   @ PROW(),60 SAY NUMFAC1       
   @ PROW(),70 SAY NUMFAC2
   @ PROW(),80 SAY NUMFAC3
   @ PROW(),90 SAY NUMFAC4
   @ PROW(),100 SAY NUMFAC5
   @ PROW(),110 SAY NUMFAC6
   @ PROW(),120 SAY NUMFAC7
   @ PROW(),130 SAY NUMFAC8
   @ PROW(),140 SAY desfac1
   @ PROW(),150 SAY DESFAC2
   @ PROW(),160 SAY DESFAC3
   @ PROW(),170 SAY DESFAC4
   *@ prow(),170 say nif
   
   WIMPORTE=WIMPORTE+IMPORTE   
  endif 
   SKIP
ENDDO
@ PROW()+3,1 SAY STR(WIMPORTE,10,2)
@ PROW()+1,1 SAY STR(WIMPORTE*166.386,12,2)

 do case
		   	case xx="I"
			   
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivoA.txt noedit
		        
		       delete file archivoA.txt
		  endcase

RETURN



