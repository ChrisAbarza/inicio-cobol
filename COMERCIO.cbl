      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                COMERCIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESPUESTA ASSIGN TO "G:\cobol\archivos\RESPUESTA.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  RESPUESTA
           RECORD CONTAINS 50 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-RESPUESTA             PIC X(50).

       WORKING-STORAGE SECTION.
       01  WS-GUIONES.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(48)     VALUE ALL "-".
           05 FILLER                 PIC X(01).

       01  WS-TIT-NUMVENTA.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(20)
                                     VALUE "NUM DE VENTA       :".
           05 WS-NUMVENTA            PIC ZZZZ9     VALUES ZERO.
           05 FILLER                 PIC X(24).

       01  WS-TIT-NOMBRE.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(20)
                                     VALUE "NOMBRE DEL PRODUCTO:".
           05 WS-NOMBRE              PIC X(25)     VALUES SPACES.
           05 FILLER                 PIC X(04).

       01  WS-TIT-PRECIO.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(20)
                                     VALUE "PRECIO DE VENTA    :".
           05 WS-PRECIO              PIC $$$$$9  VALUES ZERO.
           05 FILLER                 PIC X(20).

       PROCEDURE DIVISION.
       010-INICIO.
           PERFORM 020-INGRESA-DATOS THRU 020-FIN.
           PERFORM 030-MOSTRAR-DATOS THRU 030-FIN.
           PERFORM 040-ABRIR-ARCHIVOS THRU 040-FIN.
           PERFORM 050-IMPRIMIR-DATOS THRU 050-FIN.
           GOBACK.

       020-INGRESA-DATOS.
           DISPLAY "INGRESAR PRODUCTO:"
           ACCEPT WS-NOMBRE
           DISPLAY "INGRESAR NUM DE VENTA:"
           ACCEPT WS-NUMVENTA
           DISPLAY "INGRESAR PRECIO:"
           ACCEPT WS-PRECIO.
       020-FIN. EXIT.

       030-MOSTRAR-DATOS.
           DISPLAY "NUM DE VENTA       : " WS-NUMVENTA
           DISPLAY "NOMBRE DEL PRODUCTO: " WS-NOMBRE
           DISPLAY "PRECIO DE VENTA    : " WS-PRECIO.
       030-FIN. EXIT.

       040-ABRIR-ARCHIVOS.
           OPEN OUTPUT RESPUESTA.

       040-FIN. EXIT.

       050-IMPRIMIR-DATOS.
           WRITE REG-RESPUESTA FROM WS-GUIONES AFTER ADVANCING PAGE
           WRITE REG-RESPUESTA FROM WS-TIT-NUMVENTA AFTER ADVANCING 1
           WRITE REG-RESPUESTA FROM WS-TIT-NOMBRE AFTER ADVANCING 1
           WRITE REG-RESPUESTA FROM WS-TIT-PRECIO AFTER ADVANCING 1
           WRITE REG-RESPUESTA FROM WS-GUIONES AFTER ADVANCING 1
           CLOSE RESPUESTA.
       050-FIN. EXIT.
