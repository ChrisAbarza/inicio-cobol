      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                NOTAS010.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.           IBM-3083.
       OBJECT-COMPUTER.           IBM-3083.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS ASSIGN TO "G:\cobol\archivos\ALUMNO.TXT".
           SELECT REP-NOTAS ASSIGN TO "G:\cobol\archivos\NOTAS.TXT".

       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS
           RECORD CONTAINS 41 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-ALUMNOS            PIC X(41).
       FD  REP-NOTAS
           RECORD CONTAINS 73 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-NOTAS              PIC X(73).

       WORKING-STORAGE SECTION.
       01  WS-AREAS-A-USAR.
           05 WS-REG-ALUMNOS.
               10 WS-NOMBRE-ALUM  PIC X(20).
               10 WS-RUT-ALUM     PIC 9(09).
               10 WS-EV1          PIC 9(03).
               10 WS-EV2          PIC 9(03).
               10 WS-EV3          PIC 9(03).
               10 WS-EV4          PIC 9(03).

           05 WS-PROM             PIC 9(03).
           05 WS-STATUS           PIC X(01).
           05 WS-CAN-ALUMNOS      PIC 9(03).
           05 WS-CAN-APRO         PIC 9(03).
           05 WS-CAN-REPR         PIC 9(03).
           05 WS-PROM-TOTAL       PIC 9(03)V99.
           05 WS-FECHA-8          PIC 9(06).
           05 SW-FIN              PIC X(03) VALUE SPACES.
           05 WS-CONT-NOTAS       PIC 9(04) VALUE ZERO.
           05 WS-SUM-PROM         PIC 9(04)V9 VALUE ZERO.

       01 WS-FECHA-GREG.
           05 WS-AA-8      PIC 9(2).
           05 WS-MM-8        PIC 9(2).
           05 WS-DD-8        PIC 9(2).
       01  WS-TITULO-1.
           05 FILLER              PIC X(17) VALUES SPACES.
           05 FILLER              PIC X(39)
                       VALUES "UNIVERSIDAD TECNOLOGICA DE CHILE INACAP".
           05 FILLER              PIC X(18) VALUES SPACES.

       01  WS-TITULO-2.
           05 FILLER              PIC X(09) VALUES " FECHA: ".
           05 WS-TIT-2-DIA        PIC 9(02).
           05 FILLER              PIC X(01) VALUES "/".
           05 WS-TIT-2-MES        PIC 9(02).
           05 FILLER              PIC X(03) VALUES "/20".
           05 WS-TIT-2-ANIO       PIC 9(02).
           05 FILLER              PIC X(09) VALUES SPACES.
           05 FILLER              PIC X(17) VALUES "INFORME CALCULO 1".
           05 FILLER              PIC X(28) VALUES SPACES.

       01  WS-GUIONES.
           05 FILLER              PIC X(01) VALUES SPACES.
           05 FILLER              PIC X(72)    VALUE ALL "-".

       01  WS-SUB-TITULO-1.
           05 FILLER              PIC X(04) VALUES SPACES.
           05 FILLER              PIC X(03)  VALUE "RUT".
           05 FILLER              PIC X(13) VALUES SPACES.
           05 FILLER              PIC X(06)  VALUE "NOMBRE".
           05 FILLER              PIC X(09) VALUES SPACES.
           05 FILLER              PIC X(05)  VALUE "NOTA1".
           05 FILLER              PIC X(02) VALUES SPACES.
           05 FILLER              PIC X(05)  VALUE "NOTA2".
           05 FILLER              PIC X(02) VALUES SPACES.
           05 FILLER              PIC X(05)  VALUE "NOTA3".
           05 FILLER              PIC X(02) VALUES SPACES.
           05 FILLER              PIC X(05)  VALUE "NOTA4".
           05 FILLER              PIC X(02) VALUES SPACES.
           05 FILLER              PIC X(04)  VALUE "PROM".
           05 FILLER              PIC X(02) VALUES SPACES.
           05 FILLER              PIC X(03)  VALUE "FIN".
           05 FILLER              PIC X(01) VALUES SPACES.

       01  WS-DETALLE.
           05 FILLER              PIC X(01) VALUES SPACES.
           05 WS-DET-RUT          PIC 9(09).
           05 FILLER              PIC X(02) VALUES SPACES.
           05 WS-DET-NOMBRE       PIC X(20).
           05 FILLER              PIC X(04) VALUES SPACES.
           05 WS-DET-EV1          PIC ZZ9.
           05 FILLER              PIC X(04) VALUES SPACES.
           05 WS-DET-EV2          PIC ZZ9.
           05 FILLER              PIC X(04) VALUES SPACES.
           05 WS-DET-EV3          PIC ZZ9.
           05 FILLER              PIC X(04) VALUES SPACES.
           05 WS-DET-EV4          PIC ZZ9.
           05 FILLER              PIC X(04) VALUES SPACES.
           05 WS-DET-PROM         PIC ZZ9.
           05 FILLER              PIC X(03) VALUES SPACES.
           05 WS-DET-STATUS       PIC X(01).
           05 FILLER              PIC X(02) VALUES SPACES.

       01  WS-DETALLE-TOT-ALUM.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(21)
                                  VALUE "TOTAL DE ALUMNOS   : ".
           05 WS-TOT-ALUM         PIC ZZ9.
           05 FILLER              PIC X(48) VALUES SPACES.

       01  WS-DETALLE-TOT-APRO.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(21)
                                  VALUE "TOTAL DE APROBADOS : ".
           05 WS-TOT-APRO         PIC ZZ9.
           05 FILLER              PIC X(48) VALUES SPACES.

       01  WS-DETALLE-TOT-REPRO.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(21)
                                  VALUE "TOTAL DE REPROBADOS: ".
           05 WS-TOT-REPRO         PIC ZZ9.
           05 FILLER              PIC X(48) VALUES SPACES.

       01  WS-DETALLE-TOT-PROM.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(21)
                                  VALUE "PROMEDIO GENERAL   : ".
           05 WS-TOT-PROM         PIC ZZ9V,99.
           05 FILLER              PIC X(48) VALUES SPACES.

       PROCEDURE DIVISION.
       010-MAIN.
           PERFORM 020-ABRIR-DOC THRU 020-FIN.
           PERFORM 030-ESCRIBIR-TITULO THRU 030-FIN.
           PERFORM 040-LEER THRU 040-FIN.
           PERFORM 040-PROCESO  THRU 040-FIN
                   UNTIL SW-FIN EQUAL "FIN".
           PERFORM 050-ESCRIBIR-FOOT THRU 050-FIN
           PERFORM 100-CERRAR-DOC THRU 100-FIN.
           GOBACK.

       020-ABRIR-DOC.
           OPEN OUTPUT REP-NOTAS
                INPUT ALUMNOS.

       020-FIN. EXIT.

       030-ESCRIBIR-TITULO.
           ACCEPT WS-FECHA-8 FROM DATE.
           MOVE WS-FECHA-8 TO WS-FECHA-GREG.
           MOVE WS-AA-8 TO WS-TIT-2-ANIO.
           MOVE WS-MM-8 TO WS-TIT-2-MES.
           MOVE WS-DD-8 TO WS-TIT-2-DIA.

           WRITE REG-NOTAS FROM WS-TITULO-1 AFTER ADVANCING PAGE.
           WRITE REG-NOTAS FROM WS-TITULO-2 AFTER ADVANCING 2.
           WRITE REG-NOTAS FROM WS-GUIONES AFTER ADVANCING 1.
           WRITE REG-NOTAS FROM WS-SUB-TITULO-1 AFTER ADVANCING 1.
           WRITE REG-NOTAS FROM WS-GUIONES AFTER ADVANCING 1.
       030-FIN. EXIT.

       040-PROCESO.

           ADD 1 TO WS-CAN-ALUMNOS
           MOVE WS-NOMBRE-ALUM TO WS-DET-NOMBRE
           MOVE WS-RUT-ALUM TO WS-DET-RUT
           MOVE WS-EV1 TO WS-DET-EV1
           MOVE WS-EV2 TO WS-DET-EV2
           MOVE WS-EV3 TO WS-DET-EV3
           MOVE WS-EV4 TO WS-DET-EV4

           COMPUTE WS-PROM = (WS-EV1 + WS-EV2 + WS-EV3 + WS-EV4)/4
           MOVE WS-PROM TO WS-DET-PROM
           ADD WS-PROM TO WS-SUM-PROM.

           IF WS-PROM >=60
               MOVE "A" TO WS-STATUS
               ADD 1 TO WS-CAN-APRO
           ELSE
               MOVE "R" TO WS-STATUS
               ADD 1 TO WS-CAN-REPR
           END-IF.

           MOVE WS-STATUS TO WS-DET-STATUS.

           WRITE REG-NOTAS FROM WS-DETALLE AFTER ADVANCING 1.
       040-LEER.
           READ ALUMNOS INTO WS-REG-ALUMNOS AT END
                MOVE "FIN" TO SW-FIN.
       040-FIN. EXIT.

       050-ESCRIBIR-FOOT.
           MOVE WS-CAN-ALUMNOS TO WS-TOT-ALUM.
           WRITE REG-NOTAS FROM WS-GUIONES AFTER ADVANCING 1.
           WRITE REG-NOTAS FROM WS-DETALLE-TOT-ALUM AFTER ADVANCING 1.

           MOVE WS-CAN-APRO TO WS-TOT-APRO
           WRITE REG-NOTAS FROM WS-DETALLE-TOT-APRO AFTER ADVANCING 1.

           MOVE WS-CAN-REPR TO WS-TOT-REPRO
           WRITE REG-NOTAS FROM WS-DETALLE-TOT-REPRO AFTER ADVANCING 1.

           COMPUTE WS-PROM-TOTAL = WS-SUM-PROM / WS-CAN-ALUMNOS.
           MOVE WS-PROM-TOTAL TO WS-TOT-PROM
           WRITE REG-NOTAS FROM WS-DETALLE-TOT-PROM AFTER ADVANCING 1.
       050-FIN. EXIT.

       100-CERRAR-DOC.
           CLOSE REP-NOTAS
                 ALUMNOS.
       100-FIN. EXIT.
