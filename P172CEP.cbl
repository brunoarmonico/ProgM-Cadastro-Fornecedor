       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172CEP.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

           FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
           01 REGCEP.
               02 CEP PIC 9(8).
               02 LOGRADOURO PIC X(35) VALUE SPACES.
               02 BAIRRO PIC X(20) VALUE SPACES.
               02 CIDADE PIC X(20) VALUE SPACES.
               02 UF PIC X(2) VALUE SPACES.
               02 REFERENCIA PIC X(35) VALUE SPACES.
               02 LATITUDE PIC X(15) VALUE SPACES.
               02 LONGITUDE PIC X(15) VALUE SPACES.

       WORKING-STORAGE SECTION.

           01 TABUF PIC X(54)
          VALUE"ACALAPAMBACEDFESGOMAMTMSMGPAPBPRPEPIRJRNRSRORRSCSPSETO".
           01 TAUF REDEFINES TABUF.
              03 TUFP PIC X(2) OCCURS 27 TIMES.
           01 TABUFM PIC X(54)
          VALUE"acalapambacedfesgomamtmsmgpapbprpepirjrnrsrorrscspseto".
           01 TAUFM REDEFINES TABUFM.
              03 TUFPM PIC X(2) OCCURS 27 TIMES.
           01 IND PIC 9(2) VALUE 1.
           01 ERRO PIC X(2) VALUE "00".
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 OPC PIC 9(1).
           01 AUX PIC X(01) VALUE SPACES.

       SCREEN SECTION.

       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 01  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05  LINE 02  COLUMN 01
               VALUE  "º                               CADASTRO".
           05  LINE 02  COLUMN 41
               VALUE  " DE CEP                                º".
           05  LINE 03  COLUMN 01
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 03  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05  LINE 04  COLUMN 01
               VALUE  "º  CEP:".
           05  LINE 04  COLUMN 41
               VALUE  "                                       º".
           05  LINE 05  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 05  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 06  COLUMN 01
               VALUE  "º  LOGRADOURO:".
           05  LINE 06  COLUMN 41
               VALUE  "           UF:                         º".
           05  LINE 07  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 07  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 08  COLUMN 01
               VALUE  "º  CIDADE:                      BAIRRO:".
           05  LINE 08  COLUMN 41
               VALUE  "                                       º".
           05  LINE 09  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 09  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 10  COLUMN 01
               VALUE  "º  REFERENCIA:".
           05  LINE 10  COLUMN 41
               VALUE  "                                       º".
           05  LINE 11  COLUMN 01
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 11  COLUMN 41
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 12  COLUMN 01
               VALUE  "º  LATITUDE:".
           05  LINE 12  COLUMN 41
               VALUE  " LONGITUDE:                            º".
           05  LINE 13  COLUMN 01
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 13  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05  LINE 14  COLUMN 01
               VALUE  "º".
           05  LINE 14  COLUMN 41
               VALUE  "                                       º".
           05  LINE 15  COLUMN 01
               VALUE  "º".
           05  LINE 15  COLUMN 41
               VALUE  "                                       º".
           05  LINE 16  COLUMN 01
               VALUE  "º".
           05  LINE 16  COLUMN 41
               VALUE  "                                       º".
           05  LINE 17  COLUMN 01
               VALUE  "º".
           05  LINE 17  COLUMN 41
               VALUE  "                                       º".
           05  LINE 18  COLUMN 01
               VALUE  "º".
           05  LINE 18  COLUMN 41
               VALUE  "                                       º".
           05  LINE 19  COLUMN 01
               VALUE  "º".
           05  LINE 19  COLUMN 41
               VALUE  "                                       º".
           05  LINE 20  COLUMN 01
               VALUE  "º".
           05  LINE 20  COLUMN 41
               VALUE  "                                       º".
           05  LINE 21  COLUMN 01
               VALUE  "º".
           05  LINE 21  COLUMN 41
               VALUE  "                                       º".
           05  LINE 22  COLUMN 01
               VALUE  "º".
           05  LINE 22  COLUMN 41
               VALUE  "                                       º".
           05  LINE 23  COLUMN 01
               VALUE  "º".
           05  LINE 23  COLUMN 41
               VALUE  "                                       º".
           05  LINE 24  COLUMN 01
               VALUE  "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 24  COLUMN 41
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05  TCEP
               LINE 04  COLUMN 08  PIC 99999.999
               USING  CEP
               HIGHLIGHT BLANK ZERO.
           05  TLOGRADOURO
               LINE 06  COLUMN 15  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TUF
               LINE 06  COLUMN 55  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TCIDADE
               LINE 08  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 40  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TREFERENCIA
               LINE 10  COLUMN 15  PIC X(35)
               USING  REFERENCIA
               HIGHLIGHT.
           05  TLATI
               LINE 12  COLUMN 13  PIC X(15)
               USING  LATITUDE
               HIGHLIGHT.
           05  TLONG
               LINE 12  COLUMN 52  PIC X(15)
               USING  LONGITUDE
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

      *ABRE ARQUIVO DE REGISTRO DE CEP
       ARQUIVO.
           OPEN I-O CADCEP
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADCEP
                   CLOSE CADCEP
                  DISPLAY "ARQUIVO CADCEP FOI CRIADO" AT 0622
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               CONTINUE.

      *EXIBE MENU PRINCIPAL
       INICIALIZA.
           DISPLAY TELACEP
           CONTINUE.

      *APRESENTA OPCOES DO MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1505
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           DISPLAY AUX ACTKEY AT 1605
               IF ACTKEY = 01
                   PERFORM LIMPATELA
                   DISPLAY TELACEP
                   GO TO R-CEP
               ELSE IF ACTKEY = 02
                   PERFORM LIMPATELA
                   DISPLAY "CEP: " AT 1605
                   ACCEPT CEP AT 1610
                   PERFORM PROCURA
               ELSE IF ACTKEY = 03
                   GO TO SAIR
               ELSE
                   DISPLAY "OPCAO INVALIDA" AT 1705
                   GO TO MENU.

      *RECEBE NUMERO DO CEP
       R-CEP.
           ACCEPT TCEP
           IF CEP = ZEROES
               PERFORM LIMPATELA
               DISPLAY "CEP INVALIDO" AT 1505
               GO TO R-CEP
           ELSE
               PERFORM LIMPATELA
               GO TO R-LOGR
           END-IF.

      *RECEBE LOGRADOURO
       R-LOGR.
           ACCEPT TLOGRADOURO
           IF LOGRADOURO = SPACES
               PERFORM LIMPATELA
               DISPLAY "LOGRADOURO INVALIDO" AT 1505
               GO TO R-LOGR
           ELSE
               PERFORM LIMPATELA
               GO TO R-UF
           END-IF.

      *RECEBE UF
       R-UF.
           ACCEPT TUF
           IF UF = SPACES
               PERFORM LIMPATELA
               DISPLAY "UF INVALIDO" AT 1505
               GO TO R-UF
           ELSE
               PERFORM LIMPATELA
               GO TO VALIDA-UF
           END-IF.

      *VERIFICA SE UF INSERIDO EXISTE
       VALIDA-UF.
           IF IND > 27
               DISPLAY "UF NAO ENCONTRADO" AT 1505
               MOVE 1 TO IND
               GO TO R-UF
           ELSE
               IF UF = TUFP(IND) OR UF = TUFPM(IND)
                   IF UF = "SP" OR UF = "sp"
                       DISPLAY "SAO PAULO" AT 0658
                   ELSE IF UF = "AC" OR UF = "ac"
                       DISPLAY "ACRE" AT 0658
                   ELSE IF UF = "RJ" OR UF = "rj"
                       DISPLAY "RIO DE JANEIRO" AT 0658
                   ELSE IF UF = "MG" OR UF = "mg"
                       DISPLAY "MINAS GERAIS" AT 0658
                   ELSE IF UF = "ES" OR UF = "es"
                       DISPLAY "ESPIRITO SANTO" AT 0658
                   ELSE IF UF = "PE" OR UF = "pe"
                       DISPLAY "PERNAMBUCO" AT 0658
                   END-IF
                   GO TO R-CIDADE
               ELSE
                   ADD 1 TO IND
                   GO TO VALIDA-UF
               END-IF
           END-IF
               CONTINUE.

      *RECEBE NOMA DA CIDADE
       R-CIDADE.
           ACCEPT TCIDADE
           IF CIDADE = SPACES
               PERFORM LIMPATELA
               DISPLAY "CIDADE INVALIDO" AT 1505
               GO TO R-CIDADE
           ELSE
               PERFORM LIMPATELA
               GO TO R-BAIRRO
           END-IF.

      *RECEBE NOME DO BAIRRO
       R-BAIRRO.
           ACCEPT TBAIRRO
           IF BAIRRO = SPACES
               PERFORM LIMPATELA
               DISPLAY "BAIRRO INVALIDO" AT 1505
               GO TO R-BAIRRO
           ELSE
               PERFORM LIMPATELA
               GO TO R-REFR
           END-IF.

      *RECEBE REFERENCIA
       R-REFR.
           ACCEPT TREFERENCIA
           IF REFERENCIA = SPACES
               PERFORM LIMPATELA
               DISPLAY "REFERENCIA INVALIDO" AT 1505
               GO TO R-REFR
           ELSE
               PERFORM LIMPATELA
               GO TO R-LATI
           END-IF.

      *RECEBE LATITUDE
       R-LATI.
           ACCEPT TLATI
           IF LATITUDE = SPACES
               PERFORM LIMPATELA
               DISPLAY "LATITUDE INVALIDO" AT 1505
               GO TO R-LATI
           ELSE
               PERFORM LIMPATELA
               GO TO R-LONG
           END-IF.

      *RECEBE LONGITUDE
       R-LONG.
           ACCEPT TLONG
           IF LONGITUDE = SPACES
               PERFORM LIMPATELA
               DISPLAY "LONGITUDE INVALIDA" AT 1505
               GO TO R-LONG
           ELSE
               PERFORM LIMPATELA
               GO TO LERCEP
           END-IF.

      *FAZ LEITURA DE DADOS NO ARQUIVO
       LERCEP.
           READ CADCEP
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   PERFORM LIMPATELA
                   DISPLAY "CEP JA CADASTRADO" AT 2010
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 2110
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       PERFORM LIMPATELA
                       PERFORM ALTERA
                       GO TO MENU
                   ELSE
                       GO TO MENU
                   END-IF
                 ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADCEP" AT 2010
                   GO TO SAIR
           CONTINUE.

      *ESCREVE DADOS NO ARQUIVO
       ESCRITA.
           PERFORM LIMPATELA
           WRITE REGCEP
                IF ERRO = "00" OR "02"
                      DISPLAY "DADOS GRAVADOS" AT 2010
                      GO TO MENU
                ELSE IF ERRO = "22"
                      DISPLAY "CADCEP JA EXISTE " AT 2010
                      GO TO MENU
                ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADCEP" AT 2010
                      GO TO MENU.

      *FAZ BUSCA DE DADOS NO ARQUIVO
       PROCURA.
           READ CADCEP
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TELACEP
                   DISPLAY "F1 DELETAR CEP, F2 VOLTAR" AT 1605
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADCEP
                       IF ERRO = "00"
                           PERFORM LIMPATELA
                           DISPLAY "CEP EXCLUIDO" AT 2110
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                           DISPLAY "ERRO AO REMOVER O CEP" AT 2110
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPATELA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "CEP NAO ENCONTRADO" AT 2010
               GO TO MENU
           END-IF.

      *ALTERA DADOS REGISTRADOS NO ARQUIVO
       ALTERA.
           REWRITE REGCEP.
           IF ERRO = "00" OR "02"
               DISPLAY "INFORMACOES DE CEP ALTERADAS" AT 2110
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR O CEP" AT 2110
           GO TO MENU.

      *LIMPA INFORMACOES NA TELA
       LIMPATELA.
           DISPLAY "                                         " AT 1405
           DISPLAY "                                         " AT 1505
           DISPLAY "                                         " AT 1605
           DISPLAY "                                         " AT 1705
           DISPLAY "                                         " AT 1805
           DISPLAY "                                         " AT 1905
           DISPLAY "                                         " AT 2005
           DISPLAY "                                         " AT 2105
           DISPLAY "                                         " AT 2205
           DISPLAY "                                         " AT 2305.

      *LIMPA VARIAVEIS DE MEMORIA
       LIMPAVAR.
           MOVE ZEROES TO CEP ACTKEY
           MOVE SPACES TO LOGRADOURO BAIRRO AUX
           MOVE SPACES TO CIDADE REFERENCIA UF LATITUDE LONGITUDE.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADCEP.
           END PROGRAM P172CEP.
