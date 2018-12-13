      *AUTOR. BRUNO C.
      *ADS-4 TARDE 2017
      *
      *RODAR NO OPEN COBOL IDE
      *COMPILADOR DEFAULT OU MF
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P172FOR.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFORN ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CNPJ
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS RSOCIAL WITH DUPLICATES
               ALTERNATE RECORD KEY IS NOMEF WITH DUPLICATES.

           SELECT CADCEP ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS CEP
               FILE STATUS  IS ERRO
               ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
           FD CADFORN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFORN.DAT".

           01 REGFORN.
               02 CNPJ         PIC 9(15).
               02 RSOCIAL      PIC X(35) VALUE SPACES.
               02 NOMEF        PIC X(12) VALUE SPACES.
               02 IE           PIC X(20) VALUE SPACES.
               02 FCEP PIC 9(8).
               02 NUMERO       PIC 9(5).
               02 COMPLEMENTO  PIC X(12) VALUE SPACES.
               02 TEL1.
                   03 DDD1     PIC 9(3).
                   03 NUMERO1  PIC 9(9).
               02 TEL2.
                   03 DDD2     PIC 9(3).
                   03 NUMERO2  PIC 9(9).
               02 CONTATO      PIC X(30) VALUE SPACES.
               02 CARGO        PIC X(1)  VALUE SPACES.
               02 EMAIL        PIC X(35) VALUE SPACES.
               02 RAMO         PIC 9(1).

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
           01 ERRO PIC X(2) VALUE "00".
           01 AUX PIC X(01) VALUE SPACES.
           01 ACTKEY PIC 9(02) VALUE ZEROES.
           01 BUSCACEP PIC 9(1) VALUE 0.
           01 EXISTENCIA PIC 9(1) VALUE 0.

       SCREEN SECTION.

       01  TELAFORN.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 02  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 03  COLUMN 01
               VALUE  "บ                           CADASTRO DE".
           05  LINE 03  COLUMN 41
               VALUE  "FORNECEDORES                           บ".
           05  LINE 04  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 04  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 05  COLUMN 01
               VALUE  "บ  CNPJ:                     RAZAO SOCIA".
           05  LINE 05  COLUMN 41
               VALUE  "L:                                     บ".
           05  LINE 06  COLUMN 01
               VALUE  "บ  NOME FANTASIA:                  INSCR".
           05  LINE 06  COLUMN 41
               VALUE  "ICAO ESTADUAL:                         บ".
           05  LINE 07  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 07  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 08  COLUMN 01
               VALUE  "บ  CEP:           LOGRADOURO:".
           05  LINE 08  COLUMN 41
               VALUE  "                           NUM:        บ".
           05  LINE 09  COLUMN 01
               VALUE  "บ  COMPLEMENTO:                 BAIRRO:".
           05  LINE 09  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01
               VALUE  "บ  CIDADE:".
           05  LINE 10  COLUMN 41
               VALUE  "      UF:                              บ".
           05  LINE 11  COLUMN 01
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 11  COLUMN 41
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 12  COLUMN 01
               VALUE  "บ  TELEFONE 1:".
           05  LINE 12  COLUMN 41
               VALUE  "      TELEFONE 2:                      บ".
           05  LINE 13  COLUMN 01
               VALUE  "บ  CONTATO:".
           05  LINE 13  COLUMN 41
               VALUE  "      CARGO:                           บ".
           05  LINE 14  COLUMN 01
               VALUE  "บ  EMAIL:".
           05  LINE 14  COLUMN 41
               VALUE  "      RAMO DE ATIVIDADE:               บ".
           05  LINE 15  COLUMN 01
               VALUE  "ฬอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 15  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออน".
           05  LINE 16  COLUMN 01
               VALUE  "บ".
           05  LINE 16  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 17  COLUMN 01
               VALUE  "บ".
           05  LINE 17  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 18  COLUMN 01
               VALUE  "บ".
           05  LINE 18  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 19  COLUMN 01
               VALUE  "บ".
           05  LINE 19  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 20  COLUMN 01
               VALUE  "บ".
           05  LINE 20  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 21  COLUMN 01
               VALUE  "บ".
           05  LINE 21  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 22  COLUMN 01
               VALUE  "บ".
           05  LINE 22  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 23  COLUMN 01
               VALUE  "บ".
           05  LINE 23  COLUMN 41
               VALUE  "                                       บ".
           05  LINE 24  COLUMN 01
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TCNPJ
               LINE 05  COLUMN 09  PIC 999.999.999/9999.99
               USING  CNPJ
               HIGHLIGHT BLANK ZERO.

           05  TRAZAO
               LINE 05  COLUMN 43  PIC X(35)
               USING  RSOCIAL
               HIGHLIGHT.

           05  TNOME
               LINE 06  COLUMN 18  PIC X(12)
               USING  NOMEF
               HIGHLIGHT.

           05  TIE
               LINE 06  COLUMN 55  PIC X(20)
               USING  IE
               HIGHLIGHT.

           05  TCEP
               LINE 08  COLUMN 08  PIC 9(08)
               USING  CEP
               HIGHLIGHT BLANK ZERO.

           05  TLOGRA
               LINE 08  COLUMN 30  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.

           05  TNUM
               LINE 08  COLUMN 72  PIC 9(06)
               USING  NUMERO
               HIGHLIGHT BLANK ZERO.

           05  TCOMPLEMENTO
               LINE 09  COLUMN 16  PIC X(12)
               USING  COMPLEMENTO
               HIGHLIGHT.

           05  TBAIRRO
               LINE 09  COLUMN 40  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.

           05  TCIDADE
               LINE 10  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.

           05  TUF
               LINE 10  COLUMN 50  PIC X(02)
               USING  UF
               HIGHLIGHT.

           05  TTEL1
               LINE 12  COLUMN 15  PIC 9(12)
               USING  TEL1
               HIGHLIGHT BLANK ZERO.

           05  TTEL2
               LINE 12  COLUMN 58  PIC 9(12)
               USING  TEL2
               HIGHLIGHT BLANK ZERO.

           05  TCONTATO
               LINE 13  COLUMN 12  PIC X(30)
               USING  CONTATO
               HIGHLIGHT.

           05  TCARGO
               LINE 13  COLUMN 53  PIC X(01)
               USING  CARGO
               HIGHLIGHT.

           05  TMAIL
               LINE 14  COLUMN 10  PIC X(35)
               USING  EMAIL
               HIGHLIGHT.

           05  TRAMO
               LINE 14  COLUMN 65  PIC 9(01)
               USING  RAMO
               HIGHLIGHT BLANK ZERO.


       PROCEDURE DIVISION.
      *ABRE O ARQUIVO DE REGISTRO DOS FORNECEDORES
       ARQUIVO.
           OPEN I-O CADFORN
           IF ERRO NOT = "00"
               IF ERRO = "30" OR ERRO = "35"
                   OPEN OUTPUT CADFORN
                   CLOSE CADFORN
                  DISPLAY "ARQUIVO CADCEP FOI CRIADO" AT 0622
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA ABERTURA DO ARQUIVO CADCEP" AT 0622
           ELSE
               CONTINUE.

      *EXIBE TELA DE MENU PRINCIPAL
       INICIALIZA.
           DISPLAY TELAFORN
           CONTINUE.

      *APRESENTA OPCOES DO MENU PRINCIPAL
       MENU.
           PERFORM LIMPAVAR
           DISPLAY "F1 CADASTRAR, F2 PROCURAR, F3 SAIR" AT 1605
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           DISPLAY AUX ACTKEY AT 1705
               IF ACTKEY = 01
                   PERFORM LIMPATELA
                   DISPLAY TELAFORN
                   GO TO R-CNPJ
               ELSE IF ACTKEY = 02
                   PERFORM LIMPATELA
                   DISPLAY "CNPJ: " AT 1705
                   ACCEPT CNPJ AT 1712
                   PERFORM BUSCA
               ELSE IF ACTKEY = 03
                   GO TO SAIR
               ELSE
                   DISPLAY "OPCAO INVALIDA" AT 1705
                   GO TO MENU.

      *RECEBE CNPJ
       R-CNPJ.
           ACCEPT TCNPJ
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               PERFORM LIMPAVAR
               GO TO MENU
           ELSE IF CNPJ = ZEROES
               DISPLAY "CNPJ INVALIDO" AT 1605
               GO TO R-CNPJ
           ELSE
               PERFORM LERFORN
           CONTINUE.

      *RECEBE RAZAO SOCIAL
       R-RAZAO.
           ACCEPT TRAZAO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO RSOCIAL
               GO TO R-CNPJ
           ELSE IF RSOCIAL = SPACES
               DISPLAY "RAZAO SOCIAL INVALIDA" AT 1605
               GO TO R-RAZAO
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE NOME FANTASIA
       R-NOME.
           ACCEPT TNOME
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO NOMEF
               GO TO R-RAZAO
           ELSE IF NOMEF = SPACES
               DISPLAY "NOME FANTASIA INVALIDO" AT 1605
               GO TO R-NOME
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE INSCRICAO ESTADUAL
       R-IE.
           ACCEPT TIE
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO IE
               GO TO R-NOME
           ELSE IF IE = SPACES
               DISPLAY "INSCRICAO ESTADUAL INVALIDA" AT 1605
               GO TO R-IE
           ELSE
               PERFORM LIMPATELA

               CONTINUE.

      *RECEBE CEP
       R-CEP.
           MOVE 0 TO BUSCACEP
           ACCEPT TCEP
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO CEP
               GO TO R-IE
           ELSE IF CEP = ZEROES
               DISPLAY "CEP INVALIDO" AT 1605
               GO TO R-CEP
           ELSE
               PERFORM LIMPATELA
               PERFORM LERCEP
               IF BUSCACEP = 1
                   GO TO R-CEP
               ELSE
                   MOVE CEP TO FCEP
                   CONTINUE.

      *RECEBE NUMERO DO IMOVEL
       R-NUM.
           ACCEPT TNUM
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO NUMERO
               GO TO R-CEP
           ELSE IF NUMERO = ZEROES
               DISPLAY "NUMERO INVALIDO" AT 1605
               GO TO R-NUM
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE COMPLEMENTO DE ENDERECO
       R-COMP.
           ACCEPT TCOMPLEMENTO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO COMPLEMENTO
               GO TO R-NUM
           END-IF
           CONTINUE.

      *RECEBE NUMERO DE TELEFONE 1
       R-TEL1.
           ACCEPT TTEL1
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO TEL1
               GO TO R-COMP
           ELSE IF TEL1 = ZEROES
               DISPLAY "TELEFONE 1 INVALIDO" AT 1605
               GO TO R-TEL1
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE NUMERO DE TELEFONE 2
       R-TEL2.
           ACCEPT TTEL2
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO TEL2
               GO TO R-TEL1
           END-IF
           CONTINUE.

      *RECEBE CONTATO
       R-CONTATO.
           ACCEPT TCONTATO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO CONTATO
               GO TO R-TEL2
           ELSE IF CONTATO = SPACES
               DISPLAY "CONTATO INVALIDO" AT 1705
               GO TO R-CONTATO
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE CARGO DO REPRESENTANTE
       R-CARGO.
           DISPLAY "G = GERENTE DE VENDAS, R = REPRESENTANTE" AT 1805
           DISPLAY "V = VENDEDOR, T = TECNICO, E = ENGENHEIRO" AT 1905
           DISPLAY "D = DIRETOR, O = OUTROS" AT 2005
           ACCEPT TCARGO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO CARGO
               GO TO R-CONTATO
           END-IF
           DISPLAY "                    " AT 1355
           IF CARGO = "G" OR CARGO = "g"
               DISPLAY "GERENTE DE VENDA" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "R" OR CARGO = "r"
               DISPLAY "REPRESENTANTE" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "V" OR CARGO = "v"
               DISPLAY "VENDEDOR" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "T" OR CARGO = "t"
               DISPLAY "TECNICO" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "E" OR CARGO = "e"
               DISPLAY "ENGENHEIRO" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "D" OR CARGO = "d"
               DISPLAY "DIRETOR" AT 1355
               GO TO R-MAIL
           ELSE IF CARGO = "O" OR CARGO = "o"
               DISPLAY "OUTROS" AT 1355
               GO TO R-MAIL
           ELSE
               DISPLAY "CARGO INVALIDO" AT 1605
               GO TO R-CARGO.

      *RECEBE E-MAIL DO REPRESENTANTE
       R-MAIL.
           ACCEPT TMAIL
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE SPACES TO EMAIL
               GO TO R-CARGO
           ELSE IF EMAIL = SPACES
               DISPLAY "E-MAIL INVALIDO" AT 1605
               GO TO R-MAIL
           ELSE
               PERFORM LIMPATELA
               CONTINUE.

      *RECEBE RAMO DO FORNECEDOR
       R-RAMO.
           ACCEPT TRAMO
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               PERFORM LIMPATELA
               MOVE ZEROES TO RAMO
               GO TO R-MAIL
           END-IF
           DISPLAY "                    " AT 1467
           IF RAMO = 1
               DISPLAY "METALURGICO" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 2
               DISPLAY "MECANICO" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 3
               DISPLAY "PLASTICO" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 4
               DISPLAY "FUNDICAO" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 5
               DISPLAY "TRANSPORTE" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 6
               DISPLAY "CONSULTORIA" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 7
               DISPLAY "PROJETOS" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 8
               DISPLAY "SERVICOS" AT 1467
               GO TO CONTINUA
           ELSE IF RAMO = 9
               DISPLAY "OUTROS" AT 1467
               GO TO CONTINUA
           ELSE
               DISPLAY "RAMO INVALIDO" AT 1605
               GO TO R-RAMO.

      *VERIFICA SE JA ESTA REGISTRADO NO ARQUIVO
       CONTINUA.
           IF EXISTENCIA = 0
               GO TO ESCRITA
           ELSE
               GO TO ALTERA
           END-IF.

      *FAZ LEITURA DE DADOS NO ARQUIVO
       LERFORN.
           READ CADFORN
              IF ERRO NOT = "23"
                 IF ERRO = "00"
                   PERFORM LIMPATELA
                   DISPLAY "FORNECEDOR JA CADASTRADO" AT 2010
                   DISPLAY TELAFORN
                   DISPLAY "F1 ALTERAR, F2 CANCELAR" AT 2110
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       MOVE 1 TO EXISTENCIA
                       PERFORM LIMPATELA
                       GO TO R-RAZAO
                   ELSE
                       PERFORM LIMPATELA
                       PERFORM LIMPAVAR
                       GO TO MENU
                   END-IF
                 ELSE
                   DISPLAY "ERRO NA LEITURA ARQUIVO CADFORN" AT 2010
                   GO TO SAIR
           CONTINUE.

      *ESCREVE DADOS NO ARQUIVO
       ESCRITA.
           PERFORM LIMPATELA
           DISPLAY "GRAVAR DADOS?" AT 1605
           DISPLAY "F1 - SIM, F2 - NAO" AT 1705
           ACCEPT AUX AT 2360
           ACCEPT ACTKEY FROM ESCAPE KEY
           IF ACTKEY = 01
               WRITE REGFORN
               IF ERRO = "00" OR "02"
                   DISPLAY "DADOS GRAVADOS" AT 2010
                   GO TO MENU
               ELSE IF ERRO = "22"
                   DISPLAY "CADFORN JA EXISTE " AT 2010
                   GO TO MENU
               ELSE
                  DISPLAY "ERRO NA GRAVACAO DO ARQUIVO CADFORN" AT 2010
                   GO TO MENU
           ELSE
               PERFORM LIMPAVAR
               PERFORM LIMPATELA
           GO TO MENU.

      *BUSCA DADOS NO ARQUIVO
       BUSCA.
           READ CADFORN
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   MOVE FCEP TO CEP
                   PERFORM LERCEP
                   DISPLAY TELAFORN
                   DISPLAY "F1 DELETAR CEP, F2 VOLTAR" AT 1605
                   ACCEPT AUX AT 2360
                   ACCEPT ACTKEY FROM ESCAPE KEY
                   IF ACTKEY = 01
                       DELETE CADFORN
                       IF ERRO = "00"
                           PERFORM LIMPATELA
                           DISPLAY "FORNECEDOR EXCLUIDO" AT 2110
                           GO TO MENU
                       ELSE
                           PERFORM LIMPATELA
                           DISPLAY "ERRO AO REMOVER FORNECEDOR" AT 2110
                           GO TO MENU
                       END-IF
                   ELSE
                       PERFORM LIMPATELA
                       GO TO MENU
                   END-IF
               END-IF
           ELSE
               PERFORM LIMPATELA
               DISPLAY "FORNECEDOR NAO ENCONTRADO" AT 2010
               GO TO MENU
           END-IF.

      *ALTERA DADOS NO ARQUIVO
       ALTERA.
           REWRITE REGFORN
           IF ERRO = "00" OR "02"
               DISPLAY "INFORMACOES DE FORNECEDOR ALTERADAS" AT 2110
               GO TO MENU
           ELSE
               DISPLAY "ERRO AO ALTERAR O FORNECEDOR" AT 2110
           GO TO MENU.

      *LIMPA INFORMACOES NA TELA
       LIMPATELA.
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
           MOVE ZEROES TO CEP ACTKEY CNPJ NUMERO TEL1 TEL2
           MOVE ZEROES TO EXISTENCIA RAMO BUSCACEP
           MOVE SPACES TO LOGRADOURO BAIRRO AUX RSOCIAL NOMEF IE
           MOVE SPACES TO CIDADE REFERENCIA UF LATITUDE LONGITUDE
           MOVE SPACES TO COMPLEMENTO CONTATO CARGO EMAIL.

      *BUSCA E FAZ LEITURA DE DADOS DO CEP
       LERCEP.
           OPEN I-O CADCEP
           READ CADCEP
           IF ERRO NOT = "23"
               IF ERRO = "00"
                   DISPLAY TLOGRA
                   DISPLAY TBAIRRO
                   DISPLAY TCIDADE
                   DISPLAY TBAIRRO
                   DISPLAY TUF
                   CLOSE CADCEP
               ELSE
                   DISPLAY "ERRO AO LER O CEP" AT 2005
                   PERFORM LIMPAVAR
                   GO TO MENU
               END-IF
           ELSE
               MOVE 1 TO BUSCACEP
               DISPLAY "CEP NAO ENCONTRADO" AT 1605
           END-IF.

      *FIM DO PROGRAMA
       SAIR.
           CLOSE CADFORN.
           END PROGRAM P172FOR.
