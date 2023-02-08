# SAP-ABAP
Programa de comparação entre 3 Times 
*&---------------------------------------------------------------------*
*& Report ZCOMPARA_TIME
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcompara_time.
*&---------------------------------------------------------------------*
*&                           DECLARAÇÃO
*&---------------------------------------------------------------------*

TABLES: ztitulos,
        ztimes.

TYPES: BEGIN OF ty_ztitulos,
         numtime    TYPE ztitulos-numtime,
         ntime      TYPE ztitulos-ntime,
         quantidade TYPE ztitulos-quantidade,
         ncamp      TYPE ztitulos-ncamp,
         peso       TYPE ztitulos-peso,
       END OF ty_ztitulos.

TYPES: BEGIN OF ty_ztimes,
         numtime     TYPE ztimes-numtime,
         ntime       TYPE ztimes-ntime,
         mascote     TYPE ztimes-mascote,
         totaltitulo TYPE ztimes-totaltitulo,
       END OF ty_ztimes.

TYPES:BEGIN OF ty_saida,
        numtime     TYPE ztitulos-numtime,
        ntime       TYPE ztitulos-ntime,
        quantidade  TYPE ztitulos-quantidade,
        ncamp       TYPE ztitulos-ncamp,
        peso        TYPE ztitulos-peso,
        mascote     TYPE ztimes-mascote,
        totaltitulo TYPE ztimes-totaltitulo,
        total       TYPE p DECIMALS 2,
      END OF ty_saida.

DATA: ti_ztitulos TYPE STANDARD TABLE OF ty_ztitulos,
      ti_ztimes   TYPE STANDARD TABLE OF ty_ztimes,
      ti_saida    TYPE STANDARD TABLE OF ty_saida.

DATA: wa_ztitulos LIKE LINE OF ti_ztitulos,
      wa_ztimes   LIKE LINE OF ti_ztimes,
      wa_saida    LIKE LINE OF ti_saida.

*Declaração para o ALV------------------------------------------------
*Pool de declarações do ALV.
TYPE-POOLS: kkblo.
*Variáveis do AVL
DATA: v_repid LIKE sy-repid,
      v_save  TYPE c.
*Tabelas internas do ALV.
DATA: it_fcat     TYPE slis_t_fieldcat_alv,
      it_fieldcat TYPE kkblo_t_fieldcat,
      it_header   TYPE kkblo_t_listheader,
      it_sort     TYPE slis_t_sortinfo_alv.
*Works Area do AVL
DATA: wa_variant TYPE disvariant,
      wa_afield  TYPE kkblo_fieldcat,
      wa_colinfo TYPE kkblo_specialcol,
      wa_layout  TYPE kkblo_layout,
      wa_header  TYPE kkblo_listheader,
      wa_lay     TYPE slis_layout_alv,
      wa_sort    LIKE LINE OF it_sort.
*** Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_ntime FOR ztimes-ntime OBLIGATORY.

*PARAMETERS: P_SPRAS TYPE ZMAKTCR-SPRAS.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_minei AS CHECKBOX,
            p_brasi AS CHECKBOX,
            p_cpbra AS CHECKBOX,
            p_liber AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*&                          PROCESSAMENTO
*&---------------------------------------------------------------------*

START-OF-SELECTION.

* Busca a tabela
  PERFORM busca_ztitulos.

  PERFORM busca_ztimes.

* Processa dados
  PERFORM processa_dados.

*Imprime o relatório com o processo de AVL
  PERFORM imprime_alv.

END-OF-SELECTION.
*Limpa todas as estruturas
  PERFORM limpa_tudo.

*&---------------------------------------------------------------------*
*&                            SUB-ROTINAS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUSCA_ZTITULOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_ztitulos .

  IF p_minei IS INITIAL AND
     p_brasi IS INITIAL AND
     p_cpbra IS INITIAL AND
     p_liber IS INITIAL.

    SELECT  numtime
            ntime
            quantidade
            ncamp
            peso
      INTO TABLE ti_ztitulos
      FROM ztitulos
      WHERE ntime IN s_ntime.
  ENDIF.

  IF p_minei IS NOT INITIAL.

    SELECT  numtime
            ntime
            quantidade
            ncamp
            peso
      APPENDING TABLE ti_ztitulos
      FROM ztitulos
      WHERE ntime IN s_ntime
        AND ncamp = 'CAMP MINEIRO'.
  ENDIF.

  IF p_brasi IS NOT INITIAL.

    SELECT  numtime
            ntime
            quantidade
            ncamp
            peso
      APPENDING TABLE ti_ztitulos
      FROM ztitulos
      WHERE ntime IN s_ntime
        AND ncamp = 'CAMP BRASILEIRO'.
  ENDIF.

  IF p_cpbra IS NOT INITIAL.

    SELECT  numtime
            ntime
            quantidade
            ncamp
            peso
      APPENDING TABLE ti_ztitulos
      FROM ztitulos
      WHERE ntime IN s_ntime
        AND ncamp = 'COPA DO BRASIL'.
  ENDIF.

  IF p_liber IS NOT INITIAL.

    SELECT  numtime
            ntime
            quantidade
            ncamp
            peso
      APPENDING TABLE ti_ztitulos
      FROM ztitulos
      WHERE ntime IN s_ntime
        AND ncamp = 'LIBERTADORES'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TIMES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_ztimes .

  IF NOT ti_ztitulos[] IS INITIAL.
    SELECT numtime
           ntime
           mascote
           totaltitulo
      INTO TABLE ti_ztimes
      FROM ztimes
      FOR ALL ENTRIES IN ti_ztitulos
      WHERE ntime = ti_ztitulos-ntime.

    IF sy-subrc = 0.
      SORT ti_ztimes BY ntime.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  LOOP AT ti_ztitulos INTO wa_ztitulos.
    wa_saida-numtime      = wa_ztitulos-numtime.
    wa_saida-ntime        = wa_ztitulos-ntime.
    wa_saida-quantidade   = wa_ztitulos-quantidade.
    wa_saida-ncamp        = wa_ztitulos-ncamp.
    wa_saida-peso         = wa_ztitulos-peso.
    wa_saida-total        = wa_ztitulos-quantidade * wa_ztitulos-peso.

    READ TABLE ti_ztimes INTO wa_ztimes WITH KEY ntime = wa_ztitulos-ntime.

    IF sy-subrc = 0.
      wa_saida-totaltitulo  = wa_ztimes-totaltitulo.
      wa_saida-mascote      = wa_ztimes-mascote.
    ENDIF.
    APPEND wa_saida TO ti_saida.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_alv .

*Carrega dados do cabeçalho
  PERFORM: f_alv_header.
*Define a indexação da impressão
*PERFORM: F_ALV_SORT.
*Define as características dos campos
  PERFORM: f_alv_fieldcat.
*Imprime o relatório
  PERFORM: f_alv_imprime.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_header .

  CLEAR wa_header.
  wa_header-typ = 'H'.
  wa_header-info = 'Tabela Comparativa de Títulos'.
  APPEND wa_header TO it_header.

  CLEAR wa_header.
  wa_header-typ = 'S'.
  wa_header-key = 'Times'(004).
  wa_header-info = 'Comparação entre os Times: Atlético x Cruzeiro x América'.

  DATA: l_linha(40) TYPE c,
        l_data(10)  TYPE c.

  WRITE sy-datum TO l_data.
  CONCATENATE 'Data:' l_data INTO l_linha.
  CLEAR wa_header.
  wa_header-typ = 'A'.
  wa_header-info = l_linha.
  APPEND wa_header TO it_header.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

  DATA i TYPE i.
  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'NUMTIME'.
  wa_sort-subtot = 'X'.
  wa_sort-up = 'X'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'NTIME'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'QUANTIDADE'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'NCAMP'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'PESO'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'MASCOTE'.
  APPEND wa_sort TO it_sort.

  i = i + 1.
  CLEAR wa_sort.
  wa_sort-spos = i.
  wa_sort-fieldname = 'TOTALTITULO'.
  APPEND wa_sort TO it_sort.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .

  DATA i TYPE i.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'NTIME'.
  wa_afield-ref_fieldname = 'NTIME'.
  wa_afield-ref_tabname = 'ZTITULOS'.
  wa_afield-key = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'MASCOTE'.
  wa_afield-ref_fieldname = 'MASCOTE'.
  wa_afield-ref_tabname = 'ZTIMES'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'TOTALTITULO'.
  wa_afield-ref_fieldname = 'TOTALTITULO'.
  wa_afield-ref_tabname = 'ZTIMES'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'NCAMP'.
  wa_afield-ref_fieldname = 'NCAMP'.
  wa_afield-ref_tabname = 'ZTITULOS'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'QUANTIDADE'.
  wa_afield-ref_fieldname = 'QUANTIDADE'.
  wa_afield-ref_tabname = 'ZTITULOS'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'PESO'.
  wa_afield-ref_fieldname = 'PESO'.
  wa_afield-ref_tabname = 'ZTITULOS'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos = i.
  wa_afield-fieldname = 'TOTAL'.
  wa_afield-ref_fieldname = 'TOTAL'.
  wa_afield-ref_tabname = 'ZTITULOS'.
  APPEND wa_afield TO it_fieldcat.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_imprime .

*Transfere dados-----------------------------------
  wa_layout-colwidth_optimize = 'X'.
  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      it_fieldcat = it_fieldcat
      is_layout   = wa_layout
    IMPORTING
      et_fieldcat = it_fcat
      es_layout   = wa_lay.

*Imprime dados--------------------------------
  MOVE sy-repid TO v_repid.
  MOVE v_repid TO wa_variant-report.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = v_repid
      is_layout              = wa_lay
      it_fieldcat            = it_fcat
      i_callback_top_of_page = 'F_TOP_OF_PAGE'
*     i_background_id        = 'ALV_BACKGROUND'
      it_sort                = it_sort[]
      i_save                 = v_save
      is_variant             = wa_variant
    TABLES
      t_outtab               = ti_saida[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_top_of_page .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
      i_logo             = 'ENJOYSAP_LOGO'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIMPA_TUDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_tudo .

  CLEAR: ti_ztitulos[], ti_ztimes[], ti_saida[],
         wa_ztitulos , wa_ztimes , wa_saida ,
         v_repid , v_save , it_fcat[],
         it_fieldcat[], it_header[], it_sort[],
         wa_variant , wa_afield , wa_colinfo ,
         wa_layout , wa_header , wa_lay , wa_sort .

ENDFORM.
