class ZCL_INT_CEP_MAINTAIN definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      value(I_CEP) type ANY .
  methods EXECUTION
    returning
      value(EV_ERROR) type STRING .
protected section.

  class-data GV_CEP type STRING .
private section.

  class-data GS_RETORNO type ZSINT_CEP_RETORNO .

  methods CADASTRAR_CEP_J_1BTREG_CITY
    exporting
      !EV_SUCESSO type ABAP_BOOLEAN .
  methods CADASTRAR_CEP_J_1BTXJURV
    exporting
      !EV_SUCESSO type ABAP_BOOLEAN .
  methods CALL_VIACEP_API
    returning
      value(EV_ERROR) type STRING .
  methods CALL_VIACEP_CPI
    returning
      value(EV_ERROR) type STRING .
ENDCLASS.



CLASS ZCL_INT_CEP_MAINTAIN IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_INT_CEP_MAINTAIN->CADASTRAR_CEP_J_1BTREG_CITY
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_SUCESSO                     TYPE        ABAP_BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cadastrar_cep_j_1btreg_city.

    DATA: ls_city       TYPE j_1btreg_city.


    FREE ev_sucesso.

*   >> Preencher estrutura ls_city
    ls_city-country      = 'BR'.
    ls_city-pstcd_from   = gs_retorno-cep.
    ls_city-pstcd_to     = gs_retorno-cep.
    ls_city-region       = gs_retorno-uf.
    ls_city-taxjurcode   = |{ gs_retorno-uf } { gs_retorno-ibge }|.

    " Insere na tabela j_1btxjur (INATIVADO)
    "MODIFY j_1btreg_city FROM ls_city.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    "Cep cadastrado com sucesso
    ev_sucesso = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_INT_CEP_MAINTAIN->CADASTRAR_CEP_J_1BTXJURV
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_SUCESSO                     TYPE        ABAP_BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cadastrar_cep_j_1btxjurv.

    DATA: ls_j_1btxjur  TYPE j_1btxjur,
          ls_j_1btxjurt TYPE j_1btxjurt.

    FREE ev_sucesso.

*   >> Preenche estrutura j_1btxjur
    ls_j_1btxjur-country = 'BR'.
    ls_j_1btxjur-taxjurcode = |{ gs_retorno-uf } { gs_retorno-ibge }|.

    " Insere na tabela j_1btxjur (INATIVADO)
    "MODIFY j_1btxjur FROM ls_j_1btxjur.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

*   >> Preenche estrutura j_1btxjurt para a tabela de textos
    ls_j_1btxjurt-spras      = 'P'.
    ls_j_1btxjurt-country    = 'BR'.
    ls_j_1btxjurt-taxjurcode = |{ gs_retorno-uf } { gs_retorno-ibge }|.
    ls_j_1btxjurt-text       = gs_retorno-localidade. " Nome da cidade

    "Insere na tabela j_1btxjurt
    MODIFY j_1btxjurt FROM ls_j_1btxjurt.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    "Cadastro realizado com sucesso
    ev_sucesso = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_INT_CEP_MAINTAIN->CALL_VIACEP_API
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD call_viacep_api.
    "OBS: Todas as mensagens estão fixas no codigo até ser definido qual formato vai ser usado para fazer a conexão ABAP ou CPI

    DATA: lo_http_client   TYPE REF TO if_http_client,
          lv_url           TYPE string,
          lv_url_method    TYPE string,
          lv_proxy_host    TYPE string,
          lv_proxy_service TYPE string,
          lv_http_reason   TYPE string,
          lv_http_code     TYPE i.

    TRY.
        lv_url = |https://viacep.com.br/ws/{ gv_cep }/json/|.
        lv_url_method    = 'GET'.
        CONDENSE lv_url NO-GAPS.

        DO 1 TIMES.
*-----------------------------------------------------------------------------------------*
* 01 Instanciar Objeto HTTP
*-----------------------------------------------------------------------------------------*
          "// Create HTTP client by url
          CALL METHOD cl_http_client=>create_by_url
            EXPORTING
              url                = lv_url
              proxy_host         = 'proxy'
              proxy_service      = '3128'
            IMPORTING
              client             = lo_http_client
            EXCEPTIONS
              argument_not_found = 1
              plugin_not_active  = 2
              internal_error     = 3
              OTHERS             = 4.

          IF sy-subrc IS NOT INITIAL.
            ev_error = |{ TEXT-e03 } { sy-subrc }|. "|Erro na funcionalidade CREATE_BY_URL. Subrc: { sy-subrc }|.
            EXIT.
          ENDIF.

          "// Setting request method
          lo_http_client->request->set_method( lv_url_method ).

*-----------------------------------------------------------------------------------------*
* 02 Envio das Informações ao destino HTTP
*-----------------------------------------------------------------------------------------*
          "// Send data By Http
          CALL METHOD lo_http_client->send
            EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              http_invalid_timeout       = 4
              OTHERS                     = 5.

          "// Get reveice from Http
          IF sy-subrc IS INITIAL.
            CALL METHOD lo_http_client->receive
              EXCEPTIONS
                http_communication_failure = 1
                http_invalid_state         = 2
                http_processing_failed     = 3
                OTHERS                     = 5.

            IF sy-subrc <> 0.
              ev_error = |{ TEXT-e04 } { sy-subrc }|. "Erro na funcionalidade HTTP_CLIENT->RECEIVE. Subrc: { sy-subrc }|.

              CALL METHOD lo_http_client->get_last_error
                IMPORTING
                  code    = DATA(gv_codev_error)
                  message = DATA(gv_messagev_error).

              IF gv_messagev_error IS NOT INITIAL.
                ev_error = |{ ev_error } { gv_codev_error } { gv_messagev_error }|.
              ENDIF.

              EXIT.
            ENDIF.

          ELSE.
            ev_error = |{ TEXT-e05 } { sy-subrc }|. "Erro na funcionalidade HTTP_CLIENT->SEND. Subrc: { sy-subrc }|.
            EXIT.
          ENDIF.

          "// Get Body from Http response
          DATA(lv_response_json) = lo_http_client->response->get_cdata( ).

          "// Get HTTP Response
          CALL METHOD lo_http_client->response->get_status
            IMPORTING
              code   = lv_http_code       " HTTP Status Code
              reason = lv_http_reason.    " HTTP status description

        ENDDO.

        " Check if HTTPs code start with '2' (200,201,202..)
        IF lv_http_code IS NOT INITIAL AND NOT lv_http_code BETWEEN 200 AND 299.
          ev_error = |{ ev_error } { TEXT-e06 } { lv_http_code }-{ lv_http_reason }|. "{ ev_error } Erro técnico HTTP. Code: { lv_http_code }-{ lv_http_reason }
        ENDIF.

        SHIFT ev_error LEFT DELETING LEADING space.

        "// Close Connection
        IF lo_http_client IS NOT INITIAL.
          lo_http_client->close(
                      EXCEPTIONS
                        http_invalid_state = 1
                        OTHERS             = 2
          ).
        ENDIF.

        "// Converte o JSON na Estrutura ABAP
        /ui2/cl_json=>deserialize( EXPORTING json         = lv_response_json
                                             pretty_name  = /ui2/cl_json=>pretty_mode-none
                                    CHANGING data         = gs_retorno ).

        IF gs_retorno-erro IS NOT INITIAL.
          ev_error = TEXT-e07.
        ENDIF.

      CATCH cx_root INTO DATA(lo_cx_root).
        ev_error = lo_cx_root->get_text( ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_INT_CEP_MAINTAIN->CALL_VIACEP_CPI
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD call_viacep_cpi.
    DATA: lo_cpi_conn TYPE REF TO zcl_cpi_connection_maintain,
          lv_ambiente TYPE string,
          lt_headers  TYPE tihttpnvp,
          ls_headers  TYPE ihttpnvp.

*-----------------------------------------------------------------------------------------*
*   01 Set Headers
*-----------------------------------------------------------------------------------------*
    APPEND VALUE #( name = 'cep' value = gv_cep ) TO lt_headers.

*-----------------------------------------------------------------------------------------*
*   02 Instanciar a classe
*-----------------------------------------------------------------------------------------*
    lv_ambiente = COND #( WHEN sy-sysid EQ 'DS4' OR sy-sysid EQ 'QS4' THEN |QAS|
                          ELSE |PRD| ).

    lo_cpi_conn = NEW zcl_cpi_connection_maintain(
*      iv_body    =
      iv_path    = |/CPI/{ lv_ambiente }/VIACEP/CONSULTACEP|
      iv_method  = 'GET'
      it_headers = lt_headers
    ).

*-----------------------------------------------------------------------------------------*
*   03 Executa a classe para receber o retorno
*-----------------------------------------------------------------------------------------*
    lo_cpi_conn->execution(
      IMPORTING
        ev_error    = DATA(lv_erro_cpi)
        ev_response = DATA(ls_retorno)
    ).

    IF lv_erro_cpi IS NOT INITIAL.
      ev_error = lv_erro_cpi.
      RETURN.
    ENDIF.

*    IF lv_erro_cpi IS NOT INITIAL.
*      ev_error = |CPI: { lv_erro_cpi }|.
*      RETURN.
*    ENDIF.

    "// Converte o JSON na Estrutura ABAP
    /ui2/cl_json=>deserialize( EXPORTING json         = ls_retorno
                                         pretty_name  = /ui2/cl_json=>pretty_mode-none
                                CHANGING data         = gs_retorno ).

    IF gs_retorno-erro IS NOT INITIAL.
      ev_error = |CPI: { gs_retorno-message }|.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_CEP_MAINTAIN->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CEP                          TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    FREE gs_retorno.
    gv_cep = i_cep.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_INT_CEP_MAINTAIN->EXECUTION
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EV_ERROR                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD execution.

    DATA: is_j_1btreg_city      TYPE abap_boolean,
          is_j_1btxjurv         TYPE abap_boolean,
          lv_cepCadastrado_jurv TYPE abap_boolean,
          lv_sucesso_city       TYPE abap_boolean,
          lv_sucesso_jurv       TYPE abap_boolean.

    FREE ev_error.

    TRY.
*-----------------------------------------------------------------------------------------*
*       01 Verifica existencia do CEP na tabela CITY
*-----------------------------------------------------------------------------------------*
*       CEP já existe? Se sim, ignora
        SELECT SINGLE * FROM j_1btreg_city
          INTO @DATA(ls_city)
         WHERE country    EQ 'BR'
           AND pstcd_from LE @gv_cep
           AND pstcd_to   GE @gv_cep.

        IF sy-subrc IS INITIAL.
          ev_error = 'cep_cadastrado'.
          RETURN.
        ENDIF.
*-----------------------------------------------------------------------------------------*
*       02 Aciona API para consulta de CEP
*-----------------------------------------------------------------------------------------*
        REPLACE ALL OCCURRENCES OF '-' IN gv_cep WITH ''.

*        Aciona via ABAP
        DATA(lv_viacep_erro) = me->call_viacep_api( ).

*        "Aciona via CPI
*        DATA(lv_viacep_erro) = me->call_viacep_cpi( ).

        "Se houve erro na consulta do CEP na API
        IF lv_viacep_erro IS NOT INITIAL.
          ev_error = lv_viacep_erro .
          RETURN.
        ENDIF.

*-----------------------------------------------------------------------------------------*
*       03 Cadastrar CEP na tabela j_1btxjur (É NECESSÁRIO ESSE CADASTRO?)
*-----------------------------------------------------------------------------------------*
        DATA(lv_taxjurcode) = |{ gs_retorno-uf } { gs_retorno-ibge }|.

*       CEP já existe? Se sim, ignora
        SELECT SINGLE * FROM j_1btxjur
          INTO @DATA(ls_jur)
         WHERE country    EQ 'BR'
           AND taxjurcode EQ @lv_taxjurcode.

        IF sy-subrc IS NOT INITIAL.
          me->cadastrar_cep_j_1btxjurv(
            IMPORTING
              ev_sucesso =   lv_sucesso_jurv ).

          IF lv_sucesso_jurv IS INITIAL.
            ev_error = TEXT-e02. "Erro ao cadastrar o CEP na tabela j_1btxjur
            RETURN.
          ENDIF.

        ENDIF.

*-----------------------------------------------------------------------------------------*
*       04 Cadastrar CEP na tabela j_1btreg_city
*-----------------------------------------------------------------------------------------*
        me->cadastrar_cep_j_1btreg_city(
          IMPORTING
            ev_sucesso = lv_sucesso_city ).

        IF lv_sucesso_city IS INITIAL.
          ev_error = TEXT-e01. "Erro ao cadastrar o CEP na tabela j_1btreg_city
          RETURN.
        ENDIF.

      CATCH cx_root INTO DATA(lo_cx_root).
        ev_error = lo_cx_root->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.