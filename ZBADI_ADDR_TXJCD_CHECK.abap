  METHOD if_ex_addr_txjcd_check~switch_address_fields.

    DATA: lo_cep_maintain TYPE REF TO zcl_int_cep_maintain.

    TRY.
        lo_cep_maintain = NEW zcl_int_cep_maintain( im_adrc_struc-post_code1 ).

        DATA(ev_error) = lo_cep_maintain->execution( ).

        IF ev_error IS NOT INITIAL.
          IF ev_error EQ 'cep_cadastrado'.
            RETURN.
          ENDIF.

          MESSAGE ev_error TYPE 'E'.
        ENDIF.

        MESSAGE 'CEP atualizado no banco de dados. Favor reiniciar aplicativo e realizar o cadastro novamente!' TYPE 'W'.

      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.