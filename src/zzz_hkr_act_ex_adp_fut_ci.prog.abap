*-----------------------------------------------------------------------
* Dummy adapter
*
*-----------------------------------------------------------------------
CLASS ZCL_HKR_FUT_EX_DUMMY_SUCCESS IMPLEMENTATION.

 "/
 "    attr       type string,        " Attribute of a adapter
 "    annotation type string,
 "    mode       type c length 2,    " I: in, O: out, IO: in and out
 "/
  METHOD zif_hkr_act_run_adp_fut~get_parameter_info.

    " return parameter info list
    clear rt_param_info.
    rt_param_info = value #(
        ( attr = 'MV_I_HELLO' annotation = 'none' mode = 'I'  )
        ( attr = 'MV_I_WORLD' annotation = 'none' mode = 'I' )
        ( attr = 'MV_E_GREETING' annotation = 'none' mode = 'O'  )
    ).
  ENDMETHOD.


  METHOD zif_hkr_act_run_adp~init.
    rv_success = abap_true.
  ENDMETHOD.

  METHOD zif_hkr_act_run_adp~validation_before.
    rv_success = abap_true.
  ENDMETHOD.

  METHOD zif_hkr_act_run_adp~run.

*   initialization
    rv_success = abap_false.

*   <run function>
    me->mv_e_greeting = |{ me->mv_I_hello }  { me->mv_I_world }|.

*   <prepare response parameter>
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_hkr_act_run_adp~validation_after.
    rv_success = abap_true.
  ENDMETHOD.

ENDCLASS.
