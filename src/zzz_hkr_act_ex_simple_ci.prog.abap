class zcl_hkr_tc_ex_simple IMPLEMENTATION.
endclass.



*-----------------------------------------------------------------------
* PP WZM: Retrieve Pack Order
*
* Context In
*
* Validations before step
* @VAL_PRE: <NONE>
*
* Validation after step
* @VAL_POST: <NONE>
*
* Context Out
*
*
*-----------------------------------------------------------------------
CLASS ZCL_HKR_FUT_EX_DUMMY_SUCCESS IMPLEMENTATION.

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

*   <prepare response parameter>
    rv_success = abap_true.

  ENDMETHOD.

  METHOD zif_hkr_act_run_adp~validation_after.
    rv_success = abap_true.
  ENDMETHOD.

ENDCLASS.
