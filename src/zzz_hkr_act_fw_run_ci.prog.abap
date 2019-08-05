*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementations for executing a test instance
*/ =====================================================================



*---------------------------------------------------------------
* Test run
CLASS zcl_hkr_act_run IMPLEMENTATION.

*    data:
*      mv_state type c LENGTH 1,
*      mt_steps type tt_tc_steps,
*      mt_run type tt_run_fut.

  METHOD constructor.
    me->mr_tc    = ir_tc.
    me->mv_state = zif_hkr_act_c=>gc_run_state_planned.
    CLEAR me->mt_run.

  ENDMETHOD.


ENDCLASS.


*---------------------------------------------------------------
* Test execution
CLASS zcl_hkr_act_exec IMPLEMENTATION.

  METHOD constructor.
*     IMPORTING ir_run type ref to zcl_hkr_act_run,
    me->mr_run = ir_run.
    me->mr_run->mv_state = zif_hkr_act_c=>gc_run_state_processing.
    CLEAR me->mr_run->mt_run.
    me->mr_run->ms_run_stats-total_num_steps = lines( ir_run->mr_tc->mt_tc_steps ).
    me->mr_run->ms_run_stats-proc_num_steps = 0.
    me->mr_run->ms_run_stats-failed_step_id = ''.

  ENDMETHOD.


  METHOD execute.
*     RETURNING VALUE(rv_success) type abap_bool.

    DATA lr_ex TYPE REF TO zcx_hkr_act_base_exc.
    rv_success = abap_undefined.
    DATA(lv_error) = abap_false.

    GET TIME STAMP FIELD me->mr_run->ms_run_stats-start_ts.

    " execute all test steps
    LOOP AT me->mr_run->mr_tc->mt_tc_steps REFERENCE INTO DATA(lr_step).

      " create test adapter with actions
      "***TODO: is always the same, put the code into into a initialization method <init_fut_run()>
      DATA(ls_fut) = VALUE ts_run_fut(
          step = lr_step->*                  "// redundant with constructur of run unit
          success = abap_undefined
          actions = VALUE #(
            ( id = zif_hkr_act_c=>gc_act_init  success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_val_pre  success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_exec success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_val_post success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
          )
          logger = NEW zcl_hkr_act_fut_logger( )
      ).

      TRY.
          " execute actions of test adapter
          CLEAR lr_ex.

          DATA(lr_ts_run) = me->get_ts_runner( ir_run = me->mr_run is_step = lr_step->* ).
          lr_ts_run->execute( CHANGING cs_run_fut = ls_fut ).
          APPEND ls_fut TO me->mr_run->mt_run.

        CATCH zcx_hkr_act_base_exc INTO lr_ex.
          ls_fut-success = abap_false.
          "***TODO: implement as logger function ls_fut-logger->add_exception( )
          ls_fut-logger->mr_msg->add_from_exception(
            EXPORTING ir_ex     = lr_ex
            CHANGING  ct_return = ls_fut-logger->mt_messages
          ).
      ENDTRY.

      IF ls_fut-success = abap_false.
        " test  function failed or unexpected test function exception (internal error)
        lv_error = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF lv_error = abap_true.
      me->mr_run->ms_run_stats-failed_step_id = lr_step->id.
    ENDIF.

    " set processing status
    me->mr_run->mv_state = COND #(
      WHEN lr_ex IS BOUND THEN zif_hkr_act_c=>gc_run_state_canceled
      WHEN lv_error = abap_true THEN zif_hkr_act_c=>gc_run_state_failed
      ELSE zif_hkr_act_c=>gc_run_state_success
    ).

    " set processing statistics
    GET TIME STAMP FIELD me->mr_run->ms_run_stats-end_ts.
    me->mr_run->ms_run_stats-proc_num_steps = lines( me->mr_run->mt_run ).

    " set result code
    rv_success = xsdbool( me->mr_run->mv_state = zif_hkr_act_c=>gc_run_state_success ).

  ENDMETHOD.


*/
* Return execution unit depending on runtime parameters of test step definition
* <IS_STEP>. The execution unit is initialized with the test step definition and
* <IS_STEP> and the test case runtime container <IR_RUN>
*
* Supported unit:
* - call adapter in current session
* - call adapter in own session using local RFC
*/
  METHOD get_ts_runner.
    rr_fut_runner = SWITCH #( is_step-own_session
        WHEN abap_true THEN NEW zcl_hkr_act_exec_fut_new_sess( )
        ELSE NEW zcl_hkr_act_exec_fut_cur_sess( ) ).

    rr_fut_runner->ms_step = is_step.
    rr_fut_runner->mr_run = ir_run.

  ENDMETHOD.


*/
*
*/
  "***TODO: move it to a ABAP list log writer
  METHOD output_result.

    FORMAT INTENSIFIED OFF.

    WRITE: / |# =====================================================================|.
    WRITE: / |# Test run       : <run_name>|.
    WRITE: / |# Test scenario  : <scenario_name>|.
    WRITE: / |# User           : { sy-uname }|.
    WRITE: / |# Test run start : { me->mr_run->ms_run_stats-start_ts }|.
    WRITE: / |# Test run end   : { me->mr_run->ms_run_stats-end_ts }|.
    WRITE: / |# Test run status: { me->mr_run->mv_state }|.
    WRITE: / |# =====================================================================|.
    WRITE: / |#|.
    WRITE: / |# Number of steps: { me->mr_run->ms_run_stats-total_num_steps }|.
    WRITE: / |# Processed steps: { me->mr_run->ms_run_stats-proc_num_steps }|.
    WRITE: / |# Failed step    : { me->mr_run->ms_run_stats-failed_step_id }|.
    WRITE: / |#|.
    WRITE: / |# ---------------------------------------------------------------------|.
    WRITE: / |# Execution log begin|.


    LOOP AT me->mr_run->mt_run REFERENCE INTO DATA(lr_fut).

      DATA(lv_color) = SWITCH i( lr_fut->success WHEN abap_true THEN 5 WHEN abap_false THEN 6 ELSE 3 ).

      FORMAT INTENSIFIED OFF.

      WRITE: / |#|.
      WRITE: / |# Step { sy-tabix }: { lr_fut->step-id }|.
      WRITE 64 |{ me->get_result_text( lr_fut->success ) WIDTH = 10 }| COLOR = lv_color.

      IF lr_fut->success = abap_false OR iv_show_all = abap_true.
        LOOP AT lr_fut->actions REFERENCE INTO DATA(lr_action).
          IF lr_action->success = abap_false OR iv_show_all = abap_true.

            lv_color = SWITCH i( lr_action->success WHEN abap_true THEN 5 WHEN abap_false THEN 6 ELSE 3 ).
            FORMAT INTENSIFIED OFF.

            WRITE: / |# Action: { lr_action->id }|.
            WRITE 64 |{ me->get_result_text( lr_action->success ) WIDTH = 10 }| COLOR = lv_color.

            IF lr_action->success = abap_false.
              LOOP AT lr_action->logger->mt_messages REFERENCE INTO DATA(lr_message).
                WRITE: / |#   { me->get_text( lr_message ) }|.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    WRITE: / |#|.
    WRITE: / |# Execution log end|.
    WRITE: / |# ---------------------------------------------------------------------|.

  ENDMETHOD.


  METHOD get_result_text.
    rv_text = SWITCH #( iv_result
    WHEN abap_true THEN 'success'
    WHEN abap_false THEN 'failed'
    ELSE 'skipped' ).
  ENDMETHOD.


  METHOD get_text.
    MESSAGE ID ir_message->id TYPE ir_message->type NUMBER ir_message->number
      WITH ir_message->message_v1 ir_message->message_v2 ir_message->message_v3 ir_message->message_v4
      INTO rv_text.
  ENDMETHOD.

ENDCLASS.



*/
* Execute test function in current session
* Steps for each action:
* 1. Update binding: context to adapter propteries
* 2. execute actions
* 3. Update binding: adapter propteries to context
*/
CLASS zcl_hkr_act_exec_fut_cur_sess IMPLEMENTATION.

  METHOD zif_hkr_act_exec_fut~execute.

    DATA(lr_bdg) = NEW zcl_hkr_act_ctx_bdg_update( ).

    " update adapter context
    lr_bdg->update_binding(
        ir_ctx          = me->mr_run->mr_tc->mr_ctx              " test run context
        ir_adp_ctx      = me->ms_step-ts_adp->mr_adp_ctx         " adapter contex
        iv_mode         = zif_hkr_act_fw_c=>gc_bdg_mode_in       " to adapter
        it_binding_info = me->ms_step-binding_infos              " binding definition
    ).

    LOOP AT cs_run_fut-actions REFERENCE INTO DATA(lr_action).

      lr_action->success = SWITCH #( lr_action->id
        WHEN zif_hkr_act_c=>gc_act_init     THEN cs_run_fut-step-ts_adp->mr_adp->init( lr_action->logger )
        WHEN zif_hkr_act_c=>gc_act_val_pre  THEN cs_run_fut-step-ts_adp->mr_adp->validation_before( lr_action->logger )
        WHEN zif_hkr_act_c=>gc_act_exec     THEN cs_run_fut-step-ts_adp->mr_adp->run( lr_action->logger )
        WHEN zif_hkr_act_c=>gc_act_val_post THEN cs_run_fut-step-ts_adp->mr_adp->validation_after( lr_action->logger )
*  **            ELSE .
          " internal error
*  **TODO: Throw dynamic framework exception
      ).

      "// work around for missing msg. texts returned from adapter
      loop at lr_action->logger->mt_messages REFERENCE INTO data(lr_msg)
        where message is INITIAL and not number is INITIAL.
        MESSAGE id lr_msg->id TYPE lr_msg->type NUMBER lr_msg->number
          with lr_msg->message_v1 lr_msg->message_v2 lr_msg->message_v3 lr_msg->message_v4
          into lr_msg->message.
      endloop.

      IF lr_action->success = abap_false.
        cs_run_fut-success = abap_false.
        EXIT.
      ENDIF.
      cs_run_fut-success = abap_true.

    ENDLOOP.

    " update test case context
    lr_bdg->update_binding(
        ir_ctx          = me->mr_run->mr_tc->mr_ctx         " test run context
        ir_adp_ctx      = me->ms_step-ts_adp->mr_adp_ctx    " adapter contex
        iv_mode         = zif_hkr_act_fw_c=>gc_bdg_mode_out " to test case context
        it_binding_info = me->ms_step-binding_infos         " binding definition
    ).


  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------
* Execute test function in own session context
* (spawned by an RFC FM)
CLASS zcl_hkr_act_exec_fut_new_sess IMPLEMENTATION.

  METHOD constructor.
    me->mr_json = NEW zcl_hkr_act_json_conv( ).
  ENDMETHOD.


  METHOD zif_hkr_act_exec_fut~execute.

    DATA lv_ctx_in TYPE xstring.
    DATA lv_ctx_out TYPE xstring.
    DATA lv_result TYPE xstring.
    DATA lt_return TYPE bapirettab.


    " serialize global context
    DATA(lr_ctx_data) = me->mr_run->mr_tc->get_context( )->mr_data.
    lv_ctx_in = me->mr_json->serialize_data( lr_ctx_data ).


    " call RFC proxy to execute test function
    CALL FUNCTION 'Z_HKR_ACT_RUN_ADP_OWN_SESS'
    "      DESTINATION 'NONE'
      EXPORTING
        iv_task_id     = CONV string( me->ms_step-id )
        iv_adp_type    = CONV string( me->ms_step-ts_adp->mv_type )
        it_adp_def     = CONV zzz_hkr_act_adp_build_par_t( me->ms_step-ts_adp->mt_def )
        it_adp_bdg     = CONV zzz_hkr_act_adp_binding_t( me->ms_step-binding_infos )
        iv_ctx_json    = lv_ctx_in
      IMPORTING
        ev_ctx_json    = lv_ctx_out
        ev_result_json = lv_result
        et_return      = lt_return.       "TODO


    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination          = 'NONE'
      EXCEPTIONS
        destination_not_open = 0
        OTHERS               = 0.

    " de-serialize context
    me->mr_json->deserialize_data( EXPORTING iv_json = lv_ctx_out CHANGING cr_data = lr_ctx_data ).

    " de-serialize result
    me->deserialize_result( EXPORTING iv_json = lv_result CHANGING cs_run_fut = cs_run_fut ).

  ENDMETHOD.


*   serialize return codes and messages
  METHOD serialize_result.

    DATA ls_result_fut TYPE ts_result_fut.

    CLEAR rv_json.

    " copy function and action result
    ls_result_fut-success =  is_run_fut-success.
    ls_result_fut-act_results = VALUE #( FOR <ls_action> IN is_run_fut-actions
      ( id = <ls_action>-id success = <ls_action>-success messages = <ls_action>-logger->mt_messages )
    ).

    " serialize
    rv_json = NEW zcl_hkr_act_json_conv( )->serialize_data( REF #( ls_result_fut ) ).

  ENDMETHOD.


  METHOD deserialize_result.

    DATA ls_result_fut TYPE ts_result_fut.
    DATA(lr_res) = REF data( ls_result_fut ).

    " de-serialize
    NEW zcl_hkr_act_json_conv( )->deserialize_data(
      EXPORTING iv_json = iv_json
      CHANGING cr_data = lr_res ).

    " copy result into fut result
    "***TODO: is always the same, put the code into into a initialization method <init_fut_run()>
    " rs_run_fut-step = lr_step->*
    cs_run_fut-success = ls_result_fut-success.
    cs_run_fut-logger = NEW zcl_hkr_act_fut_logger( ).
    cs_run_fut-logger->mt_messages = ls_result_fut-messages.

    cs_run_fut-actions = VALUE #( FOR <act_result> IN ls_result_fut-act_results
      ( id = <act_result>-id
        success = <act_result>-success
        logger = NEW zcl_hkr_act_fut_logger( )->add_messages( <act_result>-messages ) )
    ).

  ENDMETHOD.

ENDCLASS.
