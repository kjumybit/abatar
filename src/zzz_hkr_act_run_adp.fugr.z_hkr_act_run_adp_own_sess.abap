*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Execute a test function in its own session
*
*/ =====================================================================
FUNCTION Z_HKR_ACT_RUN_ADP_OWN_SESS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TASK_ID) TYPE  STRING
*"     VALUE(IV_ADP_TYPE) TYPE  STRING
*"     VALUE(IT_ADP_DEF) TYPE  ZZZ_HKR_ACT_ADP_BUILD_PAR_T
*"     VALUE(IT_ADP_BDG) TYPE  ZZZ_HKR_ACT_ADP_BINDING_T
*"     VALUE(IV_CTX_JSON) TYPE  XSTRING
*"  EXPORTING
*"     VALUE(EV_CTX_JSON) TYPE  XSTRING
*"     VALUE(EV_RESULT_JSON) TYPE  XSTRING
*"     VALUE(ET_RETURN) TYPE  BAPIRETTAB
*"----------------------------------------------------------------------

  DATA(lr_json) = NEW zcl_hkr_act_json_conv( ).
  DATA(lr_msg) = zcl_hkr_msg_hlp=>get_bapi_msg_hdl( ).
  DATA(lr_tc_builer) = NEW zcl_hkr_act_tc_builder( ).

  "// initialization
  ev_ctx_json = iv_ctx_json.
  CLEAR ev_result_json.
  CLEAR et_return.


  TRY.

      "// de-serialize and build context
      DATA(lr_ctx_data) = lr_json->deserialize_to_data_ref( iv_ctx_json ).
      DATA(lr_tc_def) = lr_tc_builer->create_tc( ir_ctx_ref = lr_ctx_data ).


      "// build adapter for test step
      DATA(lr_ts_adp) = lr_tc_builer->create_ts(
          iv_adp_type  = CONV #( iv_adp_type )
          it_parameter = CONV #( it_adp_def )
      ).


      "// build test step runtime
      DATA ls_step TYPE ts_tc_step.                    "// test steps (task) with context mapping
      ls_step = VALUE #(
          id            = iv_task_id
          ts_adp        = lr_ts_adp
          binding_infos = it_adp_bdg
      ).


      "// prepare test step actions
      DATA(ls_fut) = VALUE ts_run_fut(
          step = ls_step
          success = abap_undefined
          actions = VALUE #(
            ( id = zif_hkr_act_c=>gc_act_init  success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_val_pre  success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_exec success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
            ( id = zif_hkr_act_c=>gc_act_val_post success = abap_undefined  logger = NEW zcl_hkr_act_fut_logger( ) )
          )
          logger = NEW zcl_hkr_act_fut_logger( )
      ).



      "// do binding update for adapter properties
      DATA(lr_bdg) = NEW zcl_hkr_act_ctx_bdg_update( ).

      " update adapter context
      lr_bdg->update_binding(
          ir_ctx          = lr_tc_def->mr_ctx                  " test run context
          ir_adp_ctx      = ls_step-ts_adp->mr_adp_ctx         " adapter context
          iv_mode         = zif_hkr_act_fw_c=>gc_bdg_mode_in   " to adapter
          it_binding_info = ls_step-binding_infos              " binding definition
      ).


      TRY.
          LOOP AT ls_fut-actions REFERENCE INTO DATA(lr_action).

            lr_action->success = SWITCH #( lr_action->id
              WHEN zif_hkr_act_c=>gc_act_init     THEN ls_fut-step-ts_adp->mr_adp->init( lr_action->logger )
              WHEN zif_hkr_act_c=>gc_act_val_pre  THEN ls_fut-step-ts_adp->mr_adp->validation_before( lr_action->logger )
              WHEN zif_hkr_act_c=>gc_act_exec     THEN ls_fut-step-ts_adp->mr_adp->run( lr_action->logger )
              WHEN zif_hkr_act_c=>gc_act_val_post THEN ls_fut-step-ts_adp->mr_adp->validation_after( lr_action->logger )
*  **            ELSE .
                " internal error
*  **TODO: Throw dynamic framework exception
            ).

            IF lr_action->success = abap_false.
              ls_fut-success = abap_false.
              EXIT.
            ENDIF.
            ls_fut-success = abap_true.

          ENDLOOP.

          """"TODO: catch adapter / application error first
        CATCH cx_root INTO DATA(lr_root_exc).
          " adapter runtime error
          DATA(lv_msg) = lr_root_exc->get_text( ).
          lr_msg->add_from_string( EXPORTING iv_string = lv_msg
                                   CHANGING  ct_return = et_return
          ).
      ENDTRY.

      "// update global context from adapter properties
      lr_bdg->update_binding(
          ir_ctx          = lr_tc_def->mr_ctx                 " test run context
          ir_adp_ctx      = ls_step-ts_adp->mr_adp_ctx        " adapter context
          iv_mode         = zif_hkr_act_fw_c=>gc_bdg_mode_out " to test case context
          it_binding_info = ls_step-binding_infos             " binding definition
      ).


      "// serialize context
      ev_ctx_json = lr_json->serialize_data( lr_tc_def->mr_ctx->mr_data ).

      "// serialize result
      DATA(lr_runner) = NEW zcl_hkr_act_exec_fut_new_sess( ).
      ev_result_json = lr_runner->serialize_result( ls_fut ).

    CATCH zcx_hkr_act_base_exc INTO DATA(lr_fw_exc).
      " internal framework error
      lr_msg->add_from_exception( EXPORTING ir_ex     = lr_fw_exc
                                  CHANGING  ct_return = et_return
      ).
  ENDTRY.

ENDFUNCTION.
