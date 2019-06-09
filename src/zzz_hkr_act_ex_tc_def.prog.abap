*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Simple Example
*  @version    : 0.0.1
*  @author     : Hagen Kröber
*  ---------------------------------------------------------------------
*
*  Description:
*
*  =====================================================================
*  Version history:
*
*  0.0.1: Initial implementation with dynamic context definition
*  0.0.2:
*
*  =====================================================================
*  Backlog:
*
*/ =====================================================================
REPORT zzz_hkr_act_ex_tc_def.


INCLUDE zzz_hkr_act_fw_cd.
INCLUDE zzz_hkr_act_ex_adp_fut_cd.


"/
" Test case parameters
"/
PARAMETERS pa_hello TYPE string OBLIGATORY DEFAULT 'Hello'.
PARAMETERS pa_world TYPE string OBLIGATORY DEFAULT 'World'.

SELECTION-SCREEN SKIP.

PARAMETERS pa_loga AS CHECKBOX DEFAULT space.


"// initialize ACT framework and set own enhancements
 zcl_hkr_act=>init( ).


"/
" Prepare Builders
"/
DATA(lr_tc_builer) = NEW zcl_hkr_act_tc_builder( ).
DATA lr_fut_adp TYPE REF TO zif_hkr_act_run_adp_fut.


"/
"  Prepare Context
"  Define context data structure
"/

"/ example:
"
" <root>: {
"     'SCN': {
"           'HELLO': <string>,
"           'WORLD': <string>,
"      },
"     'RUN': {
"          'GREETING': <string>
"     }
" }
"/

DATA(lr_ctx_root) = zcl_hkr_act_ctx_node=>create_root( ).
DATA(lr_child) = lr_ctx_root->add_child( 'SCN' ).

lr_child->add_attribute(
  iv_name = 'HELLO' iv_type = 'STRING'
)->add_attribute(
  iv_name = 'WORLD' iv_type = 'STRING'
)->add_sibling( 'RUN' )->add_attribute(
  iv_name = 'GREETING' iv_type = 'STRING'
)->add_attribute(
  iv_name = 'GREETING_2' iv_type = 'STRING'
).



"/
"  Create Test Case
"  Get test case definition instance
"/
DATA(lr_tc_def) = lr_tc_builer->create_tc( ir_node = lr_ctx_root ).



"/
" create and add test step / task collection
"
" create test step
" - export: adapter type, adapter parameter (class name)
" - import: zif_hkr_act_ts_def
"/
DATA(lr_step_1) = lr_tc_builer->create_ts(
    iv_adp_type  = zif_hkr_act_fw_c=>gc_adp-type-generic
    it_parameter = VALUE #( ( name = 'CLASS_NAME' value = 'ZCL_HKR_FUT_EX_DUMMY_SUCCESS' ) )
).



"/
" define execution plan and map each step to context properties
" - currently only a task sequence is supported
"/

" add run unit <sequence>
" - export: <none>
" - import: run unit instance
"
" add task to run unit
" - export: task instance, task id
"           context mapping(task, context)  # global context or local run unit context
"           runtime parameters # own context

DATA lt_steps TYPE tt_tc_steps.                    "// sequence of test steps (tasks) with context mapping

lt_steps = VALUE #(
    ( id            = 'F1'
      ts_adp        = lr_step_1
      binding_infos = VALUE #(
        ( attr = 'MV_I_HELLO'    path = '/SCN/HELLO'    mode = 'I')
        ( attr = 'MV_I_WORLD'    path = '/SCN/WORLD'    mode = 'I')
        ( attr = 'MV_E_GREETING' path = '/RUN/GREETING' mode = 'O')
      )
    )
    ( id            = 'F2'
      own_session   = abap_true
      ts_adp        = lr_step_1
      binding_infos = VALUE #(
        ( attr = 'MV_I_HELLO'    path = '/SCN/HELLO'      mode = 'I')
        ( attr = 'MV_I_WORLD'    path = '/SCN/WORLD'      mode = 'I')
        ( attr = 'MV_E_GREETING' path = '/RUN/GREETING_2' mode = 'O')
      )
    )
).




"// add steps to test case instance
lr_tc_def->set_exec_def( lt_steps ).


"/
"  Set Test Run Parameters
"/
DATA(lr_ctx) = lr_tc_def->get_context( ).
lr_ctx->set_property( iv_path = '/SCN/HELLO' iv_value = pa_hello ).
lr_ctx->set_property( iv_path = '/SCN/WORLD' iv_value = pa_world ).



"/
" Execute Test Case based in its definitions
" - Get test case runtime container.
" - Run text case.
"/

DATA(lr_tc_run) = NEW zcl_hkr_act_run( ir_tc = lr_tc_def ).
DATA(lr_exec) = NEW  zcl_hkr_act_exec( lr_tc_run ).

IF lr_exec->execute( ).
  WRITE: / 'Test run finished successfully.'.
  WRITE: / lr_ctx->get_string_property( '/RUN/GREETING').
  WRITE: / lr_ctx->get_string_property( '/RUN/GREETING_2').
ELSE.
  WRITE: / 'Test run finished with errors.'.
ENDIF.


"/
"  Display Test Run Protocol
"/
lr_exec->output_result( pa_loga ).



INCLUDE zzz_hkr_act_fw_ci.
INCLUDE zzz_hkr_act_ex_adp_fut_ci.
