*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for executing a test instance
*/ =====================================================================



CLASS zcl_hkr_act_run DEFINITION DEFERRED.
class zcl_hkr_act_json_conv DEFINITION deferred.


* Test step runtime actions with messages: ['PRE_VAL', 'EXEC', 'POS_VAL']
TYPES: BEGIN OF ts_fut_act,
         id      TYPE string,      " action ID
         success TYPE abap_bool,
         logger  TYPE REF TO zcl_hkr_act_fut_logger,
       END OF ts_fut_act.
TYPES tt_fut_act TYPE STANDARD TABLE OF ts_fut_act WITH DEFAULT KEY.


* Runtime unit for test step
TYPES: BEGIN OF ts_run_fut,
         step    TYPE ts_tc_step,
         success TYPE abap_bool,
         actions TYPE tt_fut_act,
         logger  TYPE REF TO zcl_hkr_act_fut_logger,
       END OF ts_run_fut.
TYPES tt_run_fut TYPE STANDARD TABLE OF ts_run_fut WITH DEFAULT KEY.


* Runtime statistics for test run
TYPES: BEGIN OF ts_run_stats,
         start_ts        TYPE timestamp,
         end_ts          TYPE timestamp,
         total_num_steps TYPE i,
         proc_num_steps  TYPE i,
         failed_step_id  TYPE string,
       END OF ts_run_stats.


* Execute a test function (test adapter)
INTERFACE zif_hkr_act_exec_fut.
  METHODS:
    execute changing cs_run_fut type ts_run_fut
            RAISING  zcx_hkr_act_base_exc.
  DATA:
    mr_run  TYPE REF TO zcl_hkr_act_run,
    ms_step TYPE ts_tc_step.

ENDINTERFACE.


* Execute a test function in current session context
CLASS zcl_hkr_act_exec_fut_cur_sess DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_hkr_act_exec_fut.
    ALIASES:
      mr_run FOR zif_hkr_act_exec_fut~mr_run,
      ms_step FOR zif_hkr_act_exec_fut~ms_step.
ENDCLASS.


* Execute a test function in own RFC session context
CLASS zcl_hkr_act_exec_fut_new_sess DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_hkr_act_exec_fut.

    METHODS:

      constructor,

      serialize_result IMPORTING is_run_fut     TYPE ts_run_fut
                       RETURNING VALUE(rv_json) TYPE xstring,

      deserialize_result IMPORTING iv_json    TYPE xstring
                         CHANGING  cs_run_fut TYPE ts_run_fut.

    ALIASES:
      mr_run FOR zif_hkr_act_exec_fut~mr_run,
      ms_step FOR zif_hkr_act_exec_fut~ms_step.

  PRIVATE SECTION.

    " required for deserialization
    TYPES: BEGIN OF ts_result_act,
             id       TYPE string,      " action ID
             success  TYPE abap_bool,
             messages TYPE bapirettab,
           END OF ts_result_act.
    TYPES tt_result_act TYPE STANDARD TABLE OF ts_result_act WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_result_fut,
             success     TYPE abap_bool,
             messages    TYPE bapirettab,
             act_results TYPE tt_result_act,
           END OF ts_result_fut.

    data:
      mr_json type ref to zcl_hkr_act_json_conv.

ENDCLASS.



* Test case runtime container
CLASS zcl_hkr_act_run DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ir_tc       TYPE REF TO zif_hkr_act_tc_def.

    DATA:
      mr_tc        TYPE REF TO zif_hkr_act_tc_def,
      mv_state     TYPE char1,   " processing state
      mt_run       TYPE tt_run_fut,
      ms_run_stats TYPE ts_run_stats.

ENDCLASS.


*/
* Execute a test case runtime container <IR_RUN>
*/
CLASS zcl_hkr_act_exec DEFINITION.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING ir_run TYPE REF TO zcl_hkr_act_run,
      execute RETURNING VALUE(rv_success) TYPE abap_bool,
      output_result IMPORTING iv_show_all TYPE abap_bool OPTIONAL.

    DATA:
      mr_run TYPE REF TO zcl_hkr_act_run.

  PRIVATE SECTION.

    CLASS-METHODS:

      "TODO use an own factory class
      get_ts_runner IMPORTING ir_run               TYPE REF TO zcl_hkr_act_run
                              is_step              TYPE ts_tc_step
                    RETURNING VALUE(rr_fut_runner) TYPE REF TO zif_hkr_act_exec_fut,

      get_result_text IMPORTING iv_result      TYPE abap_bool
                      RETURNING VALUE(rv_text) TYPE string,

      get_text IMPORTING ir_message     TYPE REF TO bapiret2
               RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.


* Global Constant definitions
INTERFACE zif_hkr_act_c.

  CONSTANTS:

    " actions when executing a test function
    gc_act_init             TYPE string VALUE 'ACT_INIT',
    gc_act_val_pre          TYPE string VALUE 'ACT_VAL_PRE',
    gc_act_exec             TYPE string VALUE 'ACT_EXEC',
    gc_act_val_post         TYPE string VALUE 'ACT_VAL_POST',

    " processing status of a test run
    gc_run_state_planned    TYPE char1 VALUE '9',
    gc_run_state_canceled   TYPE char1 VALUE '8',
    gc_run_state_processing TYPE char1 VALUE '2',
    gc_run_state_failed     TYPE char1 VALUE '1',
    gc_run_state_success    TYPE char1 VALUE '0'.

ENDINTERFACE.
