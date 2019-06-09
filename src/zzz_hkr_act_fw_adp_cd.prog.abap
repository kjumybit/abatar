*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for task, test and executable unit adapters
*
*  Each concrete adapter type must provide and implement a context
*  interface to get access to import and expert parameters.
*/ =====================================================================



* ----------------------------------------------------------------------
* base interface for all callable adapters
INTERFACE zif_hkr_act_run_adp.

  METHODS:
    init
      IMPORTING ir_alog           TYPE REF TO zcl_hkr_act_fut_logger
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_hkr_act_base_exc,

    run
      IMPORTING ir_alog           TYPE REF TO zcl_hkr_act_fut_logger
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_hkr_act_base_exc,

    validation_before
      IMPORTING ir_alog           TYPE REF TO zcl_hkr_act_fut_logger
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_hkr_act_base_exc,

    validation_after
      IMPORTING ir_alog           TYPE REF TO zcl_hkr_act_fut_logger
      RETURNING VALUE(rv_success) TYPE abap_bool
      RAISING   zcx_hkr_act_base_exc.

  DATA:
    mr_ctx TYPE REF TO zif_hkr_act_ctx_adp.  " access to adapter context/parameters

ENDINTERFACE.



include zzz_hkr_act_adp_fut_cd.                " FUT Adapter
