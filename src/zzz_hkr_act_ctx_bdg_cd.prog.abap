*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for context binding
*
*  context binding for adapter: <zif_hkr_act_ctx_bdg_adp>
*    implemented by an
*/ =====================================================================



* ----------------------------------------------------------------------
* Context binding
* Implements context data updates for all bindings
* between a context data reference and adapter parameters
*
CLASS zcl_hkr_act_ctx_bdg_update DEFINITION.

  PUBLIC SECTION.

    METHODS:
      update_binding IMPORTING ir_ctx          TYPE REF TO zif_hkr_act_ctx_tc
                               ir_adp_ctx      TYPE REF TO zif_hkr_act_ctx_adp
                               iv_mode         TYPE clike
                               it_binding_info TYPE tt_binding_info
                    RAISING zcx_hkr_act_base_exc.

ENDCLASS.
