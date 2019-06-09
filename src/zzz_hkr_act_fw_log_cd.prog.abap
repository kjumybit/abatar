*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for logging
*/ =====================================================================


* Logger interface
*** @TODO

* ABAP List Logger
***TODO

* ALV Logger
***TODO

* DB Logger
***TODO

* local type definitions
TYPES: BEGIN OF ts_msg_var,
         v1 TYPE symsg-msgv1,
         v2 TYPE symsg-msgv1,
         v3 TYPE symsg-msgv1,
         v4 TYPE symsg-msgv1,
       END OF ts_msg_var.


* Test function message collector
CLASS zcl_hkr_act_fut_logger DEFINITION.

  PUBLIC SECTION.

    METHODS:
      constructor,
      add_msg IMPORTING iv_msg       TYPE string
                        iv_type      TYPE msgty DEFAULT 'E'
              RETURNING VALUE(ir_me) TYPE REF TO zcl_hkr_act_fut_logger,

      add_messages IMPORTING it_messages  TYPE bapirettab
                   RETURNING VALUE(ir_me) TYPE REF TO zcl_hkr_act_fut_logger.

    DATA:
      mt_messages TYPE bapirettab,
      mr_msg      TYPE REF TO ZCL_HKR_BAPI_MSG_HLP.

ENDCLASS.
