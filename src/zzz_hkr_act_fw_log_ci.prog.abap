*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementations for logging
*/ =====================================================================



*---------------------------------------------------------------
* FUT Logging Container
CLASS zcl_hkr_act_fut_logger IMPLEMENTATION.

  METHOD constructor.
    me->mr_msg = ZCL_HKR_MSG_HLP=>GET_BAPI_MSG_HDL( ).
  ENDMETHOD.

  METHOD add_msg.
    DATA ls_msgvar TYPE ts_msg_var.
    ls_msgvar = iv_msg.
    MESSAGE ID 'ZHKR_ACT' TYPE iv_type NUMBER '001'
      WITH ls_msgvar-v1 ls_msgvar-v2 ls_msgvar-v3 ls_msgvar-v4
      INTO DATA(lv_msg) ##NEEDED.
    me->mr_msg->add_from_symsg( CHANGING ct_return = me->mt_messages ).
    ir_me = me.
  ENDMETHOD.


  METHOD add_messages.
    APPEND LINES OF it_messages TO me->mt_messages.
    ir_me = me.
  ENDMETHOD.


ENDCLASS.
