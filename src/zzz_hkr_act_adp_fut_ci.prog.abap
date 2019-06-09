*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementation for base and standard task & test adapters
*/ =====================================================================


* ----------------------------------------------------------------------
* Custom adapter builder
CLASS zcl_hkr_act_adp_fut_builder IMPLEMENTATION.


  METHOD zif_hkr_act_adp_builder~init.

    " get name of FUT class
    "***TODO: use helper class: data(lv_string_value) = get_string_parameter('<key>')

    me->mv_adp_cls_name = it_parameter[ name = 'CLASS_NAME' ]-value.

    "***TODO: check class name
  ENDMETHOD.


  "/
  "  Create FUT adapter instance for class name IT_PARAMETER['CLASS_NAME']
  "/
  METHOD zif_hkr_act_adp_builder~create_adapter.
    CREATE OBJECT rr_adp TYPE (me->mv_adp_cls_name).
  ENDMETHOD.


  "/
  "  Create FUT adapter context for FUT adapter IR_ADP
  "/
  METHOD zif_hkr_act_adp_builder~create_adapter_context.

    " create instance
    DATA(lr_ctx_adp) = NEW zcl_hkr_act_ctx_adp_fut( ).

    " set fut adapter instance
    lr_ctx_adp->mr_adp ?= ir_adp.
    lr_ctx_adp->mv_adp_cls_name = me->mv_adp_cls_name.

    " get parameter info from adapter and set fut parameter info in context
    lr_ctx_adp->register_parameters( lr_ctx_adp->mr_adp->get_parameter_info( ) ).

    " return instance
    rr_ctx_adp = lr_ctx_adp.
  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------
* Test function adapter using a dynamic context
CLASS zcl_hkr_act_ctx_adp_fut IMPLEMENTATION.

  "/
  " Get reference of value for export parameter <IV_PAR>.
  " If the parameter doesn't exist, an exception is thrown.
  "/
  METHOD zif_hkr_act_ctx_adp~get_exp_par_value.
    rr_value = me->get_class_attr_ref( iv_par ).
  ENDMETHOD.


  "/
  " Set value referenced by <IR_VALUE> value for import parameter <IV_PAR>
  " If the parameter doesn't exist, an exception is thrown.
  "/
  METHOD zif_hkr_act_ctx_adp~set_imp_par_value.

    DATA(lr_attr) = me->get_class_attr_ref( iv_par ).

    ASSIGN lr_attr->* TO FIELD-SYMBOL(<lv_attr>).
    ASSIGN ir_value->* TO FIELD-SYMBOL(<lv_value>).

    IF NOT <lv_attr> IS ASSIGNED OR NOT <lv_value> IS ASSIGNED.
      "***TODO: throw exception
    ENDIF.

    <lv_attr> = CONV #( <lv_value> ).
  ENDMETHOD.



  "/
  " Get reference of class attribute <IV_ATTR_NAME>.
  " If the attribute doesn't exist or is not accessible, an exception is thrown.
  "/
  METHOD get_class_attr_ref.
    CLEAR rr_data.

    DATA lr_fut TYPE REF TO object.
    lr_fut ?= me->mr_adp.  " cast to a class reference

    ASSIGN lr_fut->(iv_attr_name) TO FIELD-SYMBOL(<lv_attr>).
    IF NOT <lv_attr> IS ASSIGNED OR sy-subrc <> 0.
      "***TODO trigger exception
    ENDIF.

    " return data reference
    rr_data = REF data( <lv_attr> ).

  ENDMETHOD.


  "/
  "  Store parameter info of FUT adapter.
  "  Determine runtime type information for parameter and set reference
  "  to FUT instance attribute
  "/
  METHOD register_parameters.

    DATA lr_value TYPE REF TO data.

    "***TODO catch exceptions: access to class attribute
    "                        : RTTI

    CLEAR me->mt_param_rtti.
    LOOP AT it_param_info REFERENCE INTO DATA(lr_param).

      " get RTTI by adapter property
      lr_value = me->get_class_attr_ref( lr_param->attr ).

      " add parameter description
      APPEND VALUE #(
        attr       = lr_param->attr
        mode       = lr_param->mode
*         dtype
        annotation = lr_param->annotation
        data_ref   = lr_value
        rtti       = CAST #( cl_abap_typedescr=>describe_by_data_ref( lr_value ) )
      ) TO me->mt_param_rtti.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.



*---------------------------------------------------------------
* Test function adapter using a static context
CLASS zcl_hkr_act_ctx_adp_fut_s IMPLEMENTATION.

  METHOD zif_hkr_act_ctx_adp~get_exp_par_value.
    "***TODO
  ENDMETHOD.

  METHOD zif_hkr_act_ctx_adp~set_imp_par_value.
    "***TODO
  ENDMETHOD.

ENDCLASS.
