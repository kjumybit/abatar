*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*/ =====================================================================


include zzz_hkr_act_api_ser_ci.               " API: serialization
INCLUDE zzz_hkr_act_fw_ctx_ci.                " Context
INCLUDE zzz_hkr_act_fw_log_ci.                " logging
INCLUDE zzz_hkr_act_fw_adp_ci.                " Base and standard task adapters
INCLUDE zzz_hkr_act_ctx_bdg_ci.               " binding update
INCLUDE zzz_hkr_act_fw_run_ci.                " Execute and run a test case
INCLUDE zzz_hkr_act_fw_lib_ci.                " Library with helper functions



*---------------------------------------------------------------
* test case base class
* [deprecated]
CLASS zcl_hkr_act_tc_def_base IMPLEMENTATION.

  METHOD zif_hkr_act_tc_def~get_context.
    rr_ctx = me->mr_ctx.
  ENDMETHOD.

  METHOD zif_hkr_act_tc_def~set_exec_def.
    me->mt_tc_steps = it_tc_steps.
  ENDMETHOD.

ENDCLASS.



*---------------------------------------------------------------
* test case class
CLASS zcl_hkr_act_tc_def IMPLEMENTATION.

  METHOD zif_hkr_act_tc_def~get_context.
    rr_ctx = me->mr_ctx.
  ENDMETHOD.

  METHOD zif_hkr_act_tc_def~set_exec_def.
    me->mt_tc_steps = it_tc_steps.
  ENDMETHOD.

ENDCLASS.


*---------------------------------------------------------------
* test case builder
CLASS zcl_hkr_act_tc_builder IMPLEMENTATION.


  METHOD class_constructor.
    mv_cls_name = 'ZCL_HKR_ACT_TC_BUILDER'.         " default class name for instantiation
  ENDMETHOD.


  METHOD get_instance.
    CREATE OBJECT rr_instance TYPE (mv_cls_name).
  ENDMETHOD.


  " Build text case instance using configured context node instance
  METHOD create_tc.

    IF NOT ir_node IS BOUND AND NOT ir_ctx_ref IS BOUND.
    ENDIF.

    " test case definition instance
    rr_instance = NEW zcl_hkr_act_tc_def( ).

    " create context data structure
    rr_instance->mr_ctx = NEW zcl_hkr_act_ctx_tc( ).


    IF ir_node IS BOUND.
      " set context definition root (node structure) and build data structure
      rr_instance->mr_ctx_def = ir_node.
      rr_instance->mr_ctx->mr_data = rr_instance->mr_ctx_def->get_data( ).
    ELSE.
      " set pre-build context data structure
      "***TODO: instantiate or build rr_instance->mr_ctx_def from data ref
      rr_instance->mr_ctx->mr_data = ir_ctx_ref.
    ENDIF.

  ENDMETHOD.



  " build test step for adapter type and parameter
  METHOD create_ts.

    rr_instance = NEW zcl_hkr_act_ts_adp_base( ).
    rr_instance->zif_hkr_act_ts_adp~mv_type = iv_adp_type.
    rr_instance->zif_hkr_act_ts_adp~mt_def = it_parameter.

    " get adapter builder
    DATA(lr_adp_builder) = me->get_adapter_builder( iv_adp_type ).
    lr_adp_builder->init( it_parameter ).

    " build adapter
    rr_instance->zif_hkr_act_ts_adp~mr_adp = lr_adp_builder->create_adapter( it_parameter ).

    " build adapter context
    rr_instance->zif_hkr_act_ts_adp~mr_adp_ctx = lr_adp_builder->create_adapter_context(
                                                   it_parameter = it_parameter
                                                   ir_adp       = rr_instance->zif_hkr_act_ts_adp~mr_adp
                                                 ).

  ENDMETHOD.


  "// get concrete adapter builder for adapter interface
  "***TODO: evaluate parameters, use adapter builder registry based on <IV_ADP_TYPE>
  METHOD get_adapter_builder.

    rr_builder = NEW zcl_hkr_act_adp_fut_builder( ).

  ENDMETHOD.


ENDCLASS.



"/
" Central registry for main framework components
"/
CLASS zcl_hkr_act_registry IMPLEMENTATION.

  METHOD get.

    clear rr_instance.

    "// determine requested interface/object name from parameter template
    DATA(lv_name) = zcl_hkr_act_rtti=>get_object_name( iv_ifc ).

    "// lookup registered components
    read table me->mt_registry REFERENCE INTO data(lr_registry) WITH KEY objname = lv_name.
    if sy-subrc <> 0.
        " not registered: throw exception
        MESSAGE e020(ZZZ_HKR_ACT) with lv_name into data(lv_msg) ##NEEDED.
        zcx_hkr_act_base_exc=>raise_from_symsg( ).
    endif.

    "// instantiate class, if required
    "***TODO: handle exception
    if not lr_registry->instance is bound.
        create object lr_registry->instance type (lr_registry->clsname).
    endif.

    "// and return object instance
    rr_instance = lr_registry->instance.

  ENDMETHOD.


  method set.

    "// lookup registered components
    "***TODO. fire event, when registration changes
    read table me->mt_registry REFERENCE INTO data(lr_registry) WITH KEY objname = iv_objname.
    if sy-subrc = 0 and lr_registry->clsname <> iv_clsname.
        " already registered: throw exception
        MESSAGE e021(ZZZ_HKR_ACT) with iv_clsname iv_objname into data(lv_msg) ##NEEDED.
        zcx_hkr_act_base_exc=>raise_from_symsg( ).
    endif.

    "// update registry
    "***TODO: check class name
    data lr_not_bound type ref to object VALUE IS INITIAL.
    insert value #( objname = iv_objname clsname = iv_clsname instance = lr_not_bound  ) into table me->mt_registry.

  ENDMETHOD.

ENDCLASS.


"/
" Central ACT framework class.
" Provides access to core components and functions.
" - Component registry
"/
CLASS zcl_hkr_act_core IMPLEMENTATION.

  METHOD class_constructor.
  ENDMETHOD.


  METHOD _init.

    "// instantiate core components
    get_registry( ).

    "// register core components
    "   - fw serializer
    sr_registry->set( iv_objname = 'ZIF_HKR_ACT_API_SER' iv_clsname = 'ZCL_HKR_ACT_API_SER' ).

    sv_initialized = abap_true.

  ENDMETHOD.


  METHOD get_registry.
    IF sr_registry IS INITIAL.
      CREATE OBJECT sr_registry TYPE (sv_registry_class).
    ENDIF.
    rr_registry = sr_registry.
  ENDMETHOD.


ENDCLASS.


class zcl_hkr_act IMPLEMENTATION.

  method class_constructor.
    "// do bootstrap actions
    zcl_hkr_act_core=>_init( ).
  ENDMETHOD.
    "// set customer enhancements
  method init.

  ENDMETHOD.

ENDCLASS.
