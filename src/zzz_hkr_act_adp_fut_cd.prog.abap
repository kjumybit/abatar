*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for custom task adapter
*
*  A custom task adapter have access to a custom task context instance
*
*/ =====================================================================

* ----------------------------------------------------------------------
* Test function context



types: begin of ts_param_info,
         attr       type string,        " Attribute of a adapter
         annotation type string,
         mode       type c length 2,    " I: in, O: out, IO: in and out
       end of ts_param_info.
types tt_param_info type standard table of ts_param_info with default key.


types: begin of ts_param_rtti,
         attr       type string,        " Attribute of a adapter
         mode       type c length 2,    " I: in, O: out, IO: in and out
*        dtype      type rs38l_typ,     " data data type
         annotation type string,
         data_ref   type ref to data,   " ref to adapter attribute <(attr)>
         rtti       type ref to cl_abap_datadescr,
       end of ts_param_rtti.
types tt_param_rtti type standard table of ts_param_rtti with default key.



* ----------------------------------------------------------------------
* Custom adapter builder
class zcl_hkr_act_adp_fut_builder definition.

  public section.
    interfaces zif_hkr_act_adp_builder.

  private section.
    data:
      mv_adp_cls_name type seoclsname.     " name of adapter class

endclass.


interface zif_hkr_act_run_adp_fut deferred.


*---------------------------------------------------------------
* Context handler for a custom test function adapter using a
* dynamic context.
CLASS zcl_hkr_act_ctx_adp_fut DEFINITION
  friends zcl_hkr_act_adp_fut_builder.

  PUBLIC SECTION.
    INTERFACES zif_hkr_act_ctx_adp.

    methods:
      register_parameters importing it_param_info type tt_param_info.

    data:
      mr_adp type ref to zif_hkr_act_run_adp_fut.

    ALIASES:
      mr_data FOR zif_hkr_act_ctx~mr_data.

  PRIVATE SECTION.

    methods:
      get_class_attr_ref importing iv_attr_name type string
                         returning value(rr_data) type ref to data
                         exceptions zcx_hkr_act_base_exc.

    data:
      mv_adp_cls_name type seoclsname,     " name of adapter class
      mt_param_rtti type tt_param_rtti.

ENDCLASS.


*---------------------------------------------------------------
* Context handler for a custom test function adapter using a
* static context.
CLASS zcl_hkr_act_ctx_adp_fut_s DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_hkr_act_ctx_adp.

    ALIASES:
      mr_data FOR zif_hkr_act_ctx~mr_data.

  PRIVATE SECTION.
    " static context defined a design time

ENDCLASS.



*---------------------------------------------------------------
* Custom test function adapter using a dynamic context
INTERFACE zif_hkr_act_run_adp_fut.

  interfaces
    zif_hkr_act_run_adp.

  methods:
    get_parameter_info returning value(rt_param_info) type tt_param_info.


endinterface.
