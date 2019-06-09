*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.5
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions

*  =====================================================================
*  Version history:
*
*  0.0.1: Initial implementation with all core components and a dry run
*  0.0.2: Add methods for Validation actions for test function
*  0.0.3: Add Init Method for test function
*         Add process specific Library for re-usable functions
*  0.0.4: Add JSON serializer class
*         Add proxy class to execute a test function w/o own session
*  0.0.5: Add context builder class to instantiate a context data structure
*         Organize classes in new includes (context, run, lib)
*         Redesign: Use of dynamic data context, adapter context and
*         data binding
*
*  =====================================================================
*  Backlog:
*
*  - provide FW method to update context properties after executing test
*    functions (e.g. update HU data)
*  - provide assert methods (with message, with exception)
*  - registry for adapters types & builders
*  - registry for data providers types & builder
*  - registry for validators types & builders, serializer
*  - registry for execution units types & builder
*  - registry for all configured instances (adapters, data provider, validators, ...)
*  - Provide parameter meta data for adapter builder
*/ =====================================================================


*/ =====================================================================
*  Definitions
*
*  Test case def.  : Contains all configuration data to build and run a
*                    test case using a set of test parameters.
*                    Configuration data are:
*                    - the execution plan (required <test steps>[tt_tc_steps])
*                    - data context definition <context definition> to construct
*                      the context data structure in <zif_hkr_act_ctx>
*                    - a context binding for each test function
*                       <zif_hkr_act_run_adp> to the data context
*                       <zif_hkr_act_ctx>
*                    - test parameter provider <tbd>
*                    It is handled by the ACT runtime to execute the configured
*                    test functions (FUTs)
*
*  Test Step Def.   : tbd.
*  {task definition}
*
*  Test case builder: Builds <test case> and <test_step> instance by interpreting
*                     a <test run> definition
*
*
*  Context container: references parameter or runtime data of a static or dynamic
*                     complex type
*                     - provides getter and setter methods to access elements
*                       within the context structure
*
*  Context mapping  : transfer context property values from a source to a
*                     destination context by using <annotations> or
*                     <context binding>
*
*  context binding  : link a property of a source context to a property of a
*                     destination context
*
*  context definition: meta data to define the technical and semantical data
*                      structure of a context container
*                      (prop. JSON format)
*
*  context builder  : create a the technical data structure of a dynamic context
*                     object based on a <contect definition>
*
*/ =============================================================================


INCLUDE zzz_hkr_act_fw_ctx_cd.                " Context



INTERFACE zif_hkr_act_run_adp DEFERRED.


*/
* Global data types
*/
TYPES tv_bdg_mode TYPE c LENGTH 2.     " binding mode of adapter parameters
TYPES tv_adp_type TYPE c LENGTH 6.     " adapter type


"/
" A binding info object maps an adapter attribute <adapter_class>-<ATTR>
" to a context node referenced by a path <PATH> '/node/node/.../property'.
" The <MODE> attribute defines the mapping direction (data flow):
" - 'I' pass value from context to adapter
" - 'O' pass value from adapter to context
" - 'IO' two way
"/
TYPES ts_binding_info type ZZZ_HKR_ACT_ADP_BINDING_S.
TYPES tt_binding_info TYPE STANDARD TABLE OF ts_binding_info WITH DEFAULT KEY.


"/
" Test step description (aka task description).
" A test step has an ID <ID> and is wrapped by an adapter class <TS_ADP-RUN_ADP>.
" The import and export parameters of an test function (adapter) are mapped
" to context properties. The mapping is defined by the binding info list
" <BINDING_INFOS>.
" Additional technical runtime parameters can be defined also.
"/
***TODO: migrate to a class

CLASS zcl_hkr_act_ts_adp_base DEFINITION DEFERRED.

TYPES: BEGIN OF ts_tc_step,
         id            TYPE string,
         own_session   TYPE abap_bool,
         ts_adp        TYPE REF TO zcl_hkr_act_ts_adp_base,
         binding_infos TYPE tt_binding_info,
       END OF ts_tc_step.
TYPES tt_tc_steps TYPE STANDARD TABLE OF ts_tc_step WITH DEFAULT KEY.



*---------------------------------------------------------------
* test case definition
INTERFACE zif_hkr_act_tc_def.

  DATA:
    " reference to the runtime context object
    mr_ctx      TYPE REF TO zcl_hkr_act_ctx_tc,
    " internal context definition (as node structure)
    mr_ctx_def  TYPE REF TO zcl_hkr_act_ctx_node,
    " test step (task) definitions (including context binding)
    mt_tc_steps TYPE tt_tc_steps.

  METHODS:
    get_context RETURNING VALUE(rr_ctx) TYPE REF TO zcl_hkr_act_ctx_tc,

    "// add execution plan to test case
    "***TODO replace test steps list by <execution definition>
    set_exec_def IMPORTING it_tc_steps TYPE tt_tc_steps.

ENDINTERFACE.


*---------------------------------------------------------------
* test case base class
CLASS zcl_hkr_act_tc_def_base DEFINITION ABSTRACT.

  PUBLIC SECTION.

    INTERFACES:
      zif_hkr_act_tc_def.

    ALIASES:
      mr_ctx FOR zif_hkr_act_tc_def~mr_ctx,
      mr_ctx_def FOR zif_hkr_act_tc_def~mr_ctx_def,
      mt_tc_steps FOR zif_hkr_act_tc_def~mt_tc_steps.


    DATA:
      mv_my_cls_name TYPE seoclsname.

ENDCLASS.




*---------------------------------------------------------------
* test case base class
CLASS zcl_hkr_act_tc_def DEFINITION.

  PUBLIC SECTION.

    INTERFACES:
      zif_hkr_act_tc_def.

    ALIASES:
      mr_ctx FOR zif_hkr_act_tc_def~mr_ctx,
      mr_ctx_def FOR zif_hkr_act_tc_def~mr_ctx_def,
      mt_tc_steps FOR zif_hkr_act_tc_def~mt_tc_steps.

ENDCLASS.



"/
" Configuration parameter for adapters
" A parameter has a <name> and a string value <value> or a generic value reference
" <value_ref>
"/
TYPES ts_adp_build_par type ZZZ_HKR_ACT_ADP_BUILD_PAR_S.
TYPES tt_adp_build_par TYPE SORTED TABLE OF ts_adp_build_par WITH UNIQUE KEY name.


*/
* Test step adapter definition
*/
INTERFACE zif_hkr_act_ts_adp.
  DATA:
    mv_type    type tv_adp_type,                      " adapter type
    mt_def     type tt_adp_build_par,                 " adapter definition parameters
    mr_adp     TYPE REF TO zif_hkr_act_run_adp,       " adapter instance to execute a test function
    mr_adp_ctx TYPE REF TO zif_hkr_act_ctx_adp.       " adapter context (adapter property access)
ENDINTERFACE.


*---------------------------------------------------------------
* test step base class
CLASS zcl_hkr_act_ts_adp_base DEFINITION.

  PUBLIC SECTION.

    INTERFACES:
      zif_hkr_act_ts_adp.

    ALIASES:
      mv_type    for zif_hkr_act_ts_adp~mv_type,
      mt_def     for zif_hkr_act_ts_adp~mt_def,
      mr_adp     for zif_hkr_act_ts_adp~mr_adp,       " adapter for test function
      mr_adp_ctx for zif_hkr_act_ts_adp~mr_adp_ctx.   " adapter context


ENDCLASS.



*---------------------------------------------------------------
* adapter builder
INTERFACE zif_hkr_act_adp_builder.

  METHODS:

    init IMPORTING it_parameter TYPE tt_adp_build_par
         RAISING   zcx_hkr_act_base_exc,

    create_adapter
      IMPORTING it_parameter  TYPE tt_adp_build_par
      RETURNING VALUE(rr_adp) TYPE REF TO zif_hkr_act_run_adp
      RAISING   zcx_hkr_act_base_exc,

    create_adapter_context
      IMPORTING it_parameter      TYPE tt_adp_build_par
                ir_adp            TYPE REF TO zif_hkr_act_run_adp
      RETURNING VALUE(rr_ctx_adp) TYPE REF TO zif_hkr_act_ctx_adp
      RAISING   zcx_hkr_act_base_exc.


ENDINTERFACE.


*---------------------------------------------------------------
* test case builder
* Provides builder methods to construct runtime instances for:
* ...
CLASS zcl_hkr_act_tc_builder DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      class_constructor,
      get_instance RETURNING VALUE(rr_instance) TYPE REF TO zcl_hkr_act_tc_builder.


    METHODS:

      "// create test case for dynamic context
      create_tc
        IMPORTING ir_node            TYPE REF TO zcl_hkr_act_ctx_node OPTIONAL
                  ir_ctx_ref         type ref to data optional
        PREFERRED PARAMETER ir_node
        RETURNING VALUE(rr_instance) TYPE REF TO zif_hkr_act_tc_def
        RAISING   zcx_hkr_act_base_exc,

      "// create test step for dynamic context
      create_ts
        IMPORTING iv_adp_type        type tv_adp_type
                  it_parameter       TYPE tt_adp_build_par
        RETURNING VALUE(rr_instance) TYPE REF TO zcl_hkr_act_ts_adp_base
        RAISING   zcx_hkr_act_base_exc.

      "// create execution plan
      "***TODO
      "create_task_def

  PRIVATE SECTION.

    CLASS-DATA:
      mv_cls_name TYPE seoclsname.

    METHODS:

      "// get adapter builder
      get_adapter_builder
        IMPORTING ir_adp_ifc        TYPE data
        RETURNING VALUE(rr_builder) TYPE REF TO zif_hkr_act_adp_builder
        RAISING zcx_hkr_act_base_exc.

ENDCLASS.



"/
" Public framework constants
"/
INTERFACE zif_hkr_act_fw_c.

  CONSTANTS:

    " binding mode
    gc_bdg_mode_in   TYPE tv_bdg_mode VALUE 'I',
    gc_bdg_mode_out  TYPE tv_bdg_mode VALUE 'O',
    gc_bdg_mode_both TYPE tv_bdg_mode VALUE 'IO',

    " build-in adapters
    begin of gc_adp,
        begin of type,
            generic type tv_adp_type VALUE 'GENAPL',    " generic application adapter
        END OF type,
    END OF gc_adp.

ENDINTERFACE.



"/ =============================================================================
" Interface definitions for main framework components
"/ =============================================================================


"/
" Serialize and De-serialize all framework elements.
" - Test Case definition (context definition , test steps definition, execution plan)
" - Context definition
"/

INTERFACE zif_hkr_act_api_json_writer DEFERRED.
INTERFACE zif_hkr_act_api_json_reader DEFERRED.

interface zif_hkr_act_api_ser.

  methods:

    "// Get a writer object that converts the framework object <ir_object>
    "// to a JSON string
    get_json_writer IMPORTING ir_object type data
                    RETURNING VALUE(rr_writer) type ref to zif_hkr_act_api_json_writer
                    RAISING zcx_hkr_act_base_exc,

    "// Get a reader object that creats a framework element <iv_object>
    "// from a JSON string <iv_json>
    get_json_reader importing iv_object type data
                              iv_json type xstring
                    RETURNING VALUE(rr_reader) type ref to zif_hkr_act_api_json_reader
                    RAISING zcx_hkr_act_base_exc.

ENDINTERFACE.


class zcl_hkr_act_core DEFINITION DEFERRED.
class zcl_hkr_act DEFINITION DEFERRED.

"/
" Central registry for main framework components
" Build in components
" -JSON serializer for a test case definition: IFC <ZIF_HHKR_ACT_API_SER>
" -
"/
class zcl_hkr_act_registry DEFINITION CREATE PRIVATE
  FRIENDS zcl_hkr_act_core.

  public SECTION.

    methods:

      get IMPORTING iv_ifc type data
          RETURNING VALUE(rr_instance) type ref to object
          RAISING zcx_hkr_act_base_exc,

       "// register component
      set IMPORTING iv_objname type seoclsname
                    iv_clsname type seoclsname
          RAISING zcx_hkr_act_base_exc.

  private section.

    types:
      begin of ts_registry,
        objname  type seoclsname,     " interface or class name (key)
        clsname  type seoclsname,     " class name
        instance type ref to object,  " class instance
      end of ts_registry.
    types tt_registry type SORTED TABLE OF ts_registry with UNIQUE key objname.

    data:
      mt_registry type tt_registry.

endclass.



"/
" Central ACT framework class.
" Provides access to core components and functions.
" - Component registry
"/
class zcl_hkr_act_core DEFINITION
  FRIENDS zcl_hkr_act.

  PUBLIC SECTION.
    class-methods:

      class_constructor,

      get_registry RETURNING VALUE(rr_registry) type ref to zcl_hkr_act_registry.

  PRIVATE SECTION.
    class-data:

      " default implementation classes of core components
      sv_registry_class type seoclsname VALUE 'ZCL_HKR_ACT_REGISTRY',

      " singletons
      sr_registry type ref to zcl_hkr_act_registry,

      sv_initialized type abap_bool value abap_false.

    class-METHODS:

      " initialize and prepare core components
      _init.

ENDCLASS.


"/
" Main starter class of Abap Task Runner.
" Initialize framework with core build components.
" - ...
" Each app has to call the <Init> method, which can be used to
" set customer enhancement components.
" - ...
"/
class zcl_hkr_act DEFINITION.

  public section.

    CONSTANTS
       VERSION type string value '0.1.0'.

    class-METHODS:
      class_constructor,
      init IMPORTING is_prop type any OPTIONAL.

endclass.


include zzz_hkr_act_api_ser_cd.      " API: serialization
INCLUDE zzz_hkr_act_fw_log_cd.       " logging
INCLUDE zzz_hkr_act_fw_adp_cd.       " test & task adapters
INCLUDE zzz_hkr_act_ctx_bdg_cd.      " binding update
INCLUDE zzz_hkr_act_fw_run_cd.       " execute a test case
INCLUDE zzz_hkr_act_fw_lib_cd.       " library with helper functions
