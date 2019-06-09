*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for context handling
*/ =====================================================================


*---------------------------------------------------------------
* base context
INTERFACE zif_hkr_act_ctx.
  DATA:
    mr_data TYPE REF TO data.

ENDINTERFACE.


*---------------------------------------------------------------
* test case context
INTERFACE zif_hkr_act_ctx_tc.

  INTERFACES zif_hkr_act_ctx.

  METHODS:

    get_typed_property IMPORTING iv_path  TYPE string
                                 iv_dtype TYPE rs38l_typ DEFAULT 'STRING'
                       EXPORTING ev_value TYPE any
                       RAISING   zcx_hkr_act_base_exc,

    get_property IMPORTING iv_path         TYPE string
                 RETURNING VALUE(rr_value) TYPE REF TO data
                 RAISING   zcx_hkr_act_base_exc,

    set_property IMPORTING iv_path  TYPE string
                           ir_value TYPE REF TO data OPTIONAL
                           iv_value TYPE data OPTIONAL
                 RAISING   zcx_hkr_act_base_exc.

  ALIASES:
    mr_data FOR zif_hkr_act_ctx~mr_data.

ENDINTERFACE.


*/
* Adapter context
* These methods are used by the framework to access the import and
* export parameter values of any adapter.
*/
INTERFACE zif_hkr_act_ctx_adp.

  INTERFACES zif_hkr_act_ctx.

  METHODS:
    get_exp_par_value IMPORTING iv_par          TYPE string
                      RETURNING VALUE(rr_value) TYPE REF TO data
                      RAISING   zcx_hkr_act_base_exc,

    set_imp_par_value IMPORTING iv_par   TYPE string
                                ir_value TYPE REF TO data
                      RAISING   zcx_hkr_act_base_exc.

  ALIASES:
    mr_data FOR zif_hkr_act_ctx~mr_data.

ENDINTERFACE.



*/
* Global data context with property binding
* Provides setter and getter methods to access context nodes.
*/
CLASS zcl_hkr_act_ctx_tc DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_hkr_act_ctx_tc.

    ALIASES:
      mr_data FOR zif_hkr_act_ctx~mr_data,
      get_typed_property FOR zif_hkr_act_ctx_tc~get_typed_property,
      get_property FOR zif_hkr_act_ctx_tc~get_property,
      set_property FOR zif_hkr_act_ctx_tc~set_property.

    METHODS:
      get_string_property IMPORTING iv_path         TYPE string
                          RETURNING VALUE(rv_value) TYPE string
                          RAISING   zcx_hkr_act_base_exc.

  PRIVATE SECTION.

    METHODS:
      get_node IMPORTING ir_parent_node TYPE REF TO data         " parent context node
                         iv_path        TYPE string
               RETURNING VALUE(rr_node) TYPE REF TO data         " context node
               RAISING   zcx_hkr_act_base_exc,

      get_node_element IMPORTING ir_node        TYPE REF TO data        " node
                                 iv_atr_idx     TYPE string      " attribute name or table index
                       RETURNING VALUE(rr_node) TYPE REF TO data    " context node
                       RAISING   zcx_hkr_act_base_exc.

ENDCLASS.



CLASS zcl_hkr_act_ctx_node DEFINITION DEFERRED.
CLASS zcl_hkr_act_ctx_builder DEFINITION DEFERRED.

TYPES tt_nodes TYPE STANDARD TABLE OF REF TO zcl_hkr_act_ctx_node WITH DEFAULT KEY.
TYPES: BEGIN OF ts_attribute,
         name       TYPE fieldname,
         dtype      TYPE rs38l_typ,               " data type
         annotation TYPE string,
       END OF ts_attribute.

TYPES tt_attributes TYPE STANDARD TABLE OF ts_attribute WITH DEFAULT KEY.
TYPES: BEGIN OF ts_annotation,
         name     TYPE string,
         ctx_path TYPE string,
         data_ref TYPE REF TO data,
       END OF ts_annotation.
TYPES tt_annotations TYPE STANDARD TABLE OF ts_annotation.


*---------------------------------------------------------------
* Context node
* Represents a node within the hierarchical data structure of the
* context object. A node has typed attributes and/or child nodes.
CLASS zcl_hkr_act_ctx_node DEFINITION CREATE PRIVATE FRIENDS zcl_hkr_act_ctx_builder.
  PUBLIC SECTION.

    CLASS-METHODS:
      create_root RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node.

    METHODS:
      constructor IMPORTING iv_name       TYPE fieldname
                            iv_annotation TYPE string OPTIONAL
                            ir_parent     TYPE REF TO zcl_hkr_act_ctx_node OPTIONAL,

      add_child IMPORTING iv_name        TYPE fieldname
                          iv_annotation  TYPE string OPTIONAL
                RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node,

      add_sibling IMPORTING iv_name        TYPE fieldname
                            iv_annotation  TYPE string OPTIONAL
                  RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node,

      get_root RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node,

      get_parent EXPORTING ir_node        TYPE REF TO zcl_hkr_act_ctx_node
                 RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node,

      add_attribute IMPORTING iv_name        TYPE fieldname
                              iv_type        TYPE rs38l_typ
                              iv_annotation  TYPE string OPTIONAL
                    RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node,

      get_data RETURNING VALUE(rr_data) TYPE REF TO data,
      get_type_handle RETURNING VALUE(rr_handle) TYPE REF TO cl_abap_structdescr.

    DATA:
      mr_data       TYPE REF TO data,     " [optional] Reference of the created data structure
      " only filled for a root node (up to now)
      " ***TODO: get references of if internal nodes of the data structure
      mr_struc_desc TYPE REF TO cl_abap_structdescr READ-ONLY,

      mv_name       TYPE fieldname READ-ONLY,
      mv_annotation TYPE string READ-ONLY,
      mt_nodes      TYPE tt_nodes READ-ONLY,
      mt_attribues  TYPE tt_attributes READ-ONLY.


  PRIVATE SECTION.
    DATA:
      mt_annotations TYPE tt_annotations,
      mr_root        TYPE REF TO zcl_hkr_act_ctx_node,
      mr_parent      TYPE REF TO zcl_hkr_act_ctx_node,
      mr_builder     TYPE REF TO zcl_hkr_act_ctx_builder.

ENDCLASS.


*---------------------------------------------------------------
CLASS zcl_hkr_act_ctx_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ir_ctx_node TYPE REF TO zcl_hkr_act_ctx_node,
      build.

  PRIVATE SECTION.
    METHODS:
      build_structure_rtti IMPORTING ir_ctx_node      TYPE REF TO zcl_hkr_act_ctx_node
                           RETURNING VALUE(rr_handle) TYPE REF TO cl_abap_structdescr.
    DATA:
      mr_ctx_root TYPE REF TO zcl_hkr_act_ctx_node.
ENDCLASS.
