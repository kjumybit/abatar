*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for the JSON serializer API
*
*/ =====================================================================


CLASS zcl_hkr_lib_json_writer DEFINITION DEFERRED.
CLASS zcl_hkr_lib_json_reader DEFINITION DEFERRED.
CLASS zcl_hkr_act_json_ser_ctx DEFINITION DEFERRED.


INTERFACE zif_hkr_act_api_json_writer.
  DATA:
    mr_object TYPE REF TO object. " Framework object instance
  METHODS:
    get_json RETURNING VALUE(rv_json) TYPE xstring
             RAISING   zcx_hkr_act_base_exc.
ENDINTERFACE.


INTERFACE zif_hkr_act_api_json_reader.
  DATA:
    mv_object TYPE seoclsname, " Name of Framework object
    mv_json   TYPE xstring.
  METHODS:
    get_object RETURNING VALUE(rr_object) TYPE REF TO object
               RAISING   zcx_hkr_act_base_exc.
ENDINTERFACE.


INTERFACE zif_hkr_act_api_json_ser.
  INTERFACES:
    zif_hkr_act_api_json_writer,
    zif_hkr_act_api_json_reader.

ENDINTERFACE.


"/
" Serializer and De-serializer of all public Framework objects.
" Implements the JsonReader and Json Writer interface.
"/
CLASS zcl_hkr_act_api_ser DEFINITION.

  PUBLIC SECTION.

    INTERFACES:
      zif_hkr_act_api_ser.

    CLASS-METHODS:
      class_constructor.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_fw_object,
        objname TYPE seoclsname,
        sername TYPE seoclsname, " must implement zif_hkr_act_api_json_ser
      END OF ts_fw_object.
    TYPES tt_fw_objects TYPE SORTED TABLE OF ts_fw_object WITH UNIQUE KEY objname.

    CLASS-DATA:
      "// supported framework objects
      st_fw_objects TYPE tt_fw_objects.

    METHODS:

      get_fw_object IMPORTING iv_object           TYPE data
                    RETURNING VALUE(rs_fw_object) TYPE ts_fw_object
                    RAISING   zcx_hkr_act_base_exc.

ENDCLASS.


"/ ============================================================================
"  Concrete serializer
"/ ============================================================================


"/
" Global context definition serializer
"/
CLASS zcl_hkr_act_json_ser_ctx DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_hkr_act_api_json_ser.

  PRIVATE SECTION.
    METHODS:

      "// methods for serialization
      build_json IMPORTING ir_node   TYPE REF TO zcl_hkr_act_ctx_node
                           ir_writer TYPE REF TO zcl_hkr_lib_json_writer,

      build_json_for_node IMPORTING ir_node   TYPE REF TO zcl_hkr_act_ctx_node
                                    ir_writer TYPE REF TO zcl_hkr_lib_json_writer,

      build_json_for_attribute IMPORTING ir_attribute TYPE REF TO ts_attribute
                                         ir_writer    TYPE REF TO zcl_hkr_lib_json_writer,

      "// methods for de-serialization
      build_context IMPORTING ir_reader      TYPE REF TO zcl_hkr_lib_json_reader
                    RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node
                    RAISING   zcx_hkr_act_base_exc,

      read_context_node_attributes IMPORTING ir_node   TYPE REF TO zcl_hkr_act_ctx_node
                                             ir_reader TYPE REF TO zcl_hkr_lib_json_reader
                                   RAISING   zcx_hkr_act_base_exc,

      build_context_node_attribute IMPORTING ir_json_obj TYPE REF TO if_sxml_open_element
                                             ir_reader   TYPE REF TO zcl_hkr_lib_json_reader
                                   RETURNING VALUE(rs_attribute) type ts_attribute
                                   RAISING   zcx_hkr_act_base_exc,

      build_context_root_node IMPORTING ir_reader      TYPE REF TO zcl_hkr_lib_json_reader
                              RETURNING VALUE(rr_node) TYPE REF TO zcl_hkr_act_ctx_node
                              RAISING   zcx_hkr_act_base_exc,

      read_context_nodes IMPORTING ir_node   TYPE REF TO zcl_hkr_act_ctx_node
                                   ir_reader TYPE REF TO zcl_hkr_lib_json_reader
                         RAISING   zcx_hkr_act_base_exc,

      build_context_node IMPORTING ir_node     TYPE REF TO zcl_hkr_act_ctx_node
                                   ir_json_obj TYPE REF TO if_sxml_open_element
                                   ir_reader   TYPE REF TO zcl_hkr_lib_json_reader
                         RAISING   zcx_hkr_act_base_exc.

ENDCLASS.
