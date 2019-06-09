*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Definitions for helper functions
*/ =====================================================================


"/
" JSON serializer and de-serializer
"/
CLASS zcl_hkr_act_json_conv DEFINITION.

  PUBLIC SECTION.

    METHODS:
      serialize_data IMPORTING ir_data        TYPE REF TO data
                     RETURNING VALUE(rv_json) TYPE xstring,

      "/
      " The JSON string is de-serialized to an existing data structure
      " referebced by <CR_DATA>. The data type must by type compatible.
      "/
      deserialize_data IMPORTING iv_json TYPE xstring
                       CHANGING  cr_data TYPE REF TO data.


ENDCLASS.


"/
" Base methods to create a a JSON document. It wraps the native CL_SXML_STRING_WRITER
" class and provides methods for token-based rendering.
"/
CLASS zcl_hkr_lib_json_writer DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance RETURNING VALUE(rr_writer) TYPE REF TO zcl_hkr_lib_json_writer.

    METHODS:
      open_object IMPORTING iv_name TYPE string OPTIONAL,

      open_array IMPORTING iv_name TYPE string,

      open_property IMPORTING iv_name  TYPE string
                              iv_value TYPE string,

      close_element,

      get_output RETURNING VALUE(rv_output) TYPE xstring.

  PRIVATE SECTION.

    METHODS:
      constructor.

    CLASS-DATA:
      sr_writer TYPE REF TO zcl_hkr_lib_json_writer.

    DATA:
      mr_sxml_writer TYPE REF TO if_sxml_writer.

ENDCLASS.


"/
" Base methods to parse a a JSON document. It wraps the native CL_SXML_STRING_READER
" class and provides methods for token-based rendering.
"/
CLASS zcl_hkr_lib_json_reader DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.

    types tv_json_type type c LENGTH 1.

    CONSTANTS:
       gc_json_type_object type tv_json_type VALUE 'O',
       gc_json_type_array type tv_json_type VALUE 'A',
       gc_json_type_property type tv_json_type VALUE 'P'.

    CLASS-METHODS:
      get_instance IMPORTING iv_json          TYPE xstring
                   RETURNING VALUE(rr_reader) TYPE REF TO zcl_hkr_lib_json_reader.

    METHODS:
      next_node RETURNING VALUE(rr_node) TYPE REF TO if_sxml_node,

      next_object IMPORTING iv_name        TYPE string OPTIONAL
                  RETURNING VALUE(rr_node) TYPE REF TO IF_SXML_OPEN_ELEMENT
                  RAISING   zcx_hkr_act_base_exc,

      close_object RAISING zcx_hkr_act_base_exc,

      close_element IMPORTING iv_json_type TYPE tv_json_type OPTIONAL
                    RAISING   zcx_hkr_act_base_exc,

      is_close_object IMPORTING ir_node            type ref to if_sxml_node
                      RETURNING VALUE(rv_is_close) type abap_bool,

      is_close_array IMPORTING ir_node            type ref to if_sxml_node
                     RETURNING VALUE(rv_is_close) type abap_bool,

      is_json_type_object IMPORTING ir_node            type ref to if_sxml_node
                                    iv_name            type string optional
                         RETURNING VALUE(rv_is_object) type abap_bool,

      is_json_type_array IMPORTING ir_node            type ref to if_sxml_node
                                   iv_name            type string optional
                         RETURNING VALUE(rv_is_array) type abap_bool,

      is_json_type_property IMPORTING ir_node               type ref to if_sxml_node
                                      iv_name               type string optional
                            RETURNING VALUE(rv_is_property) type abap_bool,

      is_json_property_value IMPORTING ir_node            type ref to if_sxml_node
                             RETURNING VALUE(rv_is_value) type abap_bool,

      get_node_type_string IMPORTING iv_type          type IF_SXML_NODE=>node_type
                           RETURNING VALUE(rv_string) type string,

      get_element_name IMPORTING ir_element type ref to IF_SXML_OPEN_ELEMENT
                       RETURNING VALUE(rv_name) type string.

  PRIVATE SECTION.

    METHODS:
      constructor IMPORTING iv_json TYPE xstring,

      get_json_type_from_element IMPORTING iv_qname type string
                                 RETURNING VALUE(rv_json_type) type tv_json_type,

      get_json_type_string IMPORTING iv_json_type type tv_json_type
                           RETURNING VALUE(rv_string) type string.

    class-data:
      sr_reader TYPE REF TO zcl_hkr_lib_json_reader.

    DATA:
      mr_sxml_reader TYPE REF TO if_sxml_reader.

ENDCLASS.


"/
" RTTI functions
"/
CLASS zcl_hkr_act_rtti DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      "// Get relative name for interface or class represented by the
      "// <iv_ifc> object reference
      get_object_name IMPORTING iv_ifc         TYPE data
                      RETURNING VALUE(rv_name) TYPE string.

ENDCLASS.
