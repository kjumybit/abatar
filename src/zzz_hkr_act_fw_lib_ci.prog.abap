*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementation of helper functions
*/ =====================================================================


* ---------------------------------------------------------------
* JSON serializer & de-serializer
CLASS zcl_hkr_act_json_conv IMPLEMENTATION.

  METHOD serialize_data.
    CLEAR rv_json.

    FIELD-SYMBOLS <lv_data> TYPE any.
    ASSIGN ir_data->* TO <lv_data>.

    DATA(lr_writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id SOURCE root = <lv_data>
                           RESULT XML lr_writer
                           OPTIONS initial_components = 'include'.
    " return JSON string
    rv_json = lr_writer->get_output( ).
  ENDMETHOD.


  METHOD deserialize_data.

    FIELD-SYMBOLS <lv_data> TYPE any.
    ASSIGN cr_data->* TO <lv_data>.

    DATA(lr_reader) = cl_sxml_string_reader=>create( iv_json ).

    CALL TRANSFORMATION id SOURCE XML lr_reader
                           RESULT root = <lv_data>.

  ENDMETHOD.


ENDCLASS.


"/
" Base methods to create a a JSON document. It wraps the native CL_SXML_STRING_WRITER
" class and provides methods for token-based rendering.
"/
CLASS zcl_hkr_lib_json_writer IMPLEMENTATION.


  METHOD get_instance.
    "if not sr_writer is BOUND.
    "  sr_writer = new zcl_hkr_lib_json_writer( ).
    "endif.
    rr_writer = NEW zcl_hkr_lib_json_writer( ).
  ENDMETHOD.

  METHOD constructor.
    me->mr_sxml_writer = CAST if_sxml_writer(
        cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json  )
    ).
  ENDMETHOD.


  METHOD open_object.
    me->mr_sxml_writer->open_element( name = 'object' ).
    IF NOT iv_name IS INITIAL.
      me->mr_sxml_writer->write_attribute( name = 'name' value = iv_name ).
    ENDIF.
  ENDMETHOD.


  METHOD open_array.
    me->mr_sxml_writer->open_element( name = 'array').
    me->mr_sxml_writer->write_attribute( name = 'name' value = iv_name ).
  ENDMETHOD.


  METHOD open_property.
    me->mr_sxml_writer->open_element( name = 'str').
    me->mr_sxml_writer->write_attribute( name = 'name' value = iv_name ).
    me->mr_sxml_writer->write_value( value = iv_value ).
  ENDMETHOD.


  METHOD close_element.
    me->mr_sxml_writer->close_element( ).
  ENDMETHOD.


  METHOD get_output.
    rv_output = CAST cl_sxml_string_writer( me->mr_sxml_writer )->get_output(  ).
  ENDMETHOD.

ENDCLASS.


"/
" Base methods to parse a a JSON document. It wraps the native CL_SXML_STRING_READER
" class and provides methods for token-based rendering.
"/
CLASS zcl_hkr_lib_json_reader IMPLEMENTATION.
  METHOD get_instance.
    rr_reader = NEW zcl_hkr_lib_json_reader( iv_json ).
  ENDMETHOD.

  METHOD constructor.
    me->mr_sxml_reader = CAST if_sxml_reader(
        cl_sxml_string_reader=>create( iv_json  )
    ).
  ENDMETHOD.


  METHOD next_node.
    rr_node = me->mr_sxml_reader->read_next_node( ).
  ENDMETHOD.


  "/
  "    TYPE        : CL_SXML_OPEN_ELEMENT
  "    QNAME-NAME  :                                    { 'object'      |  'array'     | 'str'          }
  "    M_ATTRIBUTES[1] : CL_SXML_ATTRIBUTE->M_VALUE :   { <object name> |  <arry name> | <proprty name> }
  "                      CL_SXML_ATTRIBUTE->QNAME-NAME: { 'name'        |  'name'      | 'name'         }
  "
  "    TYPE        : CL_SXML_VALUE
  "    M_VALUE     : <property value>
  "
  "    TYPE        : CL_SXML_CLOSE_ELEMENT
  "    QNAME-NAME  :                                   { 'object', 'array', 'str' }
  "
  "
  "/
  METHOD next_object.
    CLEAR rr_node.

    "// read next node
    DATA(lr_node) = me->next_node( ).
    IF lr_node IS INITIAL.
      MESSAGE e032(zzz_hkr_act) INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    "// check sXML node type
    IF lr_node->type <> lr_node->co_nt_element_open.
      MESSAGE e033(zzz_hkr_act) WITH
        me->get_node_type_string( if_sxml_node=>co_nt_element_open )
        me->get_node_type_string( lr_node->type ) INTO lv_msg.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    DATA(lr_open_el) = CAST if_sxml_open_element( lr_node ).

    "// check JSON type
    DATA(lv_json_type) = me->get_json_type_from_element( lr_open_el->qname-name ).
    IF lv_json_type <> gc_json_type_object.
      MESSAGE e033(zzz_hkr_act) WITH
        me->get_json_type_string( gc_json_type_object )
        me->get_json_type_string( lv_json_type ) INTO lv_msg.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    IF NOT iv_name IS INITIAL.
      "// check JSON object name
      DATA(lv_obj_name) = me->get_element_name( lr_open_el ).
      IF lv_obj_name <> iv_name.
        MESSAGE e035(zzz_hkr_act) WITH iv_name lv_obj_name INTO lv_msg.
        zcx_hkr_act_base_exc=>raise_from_symsg( ).
      ENDIF.
    ENDIF.

    "// return JSON object node
    rr_node = lr_open_el.
  ENDMETHOD.


  METHOD close_element.

    "// read next node
    DATA(lr_node) = me->next_node( ).
    IF lr_node IS INITIAL.
      MESSAGE e032(zzz_hkr_act) INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    "// check sXML node type
    IF lr_node->type <> if_sxml_node=>co_nt_element_close.
      MESSAGE e033(zzz_hkr_act) WITH
        me->get_node_type_string( if_sxml_node=>co_nt_element_close )
        me->get_node_type_string( lr_node->type ) INTO lv_msg.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    IF NOT iv_json_type IS INITIAL.
      "// check JSON type of closed element
      DATA(lr_close_el) = CAST if_sxml_close_element( lr_node ).
      DATA(lv_json_type) = me->get_json_type_from_element( lr_close_el->qname-name ).
      IF iv_json_type <> lv_json_type.
        MESSAGE e033(zzz_hkr_act) WITH
           me->get_json_type_string( iv_json_type )
           me->get_json_type_string( lv_json_type ) INTO lv_msg.
        zcx_hkr_act_base_exc=>raise_from_symsg( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD close_object.
    me->close_element( gc_json_type_object ).
  ENDMETHOD.


  METHOD get_node_type_string.
    rv_string = SWITCH #( iv_type
      WHEN if_sxml_node=>co_nt_element_open THEN 'OpenElement'
      WHEN if_sxml_node=>co_nt_attribute THEN 'Attribute'
      WHEN if_sxml_node=>co_nt_element_close THEN 'CloseElement'
      WHEN if_sxml_node=>co_nt_value THEN 'Value'
      ELSE 'unknown'
    ).
  ENDMETHOD.


  "// map type name to ID
  METHOD get_json_type_from_element.
    rv_json_type = SWITCH #( iv_qname
      WHEN 'object' THEN gc_json_type_object
      WHEN 'array' THEN gc_json_type_array
      WHEN 'str' THEN gc_json_type_property
      ELSE space
    ).
  ENDMETHOD.


  METHOD get_json_type_string.
    rv_string = SWITCH #( iv_json_type
      WHEN gc_json_type_object THEN 'Object'
      WHEN gc_json_type_array THEN 'Array'
      WHEN gc_json_type_property THEN 'Property'
      ELSE 'unknown'
    ).
  ENDMETHOD.


  METHOD get_element_name.
    CLEAR rv_name.
    DATA(lr_attr) = ir_element->get_attribute_value( 'name' ).
    IF lr_attr IS BOUND.
      rv_name = lr_attr->get_value( ).
    ENDIF.
  ENDMETHOD.


  METHOD is_close_object.
     rv_is_close = xsdbool(
        ir_node->type = if_sxml_node=>co_nt_element_close and
        me->get_json_type_from_element( CAST if_sxml_close_element( ir_node )->qname-name ) = gc_json_type_object
     ).
  ENDMETHOD.


  METHOD is_close_array.
     rv_is_close = xsdbool(
        ir_node->type = if_sxml_node=>co_nt_element_close and
        me->get_json_type_from_element( CAST if_sxml_close_element( ir_node )->qname-name ) = gc_json_type_array
     ).
  ENDMETHOD.


  METHOD is_json_type_object.
    rv_is_object = abap_false.

    "// check sXML node type: element open
    if ir_node->type <> if_sxml_node=>co_nt_element_open.
      return.
    endif.

    DATA(lr_open_el) = CAST if_sxml_open_element( ir_node ).

    "// check JSON type object
    if me->get_json_type_from_element( lr_open_el->qname-name ) <> gc_json_type_object.
      return.
    endif.

    "// check property name
    if not iv_name is INITIAL and me->get_element_name( lr_open_el ) <> iv_name.
      return.
    endif.

    rv_is_object = abap_true.
  ENDMETHOD.


  METHOD is_json_type_array.
    rv_is_array = abap_false.

    "// check sXML node type: element open
    if ir_node->type <> if_sxml_node=>co_nt_element_open.
      return.
    endif.

    DATA(lr_open_el) = CAST if_sxml_open_element( ir_node ).

    "// check JSON type array
    if me->get_json_type_from_element( lr_open_el->qname-name ) <> gc_json_type_array.
      return.
    endif.

    "// check property name
    if not iv_name is INITIAL and me->get_element_name( lr_open_el ) <> iv_name.
      return.
    endif.

    rv_is_array = abap_true.
  ENDMETHOD.


  METHOD is_json_type_property.
    rv_is_property = abap_false.

    "// check sXML node type: element open
    if ir_node->type <> if_sxml_node=>co_nt_element_open.
      return.
    endif.

    DATA(lr_open_el) = CAST if_sxml_open_element( ir_node ).

    "// check JSON type property
    if me->get_json_type_from_element( lr_open_el->qname-name ) <> gc_json_type_property.
      return.
    endif.

    "// check property name
    if not iv_name is INITIAL and me->get_element_name( lr_open_el ) <> iv_name.
      return.
    endif.

    rv_is_property = abap_true.
  ENDMETHOD.


  method is_json_property_value.
    rv_is_value = xsdbool( ir_node->type = if_sxml_node=>co_nt_value ).
  ENDMETHOD.

ENDCLASS.


"/
" RTTI functions
"/
CLASS zcl_hkr_act_rtti IMPLEMENTATION.

  METHOD get_object_name.

    CLEAR rv_name.

    DATA(lr_rtti_base) = cl_abap_typedescr=>describe_by_data( iv_ifc ).

    IF lr_rtti_base->type_kind <> cl_abap_typedescr=>typekind_iref AND
       lr_rtti_base->type_kind <> cl_abap_typedescr=>typekind_oref.
      " unknown
      RETURN.
    ENDIF.

    " determine interface or class name
    DATA lr_rtti_ref TYPE REF TO cl_abap_refdescr.
    lr_rtti_ref ?= lr_rtti_base.

    " get referenced object RTTI
    DATA(lr_rtti_object) = lr_rtti_ref->get_referenced_type( ).
    rv_name = lr_rtti_object->get_relative_name( ).

  ENDMETHOD.

ENDCLASS.
