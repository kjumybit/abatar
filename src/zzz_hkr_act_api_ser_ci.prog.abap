*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementations of the JSON serializer API
*
*/ =====================================================================



CLASS zcl_hkr_act_api_ser IMPLEMENTATION.

  "// Static initialization
  METHOD class_constructor.

    " set supported framework objects, which can be serialized / de-serialized
    "***TODO: check class implements interface "zif_hkr_act_api_json_ser"
    INSERT VALUE #(
        objname = 'ZCL_HKR_ACT_CTX_NODE'
        sername = 'ZCL_HKR_ACT_JSON_SER_CTX'
    ) INTO TABLE st_fw_objects.

  ENDMETHOD.


  "// Return own instance as writer object that converts the framework object <iv_object>
  "// to a JSON string
  METHOD zif_hkr_act_api_ser~get_json_writer.
    DATA(ls_fw_object) = me->get_fw_object( ir_object ).
    CREATE OBJECT rr_writer TYPE (ls_fw_object-sername).
    rr_writer->mr_object = ir_object.
  ENDMETHOD.


  "// Return own instance as reader object that creates a framework element <iv_object>
  "// from a JSON string <iv_json>
  METHOD zif_hkr_act_api_ser~get_json_reader.
    DATA(ls_fw_object) = me->get_fw_object( iv_object ).
    CREATE OBJECT rr_reader TYPE (ls_fw_object-sername).
    rr_reader->mv_object = ls_fw_object.
    rr_reader->mv_json = iv_json.
  ENDMETHOD.


  "// get name  of framework object or throw exception if unknown / not supported
  METHOD get_fw_object.
    CLEAR rs_fw_object.

    " get relative name from RTTI
    DATA(lv_name) = zcl_hkr_act_rtti=>get_object_name( iv_object ).

    IF lv_name IS INITIAL.
      MESSAGE e030(zzz_hkr_act) INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDIF.

    " validate supported fw object
    TRY.
        rs_fw_object = st_fw_objects[ objname = lv_name ].
      CATCH  cx_sy_itab_line_not_found.
        " unknown / not supported object
        MESSAGE e031(zzz_hkr_act) WITH lv_name INTO lv_msg.
        zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.



"/ ============================================================================
"  Concrete serializer
"/ ============================================================================


"/
" Global context definition serializer
" Convert the dynamic context definition to/from JSON.
"/
CLASS zcl_hkr_act_json_ser_ctx IMPLEMENTATION.

  "/
  " Return JSON string of context definition
  "/
  METHOD zif_hkr_act_api_json_writer~get_json.
    CLEAR rv_json.

    " get root node of context definition
    DATA lr_ctx_def TYPE REF TO zcl_hkr_act_ctx_node.
    lr_ctx_def = CAST #( me->zif_hkr_act_api_json_writer~mr_object ).

    " get native JSON writer
    DATA(lr_writer) = zcl_hkr_lib_json_writer=>get_instance( ).

    " render context definition (serialize)
    me->build_json(
        ir_node   = lr_ctx_def
        ir_writer = lr_writer
    ).

    " get JSON as (x)string
    rv_json = lr_writer->get_output( ).

  ENDMETHOD.


  "/
  " Return initialized context definition root node from JSON string
  "/
  METHOD zif_hkr_act_api_json_reader~get_object.

    CLEAR rr_object.

    " get native JSON reader
    DATA(lr_reader) = zcl_hkr_lib_json_reader=>get_instance( me->zif_hkr_act_api_json_reader~mv_json ).

    " parse JSON (de-serialize)
    rr_object = me->build_context( lr_reader ).

  ENDMETHOD.


  "/ Write the JSON representation for all properties of the context definition <ir_node>
  " to the JSOON writer <ir_writer>
  " Traverse down the context node hierarchy.
  " - Nodes represents an structure (no name).
  " - Leafs represents attributes with named type.
  "
  " Format:
  "
  "   'ContextDefinition': {
  "     'root': {
  "       'attributes': [
  "          ( 'name': '<name>', 'dataType': '<data_type>'; 'ann': '<annotation>' ),
  "          ...
  "          ( 'name': '<name>', 'dataType': '<data_type>'; 'ann': '<annotation>' )
  "        ],
  "        'nodes': [
  "          ( 'name': <name>, 'attributes': [...], 'nodes': [...] ),
  "          ...
  "          ( 'name': <name>, 'attributes': [...], 'nodes': [...] )
  "        ]
  "      }
  "   }
  "
  "/
  METHOD build_json.
    "//TODO
    ir_writer->open_object( ).                      "// required for stand alone JSON format
    ir_writer->open_object( 'ContextDefinition' ).

    " Add element for root node
    me->build_json_for_node( ir_node = ir_node ir_writer = ir_writer ).

    ir_writer->close_element( ).
    ir_writer->close_element( ).
  ENDMETHOD.


  "/
  " Build JSON object for node <ir_node>
  "/
  METHOD build_json_for_node.

    "// new object with node name
    ir_writer->open_object( CONV #( ir_node->mv_name ) ).

    IF NOT ir_node->mt_attribues IS INITIAL.
      "// list of attributes
      ir_writer->open_array( 'attributes' ).
      LOOP AT ir_node->mt_attribues REFERENCE INTO DATA(lr_attribute).
        ir_writer->open_object( ).
        me->build_json_for_attribute( ir_attribute = lr_attribute ir_writer = ir_writer ).
        ir_writer->close_element( ).
      ENDLOOP.
      ir_writer->close_element( ).
    ENDIF.

    IF NOT ir_node->mt_nodes IS INITIAL.
      "// list of nodes
      ir_writer->open_array( 'nodes' ).
      LOOP AT ir_node->mt_nodes INTO DATA(lr_node).
        ir_writer->open_object( ).
        me->build_json_for_node( ir_node = lr_node ir_writer = ir_writer ).
        ir_writer->close_element( ).
      ENDLOOP.
      ir_writer->close_element( ).
    ENDIF.

    ir_writer->close_element( ).

  ENDMETHOD.


  "/
  " Add JSON properties for attribute <ir_attribute>
  " format:
  "   'name': <name>,
  "   'dataType': <data_type>
  "   'ann': '<annotation>'
  "/
  METHOD build_json_for_attribute.
    " name
    ir_writer->open_property( iv_name = 'name' iv_value = CONV #( ir_attribute->name ) ).
    ir_writer->close_element( ).
    " data type
    ir_writer->open_property( iv_name = 'dataType' iv_value = CONV #( ir_attribute->dtype ) ).
    ir_writer->close_element( ).
    IF NOT ir_attribute->annotation IS INITIAL.
      " annotation
      ir_writer->open_property( iv_name = 'ann' iv_value = ir_attribute->annotation ).
      ir_writer->close_element( ).
    ENDIF.
  ENDMETHOD.


  "/
  " Traverse (iterate) the JSON structure and return root node of context definition.
  " The context is build in top-down order, starting from the root node with its attributes
  " followed by the nodes in the node list.
  "/
  METHOD build_context.
    CLEAR rr_node.

    " try to read the expected prefix node: { 'ContextDefinition': { ... } }
    ir_reader->next_object( ).  " '{'
    ir_reader->next_object( 'ContextDefinition' ).

    " build root node
    rr_node = me->build_context_root_node( ir_reader ).

    ir_reader->close_object( ).
    ir_reader->close_object( ).

    IF rr_node IS INITIAL.
      "// error: throw exception
      "***TODO
    ENDIF.

  ENDMETHOD.


  "/
  " Build root node of context definition from JSON structure.
  " Expect JSON object with named <root>.
  "/
  METHOD build_context_root_node.

    "// get context definition root node instance
    DATA(lr_ctx_root) = zcl_hkr_act_ctx_node=>create_root( ).

    "// read JSON object with name <root>
    DATA(lr_json_obj_root) = ir_reader->next_object( 'root' ).

    "// recursively read context next elements (attributes and nodes)
    me->build_context_node(
        ir_node              = lr_ctx_root
        ir_json_obj          = lr_json_obj_root
        ir_reader            = ir_reader
    ).

    rr_node = lr_ctx_root.
  ENDMETHOD.


  "/
  " Build a new context node object with attributes and child nodes.
  "/
  METHOD build_context_node.

    "// recursively read next elements until closing element for JSON object
    DO.
      DATA(lr_node) = ir_reader->next_node( ).

      IF ir_reader->is_json_type_array( ir_node = lr_node iv_name = 'attributes' ).
        "// build context node attributes
        me->read_context_node_attributes(
            ir_node   = ir_node
            ir_reader = ir_reader
        ).
      ENDIF.

      IF ir_reader->is_json_type_array( ir_node = lr_node iv_name = 'nodes' ).
        "// build context node attributes
        me->read_context_nodes(
            ir_node   = ir_node
            ir_reader = ir_reader
        ).
      ENDIF.

      IF ir_reader->is_close_object( lr_node ).
        "// assume: close current JSON object
        EXIT.
      ENDIF.

      "// unexpected element type
      MESSAGE e040(zzz_hkr_act) WITH ir_reader->get_node_type_string( lr_node->type )
          INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).
    ENDDO.

  ENDMETHOD.


  "/
  " Read JSON objects for context nodes until <close element> token
  " for of JSON array.
  " New context attributes are added to the child node list of the current
  " context node <ir_node>
  "/
  METHOD read_context_nodes.

    DO.
      "// recursively read next object until closing element for JSON array
      DATA(lr_node) = ir_reader->next_node( ).

      IF ir_reader->is_json_type_object( lr_node ).
        "// build child node: create new instance
        DATA(lr_json_obj) = CAST if_sxml_open_element( lr_node ).

        DATA(lr_child_ctx_node) = ir_node->add_child(
            iv_name       = CONV #( ir_reader->get_element_name( lr_json_obj ) )
*            iv_annotation =
        ).

        "// go down to child context node (recursion)
        me->build_context_node(
            ir_node              = ir_node
            ir_json_obj          = lr_json_obj
            ir_reader            = ir_reader
        ).
      ENDIF.

      IF ir_reader->is_close_array( lr_node ).
        "// assume: close current JSON array
        EXIT.
      ENDIF.

      "// unexpected element type
      MESSAGE e040(zzz_hkr_act) WITH ir_reader->get_node_type_string( lr_node->type )
          INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).

    ENDDO.

  ENDMETHOD.


  "/
  " Read JSON objects for context node attributes until <close element> token
  " for of JSON array.
  " New context attributes are added to the attributes list of the current
  " context node <ir_node>
  "/
  METHOD read_context_node_attributes.

    DO.
      "// recursively read next JSON object for a context node attribute until
      "// closing element for JSON object
      DATA(lr_node) = ir_reader->next_node( ).

      IF ir_reader->is_json_type_object( lr_node ).

        "// build context attribute: create new instance
        DATA(lr_json_obj) = CAST if_sxml_open_element( lr_node ).

        "// build context property
        DATA(ls_attr) = me->build_context_node_attribute(
            ir_json_obj = lr_json_obj
            ir_reader   = ir_reader
        ).

        "// add to current context node
        ir_node->add_attribute(
            iv_name       = ls_attr-name
            iv_type       = ls_attr-dtype
            iv_annotation = ls_attr-annotation
        ).

      ENDIF.

      IF ir_reader->is_close_array( lr_node ).
        "// assume: close current JSON array
        EXIT.
      ENDIF.

      "// unexpected element type
      MESSAGE e040(zzz_hkr_act) WITH ir_reader->get_node_type_string( lr_node->type )
          INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).

    ENDDO.

  ENDMETHOD.


  "/
  " Read two JSON properties nodes with value for context attribute name and data type.
  " Exit when a <close element> token for a JSON property is read.
  "/
  METHOD build_context_node_attribute.

    CLEAR rs_attribute.

    DATA lv_read TYPE x.
    FIELD-SYMBOLS <lv_property> TYPE any.

    DO.

      "// iteratively read next JSON property / values for a context node attribute
      "// until closing element for JSON object
      DATA(lr_node) = ir_reader->next_node( ).

      IF ir_reader->is_json_type_property( lr_node ).

        DATA(lr_json_obj) = CAST if_sxml_open_element( lr_node ).
        "// check for JSON property name and assign corresponding field in result structure

        CASE ir_reader->get_element_name( lr_json_obj ).
          WHEN 'name'.
            ASSIGN COMPONENT 'NAME' OF STRUCTURE rs_attribute TO <lv_property>.

          WHEN 'dataType'.
            ASSIGN COMPONENT 'DTYPE' OF STRUCTURE rs_attribute TO <lv_property>.

          WHEN 'annotation'.
            ASSIGN COMPONENT 'ANNOTATION' OF STRUCTURE rs_attribute TO <lv_property>.

          WHEN OTHERS.
            "// unknown JSON property: ignore
        ENDCASE.

      ENDIF.


      IF ir_reader->is_json_property_value( lr_node ).
        "// get value for current JSON property
        <lv_property> = CAST if_sxml_value_node( lr_node )->get_value( ).
      ENDIF.


      IF ir_reader->is_close_object( lr_node ).
        "// assume: close current JSON array
        EXIT.
      ENDIF.


      "// unexpected element type
      MESSAGE e040(zzz_hkr_act) WITH ir_reader->get_node_type_string( lr_node->type )
          INTO DATA(lv_msg) ##NEEDED.
      zcx_hkr_act_base_exc=>raise_from_symsg( ).

    ENDDO.

    "//***TODO: check completeness of context attribute definition

  ENDMETHOD.


ENDCLASS.
