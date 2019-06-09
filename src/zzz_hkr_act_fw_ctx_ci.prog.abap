*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementation for context handling
*  =====================================================================




*---------------------------------------------------------------
CLASS zcl_hkr_act_ctx_tc IMPLEMENTATION.

 method get_string_property.

    clear rv_value.
    me->get_typed_property(
      EXPORTING  iv_path   = iv_path
                 iv_dtype  = 'STRING'
      IMPORTING  ev_value  = rv_value
    ).

  endmethod.


  method zif_hkr_act_ctx_tc~get_typed_property.
    clear ev_value.

    " get value as reference
    data(lr_value) = me->get_node(
        ir_parent_node = me->mr_data
        iv_path        = iv_path
    ).

    " cast to given type <IV_DTYPE>
    assign lr_value->* to FIELD-SYMBOL(<lv_value>) CASTING type (iv_dtype).
    if sy-subrc <> 0.
        " internal error: type conflict
        "***TODO: exception
    endif.

    " return value
    ev_value = <lv_value>.

  endmethod.

  method zif_hkr_act_ctx_tc~get_property.
    clear rr_value.
    rr_value = me->get_node(
        ir_parent_node = me->mr_data
        iv_path        = iv_path
    ).

  endmethod.


  method zif_hkr_act_ctx_tc~set_property.

    if not ir_value is bound and not iv_value is SUPPLIED.
      " internal error
      "***TODO raise dynamic parameter exception
      return.
    endif.

    " get data reference
    data(lr_ctx_val) = me->get_node(
        ir_parent_node = me->mr_data
        iv_path        = iv_path
    ).

    " assign field symbols for value assignment
    assign lr_ctx_val->* to field-symbol(<lv_ctx_value>).
    if not <lv_ctx_value> is assigned.
        " internal error
        "***TODO raise exception
        return.
    endif.

    if ir_value is bound.
      " use data reference
        assign ir_value->* to field-symbol(<lv_new_value>).
        if not <lv_new_value> is assigned.
            " internal error
            "***TODO raise runtime exception
            return.
        endif.
    else.
        " use data
        assign iv_value to <lv_new_value>.
    endif.

    " value assignment
    <lv_ctx_value> = conv #( <lv_new_value> ).

  endmethod.


  "/
  "  Get attribute node in context structure for path.
  "  Iterate all attributes and traverse the context down and return the
  "  last attribute node.
  "/
  method get_node.

    clear rr_node.
    data(lv_path) = to_upper( iv_path ).

    "***TODO: check for absolute or relative path
    lv_path = switch #( iv_path+0(1) when '/' then iv_path+1 else iv_path ).

    " get list of attribute names
    split lv_path at '/' into table data(lt_attributes).

    " initialize start context node with parent node (root node of context)
    data(lr_node) = ir_parent_node.

    " iterate all attributes and traverse the context down
    loop at lt_attributes reference into data(lr_attribute).

      if not lr_node is bound.
        exit.
      endif.

      lr_node = me->get_node_element( ir_node    = lr_node
                                      iv_atr_idx = lr_attribute->*
      ).

    endloop.

    " check result
    if not lr_node is bound.
      " internal error
      "***TODO throw exception
    endif.

    rr_node = lr_node.

  endmethod.


   "/
   "  Return reference to a node attribute.
   "  ***TODO: table index are currently not supported
   "/
   method get_node_element.

     clear rr_node.
     field-symbols <ls_node> type data.
     field-symbols <lv_attribute> type data.

     assign ir_node->* to <ls_node>.
     if <ls_node> is assigned.
       assign component iv_atr_idx of structure <ls_node> to <lv_attribute>.
     endif.

     if not <lv_attribute> is assigned.
       " internal error
       "***TODO raise exception
     endif.

     rr_node = ref #( <lv_attribute> ).

   endmethod.


ENDCLASS.


*---------------------------------------------------------------
class zcl_hkr_act_ctx_node implementation.

  method constructor.
    me->mv_name = iv_name.
    me->mv_annotation = iv_annotation.
    clear me->mr_parent.
    me->mr_parent = cond #( when ir_parent is bound then ir_parent ).
  endmethod.


  method create_root.
    rr_node = new zcl_hkr_act_ctx_node( 'root' ).
    rr_node->mr_root = rr_node.
  endmethod.


  method add_child.
    data(lr_child) = new zcl_hkr_act_ctx_node(
      iv_name   = iv_name
      iv_annotation = iv_annotation
      ir_parent = me
    ).
    lr_child->mr_root = me->mr_root.
    append lr_child to me->mt_nodes.
    rr_node = lr_child.
  endmethod.


  method add_sibling.
    data(lr_sibling) = new zcl_hkr_act_ctx_node(
      iv_name   = iv_name
      iv_annotation = iv_annotation
      ir_parent = me->mr_parent
    ).
    lr_sibling->mr_root = me->mr_root.
    append lr_sibling to me->mr_parent->mt_nodes.
    rr_node = lr_sibling.
  endmethod.


  method get_root.
    rr_node = me->mr_root.
  endmethod.


  method get_parent.
    rr_node = me->mr_parent.
  endmethod.


  method add_attribute.
    append value #( name = iv_name dtype = iv_type annotation = iv_annotation ) to me->mt_attribues.
    rr_node = me.
  endmethod.


  method get_data.

    if not me->mr_data is bound.
      new zcl_hkr_act_ctx_builder( me )->build( ).
    endif.

    rr_data = me->mr_data.
  endmethod.


  method get_type_handle.
    rr_handle = me->mr_struc_desc.
  endmethod.

endclass.


*---------------------------------------------------------------
CLASS zcl_hkr_act_ctx_builder IMPLEMENTATION.

  method constructor.
    me->mr_ctx_root = ir_ctx_node.
  endmethod.

  method build.

    " collect data structure definition
    try.
        me->mr_ctx_root->mr_struc_desc = me->build_structure_rtti( me->mr_ctx_root ).
      catch cx_sy_struct_creation into data(lr_exc).
        "***TODO
    endtry.

    " build data structure
    create data me->mr_ctx_root->mr_data type handle me->mr_ctx_root->mr_struc_desc.

  endmethod.


  "/
  " Traverse the node hierarchy in deep first order and create ABAP data types using RTTI
  " a node represents a structure which can have attributes (known data types) and child nodes
  "/
  method build_structure_rtti.

    clear rr_handle.

    data lt_comp type abap_component_tab.
    data lr_type type ref to cl_abap_datadescr.

    " create RTTI structure definition for child nodes of current node
    loop at ir_ctx_node->mt_nodes into data(lr_node).
      lr_type = me->build_structure_rtti( lr_node ).
      append value #( name = lr_node->mv_name type = lr_type ) to lt_comp.
    endloop.

    " create data structure for own node attributes
    loop at ir_ctx_node->mt_attribues reference into data(lr_attribue).
      lr_type ?= cl_abap_typedescr=>describe_by_name( lr_attribue->dtype ).
      append value #( name = lr_attribue->name type = lr_type ) to lt_comp.
    endloop.

    " collect and return structure definition
    rr_handle = cl_abap_structdescr=>get( lt_comp ).

  endmethod.



ENDCLASS.
