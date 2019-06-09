*/ =====================================================================
*  ABAP Continuous Testing
*
*  @description: Automatic Functional Test Framework
*  @version: 0.0.4
*  @author : Hagen Kröber
*
*  ---------------------------------------------------------------------
*  Implementations for context binding
*/ =====================================================================



class zcl_hkr_act_ctx_bdg_update  implementation.

  method update_binding.

    " update context for each binding info for mode <iv_mode>
    loop at it_binding_info reference into data(lr_binding_info) where mode cs iv_mode.

      case iv_mode.

        when zif_hkr_act_fw_c=>gc_bdg_mode_in.

            " pass value from context to adapter
            data(lr_value) = ir_ctx->get_property( lr_binding_info->path ).
            ir_adp_ctx->set_imp_par_value(
                iv_par   = lr_binding_info->attr
                ir_value = lr_value
            ).

        when zif_hkr_act_fw_c=>gc_bdg_mode_out.
            " pass value from adapter to context property

            lr_value = ir_adp_ctx->get_exp_par_value( lr_binding_info->attr ).
            ir_ctx->set_property(
                iv_path          =  lr_binding_info->path
                ir_value         = lr_value
            ).

        when others.
            " internal error
            "***TODO runtime exception
      ENDCASE.

    endloop.

  endmethod.


endclass.
