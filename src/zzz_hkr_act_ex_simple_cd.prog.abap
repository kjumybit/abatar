*/ =============================================================
*  Define function test adapter(s)
* Simple Test Case
*/ =============================================================

*/ =============================================================
*  Define shared context
*/
types: begin of ts_ctx,
         repid type sy-repid,
       end of ts_ctx.


*/ =============================================================
*  Define appl. specific test case class.
*  Wraps the static data structure of the context.
*/
class zcl_hkr_tc_ex_simple definition
  inheriting from zcl_hkr_act_tc_def_base.

  public section.
    data:
      ms_ctx type ts_ctx.      " the "static" context

endclass.



* Dummy
class zcl_hkr_fut_ex_dummy_success definition.
  public section.
    interfaces zif_hkr_act_run_adp.
    data:
       mr_ctx_data type ref to ts_ctx.      " the "static" context
endclass.
