*/ =============================================================
*  Define function test adapter(s)
*  Simple Test Case
*/ =============================================================




* Dummy
class zcl_hkr_fut_ex_dummy_success definition.
  public section.
    interfaces zif_hkr_act_run_adp_fut.

    "// parameters
    data:
      mv_i_hello type string,
      mv_i_world type string,
      mv_e_greeting type string.


endclass.
