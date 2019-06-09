class ZCX_HKR_ACT_BASE_EXC definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_HKR_ACT_BASE_EXC,
      msgid type symsgid value 'ZZZ_HKR_ACT',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_HKR_ACT_BASE_EXC .
  constants:
    begin of INVALID_PARAMETER,
      msgid type symsgid value 'ZZZ_HKR_ACT',
      msgno type symsgno value '050',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVALID_PARAMETER .
  constants:
    begin of GENERIC_EXC,
      msgid type symsgid value 'ZZZ_HKR_ACT',
      msgno type symsgno value '051',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of GENERIC_EXC .
  data MV_MSGV1 type SYST-MSGV1 .
  data MV_MSGV2 type SYST-MSGV1 .
  data MV_MSGV3 type SYST-MSGV1 .
  data MV_MSGV4 type SYST-MSGV4 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSGV1 type SYST-MSGV1 optional
      !MV_MSGV2 type SYST-MSGV1 optional
      !MV_MSGV3 type SYST-MSGV1 optional
      !MV_MSGV4 type SYST-MSGV4 optional .
  class-methods GET_TEXT_ID_FROM_SYMSG
    importing
      !IV_MSGID type SYST-MSGID default SY-MSGID
      !IV_MSGNO type SYST-MSGNO default SY-MSGNO
    returning
      value(RS_T100_KEY) type SCX_T100KEY .
  class-methods CREATE_FROM_SYMSG
    importing
      !IV_MSGID type SYST-MSGID default SY-MSGID
      !IV_MSGNO type SYST-MSGNO default SY-MSGNO
      !IV_MSGV1 type SYST-MSGV1 default SY-MSGV1
      !IV_MSGV2 type SYST-MSGV1 default SY-MSGV2
      !IV_MSGV3 type SYST-MSGV1 default SY-MSGV3
      !IV_MSGV4 type SYST-MSGV1 default SY-MSGV4
    returning
      value(RR_EXC) type ref to ZCX_HKR_ACT_BASE_EXC
    raising
      ZCX_HKR_ACT_BASE_EXC .
  class-methods RAISE_FROM_SYMSG
    importing
      !IV_MSGID type SYST-MSGID default SY-MSGID
      !IV_MSGNO type SYST-MSGNO default SY-MSGNO
      !IV_MSGV1 type SYST-MSGV1 default SY-MSGV1
      !IV_MSGV2 type SYST-MSGV1 default SY-MSGV2
      !IV_MSGV3 type SYST-MSGV1 default SY-MSGV3
      !IV_MSGV4 type SYST-MSGV1 default SY-MSGV4
    raising
      ZCX_HKR_ACT_BASE_EXC .
  class-methods RAISE_FROM_BAPIRETTAB
    importing
      !IT_RETURN type BAPIRETTAB
    raising
      ZCX_HKR_ACT_BASE_EXC .
  class-methods RAISE_FROM_BAPIRET
    importing
      !IS_RETURN type BAPIRET2
    raising
      ZCX_HKR_ACT_BASE_EXC .
protected section.
private section.
ENDCLASS.



CLASS ZCX_HKR_ACT_BASE_EXC IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_HKR_ACT_BASE_EXC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


method CREATE_FROM_SYMSG.

    DATA lv_msgv     TYPE symsgv.
    DATA lv_param    TYPE symsgv.

    CLEAR rr_exc.

*-- Initialize message key
    DATA(ls_t100_key) = get_text_id_from_symsg( ).

    IF iv_msgid IS INITIAL.
*     parameter invalid: throw parameter exception
      CONCATENATE iv_msgid iv_msgno iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
        INTO lv_msgv SEPARATED BY ':'.
      lv_param = 'IV_MSGID'.
      CREATE OBJECT rr_exc
        EXPORTING
          textid   = invalid_parameter
          mv_msgv1 = lv_param
          mv_msgv2 = lv_msgv.
    ELSE.
*     create instance for imported system message
      CREATE OBJECT rr_exc
        EXPORTING
          textid   = ls_t100_key
          mv_msgv1 = iv_msgv1
          mv_msgv2 = iv_msgv2
          mv_msgv3 = iv_msgv3
          mv_msgv4 = iv_msgv4.
    ENDIF.

  endmethod.


METHOD get_text_id_from_symsg.
    rs_t100_key       = generic_exc.
    rs_t100_key-msgid = iv_msgid.
    rs_t100_key-msgno = iv_msgno.
  endmethod.


method RAISE_FROM_BAPIRET.

    raise_from_symsg(
      EXPORTING
        iv_msgid                   = is_return-id
        iv_msgno                   = is_return-number
        iv_msgv1                   = is_return-message_v1
        iv_msgv2                   = is_return-message_v2
        iv_msgv3                   = is_return-message_v3
        iv_msgv4                   = is_return-message_v4
   ).

  endmethod.


METHOD raise_from_bapirettab.
  DATA(lr_msg) = zcl_hkr_msg_hlp=>get_bapi_msg_hdl( ).
  raise_from_bapiret( lr_msg->get_most_severe_msg( it_return ) ).
ENDMETHOD.


method RAISE_FROM_SYMSG.
    DATA(ls_text_id) = get_text_id_from_symsg( ).

    RAISE EXCEPTION TYPE zcx_hkr_act_base_exc
      EXPORTING
        textid   = ls_text_id
        mv_msgv1 = iv_msgv1
        mv_msgv2 = iv_msgv2
        mv_msgv3 = iv_msgv3
        mv_msgv4 = iv_msgv4.
  endmethod.
ENDCLASS.
