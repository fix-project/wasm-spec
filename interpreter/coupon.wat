(module
 (import "fixpoint" "is_equal" (func $is_equal (param externref) (param externref) (result i32)))
 (import "fixpoint" "is_storage_coupon_api" (func $is_storage_coupon_api (param externref) (result i32)))
 (import "fixpoint" "get_coupon_lhs" (func $get_coupon_lhs (param externref) (result externref)))
 (import "fixpoint" "get_coupon_rhs" (func $get_coupon_rhs (param externref) (result externref)))
 (import "fixpoint" "create_eq_coupon" (func $create_eq_coupon (param externref) (param externref) (result externref)))
 (table $coupons 0 externref)
 (func (export "ret") (param $lhs externref) (param $rhs externref) (result externref)
    (call $is_equal (local.get 0) (local.get 1))
    (if (result externref)
      (then
        local.get 0
      )
      (else
        local.get 1
      )
    ))
 (func (export "make_blob_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $c externref)
    (local.set $c (table.get $coupons (i32.const 0)))
    (call $is_storage_coupon_api (local.get $c))
    (if (result externref)
      (then
        (call $is_equal (local.get $lhs) (call $get_coupon_lhs (local.get $c)))
        (call $is_equal (local.get $rhs) (call $get_coupon_rhs (local.get $c)))
        i32.and
        (if (result externref)
          (then
            (call $create_eq_coupon (local.get $lhs) (local.get $rhs))
          )
          (else
            unreachable
          )
        )
      )
      (else
        unreachable
      )))
 (export "coupons" (table $coupons))
)
