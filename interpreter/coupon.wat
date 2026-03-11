(module
 (import "fixpoint" "is_equal" (func $is_equal (param externref) (param externref) (result i32)))
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
 (export "coupons" (table $coupons))
)
