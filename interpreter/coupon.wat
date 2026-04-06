(module
 (import "fixpoint" "is_equal" (func $is_equal (param externref) (param externref) (result i32)))
 (import "fixpoint" "is_storage_coupon" (func $is_storage_coupon (param externref) (result i32)))
 (import "fixpoint" "is_force_coupon" (func $is_force_coupon (param externref) (result i32)))
 (import "fixpoint" "is_eq_coupon" (func $is_eq_coupon (param externref) (result i32)))
 (import "fixpoint" "create_thunk" (func $create_thunk (param externref) (result externref)))
 (import "fixpoint" "create_encode" (func $create_encode (param externref) (result externref)))
 (import "fixpoint" "get_coupon_lhs" (func $get_coupon_lhs (param externref) (result externref)))
 (import "fixpoint" "get_coupon_rhs" (func $get_coupon_rhs (param externref) (result externref)))
 (import "fixpoint" "create_eq_coupon" (func $create_eq_coupon (param externref) (param externref) (result externref)))
 (import "fixpoint" "get_tree_size" (func $get_tree_size (param externref) (result i32)))
 (import "fixpoint" "get_tree_data" (func $get_tree_data (param externref) (param i32) (result externref)))
 (table $coupons 0 externref)
 (type $make_coupon_t (func (param externref externref) (result externref)))
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
 (func $make_storage_coupon (export "make_storage_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $c externref)
    (local.set $c (table.get $coupons (i32.const 0)))
    (call $is_storage_coupon (local.get $c))
    (if (result externref)
      (then
        (call $is_equal (local.get $lhs) (call $get_coupon_lhs (local.get $c)))
        (if (result externref)
          (then
            (call $is_equal (local.get $rhs) (call $get_coupon_rhs (local.get $c)))
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
          )
        )
      )
      (else
        unreachable
      )))
 (func $make_tree_coupon (export "make_tree_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $c externref) (local $size i32) (local $i i32)
   (local.set $size (table.size $coupons))
   ;; Check that all coupons are eq coupons
   (local.set $i (i32.const 0))
   (block $exit
     (loop $loop
       (local.get $i)
       (local.get $size)
       i32.ge_s
       br_if $exit

       (call $is_eq_coupon (table.get $coupons (local.get $i)))
       (if
         (then nop)
         (else unreachable)
       )

       (local.set $i (i32.add (local.get $i) (i32.const 1)))
       br $loop
     )
   )

   ;; Check tree size of lhs and rhs
   (i32.eq (call $get_tree_size (local.get $lhs)) (local.get $size))
   (if (result externref)
     (then
       (i32.eq (call $get_tree_size (local.get $rhs)) (local.get $size))
       (if (result externref)
         (then
           ;; Check that each coupon corresponds to one pair of tree entries
           (local.set $i (i32.const 0))
           (block $exit
             (loop $loop
               (local.get $i)
               (local.get $size)
               i32.ge_s
               br_if $exit

               (local.set $c (table.get $coupons (local.get $i)))
               (call $is_equal (call $get_tree_data (local.get $lhs) (local.get $i)) (call $get_coupon_lhs (local.get $c)))
               (if
                 (then
                   (call $is_equal (call $get_tree_data (local.get $rhs) (local.get $i)) (call $get_coupon_rhs (local.get $c)))
                   (if
                     (then nop)
                     (else unreachable)
                   )
                 )
                 (else
                   unreachable
                 )
               )

               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               br $loop
             )
           )
           (call $create_eq_coupon (local.get $lhs) (local.get $rhs))
         )
         (else
           unreachable
         )
       )
     )
     (else
       unreachable
     )
   ))
 (func $make_thunk_coupon (export "make_thunk_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $f1 externref) (local $f2 externref) (local $e externref)
    (local.set $f1 (table.get $coupons (i32.const 0)))
    (local.set $f2 (table.get $coupons (i32.const 1)))
    (local.set $e (table.get $coupons (i32.const 2)))
    (call $is_force_coupon (local.get $f1))
    (if (result externref)
      (then
        (call $is_force_coupon (local.get $f2))
        (if (result externref)
          (then
            (call $is_eq_coupon (local.get $e))
            (if (result externref)
              (then
                (call $is_equal (call $get_coupon_rhs (local.get $f1)) (call $get_coupon_lhs (local.get $e)))
                (if (result externref)
                  (then
                    (call $is_equal (call $get_coupon_rhs (local.get $f2)) (call $get_coupon_rhs (local.get $e)))
                    (if (result externref)
                      (then
                        (call $is_equal (call $get_coupon_lhs (local.get $f1)) (local.get $lhs))
                        (if (result externref)
                        (then
                          (call $is_equal (call $get_coupon_lhs (local.get $f2)) (local.get $rhs))
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
                        )
                      )
                    )
                    (else
                      unreachable
                    )
                  )
                )
                (else
                  unreachable
                )
              )
            )
            (else
              unreachable
            )
          )
        )
        (else
          unreachable
        )
      )
    )
    (else
      unreachable
    )
  ))
 (func $make_thunktree_coupon (export "make_thunktree_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $e externref)
   (local.set $e (table.get $coupons (i32.const 0)))
   (call $is_eq_coupon (local.get $e))
   (if (result externref)
     (then
       (local.get $lhs)
       (call $create_thunk (call $get_coupon_lhs (local.get $e)))
       (call $is_equal)
       (if (result externref)
         (then
           (local.get $rhs)
           (call $create_thunk (call $get_coupon_rhs (local.get $e)))
           (call $is_equal)
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
         )
       )
     )
     (else
       unreachable
     )
  ))
 (func $make_thunkforce_coupon (export "make_thunkforce_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $f1 externref) (local $f2 externref) (local $e externref)
    (local.set $e (table.get $coupons (i32.const 0)))
    (local.set $f1 (table.get $coupons (i32.const 1)))
    (local.set $f2 (table.get $coupons (i32.const 2)))
    (call $is_eq_coupon (local.get $e))
    (if (result externref)
      (then
        (call $is_force_coupon (local.get $f1))
        (if (result externref)
          (then
            (call $is_force_coupon (local.get $f2))
            (if (result externref)
              (then
                (call $is_equal (call $get_coupon_lhs (local.get $f1)) (call $get_coupon_lhs (local.get $e)))
                (if (result externref)
                  (then
                    (call $is_equal (call $get_coupon_lhs (local.get $f2)) (call $get_coupon_rhs (local.get $e)))
                    (if (result externref)
                      (then
                        (call $is_equal (call $get_coupon_rhs (local.get $f1)) (local.get $lhs))
                        (if (result externref)
                          (then
                            (call $is_equal (call $get_coupon_rhs (local.get $f2)) (local.get $rhs))
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
                          )
                        )
                      )
                      (else
                        unreachable
                      )
                    )
                  )
                  (else
                    unreachable
                  )
                )
              )
              (else
                unreachable
              )
            )
          )
          (else
            unreachable
          )
        )
      )
      (else
        unreachable
      )
    ))
 (func $make_encode_coupon (export "make_encode_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $e externref)
   (local.set $e (table.get $coupons (i32.const 0)))
   (call $is_eq_coupon (local.get $e))
   (if (result externref)
     (then
       (call $is_equal (call $create_encode (call $get_coupon_lhs (local.get $e))) (local.get $lhs))
       (if (result externref)
         (then
           (call $is_equal (call $create_encode (call $get_coupon_rhs (local.get $e))) (local.get $rhs))
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
         )
       )
     )
     (else
       unreachable
     )
  ))
 (func $make_sym_coupon (export "make_sym_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $e externref)
   (local.set $e (table.get $coupons (i32.const 0)))
   (call $is_eq_coupon (local.get $e))
   (if (result externref)
     (then
       (call $is_equal (call $get_coupon_rhs (local.get $e)) (local.get $lhs))
       (if (result externref)
         (then
           (call $is_equal (call $get_coupon_lhs (local.get $e)) (local.get $rhs))
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
         )
       )
     )
     (else
       unreachable
     )
   ))
 (func $make_trans_coupon (export "make_trans_coupon") (param $lhs externref) (param $rhs externref) (result externref) (local $e1 externref) (local $e2 externref)
   (local.set $e1 (table.get $coupons (i32.const 0)))
   (local.set $e2 (table.get $coupons (i32.const 1)))
   (call $is_eq_coupon (local.get $e1))
   (if (result externref)
     (then
       (call $is_eq_coupon (local.get $e2))
       (if (result externref)
         (then
           (call $is_equal (call $get_coupon_rhs (local.get $e1)) (call $get_coupon_lhs (local.get $e2)))
           (if (result externref)
             (then
               (call $is_equal (local.get $lhs) (call $get_coupon_lhs (local.get $e1)))
               (if (result externref)
                 (then
                   (call $is_equal (local.get $rhs) (call $get_coupon_rhs (local.get $e2)))
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
                 )
               )
             )
             (else
               unreachable
             )
           )
         )
         (else
           unreachable
         )
       )
     )
     (else
       unreachable
     )
  ))
 (func $make_self_coupon (export "make_self_coupon") (param $lhs externref) (param $rhs externref) (result externref)
    (call $is_equal (local.get $lhs) (local.get $rhs))
    (if (result externref)
      (then
        (call $create_eq_coupon (local.get $lhs) (local.get $rhs))
      )
      (else
        unreachable
      )
    ))
 (table $dispatch_table funcref (elem (ref.func $make_storage_coupon)
                                      (ref.func $make_tree_coupon)
                                      (ref.func $make_thunk_coupon)
                                      (ref.func $make_thunktree_coupon)
                                      (ref.func $make_thunkforce_coupon)
                                      (ref.func $make_encode_coupon)
                                      (ref.func $make_sym_coupon)
                                      (ref.func $make_trans_coupon)
                                      (ref.func $make_self_coupon)))
 (func $make_coupon (export "make_coupon") (param $request i32) (param $lhs externref) (param $rhs externref) (result externref)
    local.get $request
    table.size $dispatch_table
    i32.lt_u
    if (result externref)
       local.get $lhs
       local.get $rhs
       local.get $request
       (call_indirect $dispatch_table (type $make_coupon_t))
    else
       unreachable
    end
    )
 (export "coupons" (table $coupons))
)
