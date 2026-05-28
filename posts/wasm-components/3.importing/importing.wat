(module
    (import "demo:importing/provider@0.1.0" "get" (func $get (result i32)))

    (func $add (param $other i32) (result i32)
        call $get
        local.get $other
        i32.add
    )
    (func $mul (param $other i32) (result i32)
        call $get
        local.get $other
        i32.mul
    )

    (export "demo:importing/adder@0.1.0#add" (func $add))
    (export "demo:importing/adder@0.1.0#mul" (func $mul))
)