(module
    (func $get (result i32)
        i32.const 123
    )

    (export "demo:importing/provider@0.1.0#get" (func $get))
)