(module
    (func $add (param $a i32) (param $b i32) (result i32)
        local.get $a
        local.get $b
        i32.add
    )

    (export "demo:addcomponent/adder@0.1.0#add" (func $add))
)