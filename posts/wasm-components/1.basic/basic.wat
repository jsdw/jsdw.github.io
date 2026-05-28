(module
    ;; require that a function called "log" in the "console" namespace
    ;; is provided, which takes a single i32 and does not return 
    ;; anything.
    (import "console" "log" (func $log (param i32)))

    ;; Define a function to add two i32s
    (func $add (param $a i32) (param $b i32) (result i32)
        local.get $a ;; put $a onto the stack
        local.get $b ;; put $b onto the stack

        ;; take the top two items off the stack ($a and $b), and
        ;; add them together, putting the result back onto the stack
        i32.add      
    )

    ;; Define a function to add two i32s and log the result
    (func $addAndLog (param $a i32) (param $b i32)
        local.get $a ;; put $a onto the stack
        local.get $b ;; put $b onto the stack

        ;; $add takes two arguments from the top of the stack and
        ;; places one result back onto the stack.
        call $add

        ;; $log takes one argument from the top of the stack
        ;; (the result of $add being called) and hands it to
        ;; our imported "console" "log" function.
        call $log
    )

    ;; export (make available to the host) our $addAndLog function
    ;; under the name "addAndLog":
    (export "addAndLog" (func $addAndLog))
)