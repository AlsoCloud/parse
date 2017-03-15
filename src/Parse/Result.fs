module Result

type Result<'a,'b> =
    | Err of 'a
    | Ok of 'b