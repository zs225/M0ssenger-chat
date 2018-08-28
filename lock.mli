

type t

val create_lock: unit -> t

val enter_write: t -> unit

val leave_write: t -> unit

val enter_read: t -> unit

val leave_read: t -> unit