(* The type of the public_key to be broadcasted out; we abstract this because
 * RSA operations cannot use typical int types
 *)
type public_key_type

(* The type of the private_key to be kept secret; we abstract this because
 * RSA operations cannot use typical int types
 *)
type private_key_type

(* The public key to broadcast; it should contain information relevant to
 * the modulus [m] where [m] = p*q where p and q are large primes. It should
 * also contain the unit [k] under mod (p-1)(q-1)
 *)
val public_key: public_key_type

(* The private key to be kept secret; it should contain information relevant to
 * the modulus [m] where [m] = p*q where p and q are large primes. It should
 * also contain the inverse of the unit [k] under mod (p-1)(q-1)
 *)
val private_key: private_key_type

(* [encrypt msg pub_key] is [msg'], where [msg'] is the result following
 * encryption.
 *)
val encrypt: string -> public_key_type -> string

(* [decrypt msg pub_key] is [msg'], where [msg'] is the readable, originally
 * unencrypted message
 *)
val decrypt: string -> private_key_type -> string

(* [generate_keys ()] is [pairing], where [pairing] is sufficient for the
 * user to send and receive messages in safety
 *)
val generate_keys: unit -> public_key_type * private_key_type


