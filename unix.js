//Provides: unix_gethostname
//Requires: caml_create_string
function unix_gethostname() { return caml_create_string(8) }
