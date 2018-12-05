#include <caml/mlvalues.h>

CAMLprim value getint(value v) {
  float f;
  f = Double_val(v);
  int i = *(int *)(&f);
  return Val_int(i);
}
